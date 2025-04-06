#pragma once
#include <assert.h>
#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>
#include "common.hpp"
#include "surakarta_event.hpp"
#include "tokenizer.hpp"

namespace Parser {

class ParseError : public CompilerError {
   public:
    ParseError(ScanContext& scanContext, const std::string& message)
        : CompilerError(scanContext, "parse error: " + message) {}
};

class ProductionRule;
using ProductionRuleList = std::vector<std::shared_ptr<ProductionRule>>;

class ProductionRule : public std::enable_shared_from_this<ProductionRule> {
   public:
    std::string name;
    std::vector<std::shared_ptr<Tokenizer::TokenDefinition>> symbols;
    int priority;  // lower priority value means higher priority
    std::function<void(std::vector<std::shared_ptr<Tokenizer::Token>>&)> reduceAction;

    ProductionRule(std::string name,
                   std::vector<std::shared_ptr<Tokenizer::TokenDefinition>> symbols,
                   int priority,
                   std::function<void(std::vector<std::shared_ptr<Tokenizer::Token>>&)> reduceAction)
        : name(name), symbols(symbols), priority(priority), reduceAction(reduceAction) {}
};

// Action type for the parsing table
enum class ActionType {
    SHIFT,
    REDUCE,
    ACCEPT,
    ERROR
};

struct Action {
    ActionType type;
    int value;  // state number for shift, rule number for reduce

    Action()
        : type(ActionType::ERROR), value(0) {}
    Action(ActionType type, int value)
        : type(type), value(value) {}
};

// LR Item represents a production rule with a dot position
class LRItem {
   public:
    std::shared_ptr<ProductionRule> rule;
    size_t dotPosition;
    std::shared_ptr<Tokenizer::TokenDefinition> lookahead;

    LRItem(std::shared_ptr<ProductionRule> rule, size_t dotPosition, std::shared_ptr<Tokenizer::TokenDefinition> lookahead)
        : rule(rule), dotPosition(dotPosition), lookahead(lookahead) {}

    bool isComplete() const {
        return dotPosition >= rule->symbols.size();
    }

    std::shared_ptr<Tokenizer::TokenDefinition> nextSymbol() const {
        if (dotPosition < rule->symbols.size())
            return rule->symbols[dotPosition];
        return nullptr;
    }

    bool operator==(const LRItem& other) const {
        return rule == other.rule &&
               dotPosition == other.dotPosition &&
               lookahead == other.lookahead;
    }
};

// LR State represents a set of LR items
class LRState {
   public:
    int stateId;
    std::vector<LRItem> items;
    std::map<std::shared_ptr<Tokenizer::TokenDefinition>, std::shared_ptr<LRState>> transitions;
    std::map<std::shared_ptr<Tokenizer::TokenDefinition>, Action> actions;
    std::map<std::string, int> gotoTable;

    bool operator==(const LRState& other) const {
        if (items.size() != other.items.size())
            return false;
        for (const auto& item : items) {
            bool found = false;
            for (const auto& otherItem : other.items) {
                if (item == otherItem) {
                    found = true;
                    break;
                }
            }
            if (!found)
                return false;
        }
        return true;
    }
};

class Parser {
   private:
    std::shared_ptr<ProductionRuleList> rules;
    std::vector<std::shared_ptr<LRState>> states;
    std::vector<std::shared_ptr<Tokenizer::Token>> symbolStack;
    std::vector<int> stateStack;
    ScanContext& scanContext;

    // Build the closure of a set of items
    std::vector<LRItem> closure(const std::vector<LRItem>& items) {
        std::vector<LRItem> result = items;
        bool changed;
        do {
            changed = false;
            std::vector<LRItem> newItems;
            for (const auto& item : result) {
                auto nextSymbol = item.nextSymbol();
                if (nextSymbol && nextSymbol->name[0] >= 'A' && nextSymbol->name[0] <= 'Z') {  // Non-terminal
                    for (const auto& rule : *rules) {
                        if (rule->name == nextSymbol->name) {
                            LRItem newItem(rule, 0, item.lookahead);
                            if (std::find(result.begin(), result.end(), newItem) == result.end() &&
                                std::find(newItems.begin(), newItems.end(), newItem) == newItems.end()) {
                                newItems.push_back(newItem);
                                changed = true;
                            }
                        }
                    }
                }
            }
            result.insert(result.end(), newItems.begin(), newItems.end());
        } while (changed);
        return result;
    }

    // Build the goto set for a state and symbol
    std::vector<LRItem> gotoSet(const std::vector<LRItem>& items,
                                std::shared_ptr<Tokenizer::TokenDefinition> symbol) {
        std::vector<LRItem> result;
        for (const auto& item : items) {
            if (!item.isComplete() && item.nextSymbol() == symbol) {
                result.push_back(LRItem(item.rule, item.dotPosition + 1, item.lookahead));
            }
        }
        return closure(result);
    }

    // Build the canonical collection of LR(1) states
    void buildStates() {
        std::cerr << "Building LR(1) states..." << std::endl;

        // Start with the initial state
        std::vector<LRItem> initialItems;
        for (const auto& rule : *rules) {
            if (rule->name == "CompUnit") {
                initialItems.push_back(LRItem(rule, 0, nullptr));
            }
        }
        auto initialState = std::make_shared<LRState>();
        initialState->stateId = 0;
        initialState->items = closure(initialItems);
        states.push_back(initialState);

        // Build the rest of the states
        bool changed;
        do {
            changed = false;
            for (size_t i = 0; i < states.size(); ++i) {
                auto state = states[i];
                // Try all possible symbols
                std::set<std::shared_ptr<Tokenizer::TokenDefinition>,
                         std::less<std::shared_ptr<Tokenizer::TokenDefinition>>>
                    symbols;
                for (const auto& item : state->items) {
                    if (auto next = item.nextSymbol()) {
                        symbols.insert(next);
                    }
                }
                for (const auto& symbol : symbols) {
                    auto newItems = gotoSet(state->items, symbol);
                    if (!newItems.empty()) {
                        auto newState = std::make_shared<LRState>();
                        newState->items = newItems;
                        // Check if this state already exists
                        auto it = std::find_if(states.begin(), states.end(),
                                               [&](const std::shared_ptr<LRState>& s) { return *s == *newState; });
                        if (it == states.end()) {
                            newState->stateId = states.size();
                            states.push_back(newState);
                            state->transitions[symbol] = newState;
                            changed = true;
                        } else {
                            state->transitions[symbol] = *it;
                        }
                    }
                }
            }
        } while (changed);

        std::cerr << "Built " << states.size() << " states" << std::endl;
    }

    // Build the action and goto tables
    void buildTables() {
        std::cerr << "Building parsing tables..." << std::endl;

        // For each state
        for (const auto& state : states) {
            // For each item in the state
            for (const auto& item : state->items) {
                if (item.isComplete()) {
                    // Reduce action
                    if (item.rule->name == "CompUnit" && !item.lookahead) {
                        state->actions[nullptr] = Action(ActionType::ACCEPT, 0);
                    } else {
                        // Find the rule number
                        auto ruleIt = std::find(rules->begin(), rules->end(), item.rule);
                        if (ruleIt != rules->end()) {
                            int ruleNumber = std::distance(rules->begin(), ruleIt);
                            if (item.lookahead) {
                                state->actions[item.lookahead] = Action(ActionType::REDUCE, ruleNumber);
                            } else {
                                // If no lookahead, reduce on EOF
                                state->actions[nullptr] = Action(ActionType::REDUCE, ruleNumber);
                            }
                        }
                    }
                } else {
                    // Shift action
                    auto nextSymbol = item.nextSymbol();
                    if (nextSymbol) {
                        auto nextState = state->transitions[nextSymbol];
                        if (nextState) {
                            // Only add shift action if there isn't already a reduce action
                            // or if the shift action has higher priority
                            auto existingAction = state->actions.find(nextSymbol);
                            if (existingAction == state->actions.end() ||
                                (existingAction->second.type == ActionType::REDUCE &&
                                 (*rules)[existingAction->second.value]->priority > item.rule->priority)) {
                                state->actions[nextSymbol] = Action(ActionType::SHIFT, nextState->stateId);
                            }
                        }
                    }
                }
            }

            // Build goto table for non-terminals
            for (const auto& transition : state->transitions) {
                if (transition.first->name[0] >= 'A' && transition.first->name[0] <= 'Z') {
                    state->gotoTable[transition.first->name] = transition.second->stateId;
                }
            }

            // Special handling for Block reduction
            if (state->stateId == 21) {  // State after RBRACE
                // Add reduce action for Block
                auto blockRule = std::find_if(rules->begin(), rules->end(),
                                              [](const auto& rule) { return rule->name == "Block"; });
                if (blockRule != rules->end()) {
                    int blockRuleNumber = std::distance(rules->begin(), blockRule);
                    state->actions[nullptr] = Action(ActionType::REDUCE, blockRuleNumber);
                }
            }

            // Debug output
            std::cerr << "State " << state->stateId << " actions:" << std::endl;
            for (const auto& action : state->actions) {
                std::cerr << "  " << (action.first ? action.first->name : "EOF") << " -> ";
                switch (action.second.type) {
                    case ActionType::SHIFT:
                        std::cerr << "shift " << action.second.value;
                        break;
                    case ActionType::REDUCE:
                        std::cerr << "reduce " << action.second.value;
                        break;
                    case ActionType::ACCEPT:
                        std::cerr << "accept";
                        break;
                    case ActionType::ERROR:
                        std::cerr << "error";
                        break;
                }
                std::cerr << std::endl;
            }
        }

        std::cerr << "Parsing tables built" << std::endl;
    }

    void shift(std::shared_ptr<Tokenizer::Token> token) {
        std::cerr << "Shifting token: " << token->definition->name << " -> " << token->matched << std::endl;

        auto currentState = states[stateStack.back()];

        // Find action for this token
        Action action;
        for (const auto& [symbol, act] : currentState->actions) {
            if (symbol && symbol->name == token->definition->name) {
                action = act;
                break;
            }
        }

        if (action.type != ActionType::SHIFT) {
            throw ParseError(scanContext, "Expected shift action");
        }

        symbolStack.push_back(token);
        stateStack.push_back(action.value);

        std::cerr << "Shifted to state " << action.value << std::endl;
    }

    void reduce(std::shared_ptr<ProductionRule> rule) {
        std::cerr << "Reducing by rule: " << rule->name << std::endl;

        // Pop the right-hand side
        std::vector<std::shared_ptr<Tokenizer::Token>> matchedTokens;
        for (size_t i = 0; i < rule->symbols.size(); ++i) {
            matchedTokens.insert(matchedTokens.begin(), symbolStack.back());
            symbolStack.pop_back();
            stateStack.pop_back();
        }

        // Execute reduce action
        rule->reduceAction(matchedTokens);

        // Push the left-hand side
        auto nonTerminal = std::make_shared<Tokenizer::Token>();
        nonTerminal->definition = std::make_shared<Tokenizer::TokenDefinition>(
            rule->name,  // name
            nullptr,     // matchers
            nullptr,     // begin
            nullptr,     // end
            0            // priority
        );
        nonTerminal->matched = rule->name;
        symbolStack.push_back(nonTerminal);

        // Goto
        auto currentState = states[stateStack.back()];

        // Find action for this non-terminal
        Action action;
        for (const auto& [symbol, act] : currentState->actions) {
            if (symbol && symbol->name == nonTerminal->definition->name) {
                action = act;
                break;
            }
        }

        if (action.type != ActionType::SHIFT) {
            throw ParseError(scanContext, "Expected shift action after reduce");
        }

        stateStack.push_back(action.value);

        std::cerr << "Reduced to state " << action.value << std::endl;
    }

   public:
    Parser(std::shared_ptr<ProductionRuleList> rules, ScanContext& scanContext)
        : rules(rules), scanContext(scanContext) {
        buildStates();
        buildTables();
    }

    void parse(std::shared_ptr<Tokenizer::TokenList> tokens) {
        std::cerr << "Starting parse with " << tokens->size() << " tokens:" << std::endl;
        for (const auto& token : *tokens) {
            std::cerr << "Token: " << token.definition->name << " -> " << token.matched << std::endl;
        }

        // Initialize stacks
        stateStack.clear();
        symbolStack.clear();
        stateStack.push_back(0);  // Start with state 0

        // Process each token
        size_t pos = 0;
        while (pos < tokens->size()) {
            // Skip whitespace tokens
            while (pos < tokens->size() && (*tokens)[pos].definition->name == "WHITESPACE") {
                pos++;
            }
            if (pos >= tokens->size())
                break;

            auto currentState = states[stateStack.back()];
            auto nextToken = std::make_shared<Tokenizer::Token>((*tokens)[pos]);

            std::cerr << "State " << currentState->stateId << ", ";
            std::cerr << "next token: " << nextToken->definition->name << " -> " << nextToken->matched;

            // Find action for this token
            Action action;
            for (const auto& [symbol, act] : currentState->actions) {
                if (symbol && symbol->name == nextToken->definition->name) {
                    action = act;
                    break;
                }
            }

            std::cerr << ", action: ";
            switch (action.type) {
                case ActionType::SHIFT:
                    std::cerr << "shift " << action.value << std::endl;
                    shift(nextToken);
                    pos++;
                    break;

                case ActionType::REDUCE:
                    std::cerr << "reduce by rule " << action.value << std::endl;
                    reduce((*rules)[action.value]);
                    break;

                case ActionType::ACCEPT:
                    std::cerr << "accept" << std::endl;
                    return;

                case ActionType::ERROR:
                    // Try to find a reduce action in the current state
                    bool foundReduce = false;
                    for (const auto& [symbol, act] : currentState->actions) {
                        if (act.type == ActionType::REDUCE) {
                            std::cerr << "reduce by rule " << act.value << std::endl;
                            reduce((*rules)[act.value]);
                            foundReduce = true;
                            break;
                        }
                    }
                    if (!foundReduce) {
                        std::cerr << "error" << std::endl;
                        throw ParseError(scanContext, "No valid action found");
                    }
                    break;
            }
        }

        // Handle final reductions
        while (true) {
            auto currentState = states[stateStack.back()];
            auto action = currentState->actions[nullptr];  // EOF action

            if (action.type == ActionType::ACCEPT) {
                std::cerr << "accept" << std::endl;
                return;
            } else if (action.type == ActionType::REDUCE) {
                std::cerr << "reduce by rule " << action.value << std::endl;
                reduce((*rules)[action.value]);
            } else {
                throw ParseError(scanContext, "Incomplete parse");
            }
        }
    }
};

// Define the grammar rules
std::shared_ptr<ProductionRuleList> createGrammarRules();

}  // namespace Parser