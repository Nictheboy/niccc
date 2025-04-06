#pragma once
#include <assert.h>
#include <algorithm>
#include <map>
#include <memory>
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

class ParseState;
using ParseStateList = std::vector<std::shared_ptr<ParseState>>;

class ParseState : public std::enable_shared_from_this<ParseState> {
   public:
    std::shared_ptr<ProductionRule> rule;
    size_t dotPosition;
    std::vector<std::shared_ptr<Tokenizer::Token>> matchedTokens;

    ParseState(std::shared_ptr<ProductionRule> rule, size_t dotPosition)
        : rule(rule), dotPosition(dotPosition) {}

    bool isComplete() const {
        return dotPosition >= rule->symbols.size();
    }

    std::shared_ptr<Tokenizer::TokenDefinition> nextSymbol() const {
        if (dotPosition < rule->symbols.size())
            return rule->symbols[dotPosition];
        return nullptr;
    }

    std::shared_ptr<ParseState> advance() const {
        auto newState = std::make_shared<ParseState>(rule, dotPosition + 1);
        newState->matchedTokens = matchedTokens;
        return newState;
    }
};

class ProductionRuleBuilder : public std::enable_shared_from_this<ProductionRuleBuilder> {
   private:
    std::string _name;
    std::vector<std::shared_ptr<Tokenizer::TokenDefinition>> _symbols;
    int _priority = 0;
    std::function<void(std::vector<std::shared_ptr<Tokenizer::Token>>&)> _reduceAction;

   public:
    std::shared_ptr<ProductionRuleBuilder> name(std::string name) {
        this->_name = name;
        return shared_from_this();
    }

    std::shared_ptr<ProductionRuleBuilder> priority(int priority) {
        this->_priority = priority;
        return shared_from_this();
    }

    std::shared_ptr<ProductionRuleBuilder> symbol(std::shared_ptr<Tokenizer::TokenDefinition> symbol) {
        _symbols.push_back(symbol);
        return shared_from_this();
    }

    std::shared_ptr<ProductionRuleBuilder> reduceAction(std::function<void(std::vector<std::shared_ptr<Tokenizer::Token>>&)> action) {
        this->_reduceAction = action;
        return shared_from_this();
    }

    std::shared_ptr<ProductionRule> build() {
        return std::make_shared<ProductionRule>(_name, _symbols, _priority, _reduceAction);
    }
};

class Parser {
   private:
    std::shared_ptr<ProductionRuleList> rules;
    std::shared_ptr<ParseStateList> states;
    std::vector<std::shared_ptr<Tokenizer::Token>> tokenStack;
    ScanContext& scanContext;

    void shift(std::shared_ptr<Tokenizer::Token> token) {
        tokenStack.push_back(token);
        auto newStates = std::make_shared<ParseStateList>();

        for (auto state : *states) {
            auto nextSymbol = state->nextSymbol();
            if (nextSymbol && nextSymbol == token->definition) {
                auto newState = state->advance();
                newState->matchedTokens.push_back(token);
                newStates->push_back(newState);
            }
        }

        states = newStates;
        reduce();
    }

    void reduce() {
        while (true) {
            auto completeStates = std::make_shared<ParseStateList>();
            for (auto state : *states) {
                if (state->isComplete()) {
                    completeStates->push_back(state);
                }
            }

            if (completeStates->empty()) {
                break;
            }

            auto cmp = [](const std::shared_ptr<ParseState>& a, const std::shared_ptr<ParseState>& b) {
                return a->rule->priority < b->rule->priority;
            };
            auto stateToReduce = *std::min_element(completeStates->begin(), completeStates->end(), cmp);

            // Remove the matched tokens from the stack
            for (size_t i = 0; i < stateToReduce->matchedTokens.size(); ++i) {
                tokenStack.pop_back();
            }

            // Execute reduce action
            stateToReduce->rule->reduceAction(stateToReduce->matchedTokens);

            // Reset states and reprocess remaining tokens
            resetStates();
            for (auto token : tokenStack) {
                shift(token);
            }
        }
    }

    void resetStates() {
        states = std::make_shared<ParseStateList>();
        for (auto rule : *rules) {
            states->push_back(std::make_shared<ParseState>(rule, 0));
        }
    }

   public:
    Parser(std::shared_ptr<ProductionRuleList> rules, ScanContext& scanContext)
        : rules(rules), scanContext(scanContext) {
        resetStates();
    }

    void parse(std::shared_ptr<Tokenizer::TokenList> tokens) {
        for (auto& token : *tokens) {
            shift(std::make_shared<Tokenizer::Token>(token));
        }

        if (states->empty() || !states->front()->isComplete()) {
            throw ParseError(scanContext, "incomplete parse");
        }
    }
};

// Define the grammar rules
std::shared_ptr<ProductionRuleList> createGrammarRules();

}  // namespace Parser