#pragma once
#include <assert.h>
#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <vector>
#include "ast.hpp"
#include "common.hpp"
#include "surakarta_event.hpp"
#include "tokenizer.hpp"

namespace Parser {

class ParseError : public CompilerError {
   public:
    // Store error details
    std::string errorMessage;

    // Constructor takes token, state ID, and stack info
    ParseError(ScanContext& context,
               const Tokenizer::Token& errorToken,
               int stateId,
               const std::string& stackInfo,
               const std::string& contextMessage)
        : CompilerError(context, "parse error")  // Initialize base correctly
    {
        // Construct the detailed message
        std::stringstream ss;
        ss << "parse error: " << contextMessage << " '"
           << errorToken.matched << "' (" << errorToken.definition->name << ")";
        if (errorToken.start_row > 0 && errorToken.start_col > 0) {  // Check if position is valid
            ss << " near line " << errorToken.start_row << ", column " << errorToken.start_col;
        }
        ss << " in state " << stateId << ".";
        ss << stackInfo;  // Append stack state string
        errorMessage = ss.str();
    }

    // Constructor for errors not tied to a specific token (e.g., internal errors)
    ParseError(ScanContext& context,
               const std::string& generalMessage,
               const std::string& stackInfo)
        : CompilerError(context, "parse error")  // Initialize base correctly
    {
        std::stringstream ss;
        ss << "parse error: " << generalMessage;
        ss << stackInfo;
        errorMessage = ss.str();
    }

    // Override what() to return the detailed message
    const char* what() const noexcept override {
        // CompilerError base class might store its own message,
        // but we return our more detailed one.
        return errorMessage.c_str();
    }
};

class ProductionRule;
using ProductionRuleList = std::vector<std::shared_ptr<ProductionRule>>;

class ProductionRule : public std::enable_shared_from_this<ProductionRule> {
   public:
    std::string name;
    std::vector<std::shared_ptr<Tokenizer::TokenDefinition>> symbols;
    int priority;  // lower priority value means higher priority

    ProductionRule(std::string name,
                   std::vector<std::shared_ptr<Tokenizer::TokenDefinition>> symbols,
                   int priority)
        : name(name), symbols(symbols), priority(priority) {}
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

    // Add operator< for sorting items within a state
    bool operator<(const LRItem& other) const {
        // Compare by rule pointer first (should be stable)
        if (rule.get() != other.rule.get())
            return rule.get() < other.rule.get();
        // Then by dot position
        if (dotPosition != other.dotPosition)
            return dotPosition < other.dotPosition;
        // Finally by lookahead pointer (handle null)
        if (lookahead.get() != other.lookahead.get()) {
            // Consistent ordering for null vs non-null
            if (!lookahead)
                return true;  // Null comes first
            if (!other.lookahead)
                return false;
            // If both non-null, compare by name for stability
            return lookahead->name < other.lookahead->name;
        }
        return false;  // Items are equal
    }
};

// LR State represents a set of LR items
class LRState {
   public:
    int stateId;
    std::vector<LRItem> items;
    std::map<std::string, std::shared_ptr<LRState>> transitions;
    std::map<std::string, Action> actions;
    std::map<std::string, int> gotoTable;

    // Modify operator== to compare sorted item sets for robustness
    bool operator==(const LRState& other) const {
        if (items.size() != other.items.size()) {
            return false;
        }
        // Create sorted copies of the item vectors
        std::vector<LRItem> sortedItems = items;
        std::vector<LRItem> sortedOtherItems = other.items;
        // Sort using the defined operator< for LRItem
        std::sort(sortedItems.begin(), sortedItems.end());
        std::sort(sortedOtherItems.begin(), sortedOtherItems.end());
        // Compare the sorted vectors
        return sortedItems == sortedOtherItems;
    }
};

class Parser {
   public:
    // Special token definition for End-Of-File
    static std::shared_ptr<Tokenizer::TokenDefinition> EOF_DEFINITION;

   private:
    std::shared_ptr<ProductionRuleList> rules;
    std::shared_ptr<Tokenizer::TokenDefinitionList> terminalDefs;
    std::vector<std::shared_ptr<LRState>> states;
    std::vector<std::shared_ptr<Tokenizer::Token>> symbolStack;
    std::vector<std::shared_ptr<AST::Node>> astStack;
    std::vector<int> stateStack;
    ScanContext& scanContext;
    std::shared_ptr<AST::Node> astRoot;

    // --- LR(1) Construction Data ---
    // Map from non-terminal name to its FIRST set (terminals including EOF, nullptr represents epsilon)
    std::map<std::string, std::set<std::shared_ptr<Tokenizer::TokenDefinition>>> firstSets;
    // --- End LR(1) Construction Data ---

    // Helper function to print the current stack state
    // Modify to return a string instead of printing directly
    std::string getStackStateString(const std::string& contextMessage) {
        std::stringstream ss;
        ss << "\n=== Stack State Context: " << contextMessage << " ===\n";
        const int titleWidth = ss.str().length() - 2;

        ss << "Symbol Stack (" << symbolStack.size() << "):";
        for (const auto& token : symbolStack) {
            if (token && token->definition) {
                ss << " " << token->definition->name;  // Print symbol name
            } else if (token) {
                ss << " <NoDef?>";
            } else {
                ss << " NULL";
            }
        }
        ss << "\n";

        ss << "State Stack (" << stateStack.size() << "):";
        for (const auto& stateId : stateStack) {
            ss << " " << stateId;
        }
        ss << "\n";

        ss << "AST Stack size: " << astStack.size() << "\n";
        ss << std::string(titleWidth, '=') << "\n";
        return ss.str();
    }

    // Helper to check if a name is a known terminal
    bool isTerminal(const std::string& name) {
        if (name == EOF_DEFINITION->name)
            return true;  // EOF is a terminal
        if (!terminalDefs)
            return false;  // Should not happen if constructor is used correctly
        for (const auto& def : *terminalDefs) {
            if (def && def->name == name) {
                return true;
            }
        }
        return false;
    }

    // --- LR(1) Helper Functions ---
    // Computes and stores the FIRST sets for all non-terminals
    void computeFirstSets() {
        // std::cerr << "Computing FIRST sets..." << std::endl; // Commented out
        firstSets.clear();
        bool changed = true;
        // Nullptr represents epsilon
        const std::shared_ptr<Tokenizer::TokenDefinition> EPSILON = nullptr;

        // Initialize FIRST sets for terminals (just themselves)
        // Note: We don't strictly need to store these, but it simplifies getFirstSet
        if (terminalDefs) {
            for (const auto& termDef : *terminalDefs) {
                if (termDef) {
                    firstSets[termDef->name].insert(termDef);
                }
            }
        }
        // Add EOF to its own FIRST set
        firstSets[EOF_DEFINITION->name].insert(EOF_DEFINITION);

        // Iteratively compute FIRST sets for non-terminals
        while (changed) {
            changed = false;
            for (const auto& rule : *rules) {
                const std::string& nonTerminalName = rule->name;
                auto& currentFirstSet = firstSets[nonTerminalName];
                size_t oldSize = currentFirstSet.size();

                // Calculate FIRST set of the rule's right-hand side
                bool rhsCanBeEpsilon = true;
                for (const auto& symbol : rule->symbols) {
                    if (!symbol)
                        continue;  // Should not happen with valid grammar
                    const std::string& symbolName = symbol->name;
                    (void)symbolName;                              // Mark symbolName used if needed, or remove symbolIsTerminal
                    auto& symbolFirstSet = firstSets[symbolName];  // Get FIRST(symbol)

                    // Add FIRST(symbol) - {epsilon} to FIRST(nonTerminalName)
                    for (const auto& firstSymbol : symbolFirstSet) {
                        if (firstSymbol != EPSILON) {
                            currentFirstSet.insert(firstSymbol);
                        }
                    }

                    // If epsilon is not in FIRST(symbol), then RHS cannot be epsilon via this path
                    if (symbolFirstSet.find(EPSILON) == symbolFirstSet.end()) {
                        rhsCanBeEpsilon = false;
                        break;  // Stop processing symbols in this rule
                    }
                    // If we reached here, FIRST(symbol) contained epsilon, continue to next symbol
                }

                // If all symbols in RHS could produce epsilon, add epsilon to FIRST(nonTerminalName)
                if (rhsCanBeEpsilon) {
                    currentFirstSet.insert(EPSILON);
                }

                // Check if the FIRST set for this non-terminal changed
                if (currentFirstSet.size() > oldSize) {
                    changed = true;
                }
            }  // End loop through rules
        }  // End while changed

        // std::cerr << "Computed FIRST sets." << std::endl; // Commented out
        // Optional: Print computed FIRST sets for debugging
        // for(const auto& pair : firstSets) { ... }
    }

    // Computes the FIRST set for a sequence of symbols (using precomputed sets)
    std::set<std::shared_ptr<Tokenizer::TokenDefinition>>
    getFirstSet(const std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>& symbolSequence, size_t startIndex) {
        std::set<std::shared_ptr<Tokenizer::TokenDefinition>> result;
        const std::shared_ptr<Tokenizer::TokenDefinition> EPSILON = nullptr;
        bool allPreviousEpsilon = true;

        for (size_t i = startIndex; i < symbolSequence.size(); ++i) {
            const auto& symbol = symbolSequence[i];
            if (!symbol)
                continue;  // Should not happen
            const std::string& symbolName = symbol->name;
            auto& symbolFirstSet = firstSets[symbolName];  // Assumes computeFirstSets was called

            // Add FIRST(symbol) - {epsilon} to result
            for (const auto& firstSymbol : symbolFirstSet) {
                if (firstSymbol != EPSILON) {
                    result.insert(firstSymbol);
                }
            }

            // If epsilon is not in FIRST(symbol), stop
            if (symbolFirstSet.find(EPSILON) == symbolFirstSet.end()) {
                allPreviousEpsilon = false;
                break;
            }
        }

        // If the entire sequence could derive epsilon, add epsilon to the result
        if (allPreviousEpsilon) {
            result.insert(EPSILON);
        }

        return result;
    }
    // --- End LR(1) Helper Functions ---

    // Build the closure of a set of items (LR(1) version with correct lookaheads)
    std::vector<LRItem> closure(const std::vector<LRItem>& items) {
        std::vector<LRItem> result = items;
        bool changed;
        // Keep track of items added in the current iteration to avoid duplicates within the loop
        std::set<LRItem, bool (*)(const LRItem&, const LRItem&)> addedItems([](const LRItem& a, const LRItem& b) { return a < b; });
        for (const auto& item : items)
            addedItems.insert(item);

        const std::shared_ptr<Tokenizer::TokenDefinition> EPSILON = nullptr;

        do {
            changed = false;
            std::vector<LRItem> newItemsThisIteration;  // Items added in this specific pass

            for (const auto& item : result) {
                auto nextSymbol = item.nextSymbol();
                // If the next symbol is a non-terminal...
                if (nextSymbol && !isTerminal(nextSymbol->name)) {
                    // Calculate the sequence following B (beta) + original lookahead (a)
                    std::vector<std::shared_ptr<Tokenizer::TokenDefinition>> beta_a_sequence;
                    for (size_t k = item.dotPosition + 1; k < item.rule->symbols.size(); ++k) {
                        beta_a_sequence.push_back(item.rule->symbols[k]);
                    }
                    if (item.lookahead) {
                        beta_a_sequence.push_back(item.lookahead);
                    } else {
                        // If original lookahead was null (can happen for initial item), treat it like EOF?
                        // For LR(1), the initial item's lookahead is EOF_DEFINITION, so this case might not be strictly needed
                        // If we reach here with a null lookahead later, it might indicate an issue.
                        // Let's assume item.lookahead is always valid (or EOF) for non-initial items based on correct generation.
                        // If item.lookahead is null, beta_a_sequence just contains beta.
                        // If beta can derive epsilon, the lookahead propagation should handle it.
                    }

                    // Calculate FIRST(beta + a)
                    auto lookaheads = getFirstSet(beta_a_sequence, 0);

                    // For each production B -> gamma...
                    for (const auto& rule : *rules) {
                        if (rule->name == nextSymbol->name) {
                            // For each terminal b in FIRST(beta + a)...
                            for (const auto& b : lookaheads) {
                                // Add the item [B -> .gamma, b] if not already present
                                // Note: b can be EPSILON (nullptr) if beta+a can derive epsilon,
                                // but we only add terminal lookaheads to LRItem.
                                if (b != EPSILON) {  // Only add items with terminal lookaheads
                                    LRItem newItem(rule, 0, b);
                                    // Check if newItem is already in result or addedItems set
                                    auto findIt = std::find(result.begin(), result.end(), newItem);
                                    if (findIt == result.end() && addedItems.find(newItem) == addedItems.end()) {
                                        newItemsThisIteration.push_back(newItem);
                                        addedItems.insert(newItem);  // Add to the overall set
                                        changed = true;
                                    }
                                }
                            }
                        }
                    }
                }  // End if nextSymbol is NonTerminal
            }  // End loop through current result items

            // Add the newly generated items to the main result list for the next iteration
            result.insert(result.end(), newItemsThisIteration.begin(), newItemsThisIteration.end());

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
        // std::cerr << "Building LR(1) states..." << std::endl; // Commented out

        // Start with the initial state based on the augmented grammar
        std::vector<LRItem> initialItems;
        // Find the augmented start rule (should be the first one)
        std::shared_ptr<ProductionRule> startRule = nullptr;
        if (!rules->empty() && (*rules)[0]->name == "START") {
            startRule = (*rules)[0];
        } else {
            throw std::runtime_error("Augmented start rule 'START' not found or not first rule.");
        }
        // The initial item is [START -> . CompUnit, $EOF$]
        initialItems.push_back(LRItem(startRule, 0, EOF_DEFINITION));

        auto initialState = std::make_shared<LRState>();
        initialState->stateId = 0;
        // Compute closure using the initial item
        initialState->items = closure(initialItems);
        states.clear();  // Clear previous states if any
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
                            state->transitions[symbol->name] = newState;
                            changed = true;
                        } else {
                            state->transitions[symbol->name] = *it;
                        }
                    }
                }
            }
        } while (changed);

        // std::cerr << "Built " << states.size() << " states" << std::endl; // Commented out
    }

    // Build the action and goto tables
    void buildTables() {
        // std::cerr << "Building parsing tables..." << std::endl; // Commented out
        const std::string EOF_SYMBOL = "$EOF$";

        for (const auto& state : states) {
            state->actions.clear();
            state->gotoTable.clear();

            // --- Debugging Specific States --- START
            // Remove unused debug flag variables
            // bool debugState16 = (state->stateId == 16);
            // bool debugState121 = (state->stateId == 121);
            // bool debugState366 = (state->stateId == 366);
            /* // Comment out specific state debugging details
            if (debugState16 || debugState121 || debugState366) {
                std::cerr << "\n--- Debugging State " << state->stateId << " ---" << std::endl;
                std::cerr << "Items:" << std::endl;
                for (const auto& item : state->items) {
                    std::string lookaheadStr = (item.lookahead ? item.lookahead->name : "NULL");
                    std::cerr << "  Rule: " << item.rule->name << " ->";
                    for (size_t k = 0; k < item.rule->symbols.size(); ++k) {
                        if (k == item.dotPosition)
                            std::cerr << " .";
                        // Handle null symbol pointer just in case
                        std::cerr << " " << (item.rule->symbols[k] ? item.rule->symbols[k]->name : "<NULL_SYM>");
                    }
                    if (item.dotPosition == item.rule->symbols.size())
                        std::cerr << " .";
                    std::cerr << ", Lookahead: " << lookaheadStr << std::endl;
                }
                std::cerr << "Transitions:" << std::endl;
                for (const auto& pair : state->transitions) {
                    std::cerr << "  Symbol: " << pair.first << " -> State " << (pair.second ? pair.second->stateId : -1) << std::endl;
                }
                std::cerr << "-------------------------" << std::endl;
            }
            */
            // --- Debugging Specific States --- END

            // 1. Determine SHIFT actions and GOTO entries from transitions
            for (const auto& transitionPair : state->transitions) {
                const std::string& symbolName = transitionPair.first;
                const auto& targetState = transitionPair.second;

                if (!targetState || symbolName.empty())
                    continue;

                bool is_terminal_check = isTerminal(symbolName);

                /* // Comment out transition processing logs
                if (debugState16 || debugState121 || debugState366) {
                    std::cerr << "State " << state->stateId << ": Processing transition..." << std::endl;
                }
                */

                if (is_terminal_check) {
                    Action shiftAction(ActionType::SHIFT, targetState->stateId);
                    auto existingActionIt = state->actions.find(symbolName);
                    if (existingActionIt == state->actions.end()) {
                        /* // Comment out initial SHIFT add log
                         if(debugState16 || debugState121 || debugState366) std::cerr << "State " << state->stateId << ": Adding initial SHIFT..." << std::endl;
                         */
                        state->actions[symbolName] = shiftAction;
                    } else {
                        /* // Comment out conflict detection log
                         if(debugState16 || debugState121 || debugState366) std::cerr << "State " << state->stateId << ": Conflict detected for SHIFT..." << std::endl;
                         */
                        if (existingActionIt->second.type == ActionType::REDUCE) {
                            // std::cerr << "State " << state->stateId << ": Shift/Reduce conflict..." << std::endl; // Optionally keep S/R conflict log?
                            state->actions[symbolName] = shiftAction;
                        } else if (existingActionIt->second.type == ActionType::SHIFT) {
                            // Keep Shift/Shift critical error log
                            std::cerr << "CRITICAL ERROR: Shift/Shift conflict detected..." << std::endl;
                        }
                    }
                } else {
                    state->gotoTable[symbolName] = targetState->stateId;
                }
            }

            // 2. Determine REDUCE/ACCEPT actions from completed items
            for (const auto& item : state->items) {
                if (item.isComplete()) {
                    /* // Comment out completed item check log
                    if (debugState16 || debugState121 || debugState366) std::cerr << "State " << state->stateId << ": Checking completed item..." << std::endl;
                    */
                    if (item.rule->name == "START" && item.lookahead == EOF_DEFINITION) {
                        Action acceptAction(ActionType::ACCEPT, 0);
                        std::string eofName = EOF_DEFINITION->name;
                        if (state->actions.count(eofName)) {
                            std::cerr << "Conflict (State " << state->stateId << ", Symbol " << eofName << "): ACCEPT vs existing ..." << std::endl;  // Keep Accept conflict
                        }
                        state->actions[eofName] = acceptAction;
                        // std::cerr << "  State " << state->stateId << ": Added ACCEPT for " << eofName << std::endl; // Comment out ACCEPT add log
                    } else if (item.lookahead && !item.lookahead->name.empty()) {
                        // --- REDUCE Action ---
                        // Restore the ruleIndex finding logic:
                        int ruleIndex = -1;
                        for (size_t i = 0; i < rules->size(); ++i) {
                            if ((*rules)[i] == item.rule) {
                                ruleIndex = i;
                                break;
                            }
                        }
                        if (ruleIndex == -1) {
                            // This should not happen if the item's rule is valid
                            throw std::runtime_error("Internal Error: Could not find index for reduction rule: " + item.rule->name);
                        }
                        // End of restored logic

                        Action reduceAction(ActionType::REDUCE, ruleIndex);
                        std::string lookaheadName = item.lookahead->name;
                        auto existingActionIt = state->actions.find(lookaheadName);
                        /* // Comment out REDUCE check log
                        if (debugState16 || debugState121 || debugState366) std::cerr << "State " << state->stateId << ": Considering REDUCE..." << std::endl;
                        */
                        if (existingActionIt == state->actions.end()) {
                            /* // Comment out initial REDUCE add log
                             if(debugState16 || debugState121 || debugState366) std::cerr << "State " << state->stateId << ": Adding initial REDUCE..." << std::endl;
                            */
                            state->actions[lookaheadName] = reduceAction;
                        } else {
                            /* // Comment out conflict detection log
                              if(debugState16 || debugState121 || debugState366) std::cerr << "State " << state->stateId << ": Conflict detected for REDUCE..." << std::endl;
                             */
                            if (existingActionIt->second.type == ActionType::SHIFT) {
                                // S/R conflict handled/logged above
                            } else if (existingActionIt->second.type == ActionType::REDUCE) {
                                std::cerr << "Reduce/Reduce conflict (State " << state->stateId << "...)" << std::endl;  // Keep R/R conflict
                                if (reduceAction.value < existingActionIt->second.value) {
                                    std::cerr << "  >> Preferring new REDUCE rule " << reduceAction.value << std::endl;
                                    state->actions[lookaheadName] = reduceAction;
                                }
                            } else {
                                std::cerr << "Unhandled conflict (State " << state->stateId << "...)" << std::endl;  // Keep other conflicts
                            }
                        }
                    }
                }  // End if item.isComplete()
            }  // End loop through items

            /* // Comment out final action/GOTO table debugging
            if (debugState16 || debugState121 || debugState366) {
                 std::cerr << "\n--- Final Actions for State " << state->stateId << " ---" << std::endl;
                 for (const auto& pair : state->actions) {
                     std::cerr << "  Symbol: " << pair.first
                               << ", Action: " << (int)pair.second.type
                               << ", Value: " << pair.second.value << std::endl;
                 }
                 std::cerr << "--- GOTO Table for State " << state->stateId << " ---" << std::endl;
                 for (const auto& pair : state->gotoTable) {
                     std::cerr << "  Symbol: " << pair.first << " -> State " << pair.second << std::endl;
                 }
                 std::cerr << "--------------------------------\n" << std::endl;
            }
            if (state->stateId == 0) {
                 std::cerr << "\n--- Final Actions for State 0 ---" << std::endl;
                 for (const auto& pair : state->actions) {
                     std::cerr << "  Symbol: " << pair.first
                               << ", Action: " << (int)pair.second.type
                               << ", Value: " << pair.second.value << std::endl;
                 }
                 std::cerr << "--------------------------------\n" << std::endl;
            }
            */

        }  // End loop through states
        // std::cerr << "Built parsing tables" << std::endl; // Commented out
    }

    void shift(std::shared_ptr<Tokenizer::Token> token) {
        // std::cerr << "Shifting token: " << token->definition->name << " -> " << token->matched << std::endl; // Commented out

        auto currentStateId = stateStack.back();
        auto currentState = states[currentStateId];

        // Find SHIFT action using token name
        std::string tokenName = token->definition->name;
        auto actionIt = currentState->actions.find(tokenName);
        if (actionIt == currentState->actions.end() || actionIt->second.type != ActionType::SHIFT) {
            this->printStackState("Error during shift - no shift action found for " + tokenName);
            throw ParseError(scanContext, *token, currentStateId, getStackStateString("Error during shift"), "Error during shift");
        }
        Action action = actionIt->second;  // Action found via find

        symbolStack.push_back(token);
        astStack.push_back(std::make_shared<AST::TerminalNode>(token));
        stateStack.push_back(action.value);

        // std::cerr << "Shifted to state " << action.value << std::endl; // Commented out
    }

    void reduce(std::shared_ptr<ProductionRule> rule) {
        // std::cerr << "Reducing by rule: " << rule->name << std::endl; // Commented out

        // Pop the right-hand side
        std::vector<std::shared_ptr<Tokenizer::Token>> matchedTokens;
        std::vector<std::shared_ptr<AST::Node>> matchedNodes;
        for (size_t i = 0; i < rule->symbols.size(); ++i) {
            matchedTokens.insert(matchedTokens.begin(), symbolStack.back());
            matchedNodes.insert(matchedNodes.begin(), astStack.back());
            symbolStack.pop_back();
            astStack.pop_back();
            stateStack.pop_back();
        }

        // Create a non-terminal node for this reduction
        auto nonTerminalNode = std::make_shared<AST::NonTerminalNode>(rule->name);
        for (const auto& node : matchedNodes) {
            nonTerminalNode->addChild(node);
        }

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
        astStack.push_back(nonTerminalNode);

        // Goto (uses gotoTable with string keys - this part is correct)
        auto previousStateId = stateStack.back();
        auto previousState = states[previousStateId];
        auto gotoIt = previousState->gotoTable.find(rule->name);
        if (gotoIt == previousState->gotoTable.end()) {
            // ... (error handling) ...
        }
        int nextStateId = gotoIt->second;
        stateStack.push_back(nextStateId);

        // std::cerr << "Reduced. New state: " << nextStateId << std::endl; // Commented out
    }

   public:
    Parser(std::shared_ptr<ProductionRuleList> rules,
           std::shared_ptr<Tokenizer::TokenDefinitionList> terminalDefs,
           ScanContext& scanContext)
        : rules(rules), terminalDefs(terminalDefs), scanContext(scanContext) {
        if (!terminalDefs) {
            throw std::runtime_error("Parser requires a valid list of terminal definitions.");
        }
        if (!rules || rules->empty()) {
            throw std::runtime_error("Parser requires a non-empty list of production rules.");
        }

        computeFirstSets();  // Compute FIRST sets before building states
        buildStates();
        buildTables();
    }

    std::shared_ptr<AST::Node> parse(std::shared_ptr<Tokenizer::TokenList> tokens) {
        // std::cerr << "Starting parse with " << tokens->size() << " tokens:" << std::endl; // Commented out

        // Initialize stacks
        stateStack.clear();
        symbolStack.clear();
        astStack.clear();
        stateStack.push_back(0);  // Start with state 0

        // --- Revised Loop Logic --- START
        size_t token_idx = 0;
        bool eofSent = false;  // Track if EOF token has been *generated* for currentToken
        std::shared_ptr<Tokenizer::Token> currentToken;

        // Helper lambda to fetch the next non-whitespace token or EOF
        auto fetchNextToken = [&]() -> std::shared_ptr<Tokenizer::Token> {
            while (token_idx < tokens->size()) {
                auto& token = (*tokens)[token_idx];
                if (!token.definition) {  // Basic validation
                    printStackState("Error: Token has null definition during fetch");
                    throw ParseError(scanContext, "Token at index " + std::to_string(token_idx) + " has no definition.",
                                     getStackStateString("Invalid Token"));
                }
                if (token.definition->name != "WHITESPACE") {
                    return std::make_shared<Tokenizer::Token>(token);
                }
                token_idx++;  // Skip whitespace
            }
            // If we reached end of input tokens
            if (!eofSent) {
                eofSent = true;
                auto eofToken = std::make_shared<Tokenizer::Token>();
                eofToken->definition = EOF_DEFINITION;
                eofToken->matched = EOF_DEFINITION->name;
                return eofToken;
            } else {
                // We already generated and processed EOF, should have ACCEPTed or ERRORed
                printStackState("Error: Trying to fetch token after EOF was processed.");
                throw ParseError(scanContext, "Internal parser error: processing past EOF.",
                                 getStackStateString("EOF Processing"));
            }
        };

        // Fetch the initial token
        currentToken = fetchNextToken();

        // Loop indefinitely until ACCEPT or ERROR
        while (true) {
            int currentStateId_int = stateStack.back();  // Keep as int for stack type
            if (currentStateId_int < 0 || static_cast<size_t>(currentStateId_int) >= states.size()) {
                throw ParseError(scanContext, "Invalid state ID encountered: " + std::to_string(currentStateId_int),
                                 getStackStateString("Invalid State ID"));
            }
            auto currentState = states[currentStateId_int];

            if (!currentToken || !currentToken->definition) {
                throw ParseError(scanContext, "Internal error: Invalid token.",
                                 getStackStateString("Invalid Token"));
            }

            // std::cerr << "State " << currentStateId << ", "; // Commented out
            // std::cerr << "next token: " << currentToken->definition->name << " -> " << currentToken->matched; // Commented out

            // Find action in the table using the *currentToken*
            Action action;
            std::string actionKey = currentToken->definition->name;
            auto actionIt = currentState->actions.find(actionKey);

            if (actionIt == currentState->actions.end()) {
                throw ParseError(scanContext, *currentToken,
                                 currentStateId_int,
                                 getStackStateString("Unexpected Token"),
                                 "Unexpected token");
            }
            action = actionIt->second;

            // std::cerr << ", action: "; // Commented out
            switch (action.type) {
                case ActionType::SHIFT:
                    // std::cerr << "SHIFT to " << action.value; // Commented out
                    shift(currentToken);
                    // Consume input: Fetch the next token ONLY IF NOT EOF
                    if (currentToken->definition != EOF_DEFINITION) {  // Check if we just shifted EOF
                        token_idx++;                                   // Advance index past the shifted token
                        currentToken = fetchNextToken();
                    } else {
                        // We just shifted EOF, the next action should be ACCEPT (or ERROR)
                        // Do not fetch next token. The loop will continue, see the new state,
                        // see EOF as the currentToken again, and trigger ACCEPT.
                        // No need to explicitly set currentToken to null or anything,
                        // just don't advance or fetch.
                    }
                    break;

                case ActionType::REDUCE:
                    // std::cerr << "REDUCE by rule " << action.value; // Commented out
                    if (action.value < 0 || static_cast<size_t>(action.value) >= rules->size()) {
                        throw ParseError(scanContext, "Invalid rule index " + std::to_string(action.value) + " for reduction.",
                                         getStackStateString("Invalid Rule Index"));
                    }
                    reduce((*rules)[action.value]);
                    break;

                case ActionType::ACCEPT:
                    // std::cerr << "ACCEPT"; // Commented out
                    // In this specific implementation (ACCEPT after shifting EOF),
                    // the stack should contain [..., CompUnit_Node, EOF_Node].
                    // We want the CompUnit_Node, which is the second-to-last element.
                    if (astStack.size() < 2) {
                        throw ParseError(scanContext, "Internal error: AST stack state invalid on ACCEPT.",
                                         getStackStateString("Invalid Stack on Accept"));
                    }
                    // Get the CompUnit node (second to last)
                    astRoot = astStack[astStack.size() - 2];
                    // std::cerr << std::endl; // Commented out (moved below)
                    return astRoot;

                case ActionType::ERROR:
                default:
                    throw ParseError(scanContext, *currentToken,
                                     currentStateId_int,
                                     getStackStateString("Syntax Error from Table"),
                                     "Syntax error encountered on token");
            }
            // std::cerr << std::endl; // Newline after action output // Commented out

        }  // End while loop (while(true))
        // --- Revised Loop Logic --- END
    }

    // Error printing function (can call getStackStateString)
    void printStackState(const std::string& message) {
        // This can still be used for explicit debug printing if needed,
        // but error messages will use getStackStateString directly.
        (void)message;  // Mark 'message' as used to suppress warning
                        // std::cerr << getStackStateString(message); // Example usage
    }
};

// Define the grammar rules
std::shared_ptr<ProductionRuleList> createGrammarRules();

}  // namespace Parser