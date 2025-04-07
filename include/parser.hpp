#pragma once
#include <assert.h>
#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>
#include "ast.hpp"
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
    void printStackState(const std::string& message) {
        std::cerr << "\n=== Stack State ===" << std::endl;
        std::cerr << "Message: " << message << std::endl;

        std::cerr << "\nSymbol Stack:" << std::endl;
        for (const auto& token : symbolStack) {
            if (token && token->definition) {  // Added check for definition
                std::cerr << token->definition->name << " ";
            } else if (token) {
                std::cerr << "<Token without definition?> ";
            } else {
                std::cerr << "NULL ";
            }
        }
        std::cerr << std::endl;

        std::cerr << "\nState Stack:" << std::endl;
        for (const auto& state : stateStack) {
            std::cerr << state << " ";
        }
        std::cerr << std::endl;

        std::cerr << "\nAST Stack size: " << astStack.size() << std::endl;
        std::cerr << "================\n"
                  << std::endl;
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
        std::cerr << "Computing FIRST sets..." << std::endl;
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
                    bool symbolIsTerminal = isTerminal(symbolName);
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

        std::cerr << "Computed FIRST sets." << std::endl;
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
        std::cerr << "Building LR(1) states..." << std::endl;

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

        std::cerr << "Built " << states.size() << " states" << std::endl;
    }

    // Build the action and goto tables
    void buildTables() {
        std::cerr << "Building parsing tables..." << std::endl;
        const std::string EOF_SYMBOL = "$EOF$";

        for (const auto& state : states) {
            state->actions.clear();
            state->gotoTable.clear();

            // --- Debugging Specific States --- START
            bool debugState16 = (state->stateId == 16);
            bool debugState121 = (state->stateId == 121);
            bool debugState366 = (state->stateId == 366);
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
            // --- Debugging Specific States --- END

            // 1. Determine SHIFT actions and GOTO entries from transitions
            for (const auto& transitionPair : state->transitions) {
                const std::string& symbolName = transitionPair.first;
                const auto& targetState = transitionPair.second;

                if (!targetState || symbolName.empty())
                    continue;

                bool is_terminal_check = isTerminal(symbolName);

                // --- Debug logging for Specific States Transitions --- START
                if (debugState16 || debugState121 || debugState366) {
                    std::cerr << "State " << state->stateId << ": Processing transition for Symbol: " << symbolName
                              << ", Target State: " << targetState->stateId
                              << ", IsTerminal: " << std::boolalpha << is_terminal_check << std::endl;
                }
                // --- Debug logging for Specific States Transitions --- END

                // Check if Terminal or Non-terminal using the helper function
                if (is_terminal_check) {
                    // --- Terminal: Potential SHIFT Action ---
                    Action shiftAction(ActionType::SHIFT, targetState->stateId);
                    auto existingActionIt = state->actions.find(symbolName);
                    if (existingActionIt == state->actions.end()) {
                        // --- Debug logging for Specific States SHIFT add --- START
                        if (debugState16 || debugState121 || debugState366)
                            std::cerr << "State " << state->stateId << ": Adding initial SHIFT for " << symbolName << " to state " << targetState->stateId << std::endl;
                        // --- Debug logging for Specific States SHIFT add --- END
                        state->actions[symbolName] = shiftAction;
                    } else {
                        // Conflict resolution
                        // --- Debug logging for Specific States SHIFT conflict --- START
                        if (debugState16 || debugState121 || debugState366)
                            std::cerr << "State " << state->stateId << ": Conflict detected for SHIFT on " << symbolName << ". Existing action type: " << (int)existingActionIt->second.type << std::endl;
                        // --- Debug logging for Specific States SHIFT conflict --- END
                        if (existingActionIt->second.type == ActionType::REDUCE) {
                            // --- Debug logging for Specific States S/R conflict --- START
                            if (debugState16 || debugState121 || debugState366)
                                std::cerr << "State " << state->stateId << ": Shift/Reduce conflict for " << symbolName << ". Preferring SHIFT." << std::endl;
                            // --- Debug logging for Specific States S/R conflict --- END
                            state->actions[symbolName] = shiftAction;  // SHIFT wins
                        } else if (existingActionIt->second.type == ActionType::SHIFT) {
                            // Should not happen in a deterministic grammar/LR(1) table
                            std::cerr << "CRITICAL ERROR: Shift/Shift conflict detected in State " << state->stateId
                                      << " for symbol " << symbolName << "! Target1: " << existingActionIt->second.value
                                      << ", Target2: " << shiftAction.value << std::endl;
                        }
                    }
                } else {
                    // --- Non-Terminal: Add to GOTO Table ---
                    state->gotoTable[symbolName] = targetState->stateId;
                }
            }

            // 2. Determine REDUCE/ACCEPT actions from completed items
            for (const auto& item : state->items) {
                if (item.isComplete()) {
                    // --- Debug logging for Specific States completed items --- START
                    if (debugState16 || debugState121 || debugState366)
                        std::cerr << "State " << state->stateId << ": Checking completed item: " << item.rule->name << " -> ... . , Lookahead: " << (item.lookahead ? item.lookahead->name : "NULL") << std::endl;
                    // --- Debug logging for Specific States completed items --- END

                    // Check if this is the ACCEPT state: Rule is START -> CompUnit . and lookahead is $EOF$
                    if (item.rule->name == "START" && item.lookahead == EOF_DEFINITION) {
                        // --- ACCEPT Action ---
                        Action acceptAction(ActionType::ACCEPT, 0);  // value is not used for ACCEPT
                        std::string eofName = EOF_DEFINITION->name;
                        if (state->actions.count(eofName)) {
                            // Conflict! (Should ideally not happen with a proper grammar)
                            std::cerr << "Conflict (State " << state->stateId << ", Symbol " << eofName << "): ACCEPT vs existing "
                                      << (int)state->actions[eofName].type << std::endl;
                            // Decide conflict resolution (e.g., prefer ACCEPT? Log error?)
                            // For now, let's overwrite with ACCEPT, but log it.
                        }
                        state->actions[eofName] = acceptAction;
                        std::cerr << "  State " << state->stateId << ": Added ACCEPT for " << eofName << std::endl;
                    }
                    // Check if lookahead is valid before adding REDUCE action
                    else if (item.lookahead && !item.lookahead->name.empty()) {
                        // --- REDUCE Action ---
                        int ruleIndex = -1;
                        for (size_t i = 0; i < rules->size(); ++i) {
                            if ((*rules)[i] == item.rule) {
                                ruleIndex = i;
                                break;
                            }
                        }
                        if (ruleIndex == -1) {
                            throw std::runtime_error("Could not find index for reduction rule: " + item.rule->name);
                        }

                        Action reduceAction(ActionType::REDUCE, ruleIndex);
                        std::string lookaheadName = item.lookahead->name;
                        auto existingActionIt = state->actions.find(lookaheadName);

                        // --- Debug logging for Specific States REDUCE check --- START
                        if (debugState16 || debugState121 || debugState366)
                            std::cerr << "State " << state->stateId << ": Considering REDUCE for lookahead " << lookaheadName << " by rule " << ruleIndex << " (" << item.rule->name << ")" << std::endl;
                        // --- Debug logging for Specific States REDUCE check --- END

                        if (existingActionIt == state->actions.end()) {
                            // --- Debug logging for Specific States REDUCE add --- START
                            if (debugState16 || debugState121 || debugState366)
                                std::cerr << "State " << state->stateId << ": Adding initial REDUCE for " << lookaheadName << " (Rule " << ruleIndex << ")" << std::endl;
                            // --- Debug logging for Specific States REDUCE add --- END
                            state->actions[lookaheadName] = reduceAction;
                        } else {
                            // --- Debug logging for Specific States REDUCE conflict --- START
                            if (debugState16 || debugState121 || debugState366)
                                std::cerr << "State " << state->stateId << ": Conflict detected for REDUCE on " << lookaheadName << ". Existing action type: " << (int)existingActionIt->second.type << ", Existing value: " << existingActionIt->second.value << ", New rule: " << ruleIndex << std::endl;
                            // --- Debug logging for Specific States REDUCE conflict --- END
                            // --- Conflict Resolution (Reduce vs existing) ---
                            if (existingActionIt->second.type == ActionType::SHIFT) {
                                // Shift/Reduce conflict: logged above, SHIFT wins (do nothing here)
                            } else if (existingActionIt->second.type == ActionType::REDUCE) {
                                // Reduce/Reduce conflict
                                std::cerr << "Reduce/Reduce conflict (State " << state->stateId << ", Symbol " << lookaheadName << "): Existing Rule " << existingActionIt->second.value << ", New Rule " << reduceAction.value << std::endl;
                                // Resolve based on rule priority (e.g., lower index/priority wins)
                                // Assuming lower rule index means higher priority (defined earlier)
                                if (reduceAction.value < existingActionIt->second.value) {
                                    std::cerr << "  >> Preferring new REDUCE rule " << reduceAction.value << std::endl;
                                    state->actions[lookaheadName] = reduceAction;
                                }
                            } else {
                                // Accept/Reduce or Error/Reduce conflict (should ideally not happen)
                                std::cerr << "Unhandled conflict (State " << state->stateId << ", Symbol " << lookaheadName << "): Existing " << (int)existingActionIt->second.type << " vs REDUCE rule " << reduceAction.value << std::endl;
                            }
                        }
                    }
                }  // End if item.isComplete()
            }  // End loop through items

            // --- Debugging Specific States Final Actions --- START
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
                std::cerr << "--------------------------------\n"
                          << std::endl;
            }
            // --- Debugging Specific States Final Actions --- END

            // --- Debugging State 0 Final Actions --- (Keep this)
            if (state->stateId == 0) {
                std::cerr << "\n--- Final Actions for State 0 ---" << std::endl;
                for (const auto& pair : state->actions) {
                    std::cerr << "  Symbol: " << pair.first
                              << ", Action: " << (int)pair.second.type
                              << ", Value: " << pair.second.value << std::endl;
                }
                std::cerr << "--------------------------------\n"
                          << std::endl;
            }
        }  // End loop through states
        std::cerr << "Built parsing tables" << std::endl;
    }

    void shift(std::shared_ptr<Tokenizer::Token> token) {
        std::cerr << "Shifting token: " << token->definition->name << " -> " << token->matched << std::endl;

        auto currentStateId = stateStack.back();
        auto currentState = states[currentStateId];

        // Find SHIFT action using token name
        std::string tokenName = token->definition->name;
        auto actionIt = currentState->actions.find(tokenName);
        if (actionIt == currentState->actions.end() || actionIt->second.type != ActionType::SHIFT) {
            this->printStackState("Error during shift - no shift action found for " + tokenName);
            throw ParseError(scanContext, "Expected shift action for token " + tokenName + " in state " + std::to_string(currentStateId));
        }
        Action action = actionIt->second;  // Action found via find

        symbolStack.push_back(token);
        astStack.push_back(std::make_shared<AST::TerminalNode>(token));
        stateStack.push_back(action.value);

        std::cerr << "Shifted to state " << action.value << std::endl;
    }

    void reduce(std::shared_ptr<ProductionRule> rule) {
        std::cerr << "Reducing by rule: " << rule->name << std::endl;

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

        std::cerr << "Reduced. New state: " << nextStateId << std::endl;
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
        std::cerr << "Starting parse with " << tokens->size() << " tokens:" << std::endl;

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
                    throw ParseError(scanContext, "Token at index " + std::to_string(token_idx) + " has no definition.");
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
                throw ParseError(scanContext, "Internal parser error: processing past EOF.");
            }
        };

        // Fetch the initial token
        currentToken = fetchNextToken();

        // Loop indefinitely until ACCEPT or ERROR
        while (true) {
            int currentStateId = stateStack.back();  // Use int for consistency with State ID type
            if (currentStateId < 0 || currentStateId >= states.size()) {
                printStackState("Error: Invalid state ID on stack");
                throw ParseError(scanContext, "Invalid state ID encountered: " + std::to_string(currentStateId));
            }
            auto currentState = states[currentStateId];

            // Basic validation for currentToken (should always be valid here)
            if (!currentToken || !currentToken->definition) {
                printStackState("Error: currentToken or its definition is null before action lookup");
                throw ParseError(scanContext, "Internal error: Invalid token.");
            }

            std::cerr << "State " << currentStateId << ", ";
            std::cerr << "next token: " << currentToken->definition->name << " -> " << currentToken->matched;

            // Find action in the table using the *currentToken*
            Action action;
            std::string actionKey = currentToken->definition->name;
            auto actionIt = currentState->actions.find(actionKey);

            if (actionIt == currentState->actions.end()) {
                printStackState("Error: No action found for token \'" + actionKey + "\'");
                throw ParseError(scanContext, "Unexpected token \'" + currentToken->matched + "\' (" + actionKey + ") in state " + std::to_string(currentStateId));
            }
            action = actionIt->second;

            std::cerr << ", action: ";
            switch (action.type) {
                case ActionType::SHIFT:
                    std::cerr << "SHIFT to " << action.value;
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
                    std::cerr << "REDUCE by rule " << action.value;
                    if (action.value < 0 || action.value >= rules->size()) {
                        printStackState("Error: Invalid rule index for reduction");
                        throw ParseError(scanContext, "Invalid rule index " + std::to_string(action.value) + " for reduction.");
                    }
                    reduce((*rules)[action.value]);
                    // Do not consume input token (currentToken remains the same for next iteration)
                    break;

                case ActionType::ACCEPT:
                    std::cerr << "ACCEPT";
                    // In this specific implementation (ACCEPT after shifting EOF),
                    // the stack should contain [..., CompUnit_Node, EOF_Node].
                    // We want the CompUnit_Node, which is the second-to-last element.
                    if (astStack.size() < 2) {
                        printStackState("Error: AST stack has less than 2 elements on ACCEPT");
                        throw ParseError(scanContext, "Internal error: AST stack state invalid on ACCEPT.");
                    }
                    // Get the CompUnit node (second to last)
                    astRoot = astStack[astStack.size() - 2];
                    std::cerr << std::endl;
                    return astRoot;

                case ActionType::ERROR:
                default:
                    printStackState("Error: Table entry is ERROR");
                    throw ParseError(scanContext, "Syntax error encountered on token \'" + currentToken->matched + "\' in state " + std::to_string(currentStateId));
            }
            std::cerr << std::endl;  // Newline after action output

        }  // End while loop (while(true))
        // --- Revised Loop Logic --- END
    }
};

// Define the static EOF definition outside the class
std::shared_ptr<Tokenizer::TokenDefinition> Parser::EOF_DEFINITION =
    std::make_shared<Tokenizer::TokenDefinition>("$EOF$", nullptr, nullptr, nullptr, -1);  // Use a distinct name

// Define the grammar rules
std::shared_ptr<ProductionRuleList> createGrammarRules();

}  // namespace Parser