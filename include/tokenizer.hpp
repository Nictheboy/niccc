#pragma once
#include <assert.h>
#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include "common.hpp"
#include "surakarta_event.hpp"

// TODO: fix memory leak
// TODO: unify matchers
// TODO: reset

namespace Tokenizer {

class LexicalError : public CompilerError {
   public:
    LexicalError(ScanContext& scanContext)
        : CompilerError(scanContext, "lexical error") {}
};

class Matcher;
using MatcherList = std::vector<std::shared_ptr<Matcher>>;
class Matcher : public std::enable_shared_from_this<Matcher> {
   public:
    virtual ~Matcher() = default;
    virtual void reset() {};
    virtual std::shared_ptr<MatcherList> match(char c) = 0;
};

class EndMatcher : public Matcher {
   public:
    SurakartaEvent<> onMatch;
    std::shared_ptr<Matcher> next;
    std::shared_ptr<MatcherList> match(char c) override {
        onMatch.Invoke();
        if (next)
            return next->match(c);
        else
            return std::make_shared<std::vector<std::shared_ptr<Matcher>>>();
    }
};

using CharSet = std::shared_ptr<std::vector<char>>;

class SingleCharMatcher : public Matcher {
   private:
    const CharSet charSet;
    const std::shared_ptr<Matcher> next;

   public:
    SingleCharMatcher(CharSet charSet, std::shared_ptr<Matcher> next)
        : charSet(charSet), next(next) {}

    std::shared_ptr<MatcherList> match(char c) override {
        auto list = std::make_shared<std::vector<std::shared_ptr<Matcher>>>();
        if (std::find(charSet->begin(), charSet->end(), c) != charSet->end())
            list->push_back(next);
        return list;
    }
};

class MultiBranchMatcher : public Matcher {
   public:
    MatcherList matchers;

    std::shared_ptr<MatcherList> match(char c) override {
        auto list = std::make_shared<std::vector<std::shared_ptr<Matcher>>>();
        for (auto matcher : matchers) {
            auto ret = matcher->match(c);
            list->insert(list->end(), ret->begin(), ret->end());
        }
        return list;
    }
};

struct TokenDefinition : public std::enable_shared_from_this<TokenDefinition> {
    std::string name;
    std::shared_ptr<MatcherList> matchers;
    std::shared_ptr<Matcher> begin;
    std::shared_ptr<EndMatcher> end;
    int priority;  // lower priority value means higher priority

    TokenDefinition(std::string name, std::shared_ptr<MatcherList> matchers, std::shared_ptr<Matcher> begin, std::shared_ptr<EndMatcher> end, int priority)
        : name(name), matchers(matchers), begin(begin), end(end), priority(priority) {}

    void reset() {
        for (auto matcher : *matchers)
            matcher->reset();
    }
};

using TokenDefinitionList = std::vector<std::shared_ptr<TokenDefinition>>;

class StatusMachine {
   private:
    const std::shared_ptr<TokenDefinitionList> definitions;
    std::map<std::shared_ptr<Matcher>, std::shared_ptr<TokenDefinition>> endMap;
    std::shared_ptr<MatcherList> status;
    std::vector<std::shared_ptr<TokenDefinition>> committableTokensNewCharNotAccepted;
    ScanContext& scanContext;

    std::shared_ptr<MatcherList> nextStatus(char c) {
        auto status = std::make_shared<MatcherList>();
        for (auto matcher : *this->status) {
            auto ret = matcher->match(c);
            status->insert(status->end(), ret->begin(), ret->end());
        }
        return status;
    }

   public:
    StatusMachine(std::shared_ptr<TokenDefinitionList> definitions, ScanContext& scanContext)
        : definitions(definitions), scanContext(scanContext) {
        (void)this->scanContext;  // Mark as unused for now
        for (auto definition : *definitions) {
            definition->end->onMatch.AddListener([definition, this]() {
                committableTokensNewCharNotAccepted.push_back(definition);
            });
            endMap[definition->end] = definition;
        }
        reset();
    }
    ~StatusMachine() {
        for (auto definition : *definitions) {
            definition->end->onMatch.RemoveListeners();
        }
    }

    SurakartaEvent<std::shared_ptr<TokenDefinition>> onTokenCommitted;
    SurakartaEvent<char> onCharAcceptted;

    void reset() {
        status = std::make_shared<MatcherList>();
        for (auto definition : *definitions) {
            definition->reset();
            status->push_back(definition->begin);
        }
    }

    // returns: has c been accepted
    bool next(char c) {
        if (c != '\0')
            committableTokensNewCharNotAccepted.clear();
        auto nextAll = nextStatus(c);
        auto nextNonEnd = std::make_shared<MatcherList>();
        auto nextEnd = std::make_shared<MatcherList>();
        auto committableTokensNewCharAccepted = TokenDefinitionList();
        for (auto matcher : *nextAll) {
            if (endMap.count(matcher) == 0)
                nextNonEnd->push_back(matcher);
            else {
                nextEnd->push_back(matcher);
                committableTokensNewCharAccepted.push_back(endMap[matcher]);
            }
        }
        if (nextAll->size() > 0) {
            // Token accepted by status machine
            assert(c != '\0');  // '\0' should never be accepted
            status = nextAll;
            onCharAcceptted.Invoke(c);
            return true;
        } else {
            auto committableTokens = TokenDefinitionList();
            committableTokens.insert(committableTokens.end(), committableTokensNewCharNotAccepted.begin(), committableTokensNewCharNotAccepted.end());
            committableTokens.insert(committableTokens.end(), committableTokensNewCharAccepted.begin(), committableTokensNewCharAccepted.end());
            if (committableTokens.size() == 0) {
                if (c != '\0')
                    throw LexicalError(scanContext);
                else
                    return false;
            }
            auto cmp = [](const std::shared_ptr<TokenDefinition>& a, const std::shared_ptr<TokenDefinition>& b) {
                return a->priority < b->priority;
            };
            auto committedToken = *std::min_element(committableTokens.begin(), committableTokens.end(), cmp);
            reset();
            bool charAccepted = std::find(committableTokensNewCharAccepted.begin(), committableTokensNewCharAccepted.end(), committedToken) != committableTokensNewCharAccepted.end();
            if (charAccepted)
                onCharAcceptted.Invoke(c);
            onTokenCommitted.Invoke(committedToken);
            return charAccepted;
        }
    }
};

class TokenDefinitionBuilder : public std::enable_shared_from_this<TokenDefinitionBuilder> {
   private:
    enum class MatcherType {
        SINGLE,
        ARB_NUM,
        OPTIONAL,
        POS_NUM,
    };
    struct MatcherInfo {
        MatcherType type;
        CharSet set;
        std::shared_ptr<TokenDefinitionList> definitions = std::make_shared<TokenDefinitionList>();
    };

    std::string _name;
    int _priority = 0;
    std::vector<MatcherInfo> segs;

    std::shared_ptr<Matcher> withSubdefinitions(std::shared_ptr<Matcher> matcher,
                                                std::shared_ptr<Matcher> next,
                                                std::shared_ptr<TokenDefinitionList> subdefinitions) {
        auto multiBranchMatcher = std::make_shared<MultiBranchMatcher>();
        multiBranchMatcher->matchers.push_back(matcher);
        for (auto subdefinition : *subdefinitions) {
            subdefinition->end->next = next;
            multiBranchMatcher->matchers.push_back(subdefinition->begin);
        }
        return multiBranchMatcher;
    }

   public:
    std::shared_ptr<TokenDefinitionBuilder> name(std::string name) {
        this->_name = name;
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> priority(int priority) {
        this->_priority = priority;
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> appendOne() {
        MatcherInfo info;
        info.type = MatcherType::SINGLE;
        info.set = std::make_shared<std::vector<char>>();
        segs.push_back(info);
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> appendArbitraryNumberOf() {
        MatcherInfo info;
        info.type = MatcherType::ARB_NUM;
        info.set = std::make_shared<std::vector<char>>();
        segs.push_back(info);
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> appendOneOptional() {
        MatcherInfo info;
        info.type = MatcherType::OPTIONAL;
        info.set = std::make_shared<std::vector<char>>();
        segs.push_back(info);
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> appendOneOrMore() {
        MatcherInfo info;
        info.type = MatcherType::POS_NUM;
        info.set = std::make_shared<std::vector<char>>();
        segs.push_back(info);
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> character(char c) {
        assert(segs.size() > 0);
        auto& set = segs.crbegin()->set;
        set->push_back(c);
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> letter() {
        for (char c = 'a'; c <= 'z'; c++)
            character(c);
        for (char c = 'A'; c <= 'Z'; c++)
            character(c);
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> number() {
        for (char c = '0'; c <= '9'; c++)
            character(c);
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> whitespace() {
        character(' ');
        character('\t');
        character('\n');
        character('\r');
        character('\v');
        character('\f');
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> appendString(std::string str) {
        for (auto c : str) {
            appendOne();
            character(c);
        }
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> anyCharacter() {
        for (char c = 0; c < 127; c++)
            character(c);
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> except(char c) {
        assert(segs.size() > 0);
        auto& set = segs.crbegin()->set;
        set->erase(std::find(set->begin(), set->end(), c));
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinitionBuilder> subDefinition(std::shared_ptr<TokenDefinition> def) {
        assert(segs.size() > 0);
        auto& defs = segs.rbegin()->definitions;
        defs->push_back(def);
        return shared_from_this();
    }

    std::shared_ptr<TokenDefinition> build() {
        std::shared_ptr<EndMatcher> last = std::make_shared<EndMatcher>();
        std::shared_ptr<Matcher> curr = last;
        auto matchers = std::make_shared<MatcherList>();
        matchers->push_back(last);
        while (!segs.empty()) {
            auto last = *segs.rbegin();
            segs.pop_back();
            std::shared_ptr<Matcher> next;
            std::shared_ptr<MultiBranchMatcher> temp1, temp2;
            switch (last.type) {
                case MatcherType::SINGLE:
                    next = std::make_shared<SingleCharMatcher>(last.set, curr);
                    next = withSubdefinitions(next, curr, last.definitions);
                    break;
                case MatcherType::ARB_NUM:
                    temp2 = std::make_shared<MultiBranchMatcher>();
                    temp2->matchers.push_back(curr);
                    next = std::make_shared<SingleCharMatcher>(last.set, temp2);
                    next = withSubdefinitions(next, temp2, last.definitions);
                    temp2->matchers.push_back(next);
                    temp1 = std::make_shared<MultiBranchMatcher>();
                    temp1->matchers.push_back(next);
                    temp1->matchers.push_back(curr);
                    next = temp1;
                    break;
                case MatcherType::OPTIONAL:
                    next = std::make_shared<SingleCharMatcher>(last.set, curr);
                    next = withSubdefinitions(next, curr, last.definitions);
                    temp1 = std::make_shared<MultiBranchMatcher>();
                    temp1->matchers.push_back(next);
                    temp1->matchers.push_back(curr);
                    next = temp1;
                    break;
                case MatcherType::POS_NUM:
                    temp1 = std::make_shared<MultiBranchMatcher>();
                    temp1->matchers.push_back(curr);
                    next = std::make_shared<SingleCharMatcher>(last.set, temp1);
                    next = withSubdefinitions(next, temp1, last.definitions);
                    temp1->matchers.push_back(next);
                    break;
                default:
                    throw std::runtime_error("Impossible control flow!");
                    break;
            }
            curr = next;
            matchers->push_back(curr);
        }
        return std::make_shared<TokenDefinition>(_name, matchers, curr, last, _priority);
    }
};

struct Token {
    std::shared_ptr<TokenDefinition> definition;
    std::string matched;
    int start_row = -1;
    int start_col = -1;
};

using TokenList = std::vector<Token>;

class Tokenizer {
   private:
    std::shared_ptr<TokenDefinitionList> definitions;

   public:
    Tokenizer(std::shared_ptr<TokenDefinitionList> definitions)
        : definitions(definitions) {
    }

    std::shared_ptr<TokenList> parse(std::string str, std::string filename) {
        // std::cerr << "Tokenizer input: '" << str << "'" << std::endl; // Commented out
        ScanContext scanContext(filename);
        auto machine = StatusMachine(definitions, scanContext);
        std::string currentTokenStr;
        auto tokenList = std::make_shared<TokenList>();

        int currentTokenStartRow = scanContext.row;
        int currentTokenStartCol = scanContext.column;
        bool buildingToken = false;  // Flag to track if we are actively building a token

        machine.onCharAcceptted.AddListener([&](char c) {
            if (!buildingToken) {  // Start of a new token
                currentTokenStartRow = scanContext.row;
                currentTokenStartCol = scanContext.column;
                buildingToken = true;
            }
            currentTokenStr += c;
            // Update position *after* processing char, before potential commit
            // Column update happens later in the main loop
        });

        machine.onTokenCommitted.AddListener([&](std::shared_ptr<TokenDefinition> committedTokenDef) {
            Token token;
            token.definition = committedTokenDef;
            token.matched = currentTokenStr;
            token.start_row = currentTokenStartRow;
            token.start_col = currentTokenStartCol;
            // Only add non-whitespace/comment tokens to the final list for the parser
            if (committedTokenDef->name != "WHITESPACE" && committedTokenDef->name != "COMMENT") {
                tokenList->push_back(token);
            }
            // std::cerr << "Committed token: " << token.definition->name << " -> '" << token.matched << "'" << std::endl; // Commented out
            currentTokenStr.clear();
            buildingToken = false;  // Reset for the next token
            // Start position for the *next* token will be set when its first char is accepted
        });

        for (char c : str) {
            // std::cerr << "Processing char: '" << c << "'" << std::endl; // Commented out

            // Remember position *before* machine potentially accepts it
            int charRow = scanContext.row;
            int charCol = scanContext.column;
            if (!buildingToken) {  // If not building, this char *might* start a token
                currentTokenStartRow = charRow;
                currentTokenStartCol = charCol;
            }

            // Process the character
            while (!machine.next(c))
                ;  // Loop if char wasn't accepted but caused a commit

            // Update scan context position *after* processing the character
            if (c == '\n') {
                scanContext.row++;
                scanContext.column = 1;  // Reset column to 1 for the new line
            } else {
                scanContext.column++;  // Increment column for non-newline chars
            }
        }
        machine.next('\0');  // Process EOF to commit any final token
        // std::cerr << "Tokenizer produced " << tokenList->size() << " tokens" << std::endl; // Commented out
        return tokenList;
    }
};

// Define the tokenizer
std::pair<std::shared_ptr<Tokenizer>, std::shared_ptr<TokenDefinitionList>> createTokenizer();

}  // namespace Tokenizer
