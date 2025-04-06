#pragma once
#include <memory>
#include <string>
#include <vector>
#include "tokenizer.hpp"

namespace AST {

// Base class for all AST nodes
class Node {
   public:
    virtual ~Node() = default;
    virtual std::string toString() const = 0;
};

// Terminal node - represents a token
class TerminalNode : public Node {
   public:
    std::shared_ptr<Tokenizer::Token> token;

    TerminalNode(std::shared_ptr<Tokenizer::Token> token) : token(token) {}

    std::string toString() const override {
        return token->matched;
    }
};

// Non-terminal node - represents a production rule
class NonTerminalNode : public Node {
   public:
    std::string name;
    std::vector<std::shared_ptr<Node>> children;

    NonTerminalNode(const std::string& name) : name(name) {}

    void addChild(std::shared_ptr<Node> child) {
        children.push_back(child);
    }

    std::string toString() const override {
        std::string result = name + "(";
        for (size_t i = 0; i < children.size(); ++i) {
            if (i > 0) result += ", ";
            result += children[i]->toString();
        }
        result += ")";
        return result;
    }
};

}  // namespace AST 