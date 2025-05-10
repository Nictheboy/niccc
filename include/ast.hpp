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
    virtual std::string toString(int indent = 0) const = 0;
};

// Terminal node - represents a token
class TerminalNode : public Node {
   private:
    static std::string replaceAll(std::string str, const std::string& from, const std::string& to) {
        if (from.empty())
            return str;
        size_t start_pos = 0;
        while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
            str.replace(start_pos, from.length(), to);
            start_pos += to.length();  // 避免死循环
        }
        return str;
    }

   public:
    std::shared_ptr<Tokenizer::Token> token;

    TerminalNode(std::shared_ptr<Tokenizer::Token> token)
        : token(token) {}

    std::string toString([[maybe_unused]] int indent = 0) const override {
        std::string result = "\"";
        result += replaceAll(token->matched, "\"", "\\\"");
        result += "\"";
        return result;
    }
};

// Non-terminal node - represents a production rule
class NonTerminalNode : public Node {
   private:
    static std::string newline(int indent) {
        std::string result = "\n";
        for (int i = 0; i < indent; ++i)
            result += " ";
        return result;
    }

   public:
    std::string name;
    std::vector<std::shared_ptr<Node>> children;

    NonTerminalNode(const std::string& name)
        : name(name) {}

    void addChild(std::shared_ptr<Node> child) {
        children.push_back(child);
    }

    std::string toString(int indent = 0) const override {
        bool single_line = children.size() == 1;
        std::string result = name + "(";
        for (size_t i = 0; i < children.size(); ++i) {
            if (!single_line)
                result += newline(indent + 1);
            result += children[i]->toString(indent + (single_line ? 0 : 1));
            if (i < children.size() - 1)
                result += ", ";
        }
        if (!single_line)
            result += newline(indent);
        result += ")";
        return result;
    }
};

}  // namespace AST