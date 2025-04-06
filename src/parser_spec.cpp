#include <memory>
#include "parser.hpp"
#include "tokenizer.hpp"

namespace Parser {

// Helper function to create a token definition
std::shared_ptr<Tokenizer::TokenDefinition> token(const std::string& name) {
    return std::make_shared<Tokenizer::TokenDefinition>(name, nullptr, nullptr, nullptr, 0);
}

// Define the grammar rules
std::shared_ptr<ProductionRuleList> createGrammarRules() {
    auto rules = std::make_shared<ProductionRuleList>();

    // CompUnit → MainFuncDef
    rules->push_back(std::make_shared<ProductionRule>(
        "CompUnit",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MainFuncDef")},
        0,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // MainFuncDef → FuncType 'main' '(' ')' Block
    rules->push_back(std::make_shared<ProductionRule>(
        "MainFuncDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("FuncType"),
            token("MAINTK"),
            token("LPARENT"),
            token("RPARENT"),
            token("Block")},
        1,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // FuncType → 'int'
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncType",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("INTTK")},
        2,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // Block → '{' BlockItems '}'
    rules->push_back(std::make_shared<ProductionRule>(
        "Block",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("BlockItems"),
            token("RBRACE")},
        3,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // BlockItems → BlockItem BlockItems | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItems",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BlockItem"),
            token("BlockItems")},
        4,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItems",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{},
        4,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // BlockItem → Decl | Stmt
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItem",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Decl")},
        5,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItem",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Stmt")},
        5,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // Decl → VarDecl
    rules->push_back(std::make_shared<ProductionRule>(
        "Decl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("VarDecl")},
        6,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // VarDecl → BType VarDef ';'
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDecl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BType"),
            token("VarDef"),
            token("SEMICN")},
        7,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // BType → 'int'
    rules->push_back(std::make_shared<ProductionRule>(
        "BType",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("INTTK")},
        8,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // VarDef → Ident | Ident '=' Exp
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR")},
        9,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("ASSIGN"),
            token("Exp")},
        9,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // Stmt → LVal '=' Exp ';' | Block | 'return' Exp ';' | 'printf' '(' StringExp ',' Exp ')' ';'
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LVal"),
            token("ASSIGN"),
            token("Exp"),
            token("SEMICN")},
        10,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Block")},
        10,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RETURNTK"),
            token("Exp"),
            token("SEMICN")},
        10,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("PRINTFTK"),
            token("LPARENT"),
            token("STRCON"),
            token("COMMA"),
            token("Exp"),
            token("RPARENT"),
            token("SEMICN")},
        10,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // LVal → Ident
    rules->push_back(std::make_shared<ProductionRule>(
        "LVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR")},
        11,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // Exp → AddExp
    rules->push_back(std::make_shared<ProductionRule>(
        "Exp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp")},
        12,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // AddExp → MulExp
    rules->push_back(std::make_shared<ProductionRule>(
        "AddExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MulExp")},
        13,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // MulExp → UnaryExp
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("UnaryExp")},
        14,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // UnaryExp → PrimaryExp | 'getint' '(' ')'
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("PrimaryExp")},
        15,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("GETINTTK"),
            token("LPARENT"),
            token("RPARENT")},
        15,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    // PrimaryExp → LVal
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LVal")},
        16,
        [](std::vector<std::shared_ptr<Tokenizer::Token>>& tokens) {}));

    return rules;
}

}  // namespace Parser