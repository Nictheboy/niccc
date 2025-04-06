#include <memory>
#include "parser.hpp"
#include "tokenizer.hpp"

namespace Parser {

// Helper function to create a production rule builder
std::shared_ptr<ProductionRuleBuilder> rule() {
    return std::make_shared<ProductionRuleBuilder>();
}

// Helper function to create a token definition
std::shared_ptr<Tokenizer::TokenDefinition> token(const std::string& name) {
    return std::make_shared<Tokenizer::TokenDefinition>(name, nullptr, nullptr, nullptr, 0);
}

// Define the grammar rules
std::shared_ptr<ProductionRuleList> createGrammarRules() {
    auto rules = std::make_shared<ProductionRuleList>();

    // CompUnit → {Decl} {FuncDef} MainFuncDef
    rules->push_back(rule()
                         ->name("CompUnit")
                         ->symbol(token("Decl"))
                         ->symbol(token("FuncDef"))
                         ->symbol(token("MainFuncDef"))
                         ->priority(0)
                         ->build());

    // Decl → ConstDecl | VarDecl
    rules->push_back(rule()
                         ->name("Decl")
                         ->symbol(token("ConstDecl"))
                         ->priority(1)
                         ->build());
    rules->push_back(rule()
                         ->name("Decl")
                         ->symbol(token("VarDecl"))
                         ->priority(1)
                         ->build());

    // ConstDecl → 'const' BType ConstDef { ',' ConstDef } ';'
    rules->push_back(rule()
                         ->name("ConstDecl")
                         ->symbol(token("CONSTTK"))
                         ->symbol(token("BType"))
                         ->symbol(token("ConstDef"))
                         ->symbol(token("SEMICN"))
                         ->priority(2)
                         ->build());

    // BType → 'int'
    rules->push_back(rule()
                         ->name("BType")
                         ->symbol(token("INTTK"))
                         ->priority(3)
                         ->build());

    // ConstDef → Ident [ '[' ConstExp ']' ] '=' ConstInitVal
    rules->push_back(rule()
                         ->name("ConstDef")
                         ->symbol(token("IDENFR"))
                         ->symbol(token("ASSIGN"))
                         ->symbol(token("ConstInitVal"))
                         ->priority(4)
                         ->build());

    // ConstInitVal → ConstExp
    rules->push_back(rule()
                         ->name("ConstInitVal")
                         ->symbol(token("ConstExp"))
                         ->priority(5)
                         ->build());

    // VarDecl → BType VarDef { ',' VarDef } ';'
    rules->push_back(rule()
                         ->name("VarDecl")
                         ->symbol(token("BType"))
                         ->symbol(token("VarDef"))
                         ->symbol(token("SEMICN"))
                         ->priority(6)
                         ->build());

    // VarDef → Ident [ '[' ConstExp ']' ] [ '=' InitVal ]
    rules->push_back(rule()
                         ->name("VarDef")
                         ->symbol(token("IDENFR"))
                         ->priority(7)
                         ->build());
    rules->push_back(rule()
                         ->name("VarDef")
                         ->symbol(token("IDENFR"))
                         ->symbol(token("ASSIGN"))
                         ->symbol(token("InitVal"))
                         ->priority(7)
                         ->build());

    // InitVal → Exp
    rules->push_back(rule()
                         ->name("InitVal")
                         ->symbol(token("Exp"))
                         ->priority(8)
                         ->build());

    // FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
    rules->push_back(rule()
                         ->name("FuncDef")
                         ->symbol(token("FuncType"))
                         ->symbol(token("IDENFR"))
                         ->symbol(token("LPARENT"))
                         ->symbol(token("RPARENT"))
                         ->symbol(token("Block"))
                         ->priority(9)
                         ->build());

    // MainFuncDef → 'int' 'main' '(' ')' Block
    rules->push_back(rule()
                         ->name("MainFuncDef")
                         ->symbol(token("INTTK"))
                         ->symbol(token("MAINTK"))
                         ->symbol(token("LPARENT"))
                         ->symbol(token("RPARENT"))
                         ->symbol(token("Block"))
                         ->priority(10)
                         ->build());

    // Block → '{' {BlockItem} '}'
    rules->push_back(rule()
                         ->name("Block")
                         ->symbol(token("LBRACE"))
                         ->symbol(token("RBRACE"))
                         ->priority(11)
                         ->build());
    rules->push_back(rule()
                         ->name("Block")
                         ->symbol(token("LBRACE"))
                         ->symbol(token("BlockItem"))
                         ->symbol(token("RBRACE"))
                         ->priority(11)
                         ->build());

    // BlockItem → Decl | Stmt
    rules->push_back(rule()
                         ->name("BlockItem")
                         ->symbol(token("Decl"))
                         ->priority(12)
                         ->build());
    rules->push_back(rule()
                         ->name("BlockItem")
                         ->symbol(token("Stmt"))
                         ->priority(12)
                         ->build());

    // Stmt → LVal '=' Exp ';' | [Exp] ';' | Block | 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
    rules->push_back(rule()
                         ->name("Stmt")
                         ->symbol(token("LVal"))
                         ->symbol(token("ASSIGN"))
                         ->symbol(token("Exp"))
                         ->symbol(token("SEMICN"))
                         ->priority(13)
                         ->build());
    rules->push_back(rule()
                         ->name("Stmt")
                         ->symbol(token("SEMICN"))
                         ->priority(13)
                         ->build());
    rules->push_back(rule()
                         ->name("Stmt")
                         ->symbol(token("Block"))
                         ->priority(13)
                         ->build());

    // LVal → Ident [ '[' Exp ']' ]
    rules->push_back(rule()
                         ->name("LVal")
                         ->symbol(token("IDENFR"))
                         ->priority(14)
                         ->build());

    // Exp → AddExp
    rules->push_back(rule()
                         ->name("Exp")
                         ->symbol(token("AddExp"))
                         ->priority(15)
                         ->build());

    // AddExp → MulExp | AddExp ('+' | '-') MulExp
    rules->push_back(rule()
                         ->name("AddExp")
                         ->symbol(token("MulExp"))
                         ->priority(16)
                         ->build());

    // MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
    rules->push_back(rule()
                         ->name("MulExp")
                         ->symbol(token("UnaryExp"))
                         ->priority(17)
                         ->build());

    // UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
    rules->push_back(rule()
                         ->name("UnaryExp")
                         ->symbol(token("PrimaryExp"))
                         ->priority(18)
                         ->build());
    rules->push_back(rule()
                         ->name("UnaryExp")
                         ->symbol(token("GETINTTK"))
                         ->symbol(token("LPARENT"))
                         ->symbol(token("RPARENT"))
                         ->priority(18)
                         ->build());

    // PrimaryExp → '(' Exp ')' | LVal | Number
    rules->push_back(rule()
                         ->name("PrimaryExp")
                         ->symbol(token("LVal"))
                         ->priority(19)
                         ->build());

    return rules;
}

}  // namespace Parser