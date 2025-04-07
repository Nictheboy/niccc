#include <map>
#include <memory>
#include "parser.hpp"
#include "tokenizer.hpp"

namespace Parser {

// Map to store canonical TokenDefinition pointers for grammar symbols
static std::map<std::string, std::shared_ptr<Tokenizer::TokenDefinition>> grammarSymbolCache;

// Helper function to get/create canonical token definition for grammar rules
std::shared_ptr<Tokenizer::TokenDefinition> token(const std::string& name) {
    // Check cache first
    auto it = grammarSymbolCache.find(name);
    if (it != grammarSymbolCache.end()) {
        return it->second;  // Return cached pointer
    }
    // Create new one if not found, store it, and return
    // Note: This creates dummy TokenDefinitions just for rule matching & symbol identity
    auto newDef = std::make_shared<Tokenizer::TokenDefinition>(name, nullptr, nullptr, nullptr, 0);
    grammarSymbolCache[name] = newDef;  // Store in cache
    return newDef;
}

// Define the grammar rules
std::shared_ptr<ProductionRuleList> createGrammarRules() {
    // Clear cache at the start of creation if this function might be called multiple times
    // (Normally called only once, so often unnecessary but safe)
    grammarSymbolCache.clear();

    // Ensure EOF is in the cache if it were referenced by name via token() helper
    // We currently use Parser::EOF_DEFINITION directly, so this isn't strictly needed
    // grammarSymbolCache[Parser::EOF_DEFINITION->name] = Parser::EOF_DEFINITION;

    auto rules = std::make_shared<ProductionRuleList>();

    // --- Augmented Start Rule --- START
    rules->push_back(std::make_shared<ProductionRule>(
        "START",  // Augmented start symbol
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("CompUnit"),
            Parser::EOF_DEFINITION  // Use the static EOF definition
        },
        -1  // Highest priority (or special value)
        ));
    // --- Augmented Start Rule --- END

    // CompUnit → {Decl} {FuncDef} MainFuncDef
    rules->push_back(std::make_shared<ProductionRule>(
        "CompUnit",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Decl"),
            token("CompUnit")},
        0));
    rules->push_back(std::make_shared<ProductionRule>(
        "CompUnit",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("FuncDef"),
            token("CompUnit")},
        0));
    rules->push_back(std::make_shared<ProductionRule>(
        "CompUnit",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MainFuncDef")},
        0));

    // Decl → ConstDecl | VarDecl
    rules->push_back(std::make_shared<ProductionRule>(
        "Decl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("ConstDecl")},
        1));
    rules->push_back(std::make_shared<ProductionRule>(
        "Decl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("VarDecl")},
        1));

    // ConstDecl → 'const' BType ConstDef { ',' ConstDef } ';'
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstDecl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("CONSTTK"),
            token("BType"),
            token("ConstDef"),
            token("SEMICN")},
        2));
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstDecl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("CONSTTK"),
            token("BType"),
            token("ConstDef"),
            token("COMMA"),
            token("ConstDef"),
            token("SEMICN")},
        2));

    // BType → 'int'
    rules->push_back(std::make_shared<ProductionRule>(
        "BType",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("INTTK")},
        3));

    // ConstDef → Ident [ '[' ConstExp ']' ] '=' ConstInitVal
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("ASSIGN"),
            token("ConstInitVal")},
        4));
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LBRACK"),
            token("ConstExp"),
            token("RBRACK"),
            token("ASSIGN"),
            token("ConstInitVal")},
        4));

    // ConstInitVal → ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstInitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("ConstExp")},
        5));
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstInitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("RBRACE")},
        5));
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstInitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("ConstInitVal"),
            token("RBRACE")},
        5));
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstInitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("ConstInitVal"),
            token("COMMA"),
            token("ConstInitVal"),
            token("RBRACE")},
        5));

    // VarDecl → BType VarDef { ',' VarDef } ';'
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDecl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BType"),
            token("VarDef"),
            token("SEMICN")},
        6));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDecl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BType"),
            token("VarDef"),
            token("COMMA"),
            token("VarDef"),
            token("SEMICN")},
        6));

    // VarDef → Ident [ '[' ConstExp ']' ] | Ident [ '[' ConstExp ']' ] '=' InitVal
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR")},
        7));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LBRACK"),
            token("ConstExp"),
            token("RBRACK")},
        7));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("ASSIGN"),
            token("InitVal")},
        7));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LBRACK"),
            token("ConstExp"),
            token("RBRACK"),
            token("ASSIGN"),
            token("InitVal")},
        7));

    // InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
    rules->push_back(std::make_shared<ProductionRule>(
        "InitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Exp")},
        8));
    rules->push_back(std::make_shared<ProductionRule>(
        "InitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("RBRACE")},
        8));
    rules->push_back(std::make_shared<ProductionRule>(
        "InitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("InitVal"),
            token("RBRACE")},
        8));
    rules->push_back(std::make_shared<ProductionRule>(
        "InitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("InitVal"),
            token("COMMA"),
            token("InitVal"),
            token("RBRACE")},
        8));

    // FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("FuncType"),
            token("IDENFR"),
            token("LPARENT"),
            token("RPARENT"),
            token("Block")},
        9));
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("FuncType"),
            token("IDENFR"),
            token("LPARENT"),
            token("FuncFParams"),
            token("RPARENT"),
            token("Block")},
        9));

    // MainFuncDef → 'int' 'main' '(' ')' Block
    rules->push_back(std::make_shared<ProductionRule>(
        "MainFuncDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("INTTK"),
            token("MAINTK"),
            token("LPARENT"),
            token("RPARENT"),
            token("Block")},
        10));

    // FuncType → 'void' | 'int'
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncType",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("VOIDTK")},
        11));
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncType",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("INTTK")},
        11));

    // FuncFParams → FuncFParam { ',' FuncFParam }
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParams",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("FuncFParam")},
        12));
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParams",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("FuncFParam"),
            token("COMMA"),
            token("FuncFParam")},
        12));

    // FuncFParam → BType Ident
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParam",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BType"),
            token("IDENFR")},
        13));

    // --- Revised Block Grammar --- START
    // Block → '{' BlockItemsOpt '}'
    rules->push_back(std::make_shared<ProductionRule>(
        "Block",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("BlockItemsOpt"),  // Use the optional list non-terminal
            token("RBRACE")},
        14));  // Keep original priority/index if desired

    // BlockItemsOpt → BlockItem BlockItemsOpt | <epsilon>
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItemsOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BlockItem"),
            token("BlockItemsOpt")},
        14));  // Assign a priority/index (e.g., same as Block or new)
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItemsOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{},
        // Empty vector represents epsilon production
        14));  // Assign a priority/index
    // --- Revised Block Grammar --- END

    // BlockItem → Decl | Stmt
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItem",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Decl")},
        15));
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItem",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Stmt")},
        15));

    // Stmt → LVal '=' Exp ';' | [Exp] ';' | Block | 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
    // | 'while' '(' Cond ')' Stmt | 'break' ';' | 'continue' ';' | 'return' [Exp] ';'
    // | LVal = 'getint''('')'';' | 'printf' '('FormatString {',' Exp} ')'';'
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LVal"),
            token("ASSIGN"),
            token("Exp"),
            token("SEMICN")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("SEMICN")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Exp"),
            token("SEMICN")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Block")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IFTK"),
            token("LPARENT"),
            token("Cond"),
            token("RPARENT"),
            token("Stmt")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IFTK"),
            token("LPARENT"),
            token("Cond"),
            token("RPARENT"),
            token("Stmt"),
            token("ELSETK"),
            token("Stmt")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("WHILETK"),
            token("LPARENT"),
            token("Cond"),
            token("RPARENT"),
            token("Stmt")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BREAKTK"),
            token("SEMICN")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("CONTINUETK"),
            token("SEMICN")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RETURNTK"),
            token("SEMICN")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RETURNTK"),
            token("Exp"),
            token("SEMICN")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LVal"),
            token("ASSIGN"),
            token("GETINTTK"),
            token("LPARENT"),
            token("RPARENT"),
            token("SEMICN")},
        16));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("PRINTFTK"),
            token("LPARENT"),
            token("STRCON"),
            token("RPARENT"),
            token("SEMICN")},
        16));
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
        16));

    // Exp → AddExp
    rules->push_back(std::make_shared<ProductionRule>(
        "Exp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp")},
        17));

    // Cond → LOrExp
    rules->push_back(std::make_shared<ProductionRule>(
        "Cond",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LOrExp")},
        18));

    // LVal → Ident ['[' Exp ']']
    rules->push_back(std::make_shared<ProductionRule>(
        "LVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR")},
        19));
    rules->push_back(std::make_shared<ProductionRule>(
        "LVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LBRACK"),
            token("Exp"),
            token("RBRACK")},
        19));

    // PrimaryExp → '(' Exp ')' | LVal | Number
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LPARENT"),
            token("Exp"),
            token("RPARENT")},
        20));
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LVal")},
        20));
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Number")},
        20));

    // Number → IntConst
    rules->push_back(std::make_shared<ProductionRule>(
        "Number",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("INTCON")},
        21));

    // UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("PrimaryExp")},
        22));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LPARENT"),
            token("RPARENT")},
        22));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LPARENT"),
            token("FuncRParams"),
            token("RPARENT")},
        22));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("UnaryOp"),
            token("UnaryExp")},
        22));

    // UnaryOp → '+' | '−' | '!'
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryOp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("PLUS")},
        23));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryOp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MINU")},
        23));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryOp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("NOT")},
        23));

    // FuncRParams → Exp { ',' Exp }
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncRParams",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Exp")},
        24));
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncRParams",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Exp"),
            token("COMMA"),
            token("Exp")},
        24));

    // MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("UnaryExp")},
        25));
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MulExp"),
            token("MULT"),
            token("UnaryExp")},
        25));
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MulExp"),
            token("DIV"),
            token("UnaryExp")},
        25));
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MulExp"),
            token("MOD"),
            token("UnaryExp")},
        25));

    // AddExp → MulExp | AddExp ('+' | '−') MulExp
    rules->push_back(std::make_shared<ProductionRule>(
        "AddExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MulExp")},
        26));
    rules->push_back(std::make_shared<ProductionRule>(
        "AddExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp"),
            token("PLUS"),
            token("MulExp")},
        26));
    rules->push_back(std::make_shared<ProductionRule>(
        "AddExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp"),
            token("MINU"),
            token("MulExp")},
        26));

    // RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp")},
        27));
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RelExp"),
            token("LSS"),
            token("AddExp")},
        27));
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RelExp"),
            token("GRE"),
            token("AddExp")},
        27));
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RelExp"),
            token("LEQ"),
            token("AddExp")},
        27));
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RelExp"),
            token("GEQ"),
            token("AddExp")},
        27));

    // EqExp → RelExp | EqExp ('==' | '!=') RelExp
    rules->push_back(std::make_shared<ProductionRule>(
        "EqExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RelExp")},
        28));
    rules->push_back(std::make_shared<ProductionRule>(
        "EqExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("EqExp"),
            token("EQL"),
            token("RelExp")},
        28));
    rules->push_back(std::make_shared<ProductionRule>(
        "EqExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("EqExp"),
            token("NEQ"),
            token("RelExp")},
        28));

    // LAndExp → EqExp | LAndExp '&&' EqExp
    rules->push_back(std::make_shared<ProductionRule>(
        "LAndExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("EqExp")},
        29));
    rules->push_back(std::make_shared<ProductionRule>(
        "LAndExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LAndExp"),
            token("AND"),
            token("EqExp")},
        29));

    // LOrExp → LAndExp | LOrExp '||' LAndExp
    rules->push_back(std::make_shared<ProductionRule>(
        "LOrExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LAndExp")},
        30));
    rules->push_back(std::make_shared<ProductionRule>(
        "LOrExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LOrExp"),
            token("OR"),
            token("LAndExp")},
        30));

    // ConstExp → AddExp
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp")},
        31));

    return rules;
}

}  // namespace Parser