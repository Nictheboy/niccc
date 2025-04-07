#include <map>
#include <memory>
#include "parser.hpp"
#include "tokenizer.hpp"

namespace Parser {

// Define the static member here
std::shared_ptr<Tokenizer::TokenDefinition> Parser::EOF_DEFINITION =
    std::make_shared<Tokenizer::TokenDefinition>("$EOF$", nullptr, nullptr, nullptr, -1);

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
        "START",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("CompUnit"),
            Parser::EOF_DEFINITION}));
    // --- Augmented Start Rule --- END

    // CompUnit → CompUnitOpt
    rules->push_back(std::make_shared<ProductionRule>(
        "CompUnit",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("CompUnitOpt")}));

    // CompUnitOpt → {Decl} {FuncDef} MainFuncDef
    rules->push_back(std::make_shared<ProductionRule>(
        "CompUnitOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Decl"),
            token("CompUnitOpt")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "CompUnitOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("FuncDef"),
            token("CompUnitOpt")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "CompUnitOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MainFuncDef")}));

    // Decl → ConstDecl | VarDecl
    rules->push_back(std::make_shared<ProductionRule>(
        "Decl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("ConstDecl")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Decl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("VarDecl")}));

    // Type → INTTK | VOIDTK
    rules->push_back(std::make_shared<ProductionRule>(
        "Type",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("INTTK")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Type",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("VOIDTK")}));

    // ConstDecl → 'const' Type ConstDef ConstDeclList ';'
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstDecl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("CONSTTK"),
            token("Type"),
            token("ConstDef"),
            token("ConstDeclList"),
            token("SEMICN")}));

    // ConstDeclList → ',' ConstDef ConstDeclList | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstDeclList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("COMMA"),
            token("ConstDef"),
            token("ConstDeclList")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstDeclList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));

    // ConstDef → Ident [ '[' ConstExp ']' ] '=' ConstInitVal
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("ASSIGN"),
            token("ConstInitVal")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LBRACK"),
            token("ConstExp"),
            token("RBRACK"),
            token("ASSIGN"),
            token("ConstInitVal")}));

    // ConstInitVal → ConstExp
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstInitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("ConstExp")}));

    // ConstInitVal → '{' ConstInitValListOpt '}'
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstInitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("ConstInitValListOpt"),
            token("RBRACE")}));

    // ConstInitValListOpt -> ConstInitVal ConstInitValList | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstInitValListOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("ConstInitVal"),
            token("ConstInitValList")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstInitValListOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));

    // ConstInitValList -> ',' ConstInitVal ConstInitValList | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstInitValList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("COMMA"),
            token("ConstInitVal"),
            token("ConstInitValList")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstInitValList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));

    // VarDecl → Type VarDef VarDeclList SEMICN
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDecl",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Type"),
            token("VarDef"),
            token("VarDeclList"),
            token("SEMICN")}));

    // VarDeclList → ',' VarDef VarDeclList | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDeclList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("COMMA"),
            token("VarDef"),
            token("VarDeclList")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDeclList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));

    // VarDef → Ident [ '[' ConstExp ']' ]
    //        | Ident [ '[' ConstExp ']' ] '=' InitVal
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LBRACK"),
            token("ConstExp"),
            token("RBRACK")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("ASSIGN"),
            token("InitVal")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LBRACK"),
            token("ConstExp"),
            token("RBRACK"),
            token("ASSIGN"),
            token("InitVal")}));

    // InitVal → Exp
    rules->push_back(std::make_shared<ProductionRule>(
        "InitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Exp")}));

    // InitVal → '{' InitValListOpt '}'
    rules->push_back(std::make_shared<ProductionRule>(
        "InitVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("InitValListOpt"),
            token("RBRACE")}));

    // InitValListOpt -> InitVal InitValList | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "InitValListOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("InitVal"),
            token("InitValList")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "InitValListOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));

    // InitValList -> ',' InitVal InitValList | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "InitValList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("COMMA"),
            token("InitVal"),
            token("InitValList")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "InitValList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));

    // FuncDef → Type Ident '(' [FuncFParams] ')' Block
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Type"),
            token("IDENFR"),
            token("LPARENT"),
            token("RPARENT"),
            token("Block")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Type"),
            token("IDENFR"),
            token("LPARENT"),
            token("FuncFParams"),
            token("RPARENT"),
            token("Block")}));

    // MainFuncDef → 'int' 'main' '(' ')' Block
    rules->push_back(std::make_shared<ProductionRule>(
        "MainFuncDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("INTTK"),
            token("MAINTK"),
            token("LPARENT"),
            token("RPARENT"),
            token("Block")}));

    // FuncFParams → FuncFParam FuncFParamList
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParams",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("FuncFParam"),
            token("FuncFParamList")}));

    // FuncFParamList → ',' FuncFParam FuncFParamList | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParamList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("COMMA"),
            token("FuncFParam"),
            token("FuncFParamList")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParamList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));

    // FuncFParam → Type Ident
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParam",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Type"),
            token("IDENFR")}));

    // --- Revised Block Grammar --- START
    // Block → '{' BlockItemsOpt '}'
    rules->push_back(std::make_shared<ProductionRule>(
        "Block",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("BlockItemsOpt"),  // Use the optional list non-terminal
            token("RBRACE")}));

    // BlockItemsOpt → BlockItem BlockItemsOpt | <epsilon>
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItemsOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BlockItem"),
            token("BlockItemsOpt")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItemsOpt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));
    // --- Revised Block Grammar --- END

    // BlockItem → Decl | Stmt
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItem",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Decl")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItem",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Stmt")}));

    // Stmt → LVal '=' Exp ';'
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LVal"),
            token("ASSIGN"),
            token("Exp"),
            token("SEMICN")}));
    // Stmt → [Exp] ';'
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Exp"),
            token("SEMICN")}));
    // Stmt → Block
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Block")}));
    // Stmt → 'if' '(' Cond ')' Stmt [ 'else' Stmt ] (Dangling else ambiguity)
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IFTK"),
            token("LPARENT"),
            token("Cond"),
            token("RPARENT"),
            token("Stmt")},
        1));  // Priority 1 for if-then
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
        2));  // Priority 2 for if-then-else
    // Stmt → 'while' '(' Cond ')' Stmt
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("WHILETK"),
            token("LPARENT"),
            token("Cond"),
            token("RPARENT"),
            token("Stmt")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BREAKTK"),
            token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("CONTINUETK"),
            token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RETURNTK"),
            token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RETURNTK"),
            token("Exp"),
            token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LVal"),
            token("ASSIGN"),
            token("GETINTTK"),
            token("LPARENT"),
            token("RPARENT"),
            token("SEMICN")}));
    // Stmt → PrintfStmt
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("PrintfStmt")}));

    // PrintfStmt → 'printf' '(' [StrConst] ')' ';'
    rules->push_back(std::make_shared<ProductionRule>(
        "PrintfStmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("PRINTFTK"),
            token("LPARENT"),
            token("STRCON"),
            token("RPARENT"),
            token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "PrintfStmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("PRINTFTK"),
            token("LPARENT"),
            token("STRCON"),
            token("COMMA"),
            token("FuncRParams"),
            token("RPARENT"),
            token("SEMICN")}));

    // Exp → AddExp
    rules->push_back(std::make_shared<ProductionRule>(
        "Exp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp")}));

    // Cond → LOrExp
    rules->push_back(std::make_shared<ProductionRule>(
        "Cond",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LOrExp")}));

    // LVal → Ident ['[' Exp ']']
    rules->push_back(std::make_shared<ProductionRule>(
        "LVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "LVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LBRACK"),
            token("Exp"),
            token("RBRACK")}));

    // PrimaryExp → '(' Exp ')' | LVal | Number | STRCON
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LPARENT"),
            token("Exp"),
            token("RPARENT")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LVal")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Number")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("STRCON")}));

    // Number → IntConst
    rules->push_back(std::make_shared<ProductionRule>(
        "Number",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("INTCON")}));

    // UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("PrimaryExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LPARENT"),
            token("RPARENT")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LPARENT"),
            token("FuncRParams"),
            token("RPARENT")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("UnaryOp"),
            token("UnaryExp")}));

    // UnaryOp → '+' | '−' | '!'
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryOp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("PLUS")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryOp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MINU")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryOp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("NOT")}));

    // FuncRParams → Exp FuncRParamList
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncRParams",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Exp"),
            token("FuncRParamList")}));

    // FuncRParamList → ',' Exp FuncRParamList | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncRParamList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("COMMA"),
            token("Exp"),
            token("FuncRParamList")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncRParamList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));

    // MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("UnaryExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MulExp"),
            token("MULT"),
            token("UnaryExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MulExp"),
            token("DIV"),
            token("UnaryExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MulExp"),
            token("MOD"),
            token("UnaryExp")}));

    // AddExp → MulExp | AddExp ('+' | '−') MulExp
    rules->push_back(std::make_shared<ProductionRule>(
        "AddExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("MulExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "AddExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp"),
            token("PLUS"),
            token("MulExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "AddExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp"),
            token("MINU"),
            token("MulExp")}));

    // RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RelExp"),
            token("LSS"),
            token("AddExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RelExp"),
            token("GRE"),
            token("AddExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RelExp"),
            token("LEQ"),
            token("AddExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RelExp"),
            token("GEQ"),
            token("AddExp")}));

    // EqExp → RelExp | EqExp ('==' | '!=') RelExp
    rules->push_back(std::make_shared<ProductionRule>(
        "EqExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RelExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "EqExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("EqExp"),
            token("EQL"),
            token("RelExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "EqExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("EqExp"),
            token("NEQ"),
            token("RelExp")}));

    // LAndExp → EqExp | LAndExp '&&' EqExp
    rules->push_back(std::make_shared<ProductionRule>(
        "LAndExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("EqExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "LAndExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LAndExp"),
            token("AND"),
            token("EqExp")}));

    // LOrExp → LAndExp | LOrExp '||' LAndExp
    rules->push_back(std::make_shared<ProductionRule>(
        "LOrExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LAndExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "LOrExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LOrExp"),
            token("OR"),
            token("LAndExp")}));

    // ConstExp → AddExp
    rules->push_back(std::make_shared<ProductionRule>(
        "ConstExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("AddExp")}));

    return rules;
}

}  // namespace Parser
