#include <map>
#include <memory>
#include <string>         // 确保包含 string
#include <vector>         // 确保包含 vector
#include "parser.hpp"     // 假设 ProductionRule 和 ProductionRuleList 在这里定义
#include "tokenizer.hpp"  // 假设 TokenDefinition 和 Token 在这里定义

namespace Parser {

// Define the static member here
std::shared_ptr<Tokenizer::TokenDefinition> Parser::EOF_DEFINITION =
    std::make_shared<Tokenizer::TokenDefinition>("$EOF$", nullptr, nullptr, nullptr, -1);

// Map to store canonical TokenDefinition pointers for grammar symbols
// Key: Symbol name (string), Value: Shared pointer to TokenDefinition
static std::map<std::string, std::shared_ptr<Tokenizer::TokenDefinition>> grammarSymbolCache;

// Helper function to get/create canonical token definition for grammar rules
// Ensures that each grammar symbol (terminal or non-terminal) is represented
// by a single unique TokenDefinition object throughout the grammar rules.
std::shared_ptr<Tokenizer::TokenDefinition> token(const std::string& name) {
    // Check cache first
    auto it = grammarSymbolCache.find(name);
    if (it != grammarSymbolCache.end()) {
        return it->second;  // Return cached pointer if symbol already exists
    }

    // If not found, create a new dummy TokenDefinition.
    // For actual TERMINALS (like "INTTK", "IDENFR", "PLUS"), the parser generator
    // or runtime will need to map these names back to the actual TokenDefinitions
    // used by the Tokenizer. This cache primarily ensures consistent identity
    // for symbols *within* the grammar definition phase, especially for non-terminals.
    auto newDef = std::make_shared<Tokenizer::TokenDefinition>(name, nullptr, nullptr, nullptr, 0);
    grammarSymbolCache[name] = newDef;  // Store the new definition in the cache
    return newDef;
}

// Define the grammar rules based on the SysY specification or a similar C subset
std::shared_ptr<ProductionRuleList> createGrammarRules() {
    // Clear cache at the start of creation, especially if this function
    // could theoretically be called multiple times during setup.
    grammarSymbolCache.clear();

    // Pre-cache EOF if it were to be referenced by name via token()
    // grammarSymbolCache[Parser::EOF_DEFINITION->name] = Parser::EOF_DEFINITION;
    // Direct use of Parser::EOF_DEFINITION is generally clearer.

    auto rules = std::make_shared<ProductionRuleList>();

    // --- Augmented Start Rule ---
    // START -> CompUnit $EOF$
    rules->push_back(std::make_shared<ProductionRule>(
        "START",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("GlobalItemList"),
            Parser::EOF_DEFINITION}));

    // GlobalItemList -> GlobalItem GlobalItemList | GlobalItem (Non-empty list)
    rules->push_back(std::make_shared<ProductionRule>(
        "GlobalItemList",  // Recursive step: Item followed by more items
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("GlobalItem"),
            token("GlobalItemList")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "GlobalItemList",  // Base case: A single item ends the list conceptually
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("GlobalItem")}));

    // GlobalItem -> VarDef | FuncDef (A global item is a VarDef or FuncDef)
    rules->push_back(std::make_shared<ProductionRule>(
        "GlobalItem",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("VarDef")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "GlobalItem",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("FuncDef")}));

    // --- Type Specifier ---
    // Type -> INTTK | VOIDTK (Simple types)
    rules->push_back(std::make_shared<ProductionRule>(
        "Type",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("INTTK")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Type",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("VOIDTK")}));

    // --- Variable VarDefinition ---
    // VarDef -> [CONSTTK] Type VarDefItem (',' VarDefItem)* ';'
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Type"),
            token("VarDefItem"),            // First variable definition
            token("VarDefItemsFollowing"),  // Remaining definitions
            token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDef",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("CONSTTK"),
            token("Type"),
            token("VarDefItem"),
            token("VarDefItemsFollowing"),
            token("SEMICN")}));

    // VarDefItemsFollowing -> ',' VarDefItem VarDefItemsFollowing | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDefItemsFollowing",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("COMMA"),
            token("VarDefItem"),
            token("VarDefItemsFollowing")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDefItemsFollowing",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));  // Epsilon

    // ArrayDimension -> '[' Exp ']' | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "ArrayDimension",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACK"),
            token("Exp"),
            token("RBRACK")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "ArrayDimension",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));  // Epsilon

    // VarDefItem -> Ident ArrayDimension [ '=' InitVal ]
    // Defines a variable (scalar or multi-D array), optionally initialized.
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDefItem",  // No initializer
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("ArrayDimension")}));  // Handles dimensions or scalar case
    rules->push_back(std::make_shared<ProductionRule>(
        "VarDefItem",  // With initializer
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("ArrayDimension"),
            token("ASSIGN"),
            token("InitVal")}));

    // --- Variable Initializer ---
    // InitVal -> Exp | '{' [ InitVal (',' InitVal)* ] '}'
    rules->push_back(std::make_shared<ProductionRule>(
        "InitVal",  // Single expression initializer
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Exp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "InitVal",  // Aggregate initializer
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("RBRACE")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "InitVal",  // Aggregate initializer
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("InitVal"),
            token("InitValList"),
            token("RBRACE")}));

    // InitValList -> ',' InitVal InitValList | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "InitValList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("COMMA"),
            token("InitVal"),
            token("InitValList")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "InitValList",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));  // Epsilon

    // --- Function Definition ---
    // FuncDef -> Type Ident '(' FuncFParams ')' Block
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncDef",  // With parameters
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Type"), token("IDENFR"), token("LPARENT"),
            token("FuncFParams"),  // Parameter list
            token("RPARENT"), token("Block")}));

    // --- Formal Parameters (Function Definition) ---
    // FuncFParams -> FuncFParam FuncFParamsFollowing | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParams",  // List starts with one parameter
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("FuncFParam"),
            token("FuncFParamsFollowing")}));  // Followed by the rest
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParams",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));

    // FuncFParamsFollowing -> ',' FuncFParam FuncFParamsFollowing | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParamsFollowing",  // Subsequent comma-separated parameters
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("COMMA"),
            token("FuncFParam"),
            token("FuncFParamsFollowing")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParamsFollowing",                                        // End of parameter list
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));  // Epsilon

    // FuncFParam -> Type Ident
    // Parameter can be a scalar.
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncFParam",  // Scalar parameter
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Type"),
            token("IDENFR")}));

    // --- Code Block ---
    // Block -> '{' BlockItems '}'
    rules->push_back(std::make_shared<ProductionRule>(
        "Block",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LBRACE"),
            token("BlockItems"),  // Optional list of items inside the block
            token("RBRACE")}));

    // BlockItems -> BlockItem BlockItems | ε
    // Represents zero or more VarDefarations or statements within a block.
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItems",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BlockItem"),
            token("BlockItems")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItems",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));  // Epsilon (empty block)

    // BlockItem -> VarDef | Stmt
    // An item within a block can be either a VarDefaration or a statement.
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItem",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("VarDef")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "BlockItem",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Stmt")}));

    // --- Statements ---
    // Stmt -> LVal '=' Exp ';' | [Exp] ';' | Block | IfStmt | WhileStmt | ControlStmt ...
    // Note: IfStmt needs care for dangling else. Using priorities here.

    // Assignment Statement: LVal = Exp ;
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LVal"), token("ASSIGN"), token("Exp"), token("SEMICN")}));

    // Expression Statement: [Exp] ; (Covers function calls like printf, or just ';')
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",  // Expression evaluated for side effects
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Exp"), token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",  // Empty statement
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("SEMICN")}));

    // Block Statement
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("Block")}));

    // If Statement (Handling Dangling Else with Priorities)
    // Priority 1: Lower priority for the simple if-then
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",  // if ( Exp ) Stmt
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IFTK"), token("LPARENT"), token("Exp"), token("RPARENT"),
            token("Stmt")  // The 'then' statement
        },
        1 /* Priority 1 */));
    // Priority 2: Higher priority for if-then-else, binding 'else' to the nearest 'if'
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",  // if ( Exp ) Stmt else Stmt
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IFTK"), token("LPARENT"), token("Exp"), token("RPARENT"),
            token("Stmt"),  // The 'then' statement
            token("ELSETK"),
            token("Stmt")  // The 'else' statement
        },
        2 /* Priority 2 */));

    // While Statement
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",  // while ( Exp ) Stmt
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("WHILETK"), token("LPARENT"), token("Exp"), token("RPARENT"),
            token("Stmt")}));

    // Control Flow Statements
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",  // break ;
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("BREAKTK"), token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",  // continue ;
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("CONTINUETK"), token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",  // return ;
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RETURNTK"), token("SEMICN")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "Stmt",  // return Exp ;
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("RETURNTK"), token("Exp"), token("SEMICN")}));

    // --- Expressions ---
    // Standard operator precedence implemented via grammar structure:
    // Exp -> LOrExp -> LAndExp -> EqExp -> RelExp -> AddExp -> MulExp -> UnaryExp -> PrimaryExp

    // Exp -> LOrExp
    rules->push_back(std::make_shared<ProductionRule>(
        "Exp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("LOrExp")}));

    // LVal -> Ident ['[' Exp ']']
    // Represents a modifiable location: variable or array element access.
    // Covers Ident, Ident[Exp]
    rules->push_back(std::make_shared<ProductionRule>(
        "LVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
        }));
    rules->push_back(std::make_shared<ProductionRule>(
        "LVal",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"),
            token("LBRACK"), token("Exp"), token("RBRACK")}));

    // --- Primary Expression ---
    // PrimaryExp -> '(' Exp ')' | LVal | Number | STRCON
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",  // Parenthesized expression
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("LPARENT"), token("Exp"), token("RPARENT")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",  // Value of a variable or array element
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("LVal")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",  // Numeric literal
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("Number")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "PrimaryExp",  // String literal (yields a pointer, e.g., char*)
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("STRCON")}));

    // Number -> IntConst (Assuming only integer constants for simplicity)
    rules->push_back(std::make_shared<ProductionRule>(
        "Number",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("INTCON")}));

    // --- Unary Expression ---
    // UnaryExp -> PrimaryExp | Ident '(' FuncRParams ')' | UnaryOp UnaryExp
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",  // Base case: a primary expression
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("PrimaryExp")}));
    // Function Call (Handles getint(), printf(), user functions)
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("IDENFR"), token("LPARENT"), token("FuncRParams"), token("RPARENT")}));
    // Unary Operator Application
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryExp",  // UnaryOp applied to UnaryExp (right-associative)
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("UnaryOp"), token("UnaryExp")}));

    // UnaryOp -> '+' | '-' | '!'
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryOp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("PLUS")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryOp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("MINU")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "UnaryOp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("NOT")}));

    // --- Actual Parameters (Function Call) ---
    // FuncRParams -> Exp FuncRParamsFollowing | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncRParams",  // List starts with one expression (argument)
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("Exp"),
            token("FuncRParamsFollowing")}));  // Followed by the rest
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncRParams",
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));  // Epsilon

    // FuncRParamsFollowing -> ',' Exp FuncRParamsFollowing | ε
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncRParamsFollowing",  // Subsequent comma-separated arguments
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
            token("COMMA"), token("Exp"), token("FuncRParamsFollowing")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "FuncRParamsFollowing",                                        // End of argument list
        std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{}));  // Epsilon

    // --- Multiplicative Expression ---
    // MulExp -> UnaryExp | MulExp ('*' | '/' | '%') UnaryExp (Left-associative)
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("UnaryExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                      token("MulExp"), token("MULT"), token("UnaryExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                      token("MulExp"), token("DIV"), token("UnaryExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "MulExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                      token("MulExp"), token("MOD"), token("UnaryExp")}));

    // --- Additive Expression ---
    // AddExp -> MulExp | AddExp ('+' | '-') MulExp (Left-associative)
    rules->push_back(std::make_shared<ProductionRule>(
        "AddExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("MulExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "AddExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                      token("AddExp"), token("PLUS"), token("MulExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "AddExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                      token("AddExp"), token("MINU"), token("MulExp")}));

    // --- Relational Expression ---
    // RelExp -> AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp (Non-associative or Left)
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("AddExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                      token("RelExp"), token("LSS"), token("AddExp")}));  // <
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                      token("RelExp"), token("GRE"), token("AddExp")}));  // >
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                      token("RelExp"), token("LEQ"), token("AddExp")}));  // <=
    rules->push_back(std::make_shared<ProductionRule>(
        "RelExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                      token("RelExp"), token("GEQ"), token("AddExp")}));  // >=

    // --- Equality Expression ---
    // EqExp -> RelExp | EqExp ('==' | '!=') RelExp (Non-associative or Left)
    rules->push_back(std::make_shared<ProductionRule>(
        "EqExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("RelExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "EqExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                     token("EqExp"), token("EQL"), token("RelExp")}));  // ==
    rules->push_back(std::make_shared<ProductionRule>(
        "EqExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                     token("EqExp"), token("NEQ"), token("RelExp")}));  // !=

    // --- Logical AND Expression ---
    // LAndExp -> EqExp | LAndExp '&&' EqExp (Left-associative)
    rules->push_back(std::make_shared<ProductionRule>(
        "LAndExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("EqExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "LAndExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                       token("LAndExp"), token("AND"), token("EqExp")}));  // &&

    // --- Logical OR Expression ---
    // LOrExp -> LAndExp | LOrExp '||' LAndExp (Left-associative)
    rules->push_back(std::make_shared<ProductionRule>(
        "LOrExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{token("LAndExp")}));
    rules->push_back(std::make_shared<ProductionRule>(
        "LOrExp", std::vector<std::shared_ptr<Tokenizer::TokenDefinition>>{
                      token("LOrExp"), token("OR"), token("LAndExp")}));  // ||

    return rules;  // Return the list of all defined production rules
}

}  // namespace Parser
