#include "ir_generator.hpp"
#include <algorithm>  // Required for std::remove
#include <iostream>   // Required for std::cerr
#include <memory>     // Required for std::shared_ptr
#include <string>     // Required for std::string manipulations
#include <vector>     // Required for std::vector

namespace IRGenerator {

// Helper function to process raw string literals
// Removes surrounding quotes and unescapes common sequences.
std::string processRawStringLiteral(const std::string& raw_literal) {
    if (raw_literal.length() < 2 || raw_literal.front() != '"' || raw_literal.back() != '"') {
        // This shouldn't happen if the tokenizer is correct, but good to handle.
        // Consider logging an error or throwing an exception.
        std::cerr << "[IR_GEN_ERR] Invalid raw string literal format: " << raw_literal << std::endl;
        return raw_literal;  // Or an empty string, or throw
    }

    std::string content = raw_literal.substr(1, raw_literal.length() - 2);
    std::string unescaped_content;
    unescaped_content.reserve(content.length());

    for (size_t i = 0; i < content.length(); ++i) {
        if (content[i] == '\\' && i + 1 < content.length()) {
            switch (content[i + 1]) {
                case 'n':
                    unescaped_content += '\n';
                    i++;
                    break;
                case 't':
                    unescaped_content += '\t';
                    i++;
                    break;
                case '\'':
                    unescaped_content += '\'';
                    i++;
                    break;  // Escape for single quote
                case '"':
                    unescaped_content += '"';
                    i++;
                    break;
                case '\\':
                    unescaped_content += '\\';
                    i++;
                    break;  // Actual backslash
                // Add other escapes as needed (e.g., \r, \0)
                default:
                    unescaped_content += '\\';
                    break;  // Not a recognized escape, keep backslash
            }
        } else {
            unescaped_content += content[i];
        }
    }
    return unescaped_content;
}

void IRGenerator::visitFuncDef(PNNode node) {
    std::cerr << "[IR_GEN] visitFuncDef() called for node: " << (node ? node->name : "null") << " with " << (node ? node->children.size() : 0) << " children." << std::endl;

    // AST based on log: FuncDef(Type("int"), "main", "(", FuncFParams(), ")", Block(...))
    // Children indices:
    // 0: Type("int")
    // 1: "main" (TerminalNode)
    // 2: "(" (TerminalNode)
    // 3: FuncFParams()
    // 4: ")" (TerminalNode)
    // 5: Block(...)

    if (!node || node->name != "FuncDef" || node->children.size() < 6) {  // Adjusted size check
        std::cerr << "[IR_GEN] visitFuncDef: Exiting due to node validation or insufficient children. Name: " << (node ? node->name : "null") << ", Child count: " << (node ? node->children.size() : 0) << std::endl;
        return;
    }

    // 1. Extract and convert return type
    auto ast_return_type_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(0));
    if (!ast_return_type_node) {
        std::cerr << "[IR_GEN] visitFuncDef: Failed to cast return type node." << std::endl;
        return;
    }
    std::shared_ptr<IR::IRType> ir_return_type = this->astTypeToIrType(ast_return_type_node);
    if (!ir_return_type) {
        std::cerr << "[IR_GEN] visitFuncDef: astTypeToIrType failed for return type." << std::endl;
        return;
    }
    std::cerr << "[IR_GEN] visitFuncDef: Return type: " << ir_return_type->toString() << std::endl;

    // 2. Extract function name
    auto func_id_node = std::dynamic_pointer_cast<AST::TerminalNode>(node->children.at(1));
    if (!func_id_node || !func_id_node->token) {
        std::cerr << "[IR_GEN] visitFuncDef: Failed to cast function ID node or get token." << std::endl;
        return;
    }
    std::string func_name = func_id_node->token->matched;
    std::cerr << "[IR_GEN] visitFuncDef: Function name: " << func_name << std::endl;

    // 3. Process formal parameters (child at index 3)
    auto func_fparams_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(3));
    if (!func_fparams_node) {
        std::cerr << "[IR_GEN] visitFuncDef: Failed to cast FuncFParams node from child @ index 3." << std::endl;
        return;
    }
    std::cerr << "[IR_GEN] visitFuncDef: Processing FuncFParams node: " << func_fparams_node->name << std::endl;
    std::vector<std::shared_ptr<IR::IRVariable>> ir_params = this->visitFuncFParams(func_fparams_node);
    std::cerr << "[IR_GEN] visitFuncDef: Parsed " << ir_params.size() << " parameters." << std::endl;

    // 4. Create the IR NormalFunction
    auto ir_function = std::make_shared<IR::NormalIRFunction>(func_name, ir_params, ir_return_type);
    std::cerr << "[IR_GEN] visitFuncDef: IR::NormalIRFunction object created." << std::endl;

    // 5. Add the function to the program's list of normal functions
    if (this->program) {
        this->program->addNormalFunction(ir_function);
        if (this->program->normalFunctions.count(func_name)) {
            std::cerr << "[IR_GEN] visitFuncDef: Successfully ADDED function '" << func_name << "' to program." << std::endl;
        } else {
            std::cerr << "[IR_GEN] visitFuncDef: FAILED to add function '" << func_name << "' to program map (though addNormalFunction was called)." << std::endl;
        }
    } else {
        std::cerr << "[IR_GEN] visitFuncDef: this->program is null! Cannot add function." << std::endl;
        return;  // Cannot proceed without a program object
    }

    this->currentNormalFunction = ir_function;
    std::cerr << "[IR_GEN] visitFuncDef: currentNormalFunction set." << std::endl;

    symbolTable.enterScope(true);
    std::cerr << "[IR_GEN] visitFuncDef: Symbol table function scope entered for '" << func_name << "'." << std::endl;
    for (const auto& param : ir_params) {
        if (!symbolTable.addSymbol(param->name, param, param->type, false /*is_const*/)) {
            std::cerr << "[IR_GEN_ERR] visitFuncDef: Failed to add parameter '" << param->name << "' to symbol table for function '" << func_name << "'." << std::endl;
            // Error, potentially stop compilation or log severe error
        } else {
            std::cerr << "[IR_GEN] visitFuncDef: Added parameter '" << param->name << "' to symbol table for function '" << func_name << "'." << std::endl;
        }
    }
    std::cerr << "[IR_GEN] visitFuncDef: Parameters added to symbol table." << std::endl;

    // 9. Visit the function body (Block node - child at index 5)
    auto block_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(5));
    if (!block_node) {
        std::cerr << "[IR_GEN] visitFuncDef: Failed to cast Block node for function '" << func_name << "'." << std::endl;
        symbolTable.leaveScope();
        this->currentNormalFunction = nullptr;
        std::cerr << "[IR_GEN] visitFuncDef: Scope left, currentNormalFunction reset. Completed for function '" << func_name << "' (no body)." << std::endl;
        return;
    } else {
        std::cerr << "[IR_GEN] visitFuncDef: Visiting block node: " << block_node->name << " for function '" << func_name << "'." << std::endl;
        visitBlock(block_node, false);
    }

    symbolTable.leaveScope();
    this->currentNormalFunction = nullptr;
    std::cerr << "[IR_GEN] visitFuncDef: Completed for function '" << func_name << "'." << std::endl;
}

std::shared_ptr<IR::IRType> IRGenerator::astTypeToIrType(PNNode typeNode) {
    if (!typeNode || typeNode->children.empty()) {
        // TODO: Handle error: Invalid type node
        // For now, returning a default or null to prevent crashes, but proper error reporting is needed.
        // Consider throwing std::runtime_error("Invalid AST type node");
        return nullptr;  // Or a default type like Integer for robustness in early stages
    }

    // Assuming the type information is in a child, often the first one, which is a TerminalNode.
    // E.g., Type(Terminal("int")) or TypeSpecifier(Terminal("int"))
    // The actual structure depends on your parser's AST generation.
    // Let's try to find a TerminalNode among children, or assume first child if specific structure is not given.

    std::string type_str;

    // Attempt to find a terminal node directly under typeNode, assuming it holds the type string.
    // This is a common pattern, e.g. Type -> "int".
    // If Type -> Specifiers -> "int", this logic would need adjustment.
    if (typeNode->children.size() == 1 && std::dynamic_pointer_cast<AST::TerminalNode>(typeNode->children[0])) {
        auto terminal_node = std::dynamic_pointer_cast<AST::TerminalNode>(typeNode->children[0]);
        type_str = terminal_node->token->matched;
    } else if (typeNode->name == "Type" || typeNode->name == "TypeSpecifier" || typeNode->name == "BType") {
        // If the typeNode itself is named in a way that implies it's a wrapper for a primitive type string child
        // Look for a terminal child directly.
        for (const auto& child : typeNode->children) {
            if (auto terminal_child = std::dynamic_pointer_cast<AST::TerminalNode>(child)) {
                type_str = terminal_child->token->matched;
                break;
            }
        }
        if (type_str.empty() && !typeNode->children.empty()) {
            // Fallback or more specific logic based on your AST structure, e.g. if it's always first child
            // For a robust solution, you'd have specific visitors or checks based on typeNode->name
            // For now, if not found above and typeNode is named like a type specifier, try its first child's name IF that child is a NonTerminal
            // This handles cases like Type -> IntKeyword
            if (auto non_terminal_child = std::dynamic_pointer_cast<AST::NonTerminalNode>(typeNode->children[0])) {
                type_str = non_terminal_child->name;  // e.g. if the node is IntKeyword, FloatKeyword from parser
            } else if (auto terminal_child = std::dynamic_pointer_cast<AST::TerminalNode>(typeNode->children[0])) {
                type_str = terminal_child->token->matched;
            }
        }
    }

    if (type_str == "int") {
        return std::make_shared<IR::SimpleIRType>(IR::SimpleTypeKind::INTEGER);
    } else if (type_str == "void") {
        return std::make_shared<IR::VoidIRType>();
    } else if (type_str == "IntKeyword") {  // Example if your parser makes nodes like IntKeyword
        return std::make_shared<IR::SimpleIRType>(IR::SimpleTypeKind::INTEGER);
    } else if (type_str == "VoidKeyword") {  // Example
        return std::make_shared<IR::VoidIRType>();
    }
    // TODO: Handle other types (e.g., arrays, pointers, custom types) if your language supports them.
    // For now, unrecognised types lead to an error or default.
    // throw std::runtime_error("Unsupported AST type: " + type_str);
    return nullptr;  // Placeholder for unhandled types
}

std::shared_ptr<IR::IRVariable> IRGenerator::visitFuncFParam(PNNode node) {
    // Assuming FuncFParam AST Node structure, e.g.:
    // node->children[0] (PNNode): AST node for the parameter's type (e.g., a "Type" node).
    // node->children[1] (PTNode): AST TerminalNode for the parameter's identifier.

    if (!node || node->name != "FuncFParam" || node->children.size() < 2) {
        // TODO: Handle error: Invalid FuncFParam node structure
        // throw std::runtime_error("Invalid FuncFParam AST node");
        return nullptr;
    }

    // 1. Get parameter type
    auto param_type_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(0));
    if (!param_type_node) { /* TODO: Error handling */
        return nullptr;
    }
    std::shared_ptr<IR::IRType> ir_param_type = this->astTypeToIrType(param_type_node);
    if (!ir_param_type) {
        // TODO: Handle error: Unknown type for parameter
        // throw std::runtime_error("Unknown type for function parameter");
        return nullptr;
    }

    // 2. Get parameter name
    auto param_id_node = std::dynamic_pointer_cast<AST::TerminalNode>(node->children.at(1));
    if (!param_id_node || !param_id_node->token) { /* TODO: Error handling */
        return nullptr;
    }
    std::string param_name = param_id_node->token->matched;

    // 3. Create an IRVariable for the parameter
    // Using createNamedVar helper if available and suitable, or directly constructing.
    // createNamedVar is defined as: createNamedVar(const std::string& name, std::shared_ptr<IR::IRType> type)
    return this->createNamedVar(param_name, ir_param_type);
}

std::vector<std::shared_ptr<IR::IRVariable>> IRGenerator::visitFuncFParams(PNNode node) {
    std::vector<std::shared_ptr<IR::IRVariable>> ir_params;

    // Assuming FuncFParams AST Node structure:
    // Its children are multiple FuncFParam nodes, or it's empty if no parameters.
    // E.g., FuncFParams -> [FuncFParam1, FuncFParam2, ...]

    if (!node || node->name != "FuncFParams") {
        // It's possible for FuncFParams to be valid but have no children (no parameters)
        // If the node itself is null or not named "FuncFParams", it might be an error
        // or an optional node that's absent. For now, if it's not the expected node, return empty.
        // Consider if an error should be thrown if !node and it was expected.
        if (node) { /* TODO: Error handling for wrong node type */
        }
        return ir_params;  // Return empty vector
    }

    for (const auto& child_node : node->children) {
        auto func_fparam_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(child_node);
        if (func_fparam_node) {
            std::shared_ptr<IR::IRVariable> ir_param = this->visitFuncFParam(func_fparam_node);
            if (ir_param) {
                ir_params.push_back(ir_param);
            } else {
                // TODO: Error from visitFuncFParam, decide if to continue or propagate error
                // throw std::runtime_error("Failed to process a function parameter.");
                // For now, we skip the bad parameter and continue.
            }
        }
    }
    return ir_params;
}

void IRGenerator::visitBlockItem(PNNode blockItemNode) {
    std::cerr << "[IR_GEN] visitBlockItem() called for node: " << (blockItemNode ? blockItemNode->name : "null") << std::endl;
    if (!blockItemNode || blockItemNode->name != "BlockItem" || blockItemNode->children.empty()) {
        std::cerr << "[IR_GEN] visitBlockItem: Invalid or empty BlockItem node." << std::endl;
        return;
    }

    // A BlockItem usually contains one Stmt or one VarDef.
    // AST: BlockItem(Stmt(...))
    auto actual_statement_or_decl_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(blockItemNode->children.at(0));

    if (!actual_statement_or_decl_node) {
        std::cerr << "[IR_GEN_ERR] visitBlockItem: Child of BlockItem is not a NonTerminalNode." << std::endl;
        return;
    }

    std::cerr << "[IR_GEN] visitBlockItem: Child node is '" << actual_statement_or_decl_node->name << "'." << std::endl;

    if (actual_statement_or_decl_node->name == "VarDef") {
        std::cerr << "[IR_GEN] visitBlockItem: Dispatching to visitVarDef." << std::endl;
        this->visitVarDef(actual_statement_or_decl_node);
    } else if (actual_statement_or_decl_node->name == "ConstDef") {
        std::cerr << "[IR_GEN] visitBlockItem: Dispatching to visitConstDef." << std::endl;
        this->visitConstDef(actual_statement_or_decl_node);
    } else if (actual_statement_or_decl_node->name == "Stmt") {
        std::cerr << "[IR_GEN] visitBlockItem: Dispatching to visitStmt." << std::endl;
        this->visitStmt(actual_statement_or_decl_node);
    } else {
        std::cerr << "[IR_GEN_ERR] visitBlockItem: Unexpected node type '" << actual_statement_or_decl_node->name << "' inside BlockItem." << std::endl;
        // Fallback or error: For robustness, you might still call visitStmt or log a more severe error.
        // For now, just logging. If it's an expression or other statement-like non-terminal, visitStmt might be a guess.
        // this->visitStmt(actual_statement_or_decl_node);
    }
}

void IRGenerator::visitBlockItems(PNNode blockItemsNode) {
    std::cerr << "[IR_GEN] visitBlockItems() called for node: " << (blockItemsNode ? blockItemsNode->name : "null") << std::endl;
    if (!blockItemsNode || blockItemsNode->name != "BlockItems") {
        std::cerr << "[IR_GEN] visitBlockItems: Invalid BlockItems node." << std::endl;
        return;
    }

    // BlockItems -> BlockItem BlockItems | epsilon (potentially)
    // The AST shows: BlockItems(BlockItem(...), BlockItems()) for a list, or BlockItems() for empty.
    for (const auto& child : blockItemsNode->children) {
        auto non_terminal_child = std::dynamic_pointer_cast<AST::NonTerminalNode>(child);
        if (non_terminal_child) {
            if (non_terminal_child->name == "BlockItem") {
                this->visitBlockItem(non_terminal_child);
            } else if (non_terminal_child->name == "BlockItems") {  // Handle recursion for list structure
                this->visitBlockItems(non_terminal_child);          // Recursive call for nested BlockItems
            }
        }
    }
}

void IRGenerator::visitBlock(PNNode node, bool createNewScope) {
    std::cerr << "[IR_GEN] visitBlock() called for node: " << (node ? node->name : "null") << std::endl;
    // AST: Block("{", BlockItems(...), "}")
    if (!node || node->name != "Block" || node->children.size() < 2) {  // At least LBrace and RBrace, maybe BlockItems
        std::cerr << "[IR_GEN] visitBlock: Invalid Block node or not enough children." << std::endl;
        return;
    }

    if (createNewScope) {
        this->symbolTable.enterScope();
        std::cerr << "[IR_GEN] visitBlock: New scope entered." << std::endl;
    }

    // Find the BlockItems node among the children of Block.
    // It's typically between LBRACE and RBRACE.
    // AST shows: child 0 is "{", child 1 is BlockItems, child 2 is "}". (for non-empty block)
    PNNode block_items_node = nullptr;
    if (node->children.size() > 1) {  // If there's more than just braces (or if braces are optional)
        for (const auto& child : node->children) {
            auto nt_child = std::dynamic_pointer_cast<AST::NonTerminalNode>(child);
            if (nt_child && nt_child->name == "BlockItems") {
                block_items_node = nt_child;
                break;
            }
        }
    }

    if (block_items_node) {
        std::cerr << "[IR_GEN] visitBlock: Found BlockItems child. Visiting it." << std::endl;
        this->visitBlockItems(block_items_node);
    } else {
        std::cerr << "[IR_GEN] visitBlock: No BlockItems child found in Block (block might be empty or AST structure differs)." << std::endl;
        // If a block can be empty like Block -> LBrace RBrace, without BlockItems, this is fine.
    }

    if (createNewScope) {
        this->symbolTable.leaveScope();
        std::cerr << "[IR_GEN] visitBlock: Scope left." << std::endl;
    }
}

void IRGenerator::visitReturnStmt(PNNode node) {
    std::cerr << "[IR_GEN] visitReturnStmt() called for node: " << (node ? node->name : "null") << " with " << (node ? node->children.size() : 0) << " children." << std::endl;
    if (!currentNormalFunction) {
        std::cerr << "[IR_GEN_ERR] No current function to return from." << std::endl;
        return;
    }

    // std::optional<std::shared_ptr<IR::IROperand>> return_value_operand = std::nullopt;
    std::shared_ptr<IR::IROperand> return_value_operand = nullptr;

    // ReturnStmt -> 'return' Exp ';'
    // ReturnStmt -> 'return' ';'
    // Child 0 is 'return' token
    // Child 1 (if exists) is Exp
    // Child 2 (or 1 if no Exp) is ';'

    if (node->children.size() > 2) {  // Has Exp (e.g., 'return', Exp, ';')
        auto exp_ast_node_p = node->children.at(1);
        if (!exp_ast_node_p) {
            std::cerr << "[IR_GEN_ERR] visitReturnStmt: Exp AST node is null." << std::endl;
            // Potentially add a void return or error recovery
            auto void_return_inst = std::make_shared<IR::ReturnInst>(nullptr);  // Explicitly pass nullptr
            addInstruction(void_return_inst);
            return;
        }
        std::cerr << "[IR_GEN] visitReturnStmt: Visiting Exp node for return value." << std::endl;
        return_value_operand = this->dispatchVisitExp(exp_ast_node_p);

        // if (!return_value_operand.has_value() || !return_value_operand.value()) {
        if (!return_value_operand) {
            std::cerr << "[IR_GEN_ERR] visitReturnStmt: dispatchVisitExp for return value resulted in null or empty optional." << std::endl;
            // This case might mean an error in expression evaluation or a void-returning expression used as value.
            // Depending on language semantics, this could be an error or imply a default/void return.
            // For now, let's assume if an Exp was provided, a value was expected. This might be a point for stricter error handling.
            // Consider adding a void return instruction if the function is void, or error if not.
            // Forcing a void return for now if expression evaluation failed to produce an operand.
            auto error_return_inst = std::make_shared<IR::ReturnInst>(nullptr);  // Explicitly pass nullptr
            addInstruction(error_return_inst);
            return;
        }
        std::cerr << "[IR_GEN] visitReturnStmt: Return value operand: " << return_value_operand->toString() << std::endl;

    } else {
        std::cerr << "[IR_GEN] visitReturnStmt: No Exp node, void return." << std::endl;
        // No expression, so it's a void return. return_value_operand remains std::nullopt (now nullptr)
    }

    // Check if the function's return type is void and if we have a value
    auto func_ret_type = currentNormalFunction->returnType;
    bool is_func_void = (std::dynamic_pointer_cast<IR::VoidIRType>(func_ret_type) != nullptr);

    // if (return_value_operand.has_value() && is_func_void) {
    if (return_value_operand && is_func_void) {
        std::cerr << "[IR_GEN_WARN] visitReturnStmt: Value returned from a void function. Ignoring return value." << std::endl;
        return_value_operand = nullptr;  // Discard the value for a void function
        // } else if (!return_value_operand.has_value() && !is_func_void) {
    } else if (!return_value_operand && !is_func_void) {
        std::cerr << "[IR_GEN_ERR] visitReturnStmt: No value returned from a non-void function (";
        if (func_ret_type)
            std::cerr << func_ret_type->toString();
        else
            std::cerr << "unknown_type";
        std::cerr << "). This might be an error." << std::endl;
        // This is problematic. A non-void function must return a value.
        // Depending on error strategy, either create a dummy error value or stop.
        // For now, we'll still create a ReturnInst, which will take nullptr if return_value_operand is still null.
        // The IR verifier or later stages should catch this inconsistency if the function expects a value.
    }

    auto actual_return_inst = std::make_shared<IR::ReturnInst>(return_value_operand);
    addInstruction(actual_return_inst);
    std::cerr << "[IR_GEN] visitReturnStmt: ReturnInst added to function '" << currentNormalFunction->name << "'." << std::endl;
}

void IRGenerator::visitStmt(PNNode stmtNode) {
    std::cerr << "[IR_GEN] visitStmt() called for node: " << (stmtNode ? stmtNode->name : "null") << std::endl;
    if (!stmtNode || stmtNode->children.empty()) {
        std::cerr << "[IR_GEN] visitStmt: Empty or null Stmt node." << std::endl;
        return;
    }

    // The AST for return is: Stmt("return", Exp, ";")
    // The AST for assignment could be: Stmt(LVal, "=", Exp, ";")
    // The AST for ExpStmt could be: Stmt(Exp, ";")

    auto first_child_base = stmtNode->children.at(0);

    // Case 1: Keyword-based statements (like return)
    if (auto terminal_child = std::dynamic_pointer_cast<AST::TerminalNode>(first_child_base)) {
        std::cerr << "[IR_GEN] visitStmt: First child is terminal: " << terminal_child->token->matched << std::endl;
        if (terminal_child->token->matched == "return") {
            this->visitReturnStmt(stmtNode);  // visitReturnStmt itself takes the Stmt node
        }
        // ... other keyword-based statements like "if", "while", etc.
        else {
            std::cerr << "[IR_GEN] visitStmt: Unhandled keyword-based statement start: " << terminal_child->token->matched << std::endl;
        }
    }
    // Case 2: Expression-based statements (Assignment or ExpStmt)
    else if (auto non_terminal_child = std::dynamic_pointer_cast<AST::NonTerminalNode>(first_child_base)) {
        std::cerr << "[IR_GEN] visitStmt: First child is non-terminal: " << non_terminal_child->name << std::endl;

        // Check for Assignment: LVal ASSIGN Exp SEMI
        // Stmt -> LVal ASSIGN_OP Exp SEMI_COLON
        // Children: 0: LVal, 1: ASSIGN_OP, 2: Exp, (3: SEMI_COLON, optional if absorbed)
        if (non_terminal_child->name == "LVal" && stmtNode->children.size() >= 3) {
            auto assign_op_node = std::dynamic_pointer_cast<AST::TerminalNode>(stmtNode->children.at(1));
            if (assign_op_node && assign_op_node->token && assign_op_node->token->matched == "=") {
                // This is an assignment statement
                std::cerr << "[IR_GEN] visitStmt: Detected assignment statement." << std::endl;
                PNNode lval_node = non_terminal_child;  // This is children.at(0)
                PNode exp_node = stmtNode->children.at(2);

                std::shared_ptr<IR::IROperand> lval_operand = this->visitLVal(lval_node);
                std::shared_ptr<IR::IROperand> rhs_operand = this->dispatchVisitExp(exp_node);

                if (lval_operand && rhs_operand) {
                    // visitLVal returns an IRVariable (which is an IROperand).
                    // We need to ensure it's actually an IRVariable for the AssignInst.
                    auto dest_variable = std::dynamic_pointer_cast<IR::IRVariable>(lval_operand);
                    if (dest_variable) {
                        // Check if LVal is const
                        SymbolInfo symbol_info = symbolTable.lookupSymbol(dest_variable->name);
                        if (symbol_info.is_const) {
                            std::cerr << "[IR_GEN_ERR] visitStmt: Cannot assign to const variable '" << dest_variable->name << "'." << std::endl;
                            // Optionally throw an error or stop compilation.
                            return;
                        }

                        auto assign_inst = std::make_shared<IR::AssignInst>(dest_variable, rhs_operand);
                        this->addInstruction(assign_inst);
                        std::cerr << "[IR_GEN] visitStmt: Added AssignInst for: " << dest_variable->name << " = ..." << std::endl;
                    } else {
                        std::cerr << "[IR_GEN_ERR] visitStmt: Destination of assignment is not a variable." << std::endl;
                    }
                } else {
                    std::cerr << "[IR_GEN_ERR] visitStmt: Failed to generate IR for LVal or RHS of assignment." << std::endl;
                }
                return;  // Handled assignment
            }
        }

        // If not an assignment starting with LVal, it might be an ExpStmt.
        // Stmt -> Exp SEMI
        // (Covers function calls like printf(); or simple expressions evaluated for side effects)
        // Check if non_terminal_child->name is one of your expression types
        if (non_terminal_child->name == "Exp" ||
            non_terminal_child->name == "LOrExp" ||    // ... and other expression non-terminals from your dispatchVisitExp
            non_terminal_child->name == "UnaryExp" ||  // UnaryExp can be a function call
            non_terminal_child->name == "PrimaryExp"
            /* etc. */
        ) {
            std::cerr << "[IR_GEN] visitStmt: Visiting expression child " << non_terminal_child->name << " for side effects (ExpStmt)." << std::endl;
            this->dispatchVisitExp(non_terminal_child);  // Visit the expression, result is discarded
        } else {
            std::cerr << "[IR_GEN] visitStmt: Unhandled non-terminal statement type: " << non_terminal_child->name << std::endl;
        }
    } else {
        std::cerr << "[IR_GEN] visitStmt: First child of Stmt is neither Terminal nor NonTerminal. Node type ID: "
                  << (first_child_base ? typeid(*first_child_base).name() : "null_child_pointer") << std::endl;
    }
}

// Helper to visit a list of actual parameters (arguments) in a function call
std::vector<std::shared_ptr<IR::IROperand>> IRGenerator::visitFuncRParams(PNNode node) {
    std::vector<std::shared_ptr<IR::IROperand>> ir_args;

    if (!node) {
        std::cerr << "[IR_GEN] visitFuncRParams: Called with null node (no arguments)." << std::endl;
        return ir_args;
    }

    // AST structure from dump for FuncRParams (e.g., for printf("format %d", 1)) seems to be:
    // FuncRParams(
    //   Exp(LOrExp(...PrimaryExp("\"format %d\""))),  // Child 0
    //   FuncRParamsFollowing(                         // Child 1
    //     TerminalNode(","),                        // Child 0 of FuncRParamsFollowing
    //     Exp(LOrExp(...PrimaryExp(Number("1")))), // Child 1 of FuncRParamsFollowing
    //     FuncRParamsFollowing()                     // Child 2 of FuncRParamsFollowing (empty)
    //   )
    // )
    // Or for a single argument: FuncRParams(Exp(...), FuncRParamsFollowing()_empty)

    std::cerr << "[IR_GEN] visitFuncRParams: Processing FuncRParams node: " << node->name
              << " with " << node->children.size() << " direct children." << std::endl;

    if (node->name != "FuncRParams") {
        std::cerr << "[IR_GEN_ERR] visitFuncRParams: Expected FuncRParams node, got " << node->name << std::endl;
        return ir_args;
    }

    if (node->children.empty()) {
        std::cerr << "[IR_GEN] visitFuncRParams: FuncRParams node has no children (no arguments)." << std::endl;
        return ir_args;  // No arguments
    }

    // Process the first argument (Exp)
    auto first_arg_exp_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(0));
    if (first_arg_exp_node) {
        std::cerr << "[IR_GEN] visitFuncRParams: Visiting first argument Exp node: " << first_arg_exp_node->name << std::endl;
        std::shared_ptr<IR::IROperand> arg_operand = this->dispatchVisitExp(first_arg_exp_node);
        if (arg_operand) {
            ir_args.push_back(arg_operand);
            std::cerr << "[IR_GEN] visitFuncRParams: Added first argument. Total args: " << ir_args.size() << std::endl;
        } else {
            std::cerr << "[IR_GEN_ERR] visitFuncRParams: Failed to generate IR for first argument expression." << std::endl;
        }
    } else {
        std::cerr << "[IR_GEN_ERR] visitFuncRParams: First child of FuncRParams is not a NonTerminalNode (Exp)." << std::endl;
        return ir_args;  // Critical error in AST structure for FuncRParams
    }

    // Process subsequent arguments from FuncRParamsFollowing
    if (node->children.size() > 1) {
        auto current_frpf_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(1));
        while (current_frpf_node && current_frpf_node->name == "FuncRParamsFollowing" && !current_frpf_node->children.empty()) {
            std::cerr << "[IR_GEN] visitFuncRParams: Processing FuncRParamsFollowing node with "
                      << current_frpf_node->children.size() << " children." << std::endl;
            // Expected structure of FuncRParamsFollowing: Terminal(","), Exp, FuncRParamsFollowing (recursive)
            if (current_frpf_node->children.size() < 2) {  // Need at least COMMA and Exp
                std::cerr << "[IR_GEN] visitFuncRParams: FuncRParamsFollowing node is empty or missing Exp. Stopping arg processing." << std::endl;
                break;  // End of arguments or malformed
            }

            // Child 0 should be COMMA (TerminalNode), Child 1 should be Exp (NonTerminalNode)
            auto comma_node = std::dynamic_pointer_cast<AST::TerminalNode>(current_frpf_node->children.at(0));
            auto next_arg_exp_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(current_frpf_node->children.at(1));

            if (!comma_node || !comma_node->token || comma_node->token->matched != ",") {
                std::cerr << "[IR_GEN_ERR] visitFuncRParams: Expected COMMA in FuncRParamsFollowing, but found: "
                          << (comma_node && comma_node->token ? comma_node->token->matched : "<not a comma or missing token>") << std::endl;
                break;  // Malformed, stop processing arguments
            }

            if (next_arg_exp_node) {
                std::cerr << "[IR_GEN] visitFuncRParams: Visiting next argument Exp node: " << next_arg_exp_node->name << std::endl;
                std::shared_ptr<IR::IROperand> arg_operand = this->dispatchVisitExp(next_arg_exp_node);
                if (arg_operand) {
                    ir_args.push_back(arg_operand);
                    std::cerr << "[IR_GEN] visitFuncRParams: Added subsequent argument. Total args: " << ir_args.size() << std::endl;
                } else {
                    std::cerr << "[IR_GEN_ERR] visitFuncRParams: Failed to generate IR for a subsequent argument expression." << std::endl;
                }
            } else {
                std::cerr << "[IR_GEN_ERR] visitFuncRParams: Child 1 of FuncRParamsFollowing is not a NonTerminalNode (Exp)." << std::endl;
                break;  // Malformed
            }

            // Move to the next FuncRParamsFollowing node, if it exists
            if (current_frpf_node->children.size() > 2) {
                current_frpf_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(current_frpf_node->children.at(2));
            } else {
                current_frpf_node = nullptr;  // No more FuncRParamsFollowing
            }
        }
    }

    std::cerr << "[IR_GEN] visitFuncRParams: Finished processing. Total arguments collected: " << ir_args.size() << std::endl;
    return ir_args;
}

std::shared_ptr<IR::IROperand> IRGenerator::visitFunctionCall(PTNode idNode, PNNode paramsNode) {
    std::cerr << "[IR_GEN] visitFunctionCall for function '" << idNode->token->matched << "'" << std::endl;
    if (!currentNormalFunction) {
        std::cerr << "[IR_GEN_ERR] No current function context for function call." << std::endl;
        return nullptr;
    }

    std::string func_name = idNode->token->matched;
    std::vector<std::shared_ptr<IR::IROperand>> ir_args = visitFuncRParams(paramsNode);

    // Before creating CallNormalInst, check if the function exists in the program
    if (!this->program || (!this->program->normalFunctions.count(func_name) && !this->program->pureFunctions.count(func_name))) {
        std::cerr << "[IR_GEN_ERR] Undeclared function called: " << func_name << std::endl;
        // Potentially return a special error operand or handle as per language spec (e.g., implicit declaration)
        // For now, returning nullptr, which will likely lead to issues downstream if not handled.
        return nullptr;
    }

    // Determine if it's a call to a normal or pure function and get its return type / result destination handling
    // std::optional<std::shared_ptr<IR::IRVariable>> result_destination = std::nullopt;
    std::shared_ptr<IR::IRVariable> result_destination = nullptr;
    std::shared_ptr<IR::IRType> expected_call_ret_type = nullptr;
    bool is_normal_call = false;

    if (this->program->normalFunctions.count(func_name)) {
        is_normal_call = true;
        auto target_func = this->program->normalFunctions.at(func_name);
        expected_call_ret_type = target_func->returnType;
        std::cerr << "[IR_GEN] Function '" << func_name << "' is a Normal Function. Return type: " << (expected_call_ret_type ? expected_call_ret_type->toString() : "null_type") << std::endl;

        // If the normal function is not void, create a temporary variable for the result.
        if (std::dynamic_pointer_cast<IR::VoidIRType>(expected_call_ret_type) == nullptr && expected_call_ret_type != nullptr) {
            std::string temp_name = "%tmp_call_" + func_name + "_" + std::to_string(tempVarCounter++);
            result_destination = createNamedVar(temp_name, expected_call_ret_type);
            std::cerr << "[IR_GEN] Created temp var '" << temp_name << "' for non-void normal call result." << std::endl;
            if (this->currentNormalFunction && result_destination) {        // Ensure context and variable exist
                this->currentNormalFunction->addLocal(result_destination);  // Add to locals
                // Also add to symbol table so it can be found if needed by other IR stages, though for temps it's less critical
                // symbolTable.addSymbol(result_destination->name, result_destination, result_destination->type, false);
                std::cerr << "[IR_GEN] Added temp var '" << temp_name << "' to locals of function " << this->currentNormalFunction->name << std::endl;
            }
        } else {
            std::cerr << "[IR_GEN] Normal function '" << func_name << "' is void or has null ret type. No result destination needed." << std::endl;
        }
    } else if (this->program->pureFunctions.count(func_name)) {
        // This is a CallPureInst situation, which has a different structure for results.
        // visitFunctionCall is primarily for expressions, which expect a single IROperand result (or void).
        // Pure functions can have multiple results. If a pure function is called in an expression context,
        // it should ideally return a single value or be an error if it returns multiple/zero and one is expected.
        std::cerr << "[IR_GEN_ERR] Calling a Pure Function ('" << func_name << "') in a context that expects a Normal Function Call. This is not yet fully supported here." << std::endl;
        // How to handle this? If pure functions are callable like normal ones in expressions, you need a convention.
        // e.g. only pure functions with exactly one output can be used here.
        // For now, this path will likely lead to an error or misbehavior because CallNormalInst is used below.
        // A CallPureInst should be generated instead if we are in a NormalIRFunction body.
        // If this function call is itself within a PureIRFunction, then a PureInternalCallInst is needed.
        // This indicates a need to differentiate call sites or how pure functions are invoked from expression contexts.
        return nullptr;  // Placeholder: Error or specific handling for pure func in expr
    }

    // The result_destination (std::shared_ptr<IR::IRVariable>) is passed to CallNormalInst
    // The constructor CallNormalInst(..., std::shared_ptr<IRVariable> resDest) will set hasResultDestination.
    auto call_inst = std::make_shared<IR::CallNormalInst>(func_name, ir_args, result_destination);
    addInstruction(call_inst);
    std::cerr << "[IR_GEN] CallNormalInst generated for '" << func_name << "'." << std::endl;

    // The value of the function call expression is the temporary variable that stores the result.
    // If the function is void (result_destination is nullopt/nullptr), then the call expr has no value (returns nullptr operand).
    // if (result_destination.has_value()) {
    //     return result_destination.value(); // This would be std::shared_ptr<IR::IRVariable>
    // }
    if (result_destination) {  // If a temp var was created (i.e., function is not void)
        return result_destination;
    }

    return nullptr;  // For void function calls or errors
}

std::shared_ptr<IR::IROperand> IRGenerator::visitNumber(PTNode node) {
    std::cerr << "[IR_GEN] visitNumber() called for token: " << (node && node->token ? node->token->matched : "<null_token_or_node>") << std::endl;
    if (!node || !node->token) {
        // TODO: Handle error: Invalid number node or token
        // throw std::runtime_error("Invalid AST Number node");
        return nullptr;
    }
    try {
        int value = std::stoi(node->token->matched);
        return std::make_shared<IR::IRConstant>(value);
    } catch (const std::invalid_argument& ia) {
        // TODO: Handle error: Number format invalid
        // throw std::runtime_error("Invalid number format: " + node->token->matched);
        return nullptr;
    } catch (const std::out_of_range& oor) {
        // TODO: Handle error: Number out of range
        // throw std::runtime_error("Number out of range: " + node->token->matched);
        return nullptr;
    }
}

std::shared_ptr<IR::IROperand> IRGenerator::visitPrimaryExp(PNNode node) {
    std::cerr << "[IR_GEN] visitPrimaryExp() called for node: " << (node ? node->name : "<null_node>") << std::endl;
    if (!node || node->children.empty()) {
        std::cerr << "[IR_GEN_ERR] visitPrimaryExp: Invalid or empty PrimaryExp node." << std::endl;
        return nullptr;
    }

    auto first_child_base = node->children.at(0);

    // Case 1: Parenthesized Expression '(' Exp ')'
    if (auto non_terminal_exp_child = std::dynamic_pointer_cast<AST::NonTerminalNode>(first_child_base)) {
        if (non_terminal_exp_child->name == "Exp") {
            std::cerr << "[IR_GEN] visitPrimaryExp: Visiting parenthesized Exp." << std::endl;
            return this->dispatchVisitExp(non_terminal_exp_child);
        }
        // Case 2: LVal (variable usage)
        else if (non_terminal_exp_child->name == "LVal") {
            std::cerr << "[IR_GEN] visitPrimaryExp: Visiting LVal." << std::endl;
            return this->visitLVal(non_terminal_exp_child);  // visitLVal returns IRVariable
        }
        // Case 3 (part 2): Number wrapped in a NonTerminalNode (e.g., PrimaryExp -> NumberNode -> TerminalToken)
        else if (non_terminal_exp_child->name == "Number" && !non_terminal_exp_child->children.empty()) {
            if (auto actual_number_terminal = std::dynamic_pointer_cast<AST::TerminalNode>(non_terminal_exp_child->children.at(0))) {
                std::cerr << "[IR_GEN] visitPrimaryExp: Visiting Number (wrapped)." << std::endl;
                return this->visitNumber(actual_number_terminal);
            }
        }
    }
    // Case 3 (part 1) & 4: Number or String Literal as a direct TerminalNode child of PrimaryExp
    else if (auto terminal_node = std::dynamic_pointer_cast<AST::TerminalNode>(first_child_base)) {
        if (terminal_node->token && !terminal_node->token->matched.empty()) {
            const std::string& matched_text = terminal_node->token->matched;
            // String Literal
            if (matched_text.length() >= 2 && matched_text.front() == '"' && matched_text.back() == '"') {
                std::cerr << "[IR_GEN] visitPrimaryExp: Found string literal: " << matched_text << std::endl;
                std::string processed_string = processRawStringLiteral(matched_text);
                if (!this->program) {
                    std::cerr << "[IR_GEN_ERR] visitPrimaryExp: this->program is null! Cannot add string literal." << std::endl;
                    return nullptr;
                }
                std::string string_label = this->program->addStringLiteral(processed_string);
                std::cerr << "[IR_GEN] visitPrimaryExp: String literal '" << processed_string << "' assigned label: " << string_label << std::endl;
                return std::make_shared<IR::IRLabelOperand>(string_label);
            }
            // Number
            else if (isdigit(matched_text[0]) || (matched_text.length() > 1 && matched_text[0] == '-' && isdigit(matched_text[1]))) {
                std::cerr << "[IR_GEN] visitPrimaryExp: Visiting Number (direct terminal)." << std::endl;
                return this->visitNumber(terminal_node);
            }
        }
    }

    // Fallback or error for PrimaryExp
    std::string child_info_str = "null_child_base_pointer";
    if (first_child_base) {
        if (auto nt_child = std::dynamic_pointer_cast<AST::NonTerminalNode>(first_child_base)) {
            child_info_str = "NonTerminalNode(name: " + nt_child->name + ")";
        } else if (auto t_child = std::dynamic_pointer_cast<AST::TerminalNode>(first_child_base)) {
            child_info_str = "TerminalNode(token: " + (t_child->token ? t_child->token->matched : "<no_token>") + ")";
        } else {
            child_info_str = "UnknownChildNodeType(typeid: " + std::string(typeid(*first_child_base).name()) + ")";
        }
    }
    std::cerr << "[IR_GEN_ERR] visitPrimaryExp: Unsupported PrimaryExp structure. Node: " << (node ? node->name : "null_node")
              << " with child: " << child_info_str << std::endl;
    return nullptr;
}

// For simplicity, in a sequence like Exp -> AddExp -> MulExp -> UnaryExp -> PrimaryExp,
// if there are no actual operators, these just pass through the result from their child.

std::shared_ptr<IR::IROperand> IRGenerator::visitUnaryExp(PNNode node) {
    std::cerr << "[IR_GEN] visitUnaryExp() called for node: " << (node ? node->name : "<null_node>") << std::endl;
    if (!node || node->children.empty()) {
        std::cerr << "[IR_GEN_ERR] visitUnaryExp: Null or empty UnaryExp node." << std::endl;
        return nullptr;
    }

    // Try to identify a function call structure first, e.g., ID ( ARG_LIST )
    // AST seems to be: UnaryExp(ID_Terminal, LPAREN_Terminal, FuncRParams_NonTerminal, RPAREN_Terminal)
    // Or for no-arg call: UnaryExp(ID_Terminal, LPAREN_Terminal, FuncRParams_NonTerminal_empty, RPAREN_Terminal)
    // The FuncRParams node itself might be optional or its children list empty if grammar is like that.

    if (node->children.size() >= 2) {  // Minimum for ID() assuming FuncRParams might be absent or children[1] is '('
        auto first_child = node->children.at(0);
        auto id_node = std::dynamic_pointer_cast<AST::TerminalNode>(first_child);

        // Check for function call pattern: ID ( ... )
        // More robustly, check token types of children for ID, LPAREN, etc.
        // For now, assume structure based on typical parsers: ID is first child, then '('
        if (id_node && node->children.size() >= 3) {  // Need at least ID, '(', ')' for a basic call ID()
                                                      // If FuncRParams is child 2: ID, '(', FuncRParams, ')' -> size 4
                                                      // If FuncRParams is child 1 (after ID): ID, FuncRParams -> size 2, FuncRParams has '(',')'
            // Let's assume based on the AST dump: UnaryExp("printf", "(", FuncRParams(...), ")")
            // Child 0: "printf" (TerminalNode)
            // Child 1: "(" (TerminalNode)
            // Child 2: FuncRParams (NonTerminalNode)
            // Child 3: ")" (TerminalNode)

            if (node->children.size() == 4) {  // Expecting ID, LPAREN, FuncRParams, RPAREN
                auto lparen_node = std::dynamic_pointer_cast<AST::TerminalNode>(node->children.at(1));
                auto func_rparams_node_base = node->children.at(2);  // This could be FuncRParams or ExpList etc.
                auto rparen_node = std::dynamic_pointer_cast<AST::TerminalNode>(node->children.at(3));

                // Basic structural check
                if (id_node && lparen_node && lparen_node->token && lparen_node->token->matched == "(" &&
                    rparen_node && rparen_node->token && rparen_node->token->matched == ")") {
                    auto func_rparams_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(func_rparams_node_base);
                    // func_rparams_node can be null if FuncRParams is optional and not present for a no-arg call,
                    // OR if the grammar always includes a FuncRParams node which might be empty.
                    // visitFunctionCall should handle a potentially null paramsNode.

                    std::cerr << "[IR_GEN] visitUnaryExp: Detected function call for ID: " << id_node->token->matched << std::endl;
                    return this->visitFunctionCall(id_node, func_rparams_node);
                }
            }
        }
    }

    // Original logic for UnaryExp -> PrimaryExp or UnaryExp -> other single expression child
    // This path is taken if it's not a function call structure like ID (PARAMS)
    auto child_exp_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(0));
    if (child_exp_node) {
        if (child_exp_node->name == "PrimaryExp") {
            std::cerr << "[IR_GEN] visitUnaryExp: UnaryExp -> PrimaryExp. Visiting PrimaryExp." << std::endl;
            return this->visitPrimaryExp(child_exp_node);
        } else if (child_exp_node->name == "FunctionCall") {
            // This case might be if your AST *already* has a dedicated "FunctionCall" non-terminal under UnaryExp
            // The AST dump showed UnaryExp directly containing terminals for ID, '(', etc. not a FunctionCall node.
            // If it *can* be a FunctionCall node, its structure would be: FunctionCall(ID, FuncRParams)
            std::cerr << "[IR_GEN] visitUnaryExp: UnaryExp -> FunctionCall node. Visiting FunctionCall node." << std::endl;
            if (child_exp_node->children.size() >= 1) {  // At least ID
                auto func_id_node = std::dynamic_pointer_cast<AST::TerminalNode>(child_exp_node->children.at(0));
                PNNode func_args_node = nullptr;
                if (child_exp_node->children.size() > 1) {  // Optional args node
                    func_args_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(child_exp_node->children.at(1));
                }
                return this->visitFunctionCall(func_id_node, func_args_node);
            }
        }
        // Fallback for other non-terminal children of UnaryExp if not PrimaryExp or FunctionCall node
        // This is a bit of a catch-all; ideally, UnaryExp productions are more specific.
        std::cerr << "[IR_GEN] visitUnaryExp: UnaryExp -> NonTerminal(" << child_exp_node->name << "). Attempting generic dispatch." << std::endl;
        return this->dispatchVisitExp(child_exp_node);  // Generic dispatch for other expression types under UnaryExp
    }

    std::cerr << "[IR_GEN_ERR] visitUnaryExp: Unhandled UnaryExp structure. First child is not a recognized NonTerminal for expression, nor does it match function call pattern. Child type: "
              << (node->children.at(0) ? typeid(*node->children.at(0)).name() : "null_child_pointer") << std::endl;
    return nullptr;
}

std::shared_ptr<IR::IROperand> IRGenerator::visitMulExp(PNNode node) {
    std::cerr << "[IR_GEN] visitMulExp() called for node: " << (node ? node->name : "<null_node>") << std::endl;
    // Assuming MulExp -> UnaryExp ( ('*'|'/'|'%') UnaryExp )*
    // For "0", it's MulExp -> UnaryExp.
    if (!node || node->children.empty()) { /* TODO: Error */
        return nullptr;
    }
    // TODO: Implement operator handling for '*' '/' '%'.
    // For now, just visit the first child (UnaryExp).
    auto unary_exp_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(0));
    if (unary_exp_node) {
        return this->visitUnaryExp(unary_exp_node);
    }
    return nullptr;
}

std::shared_ptr<IR::IROperand> IRGenerator::visitAddExp(PNNode node) {
    std::cerr << "[IR_GEN] visitAddExp() called for node: " << (node ? node->name : "<null_node>") << std::endl;
    // Assuming AddExp -> MulExp ( ('+'|'-') MulExp )*
    // For "0", it's AddExp -> MulExp.
    if (!node || node->children.empty()) { /* TODO: Error */
        return nullptr;
    }
    // TODO: Implement operator handling for '+' '-'.
    // For now, just visit the first child (MulExp).
    auto mul_exp_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(0));
    if (mul_exp_node) {
        return this->visitMulExp(mul_exp_node);
    }
    return nullptr;
}

std::shared_ptr<IR::IROperand> IRGenerator::visitExp(PNNode node) {  // node is Exp, LOrExp, LAndExp, EqExp, RelExp, AddExp, or MulExp
    std::cerr << "[IR_GEN] visitExp (generic pass-through) called for node: " << (node ? node->name : "<null_node>") << std::endl;
    if (!node || node->children.empty()) {
        std::cerr << "[IR_GEN] visitExp: Node " << (node ? node->name : "<null_node>") << " is null or has no children." << std::endl;
        return nullptr;
    }
    // These nodes, in the absence of an operator, pass to their first child (next precedence level).
    PNode child_ast_node = node->children.at(0);  // PNode is std::shared_ptr<AST::Node>
    std::cerr << "[IR_GEN] visitExp: Dispatching to child of " << node->name << "." << std::endl;
    return this->dispatchVisitExp(child_ast_node);
}

std::shared_ptr<IR::IROperand> IRGenerator::dispatchVisitExp(PNode node_base) {
    std::cerr << "[IR_GEN_DBG] ENTERING dispatchVisitExp. node_base is " << (node_base ? "valid_ptr" : "nullptr") << std::endl;

    if (!node_base) {
        std::cerr << "[IR_GEN] dispatchVisitExp: Null node_base." << std::endl;
        return nullptr;
    }

    // Try to cast to NonTerminalNode first, as most Exp structures will be NonTerminals
    auto pn_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node_base);

    if (pn_node) {  // If cast to NonTerminalNode is successful
        std::cerr << "[IR_GEN] dispatchVisitExp: Dispatching for NonTerminalNode: " << pn_node->name << std::endl;
        if (pn_node->name == "Exp" || pn_node->name == "LOrExp" || pn_node->name == "LAndExp" ||
            pn_node->name == "EqExp" || pn_node->name == "RelExp" ||
            pn_node->name == "AddExp" || pn_node->name == "MulExp") {  // Added common precedence ops
            return this->visitExp(pn_node);                            // visitExp likely handles precedence by visiting its child
        } else if (pn_node->name == "UnaryExp") {
            return this->visitUnaryExp(pn_node);
        } else if (pn_node->name == "PrimaryExp") {
            return this->visitPrimaryExp(pn_node);
        } else if (pn_node->name == "Number") {
            if (!pn_node->children.empty()) {
                if (auto terminal_child = std::dynamic_pointer_cast<AST::TerminalNode>(pn_node->children.at(0))) {
                    std::cerr << "[IR_GEN] dispatchVisitExp: Dispatching to visitNumber for child of Number node." << std::endl;
                    return this->visitNumber(terminal_child);
                }
            }
            std::cerr << "[IR_GEN] dispatchVisitExp: Number node does not have expected terminal child." << std::endl;
            return nullptr;
        } else if (pn_node->name == "LVal") {
            std::cerr << "[IR_GEN] dispatchVisitExp: LVal encountered. Needs visitLVal implementation." << std::endl;
            // return this->visitLVal(pn_node); // Implement this
            return nullptr;
        } else if (pn_node->name == "FunctionCall") {
            std::cerr << "[IR_GEN] dispatchVisitExp: FunctionCall node encountered." << std::endl;
            if (pn_node->children.size() >= 1) {
                auto func_id_node = std::dynamic_pointer_cast<AST::TerminalNode>(pn_node->children.at(0));
                PNNode func_args_node = nullptr;
                if (pn_node->children.size() > 1) {
                    func_args_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(pn_node->children.at(1));
                }
                return this->visitFunctionCall(func_id_node, func_args_node);
            }
            return nullptr;
        }
        // ... other specific expression types like LVal, FunctionCall ...
        std::cerr << "[IR_GEN] dispatchVisitExp: Unhandled NonTerminalNode expression type: " << pn_node->name << std::endl;
        return nullptr;  // Unhandled type

    } else if (auto pt_node = std::dynamic_pointer_cast<AST::TerminalNode>(node_base)) {
        // This case would be if an expression could be a single terminal
        std::cerr << "[IR_GEN] dispatchVisitExp: Node is a TerminalNode. Token matched: \"" << (pt_node->token ? pt_node->token->matched : "<no_token>") << "\"" << std::endl;
        // Example: if your INT_CONST token type maps to numbers:
        // We need to find your actual token type for integers. Let's assume it's "IntegerLiteral" or "NUMBER"
        // This needs to be checked against your tokenizer_spec.cpp or tokenizer.hpp
        // For now, let's assume visitNumber can handle a PTNode directly
        // if (pt_node->token && pt_node->token->token_type == Tokenizer::TokenType::YOUR_INT_TOKEN_TYPE_HERE) { // Replace with actual token type
        //    return this->visitNumber(pt_node);
        // }
        std::cerr << "[IR_GEN] dispatchVisitExp: Unhandled TerminalNode for expression. Matched: \"" << (pt_node->token ? pt_node->token->matched : "<no_token>") << "\"" << std::endl;
        return nullptr;
    }

    std::cerr << "[IR_GEN] dispatchVisitExp: Node type not recognized for dispatch. node_base is " << (node_base ? "valid_ptr" : "nullptr") << ".";
    if (node_base) {
        std::cerr << " TypeID: " << typeid(*node_base).name();
    }
    std::cerr << std::endl;
    return nullptr;
}

// --- Implementation of missing helper methods ---

void IRGenerator::addInstruction(std::shared_ptr<IR::IRInstruction> inst) {
    if (this->currentNormalFunction) {
        this->currentNormalFunction->addInstruction(inst);
    } else {
        // TODO: Handle error: Trying to add instruction while not in a function context.
        // This might happen if a global initializer needs to generate instructions,
        // which would require a different mechanism or a special global init function.
        // For typical function body generation, currentNormalFunction should be set.
        // throw std::runtime_error("Attempted to add instruction outside of a function context.");
    }
}

std::shared_ptr<IR::IRVariable> IRGenerator::createNamedVar(
    const std::string& name,
    std::shared_ptr<IR::IRType> type) {
    // This helper primarily creates the IRVariable object.
    // Adding it to symbol table or function locals depends on context (parameter, local decl, etc.)
    // and is usually handled by the specific visitor (visitFuncFParam, visitVarDef).
    return std::make_shared<IR::IRVariable>(name, type);
}

std::shared_ptr<IR::IRVariable> IRGenerator::createTempSimpleVar(
    IR::SimpleTypeKind kind,
    const std::string& prefix) {
    std::string temp_name = prefix + std::to_string(this->tempVarCounter++);
    auto var_type = std::make_shared<IR::SimpleIRType>(kind);
    auto temp_var = std::make_shared<IR::IRVariable>(temp_name, var_type);

    if (this->currentNormalFunction) {
        // Add to the current function's list of local variables if we are in a function context.
        // PureIRFunctions also have locals, but this helper is more for NormalIRFunctions usually.
        this->currentNormalFunction->addLocal(temp_var);
    } else {
        // TODO: Handle error or decide behavior if not in a function.
        // Temporaries are usually function-local.
        // throw std::runtime_error("Attempted to create temporary variable outside of a function context.");
    }
    return temp_var;
}

std::shared_ptr<IR::IRLabelOperand> IRGenerator::createLabel(const std::string& prefix) {
    std::string label_name = prefix + std::to_string(this->labelCounter++);
    return std::make_shared<IR::IRLabelOperand>(label_name);
}

// Constructor might need to initialize counters
IRGenerator::IRGenerator()
    : program(nullptr), currentNormalFunction(nullptr), tempVarCounter(0), labelCounter(0) {  // Initialize program and currentNormalFunction to nullptr
    // program = std::make_shared<IR::IRProgram>(); // Usually done in generate()
    // defineBuiltinPureFunctions(); // If you have this method for pre-populating pure functions
}

std::shared_ptr<IR::IRProgram> IRGenerator::generate(PNode rootAstNode) {
    std::cerr << "[IR_GEN] generate() called" << std::endl;
    this->program = std::make_shared<IR::IRProgram>();
    this->tempVarCounter = 0;
    this->labelCounter = 0;

    // Pre-define library functions like printf
    auto void_type = std::make_shared<IR::VoidIRType>();
    // For printf's first parameter (format string), use a placeholder type.
    // Addresses/pointers are often treated as integers.
    auto placeholder_ptr_type = std::make_shared<IR::SimpleIRType>(IR::SimpleTypeKind::INTEGER);
    auto format_param_var = std::make_shared<IR::IRVariable>("%format_str_param", placeholder_ptr_type);
    std::vector<std::shared_ptr<IR::IRVariable>> printf_params;
    printf_params.push_back(format_param_var);

    auto printf_func_decl = std::make_shared<IR::NormalIRFunction>("printf", printf_params, void_type);
    this->program->addNormalFunction(printf_func_decl);
    std::cerr << "[IR_GEN] Pre-defined library function: printf" << std::endl;

    if (rootAstNode) {
        this->dispatchVisit(rootAstNode);
    }
    return this->program;
}

void IRGenerator::dispatchVisit(PNode node_base) {
    auto node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node_base);
    if (!node) {
        if (node_base) {
            std::cerr << "[IR_GEN] dispatchVisit() called with TerminalNode or unknown node type. TypeID: " << typeid(*node_base).name() << std::endl;
        } else {
            std::cerr << "[IR_GEN] dispatchVisit() called with null" << std::endl;
        }
        return;
    }
    std::cerr << "[IR_GEN] dispatchVisit() trying to dispatch node: " << node->name << std::endl;

    if (node->name == "GlobalItemList") {
        this->visitGlobalItemList(node);
    } else if (node->name == "GlobalItem") {
        this->visitGlobalItem(node);
    } else if (node->name == "FuncDef") {
        this->visitFuncDef(node);
    } else if (node->name == "CompUnit") {
        this->visitCompUnit(node);
    } else if (node->name == "VarDef") {    // For global VarDefs
        this->visitVarDef(node);            // Uncommented and will call the VarDef visitor
    } else if (node->name == "ConstDef") {  // Added for global ConstDefs
        this->visitConstDef(node);          // Will call the ConstDef visitor
    } else {
        std::cerr << "[IR_GEN] dispatchVisit: Unknown node type for general dispatch: " << node->name << std::endl;
    }
}

void IRGenerator::visitCompUnit(PNNode node) {
    if (!node || node->name != "CompUnit") {
        return;
    }
    for (const auto& child_node_base : node->children) {
        this->dispatchVisit(child_node_base);
    }
}

void IRGenerator::visitGlobalItem(PNNode node) {
    std::cerr << "[IR_GEN] visitGlobalItem() called for node: " << (node ? node->name : "null") << std::endl;
    if (!node || node->name != "GlobalItem" || node->children.empty()) {
        return;
    }
    this->dispatchVisit(node->children.at(0));
}

void IRGenerator::visitGlobalItemList(PNNode node) {
    std::cerr << "[IR_GEN] visitGlobalItemList() called for node: " << (node ? node->name : "null") << std::endl;
    if (!node || node->name != "GlobalItemList") {
        return;
    }
    for (const auto& child_node_base : node->children) {
        auto global_item_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(child_node_base);
        if (global_item_node && global_item_node->name == "GlobalItem") {
            this->visitGlobalItem(global_item_node);
        } else if (global_item_node) {
            std::cerr << "[IR_GEN] visitGlobalItemList() dispatching directly for child: " << global_item_node->name << std::endl;
            this->dispatchVisit(global_item_node);
        }
    }
}

// Modify visitDecl to handle global constant initializers
void IRGenerator::visitDecl(PNNode decl_list_node, std::shared_ptr<IR::IRType> base_ir_type, bool is_const) {
    std::cerr << "[IR_GEN] visitDecl() for " << (decl_list_node ? decl_list_node->name : "null")
              << " with base_type: " << (base_ir_type ? base_ir_type->toString() : "null_type")
              << ", is_const: " << is_const << std::endl;

    if (!decl_list_node || !base_ir_type) {
        std::cerr << "[IR_GEN_ERR] visitDecl: Null decl_list_node or base_ir_type." << std::endl;
        return;
    }

    for (const auto& decl_child_base : decl_list_node->children) {
        auto decl_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(decl_child_base);
        if (!decl_node)
            continue;

        // Modified to accept "VarDefItem" as it's collected by visitVarDef/visitConstDef
        if (decl_node->name == "VarDecl" || decl_node->name == "ConstDecl" || decl_node->name == "VarDefItem") {
            if (decl_node->children.empty()) {
                std::cerr << "[IR_GEN_ERR] visitDecl: Empty VarDecl/ConstDecl/VarDefItem node." << std::endl;
                continue;
            }

            // VarDefItem structure: IDENT (children[0]), (optional: ArrayDim), (optional: ASSIGN, InitVal)
            // visitDecl expects IDENT at children[0] of the item node.
            auto ident_node = std::dynamic_pointer_cast<AST::TerminalNode>(decl_node->children.at(0));
            if (!ident_node || !ident_node->token) {
                std::cerr << "[IR_GEN_ERR] visitDecl: Could not get identifier from " << decl_node->name << "." << std::endl;
                continue;
            }
            std::string var_name = ident_node->token->matched;
            std::cerr << "[IR_GEN] visitDecl: Processing variable: " << var_name << " from node named " << decl_node->name << std::endl;

            auto ir_variable = std::make_shared<IR::IRVariable>(var_name, base_ir_type);
            ir_variable->is_const = is_const;  // Explicitly set based on parameter

            if (!symbolTable.addSymbol(var_name, ir_variable, base_ir_type, is_const)) {
                std::cerr << "[IR_GEN_ERR] visitDecl: Failed to add symbol '" << var_name << "' to symbol table." << std::endl;
                continue;
            }
            std::cerr << "[IR_GEN] visitDecl: Added '" << var_name << "' to symbol table. is_const: " << is_const << std::endl;

            bool is_global = (this->currentNormalFunction == nullptr);
            ir_variable->is_global = is_global;  // Mark global status on IRVariable

            if (!is_global) {                       // Local variable
                if (this->currentNormalFunction) {  // Ensure current function context exists
                    this->currentNormalFunction->addLocal(ir_variable);
                    std::cerr << "[IR_GEN] visitDecl: Added '" << var_name << "' to local variables of function " << this->currentNormalFunction->name << std::endl;
                } else {
                    std::cerr << "[IR_GEN_ERR] visitDecl: currentNormalFunction is null for a non-global variable '" << var_name << "'." << std::endl;
                    continue;  // Cannot add local var without function context
                }
            } else {  // Global variable
                if (this->program) {
                    this->program->addGlobalVariable(ir_variable);
                    std::cerr << "[IR_GEN] visitDecl: Added '" << var_name << "' to global variables." << std::endl;
                } else {
                    std::cerr << "[IR_GEN_ERR] visitDecl: this->program is null, cannot add global variable '" << var_name << "'." << std::endl;
                    continue;
                }
            }

            // Initializer processing: A VarDefItem can have children like: IDENT, "=", InitVal
            // or IDENT, ArrayDim, "=", InitVal
            // We need to find the "=" and then the InitVal node.
            // Let's search for "=" token among children of decl_node (VarDefItem).
            PNode initializer_node = nullptr;
            for (size_t i = 1; i < decl_node->children.size(); ++i) {  // Start search after IDENT
                auto child = decl_node->children[i];
                if (auto terminal_child = std::dynamic_pointer_cast<AST::TerminalNode>(child)) {
                    if (terminal_child->token && terminal_child->token->matched == "=") {
                        if (i + 1 < decl_node->children.size()) {
                            // The node after "=" should be InitVal (which itself contains an Exp)
                            auto init_val_wrapper_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(decl_node->children[i + 1]);
                            if (init_val_wrapper_node && init_val_wrapper_node->name == "InitVal" && !init_val_wrapper_node->children.empty()) {
                                initializer_node = init_val_wrapper_node->children.at(0);  // Get the Exp node from InitVal
                                std::string init_node_name_for_log = "<unknown_init_node_type>";
                                if (auto nt_init_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(initializer_node)) {
                                    init_node_name_for_log = nt_init_node->name;
                                } else if (auto t_init_node = std::dynamic_pointer_cast<AST::TerminalNode>(initializer_node)) {
                                    init_node_name_for_log = "TerminalNode(" + (t_init_node->token ? t_init_node->token->matched : "<no_token>") + ")";
                                }
                                std::cerr << "[IR_GEN] visitDecl: Found initializer for '" << var_name << "'. InitVal contains node: " << init_node_name_for_log << std::endl;
                            } else {
                                std::cerr << "[IR_GEN_ERR] visitDecl: Expected InitVal node after \'=\' for '" << var_name << "', found different structure." << std::endl;
                            }
                        }
                        break;
                    }
                }
            }

            if (initializer_node) {
                std::shared_ptr<IR::IROperand> init_val_operand = this->dispatchVisitExp(initializer_node);
                if (init_val_operand) {
                    if (is_global) {
                        if (auto const_init_val = std::dynamic_pointer_cast<IR::IRConstant>(init_val_operand)) {
                            ir_variable->global_initializer_constant = const_init_val;
                            std::cerr << "[IR_GEN] visitDecl: Stored constant initializer for global '" << var_name << "'." << std::endl;
                        } else {
                            std::cerr << "[IR_GEN_ERR] visitDecl: Non-constant initializer for global variable '" << var_name << "' is not supported for direct data segment init." << std::endl;
                        }
                    } else {                                // Local variable initializer
                        if (this->currentNormalFunction) {  // Ensure context for adding instruction
                            auto assign_inst = std::make_shared<IR::AssignInst>(ir_variable, init_val_operand);
                            this->addInstruction(assign_inst);
                            std::cerr << "[IR_GEN] visitDecl: Added AssignInst for local initializer of '" << var_name << "'." << std::endl;
                        } else {
                            std::cerr << "[IR_GEN_ERR] visitDecl: Cannot add AssignInst for '" << var_name << "' due to null currentNormalFunction." << std::endl;
                        }
                    }
                } else {
                    std::cerr << "[IR_GEN_ERR] visitDecl: Failed to generate IR for initializer of '" << var_name << "'." << std::endl;
                }
            } else if (is_const) {
                std::cerr << "[IR_GEN_ERR] visitDecl: ConstDecl for '" << var_name << "' is missing an initializer." << std::endl;
            }
            // Removed recursive call: else if (decl_node->name == decl_list_node->name) { visitDecl(decl_node, base_ir_type, is_const); }
            // The new structure with visitVarDef/visitConstDef and collectVarDefItems should handle iteration correctly.
        }
    }
}

std::shared_ptr<IR::IROperand> IRGenerator::visitLVal(PNNode node, bool forAssignment /*= false*/) {
    std::cerr << "[IR_GEN] visitLVal() called for node: " << (node ? node->name : "null")
              << ", forAssignment: " << forAssignment << std::endl;
    if (!node || node->name != "LVal" || node->children.empty()) {
        std::cerr << "[IR_GEN_ERR] visitLVal: Invalid LVal node." << std::endl;
        return nullptr;
    }

    auto ident_node = std::dynamic_pointer_cast<AST::TerminalNode>(node->children.at(0));
    if (!ident_node || !ident_node->token) {
        std::cerr << "[IR_GEN_ERR] visitLVal: LVal's child is not a valid IDENT token." << std::endl;
        return nullptr;
    }

    std::string var_name = ident_node->token->matched;
    std::cerr << "[IR_GEN] visitLVal: Looking up variable '" << var_name << "'." << std::endl;

    SymbolInfo symbol_info = symbolTable.lookupSymbol(var_name);

    if (!symbol_info.variable) {
        std::cerr << "[IR_GEN_ERR] visitLVal: Variable '" << var_name << "' not found in symbol table." << std::endl;
        return nullptr;
    }

    std::cerr << "[IR_GEN] visitLVal: Found variable '" << var_name << "' in symbol table. Type: "
              << (symbol_info.ir_type ? symbol_info.ir_type->toString() : "unknown_type") << std::endl;

    // The forAssignment flag could be used here later if needed, e.g., to return an address operand
    // for writes vs. a value operand for reads, but for now, returning the IRVariable itself is fine
    // as the AssignInst handles the store, and expressions will load the value.

    return symbol_info.variable;
}

// Helper function to recursively collect VarDefItem nodes
void collectVarDefItems(IRGenerator::PNNode node, std::vector<IRGenerator::PNNode>& items) {
    if (!node)
        return;

    if (node->name == "VarDefItem") {
        items.push_back(node);
    } else if (node->name == "VarDefItemsFollowing") {
        // Structure: VarDefItemsFollowing -> "," VarDefItem VarDefItemsFollowing
        // Or VarDefItemsFollowing -> epsilon (empty children)
        if (node->children.size() >= 2) {  // Expect at least COMMA and VarDefItem
            auto item_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(1));
            if (item_node && item_node->name == "VarDefItem") {
                items.push_back(item_node);
            }
            if (node->children.size() >= 3) {  // Check for nested VarDefItemsFollowing
                auto following_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(2));
                collectVarDefItems(following_node, items);
            }
        }
    }
    // Add other relevant checks if AST structure for lists can vary
}

void IRGenerator::visitVarDef(PNNode var_def_node) {
    std::cerr << "[IR_GEN] visitVarDef() called for node: " << (var_def_node ? var_def_node->name : "null") << std::endl;
    if (!var_def_node || var_def_node->name != "VarDef" || var_def_node->children.empty()) {
        std::cerr << "[IR_GEN_ERR] visitVarDef: Invalid VarDef node." << std::endl;
        return;
    }

    // VarDef can be: Type VarDefItem ... (for non-const)
    // OR: "const" Type VarDefItem ... (for const, if parser emits VarDef for consts)

    PNNode type_node = nullptr;
    size_t items_start_index = 0;  // Index of the first VarDefItem
    bool is_const_decl = false;

    auto first_child = var_def_node->children.at(0);
    if (auto terminal_const = std::dynamic_pointer_cast<AST::TerminalNode>(first_child)) {
        if (terminal_const->token && terminal_const->token->matched == "const") {
            is_const_decl = true;
            if (var_def_node->children.size() < 2) {
                std::cerr << "[IR_GEN_ERR] visitVarDef: VarDef starts with 'const' but missing Type node." << std::endl;
                return;
            }
            type_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(var_def_node->children.at(1));
            items_start_index = 2;  // Type is child 1, first item is child 2
            std::cerr << "[IR_GEN] visitVarDef: Detected 'const' prefix. is_const_decl = true." << std::endl;
        } else {
            // This case should ideally not happen if VarDef starts with a terminal that isn't 'const'.
            // It implies a malformed AST or an unexpected terminal.
            std::cerr << "[IR_GEN_ERR] visitVarDef: VarDef starts with an unexpected terminal: " << terminal_const->token->matched << std::endl;
            return;
        }
    } else if (auto non_terminal_type = std::dynamic_pointer_cast<AST::NonTerminalNode>(first_child)) {
        // Assumed to be Type node directly for non-const VarDef
        type_node = non_terminal_type;
        items_start_index = 1;  // Type is child 0, first item is child 1
        is_const_decl = false;
        std::cerr << "[IR_GEN] visitVarDef: No 'const' prefix, assuming non-const. is_const_decl = false." << std::endl;
    } else {
        std::cerr << "[IR_GEN_ERR] visitVarDef: First child of VarDef is neither 'const' terminal nor Type NonTerminalNode." << std::endl;
        return;
    }

    if (!type_node) {
        std::cerr << "[IR_GEN_ERR] visitVarDef: Failed to identify Type node in VarDef." << std::endl;
        return;
    }
    std::shared_ptr<IR::IRType> base_ir_type = this->astTypeToIrType(type_node);
    if (!base_ir_type) {
        std::cerr << "[IR_GEN_ERR] visitVarDef: Failed to get base IR type." << std::endl;
        return;
    }
    std::cerr << "[IR_GEN] visitVarDef: Base type is " << base_ir_type->toString() << std::endl;

    std::vector<PNNode> actual_decl_items;
    if (var_def_node->children.size() > items_start_index) {
        auto first_item_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(var_def_node->children.at(items_start_index));
        collectVarDefItems(first_item_node, actual_decl_items);  // Collect first VarDefItem

        if (var_def_node->children.size() > items_start_index + 1) {
            auto following_items_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(var_def_node->children.at(items_start_index + 1));
            if (following_items_node && following_items_node->name == "VarDefItemsFollowing") {
                collectVarDefItems(following_items_node, actual_decl_items);  // Collect subsequent items
            }
        }
    }

    if (actual_decl_items.empty()) {
        std::cerr << "[IR_GEN_WARN] visitVarDef: No VarDefItem nodes found in VarDef." << std::endl;
        return;
    }

    // Create a synthetic decl_list_node for visitDecl
    auto synthetic_decl_list_node = std::make_shared<AST::NonTerminalNode>("SyntheticVarDeclList");
    for (const auto& item : actual_decl_items) {
        // visitDecl expects children to be "VarDecl" or "ConstDecl"
        // My VarDefItem seems to match this role. Let's ensure its name is "VarDecl" for visitDecl.
        // This is a bit of a hack; ideally visitDecl is more flexible or VarDefItem is already named VarDecl.
        // For now, we assume VarDefItem's structure is what visitDecl expects from a "VarDecl" node.
        // If VarDefItem has a different name, visitDecl's check `decl_node->name == "VarDecl"` will fail.
        // Let's assume the AST has 'VarDefItem' and `visitDecl` is updated or okay with it.
        // The `visitDecl` currently checks for node names "VarDecl" or "ConstDecl".
        // My VarDefItem nodes are named "VarDefItem". This will be an issue.
        // For now, I will proceed and then fix visitDecl if needed, or rename nodes if simpler.
        // Let's try to make VarDefItem nodes appear as "VarDecl" to visitDecl:
        // One way is to copy them into new nodes with the right name, or rely on visitDecl being tolerant.
        // The provided visitDecl code explicitly checks decl_node->name == "VarDecl".
        // So, the items pushed to synthetic_decl_list_node should be "VarDecl" nodes.
        // My current VarDefItem nodes are likely the ones.
        // Let's assume the `VarDefItem` structure is compatible with what `visitDecl` expects for a "VarDecl".
        // I will rename `item->name` temporarily if that's feasible, or adjust `visitDecl`.
        // The best solution is that `visitDecl` should just process the structure passed.
        // The `decl_node->name == "VarDecl"` check in `visitDecl` is the key.
        // My `collectVarDefItems` collects nodes named `VarDefItem`.
        // I will adjust `visitDecl` to accept `VarDefItem` as well. (This edit will be next).
        synthetic_decl_list_node->children.push_back(item);
    }

    std::cerr << "[IR_GEN] visitVarDef: Calling visitDecl with " << actual_decl_items.size() << " items. is_const_decl: " << is_const_decl << std::endl;
    this->visitDecl(synthetic_decl_list_node, base_ir_type, is_const_decl);
}

void IRGenerator::visitConstDef(PNNode const_def_node) {
    std::cerr << "[IR_GEN] visitConstDef() called for node: " << (const_def_node ? const_def_node->name : "null") << std::endl;

    // AST structure for "const int c = 4;" based on previous logs when it was misparsed by visitStmt:
    // VarDef("const", Type("int"), VarDefItem("c",...), VarDefItemsFollowing(), ";")
    // If the parser creates a "ConstDef" node, its structure might be:
    // ConstDef(Type("int"), ConstDefItem("c",...), ConstDefItemsFollowing(), ";") (if "const" is implicit in node type)
    // Or: ConstDef("const", Type("int"), ...) -> similar to VarDef but with different top node name.

    // Let's assume const_def_node->name is "ConstDef" or "VarDef" (if parser uses VarDef for consts).
    // The first child could be "const" terminal, or Type node.

    if (!const_def_node || const_def_node->children.empty()) {
        std::cerr << "[IR_GEN_ERR] visitConstDef: Invalid ConstDef node." << std::endl;
        return;
    }

    PNNode type_node = nullptr;
    size_t items_start_index = 0;

    auto first_child = const_def_node->children.at(0);
    if (auto terminal_const = std::dynamic_pointer_cast<AST::TerminalNode>(first_child)) {
        if (terminal_const->token && terminal_const->token->matched == "const") {
            // Structure: "const", Type, VarDefItem/ConstDefItem, ...
            if (const_def_node->children.size() < 2) {
                std::cerr << "[IR_GEN_ERR] visitConstDef: Node starts with \'const\' but not enough children for Type." << std::endl;
                return;
            }
            type_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(const_def_node->children.at(1));
            items_start_index = 2;
        } else {
            std::cerr << "[IR_GEN_ERR] visitConstDef: First child is terminal but not \'const\'." << std::endl;
            return;  // Or assume type_node is child 0 if ConstDef node implies constness
        }
    } else if (auto non_terminal_type = std::dynamic_pointer_cast<AST::NonTerminalNode>(first_child)) {
        // Structure: Type, VarDefItem/ConstDefItem, ... (assuming ConstDef node name itself implies constness)
        type_node = non_terminal_type;
        items_start_index = 1;
    } else {
        std::cerr << "[IR_GEN_ERR] visitConstDef: Cannot determine Type node from ConstDef structure." << std::endl;
        return;
    }

    if (!type_node) {
        std::cerr << "[IR_GEN_ERR] visitConstDef: Type node is null." << std::endl;
        return;
    }
    std::shared_ptr<IR::IRType> base_ir_type = this->astTypeToIrType(type_node);
    if (!base_ir_type) {
        std::cerr << "[IR_GEN_ERR] visitConstDef: Failed to get base IR type." << std::endl;
        return;
    }
    std::cerr << "[IR_GEN] visitConstDef: Base type is " << base_ir_type->toString() << std::endl;

    std::vector<PNNode> actual_decl_items;
    if (const_def_node->children.size() > items_start_index) {
        auto first_item_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(const_def_node->children.at(items_start_index));
        collectVarDefItems(first_item_node, actual_decl_items);  // Collect first VarDefItem/ConstDefItem

        if (const_def_node->children.size() > items_start_index + 1) {
            auto following_items_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(const_def_node->children.at(items_start_index + 1));
            if (following_items_node && following_items_node->name == "VarDefItemsFollowing") {  // Or ConstDefItemsFollowing
                collectVarDefItems(following_items_node, actual_decl_items);                     // Collect subsequent items
            }
        }
    }

    if (actual_decl_items.empty()) {
        std::cerr << "[IR_GEN_WARN] visitConstDef: No VarDefItem/ConstDefItem nodes found in ConstDef." << std::endl;
        return;
    }

    auto synthetic_decl_list_node = std::make_shared<AST::NonTerminalNode>("SyntheticConstDeclList");
    for (const auto& item : actual_decl_items) {
        // Assuming `visitDecl` will be updated to handle "VarDefItem" as if it were "ConstDecl"
        // when `is_const` is true.
        synthetic_decl_list_node->children.push_back(item);
    }

    std::cerr << "[IR_GEN] visitConstDef: Calling visitDecl with " << actual_decl_items.size() << " items." << std::endl;
    this->visitDecl(synthetic_decl_list_node, base_ir_type, true /*is_const*/);
}

}  // namespace IRGenerator