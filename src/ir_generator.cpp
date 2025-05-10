#include "ir_generator.hpp"
#include <iostream>  // Required for std::cerr

namespace IRGenerator {

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
    for (const auto& param : ir_params) {
        if (!symbolTable.addSymbol(param->name, param, param->type)) {
            // Error
        }
    }
    std::cerr << "[IR_GEN] visitFuncDef: Symbol table scope entered, parameters added." << std::endl;

    // 9. Visit the function body (Block node - child at index 5)
    auto block_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(5));
    if (!block_node) {
        std::cerr << "[IR_GEN] visitFuncDef: Failed to cast Block node from child @ index 5." << std::endl;
        // Potentially allow functions with no body if grammar supports declarations vs definitions differently
        // but for a definition, a block is usually expected.
    } else {
        std::cerr << "[IR_GEN] visitFuncDef: Visiting block node: " << block_node->name << std::endl;
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
    auto actual_statement_or_vardef_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(blockItemNode->children.at(0));

    if (!actual_statement_or_vardef_node) {
        std::cerr << "[IR_GEN] visitBlockItem: Child of BlockItem is not a NonTerminalNode." << std::endl;
        return;
    }

    std::cerr << "[IR_GEN] visitBlockItem: Dispatching to visitStmt (or VarDef) for child: " << actual_statement_or_vardef_node->name << std::endl;
    // TODO: Differentiate between Stmt and VarDef based on actual_statement_or_vardef_node->name
    // if (actual_statement_or_vardef_node->name == "VarDef") { this->visitVarDef(actual_statement_or_vardef_node); }
    // else { this->visitStmt(actual_statement_or_vardef_node); }
    this->visitStmt(actual_statement_or_vardef_node);  // Assuming it's a Stmt for now
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
    std::cerr << "[IR_GEN] visitReturnStmt() called for node: " << (node ? node->name : "null") << std::endl;

    std::optional<std::shared_ptr<IR::IROperand>> return_value_operand = std::nullopt;

    // AST for return statement: Stmt -> "return" Exp ";"
    // Or for void return: Stmt -> "return" ";"
    if (node && !node->children.empty()) {
        // Child 0 is "return" keyword.
        // Child 1 could be Exp or ";"
        if (node->children.size() > 1) {
            auto second_child = node->children.at(1);
            // Check if the second child is not the semicolon, implying it's an expression.
            bool is_expression_present = true;
            if (auto terminal_node = std::dynamic_pointer_cast<AST::TerminalNode>(second_child)) {
                if (terminal_node->token && terminal_node->token->matched == ";") {
                    is_expression_present = false;  // It's "return ;"
                }
            }

            if (is_expression_present) {
                // Ensure we are not trying to visit a semicolon as an expression
                if (node->children.size() >= 2) {  // "return" Exp ...
                    // The expression is the second child.
                    PNode exp_ast_node = node->children.at(1);
                    std::cerr << "[IR_GEN] visitReturnStmt: Visiting expression for return." << std::endl;
                    std::cerr << "[IR_GEN] visitReturnStmt: BEFORE calling dispatchVisitExp. exp_ast_node is " << (exp_ast_node ? "valid_ptr" : "nullptr") << "." << std::endl;
                    return_value_operand = this->dispatchVisitExp(exp_ast_node);
                    std::cerr << "[IR_GEN] visitReturnStmt: AFTER calling dispatchVisitExp. Operand has_value(): "
                              << return_value_operand.has_value() << "." << std::endl;
                    if (return_value_operand.has_value()) {
                        std::cerr << "[IR_GEN] visitReturnStmt: Operand value is " << (return_value_operand.value() ? "valid_ptr" : "nullptr") << "." << std::endl;
                    }

                    if (!return_value_operand.has_value() || !return_value_operand.value()) {
                        std::cerr << "[IR_GEN] visitReturnStmt: Failed to generate IR for return expression (operand is nullopt or nullptr)." << std::endl;
                    }
                } else {
                    std::cerr << "[IR_GEN] visitReturnStmt: Return statement with expression expected at least 2 children (return, Exp), found " << node->children.size() << std::endl;
                }
            } else {
                std::cerr << "[IR_GEN] visitReturnStmt: Detected void return (return ;)." << std::endl;
            }
        } else {
            // This case (e.g. only "return" child) might be an incomplete AST or a grammar that allows "return" without ";"
            std::cerr << "[IR_GEN] visitReturnStmt: Return statement has only one child (the 'return' keyword). Assuming void return or expecting semicolon to be handled by parser." << std::endl;
        }
    } else {
        std::cerr << "[IR_GEN] visitReturnStmt: Return Stmt node is null or has no children. Cannot process." << std::endl;
        // Potentially throw an error or handle as appropriate
        // For now, will proceed and create a ReturnInst with no operand.
    }

    auto actual_return_inst = std::make_shared<IR::ReturnInst>(return_value_operand);
    this->addInstruction(actual_return_inst);
    std::cerr << "[IR_GEN] visitReturnStmt: Added RETURN instruction." << std::endl;
}

void IRGenerator::visitStmt(PNNode stmtNode) {
    std::cerr << "[IR_GEN] visitStmt() called for node: " << (stmtNode ? stmtNode->name : "null") << std::endl;
    if (!stmtNode) {
        return;
    }

    // The AST for return is: Stmt("return", Exp, ";")
    // So, stmtNode->name might be generic like "Stmt".
    // We need to look at its children to determine the kind of statement.
    // Child 0: TerminalNode("return")
    // Child 1: Exp node
    // Child 2: TerminalNode(";")

    if (stmtNode->children.empty()) {
        std::cerr << "[IR_GEN] visitStmt: Empty Stmt node." << std::endl;
        return;
    }

    auto first_child_of_stmt = stmtNode->children.at(0);
    if (auto terminal_child = std::dynamic_pointer_cast<AST::TerminalNode>(first_child_of_stmt)) {
        std::cerr << "[IR_GEN] visitStmt: First child is terminal: " << terminal_child->token->matched << std::endl;
        if (terminal_child->token->matched == "return") {
            // Pass the Stmt node itself to visitReturnStmt, which expects it.
            // visitReturnStmt will then look into its children for Exp.
            this->visitReturnStmt(stmtNode);
        }
        // else if (terminal_child->token->matched == "if") { ... }
        // ... other keyword-based statements
    } else if (auto non_terminal_child = std::dynamic_pointer_cast<AST::NonTerminalNode>(first_child_of_stmt)) {
        // This could be an assignment (LVal = Exp) or an ExpStmt
        // If Stmt -> Exp (for expression statements)
        // if (non_terminal_child->name == "Exp") { this->visitExpStmt(stmtNode); }
        // If Stmt -> LVal AssignOp Exp (for assignments)
        // Need to check for assignment structure here.
        std::cerr << "[IR_GEN] visitStmt: First child is non-terminal: " << non_terminal_child->name << ". (Might be ExpStmt or AssignStmt, unhandled for now)" << std::endl;
    }
    // else if (node->name == "ExpStmt") { this->visitExpStmt(node); }
    // ... other specific statement type checks based on node->name if your AST has them ...
}

// Helper to visit a list of actual parameters (arguments) in a function call
std::vector<std::shared_ptr<IR::IROperand>> IRGenerator::visitFuncRParams(PNNode node) {
    // Assuming FuncRParams AST Node structure, e.g.:
    // node->name == "FuncRParams"
    // node->children are a list of Exp nodes for each argument.
    std::vector<std::shared_ptr<IR::IROperand>> ir_args;

    if (!node) {  // Can be null if no arguments are passed and grammar allows it.
        return ir_args;
    }

    if (node->name != "FuncRParams" && node->name != "ExpList") {  // Adjust to your AST names for arg lists
        // TODO: Error handling or specific logic if an unexpected node type is here.
        // If it's a single expression, it might not be wrapped in a list node.
        // For now, assume it is a list or empty.
        return ir_args;
    }

    for (const auto& child_exp_node : node->children) {
        auto exp_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(child_exp_node);
        if (exp_node) {
            std::shared_ptr<IR::IROperand> arg_operand = this->dispatchVisitExp(exp_node);
            if (arg_operand) {
                ir_args.push_back(arg_operand);
            } else {
                // TODO: Handle error: failed to generate IR for an argument expression
                // Consider adding a placeholder or throwing
            }
        } else {
            // TODO: Handle error: argument node is not an AST::NonTerminalNode
        }
    }
    return ir_args;
}

std::shared_ptr<IR::IROperand> IRGenerator::visitFunctionCall(PTNode idNode, PNNode paramsNode) {
    // idNode: TerminalNode for the function identifier.
    // paramsNode: NonTerminalNode for actual parameters ("FuncRParams" or similar), can be null.

    if (!this->currentNormalFunction) {
        // TODO: Error or special handling if function call happens outside a normal function context.
        // (This might be valid for global initializers in some languages, but not for simple C-like calls)
        return nullptr;
    }
    if (!idNode || !idNode->token) {
        // TODO: Error: invalid function identifier node
        return nullptr;
    }

    std::string callee_name = idNode->token->matched;

    // 1. Visit actual parameters to get their IR operands
    std::vector<std::shared_ptr<IR::IROperand>> ir_arguments = this->visitFuncRParams(paramsNode);

    // 2. Determine if function is void or returns a value.
    // This requires looking up the function signature from the symbol table
    // or having conventions for external functions.
    // For now, let's assume we need to look it up. If it's an external function like "printf",
    // we might have a predefined signature or assume it based on usage.

    // Placeholder: How to get return type?
    // SymbolEntry* func_symbol = symbolTable.lookupSymbolEntry(callee_name);
    // std::shared_ptr<IR::IRType> return_type = nullptr;
    // if (func_symbol && func_symbol->type) { // Assuming type stores FunctionType or similar
    //    auto func_type = std::dynamic_pointer_cast<IR::FunctionType>(func_symbol->type); // Need IR::FunctionType
    //    if (func_type) return_type = func_type->returnType;
    // }
    // For a "print" function, it's likely void. For others, you'd need this info.
    // Let's assume for now we might need a result variable if it's not known to be void.

    bool is_known_void_function = (callee_name == "print" || callee_name == "printf");  // Simplification

    std::optional<std::shared_ptr<IR::IRVariable>> result_destination = std::nullopt;
    std::shared_ptr<IR::IRVariable> result_operand_for_caller = nullptr;

    // If the function is not known to be void, create a temporary to store its result.
    // (A more sophisticated approach would get the actual return type)
    if (!is_known_void_function) {
        // Assuming it returns an INTEGER if not void. This is a big simplification.
        // You need the actual return type of the function here.
        // For now, let's create a temp integer var if we expect a result.
        // The decision to create this should ideally depend on whether the call's result is USED.
        auto temp_var = this->createTempSimpleVar(IR::SimpleTypeKind::INTEGER);
        result_destination = temp_var;
        result_operand_for_caller = temp_var;
    }

    // 3. Create CallNormalInst (assuming all calls are to normal functions for now)
    //    If you have pure functions, you'd need logic to choose CallPureInst.
    auto call_inst = std::make_shared<IR::CallNormalInst>(callee_name, ir_arguments, result_destination);
    this->addInstruction(call_inst);

    // 4. Return the variable where the result is stored (if any)
    return result_operand_for_caller;  // This will be nullptr if function is void or result not captured
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
    // Assuming PrimaryExp AST Node structure, e.g.:
    // node->name == "PrimaryExp"
    // Child can be:
    // 1. '(' Exp ')'  (paren_exp_node)
    // 2. LVal
    // 3. Number (TerminalNode or a NonTerminalNode wrapping a TerminalNode)

    if (!node || node->children.empty()) {
        // TODO: Handle error: Invalid PrimaryExp node
        return nullptr;
    }

    auto first_child_base = node->children.at(0);

    // Case 1: Parenthesized Expression '(' Exp ')'
    if (auto non_terminal_exp_child = std::dynamic_pointer_cast<AST::NonTerminalNode>(first_child_base)) {
        if (non_terminal_exp_child->name == "Exp") {  // Or whatever your AST calls the top-level expression node
            return this->dispatchVisitExp(non_terminal_exp_child);
        }
        // Case 3 (part 2): Number wrapped in a NonTerminalNode (e.g., PrimaryExp -> NumberNode -> TerminalToken)
        else if (non_terminal_exp_child->name == "Number" && !non_terminal_exp_child->children.empty()) {
            if (auto actual_number_terminal = std::dynamic_pointer_cast<AST::TerminalNode>(non_terminal_exp_child->children.at(0))) {
                return this->visitNumber(actual_number_terminal);
            }
        }
        // Case 2: LVal (if PrimaryExp -> LVal)
        // else if (non_terminal_exp_child->name == "LVal") {
        //     return this->visitLVal(non_terminal_exp_child); // visitLVal would handle loading the value
        // }
    }

    // Case 3 (part 1): Number as a direct TerminalNode child of PrimaryExp
    else if (auto terminal_node = std::dynamic_pointer_cast<AST::TerminalNode>(first_child_base)) {
        // We need a way to confirm this terminal IS a number.
        // Using its token type or relying on visitNumber to handle non-numbers gracefully.
        // A more robust check would be: if (terminal_node->token->type == Tokenizer::TokenType::INTEGER_LITERAL) { ... }
        // For now, a basic check on the matched string. This should ideally be based on token type.
        if (terminal_node->token && !terminal_node->token->matched.empty() &&
            (isdigit(terminal_node->token->matched[0]) ||
             (terminal_node->token->matched.length() > 1 && terminal_node->token->matched[0] == '-' && isdigit(terminal_node->token->matched[1])))) {
            return this->visitNumber(terminal_node);
        }
    }

    // Fallback or error
    // throw std::runtime_error("Unsupported PrimaryExp structure: " + node->name + " with child " + first_child_base->toString());
    return nullptr;
}

// For simplicity, in a sequence like Exp -> AddExp -> MulExp -> UnaryExp -> PrimaryExp,
// if there are no actual operators, these just pass through the result from their child.

std::shared_ptr<IR::IROperand> IRGenerator::visitUnaryExp(PNNode node) {
    std::cerr << "[IR_GEN] visitUnaryExp() called for node: " << (node ? node->name : "<null_node>") << std::endl;
    // Assuming UnaryExp -> PrimaryExp or UnaryExp -> UnaryOp UnaryExp
    // For "0", it's UnaryExp -> PrimaryExp.
    if (!node || node->children.empty()) { /* TODO: Error */
        return nullptr;
    }

    // TODO: Handle UnaryOp (e.g., '-', '!') here if present.
    // Example: if child is UnaryOp and then another UnaryExp.
    // For now, assume it directly contains a PrimaryExp or an equivalent expression node.

    // If UnaryExp -> PrimaryExp (common case for non-operator unary expressions)
    // The child node might be named "PrimaryExp" or could be some other expression type
    // that UnaryExp can directly produce based on your grammar.
    auto child_exp_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node->children.at(0));
    if (child_exp_node) {
        if (child_exp_node->name == "PrimaryExp") {
            return this->visitPrimaryExp(child_exp_node);
        } else if (child_exp_node->name == "FunctionCall") {  // If UnaryExp can be FuncCall ID(Args)
            // Assuming FunctionCall node has ID as child 0 (Terminal) and Args as child 1 (NonTerminal)
            auto func_id_node = std::dynamic_pointer_cast<AST::TerminalNode>(child_exp_node->children.at(0));
            PNNode func_args_node = nullptr;
            if (child_exp_node->children.size() > 1) {
                func_args_node = std::dynamic_pointer_cast<AST::NonTerminalNode>(child_exp_node->children.at(1));
            }
            return this->visitFunctionCall(func_id_node, func_args_node);
        }
        // Add other direct productions of UnaryExp if necessary, e.g. if it can directly be a FunctionCall node.
        // Fallback: if the child is some other Exp type, dispatch generically.
        // This requires child_exp_node to be a valid AST structure for dispatchVisitExp.
        // return this->dispatchVisitExp(child_exp_node);
    }
    // TODO: Error if child is not as expected.
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
    : tempVarCounter(0), labelCounter(0) {
    // program = std::make_shared<IR::IRProgram>(); // Usually done in generate()
    // defineBuiltinPureFunctions(); // If you have this method for pre-populating pure functions
}

std::shared_ptr<IR::IRProgram> IRGenerator::generate(PNode rootAstNode) {
    std::cerr << "[IR_GEN] generate() called" << std::endl;
    this->program = std::make_shared<IR::IRProgram>();
    this->tempVarCounter = 0;
    this->labelCounter = 0;
    if (rootAstNode) {
        this->dispatchVisit(rootAstNode);
    }
    return this->program;
}

void IRGenerator::dispatchVisit(PNode node_base) {
    auto node = std::dynamic_pointer_cast<AST::NonTerminalNode>(node_base);
    if (!node) {
        std::cerr << "[IR_GEN] dispatchVisit() called with null or TerminalNode" << std::endl;
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
    } else if (node->name == "VarDef") {
        // this->visitVarDef(node);
    } else {
        std::cerr << "[IR_GEN] dispatchVisit: Unknown node type: " << node->name << std::endl;
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
}  // namespace IRGenerator