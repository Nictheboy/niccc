#pragma once

#include <map>
#include <memory>
#include <stdexcept>  // For std::runtime_error
#include <string>
#include <vector>
#include <optional>
#include "ast.hpp"           // 你的 AST 头文件
#include "ir.hpp"            // 你的 IR 头文件
#include "symbol_table.hpp"  // 符号表头文件 (下面会给出概念)

namespace IRGenerator {

class IRGenerator {
   public:
    using PNode = std::shared_ptr<AST::Node>;
    using PNNode = std::shared_ptr<AST::NonTerminalNode>;
    using PTNode = std::shared_ptr<AST::TerminalNode>;
    IRGenerator();

    std::shared_ptr<IR::IRProgram> generate(PNode rootAstNode);

   private:
    std::shared_ptr<IR::IRProgram> program;
    std::shared_ptr<IR::NormalIRFunction> currentNormalFunction;  // 指向当前构建的普通函数
    SymbolTable symbolTable;

    int tempVarCounter;
    int labelCounter;
    std::vector<std::pair<std::shared_ptr<IR::IRLabelOperand>, std::shared_ptr<IR::IRLabelOperand>>> loopLabelStack; // {continue_target, break_target}

    // --- 工具方法 ---
    std::shared_ptr<IR::IRVariable> createTempSimpleVar(IR::SimpleTypeKind kind = IR::SimpleTypeKind::INTEGER, const std::string& prefix = "%t");
    std::shared_ptr<IR::IRVariable> createNamedVar(const std::string& name, std::shared_ptr<IR::IRType> type);
    std::shared_ptr<IR::IRLabelOperand> createLabel(const std::string& prefix = ".L");
    void addInstruction(std::shared_ptr<IR::IRInstruction> inst);
    std::shared_ptr<IR::IRType> astTypeToIrType(PNNode typeNode);  // 将AST类型节点转为IRType

    // --- 内置纯函数定义 ---
    void defineBuiltinPureFunctions();
    std::string getBuiltinPureFunctionName(const std::string& astOperatorTokenName, int numOperands = 2);

    // --- AST 节点访问者方法 (返回IROperand主要用于表达式，其他返回void) ---
    // T visit(PNode node); // 泛型或重载版本
    // 这里使用具体的返回类型或void，并依赖dynamic_cast进行分发
    void dispatchVisit(PNode node_base);                               // 用于无返回值的访问
    std::shared_ptr<IR::IROperand> dispatchVisitExp(PNode node_base);  // 用于有返回值的表达式访问

    // 全局结构
    void visitCompUnit(PNNode node);
    void visitGlobalItemList(PNNode node);
    void visitGlobalItem(PNNode node);

    // 函数定义
    void visitFuncDef(PNNode node);
    std::vector<std::shared_ptr<IR::IRVariable>> visitFuncFParams(PNNode node);
    std::shared_ptr<IR::IRVariable> visitFuncFParam(PNNode node);
    // visitType 已通过 astTypeToIrType 处理

    // 变量定义
    void visitVarDef(PNNode node);  // 处理全局或局部变量定义
    // visitVarDefItem, visitInitVal (可能需要返回 IROperand 作为初始值)

    // 语句块和语句
    void visitBlock(PNNode node, bool createNewScope = true);
    void visitBlockItems(PNNode node);
    void visitBlockItem(PNNode node);

    void visitStmt(PNNode node);
    void visitIfStmt(PNNode node);
    void visitReturnStmt(PNNode node /* PNode expNode nullable, but ReturnStmt AST node is passed */);
    void visitWhileStmt(PNNode node);
    void visitBreakStmt(PNNode node);
    void visitContinueStmt(PNNode node);

    // 表达式 (返回 std::shared_ptr<IR::IROperand>，通常是一个 IR::IRVariable)
    std::shared_ptr<IR::IROperand> visitExp(PNNode node);
    std::shared_ptr<IR::IROperand> visitLOrExp(PNNode node);
    std::shared_ptr<IR::IROperand> visitLAndExp(PNNode node);
    std::shared_ptr<IR::IROperand> visitEqExp(PNNode node);
    std::shared_ptr<IR::IROperand> visitRelExp(PNNode node);
    std::shared_ptr<IR::IROperand> visitAddExp(PNNode node);
    std::shared_ptr<IR::IROperand> visitMulExp(PNNode node);
    std::shared_ptr<IR::IROperand> visitUnaryExp(PNNode node);
    std::shared_ptr<IR::IROperand> visitPrimaryExp(PNNode node);
    std::shared_ptr<IR::IROperand> visitLVal(PNNode node, bool forAssignment = false);  // forAssignment区分读写
    std::shared_ptr<IR::IROperand> visitNumber(PTNode node);
    std::vector<std::shared_ptr<IR::IROperand>> visitFuncRParams(PNNode node);
    std::shared_ptr<IR::IROperand> visitFunctionCall(PTNode idNode, PNNode paramsNode);

    // 从AST节点获取操作数 (通常是标识符或字面量)
    std::shared_ptr<IR::IROperand> getOperandFromPrimary(PNNode primaryNode);

    // Declaration visitors
    void visitDecl(PNNode decl_list_node, std::shared_ptr<IR::IRType> base_ir_type, bool is_const);
    void visitConstDef(PNNode node);
};

};  // namespace IRGenerator
