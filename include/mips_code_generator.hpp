#pragma once

#include <iostream>  // 用于输出汇编
#include <map>
#include <memory>
#include <set>
#include <string>
// #include <variant>  // Removed for C++11
#include <vector>
#include "ir.hpp"  // 你的IR头文件

namespace IR {
class IRProgram;
class NormalIRFunction;
class ReturnInst;
class CallPureInst;
class LoadArrayInst;
class StoreArrayInst;
class LabelInst;
class JumpInst;
class CondJumpInst;
class CallNormalInst;  // Forward declare CallNormalInst
class IROperand;       // Forward declare IROperand
class IRConstant;      // Forward declare IRConstant
class IRVariable;      // Forward declare IRVariable
class IRLabelOperand;  // Added
class IRType;          // Forward declare IRType
}  // namespace IR

namespace MipsCodeGenerator {

// 前向声明
class MipsFunctionContext;

struct VarLocation {
    enum class Type {
        STRING, // Represents a string value, e.g., register name or global label
        INT,    // Represents an integer value, e.g., stack offset
        UNINITIALIZED
    };

    Type type;
    std::string s_val;
    int i_val;

    // Default constructor
    VarLocation() : type(Type::UNINITIALIZED), i_val(0) {}

    // Constructor for string type (e.g., register name, global label)
    explicit VarLocation(const std::string& s_value) : type(Type::STRING), s_val(s_value), i_val(0) {}

    // Constructor for int type (e.g., stack offset)
    explicit VarLocation(int i_value) : type(Type::INT), s_val(""), i_val(i_value) {}

    bool isString() const { return type == Type::STRING; }
    bool isInt() const { return type == Type::INT; }
    bool isUninitialized() const { return type == Type::UNINITIALIZED; }

    // Getter for string value, with basic check
    const std::string& getString() const {
        if (!isString()) {
            // Basic error handling: return an empty string or throw
            // For now, returning a static empty string to avoid dynamic allocation in error path
            static const std::string empty_string_on_error = "";
            // std::cerr << "Error: VarLocation::getString() called on non-string type." << std::endl; // Optional: log error
            return empty_string_on_error;
        }
        return s_val;
    }

    // Getter for int value, with basic check
    int getInt() const {
        if (!isInt()) {
            // Basic error handling: return 0 or throw
            // std::cerr << "Error: VarLocation::getInt() called on non-int type." << std::endl; // Optional: log error
            return 0; // Or some other sentinel error value like std::numeric_limits<int>::min()
        }
        return i_val;
    }
};

class MipsCodeGenerator {
    friend class MipsFunctionContext;

   public:
    MipsCodeGenerator(std::ostream& out);  // 输出流，例如 std::cout 或文件流

    void generateProgram(std::shared_ptr<IR::IRProgram> irProgram);

    // Instruction visitors
    void visit(std::shared_ptr<IR::ReturnInst> inst);
    void visit(std::shared_ptr<IR::CallPureInst> inst);
    void visit(std::shared_ptr<IR::LoadArrayInst> inst);
    void visit(std::shared_ptr<IR::StoreArrayInst> inst);
    void visit(std::shared_ptr<IR::LabelInst> inst);
    void visit(std::shared_ptr<IR::JumpInst> inst);
    void visit(std::shared_ptr<IR::CondJumpInst> inst);
    void visit(std::shared_ptr<IR::CallNormalInst> inst);
    void visit(std::shared_ptr<IR::AssignInst> inst);

   private:
    std::ostream& output;  // 汇编代码输出流
    std::shared_ptr<IR::IRProgram> currentProgram;
    MipsFunctionContext* currentFunctionContext;  // 当前正在处理的函数上下文

    // --- 数据段管理 ---
    std::string dataSegment;
    void addData(const std::string& label, const std::string& type, const std::string& value);                                 // 例如 .word, .space, .asciiz
    void addGlobalVariable(const std::string& name, std::shared_ptr<IR::IRType> type);                                         // 处理全局变量
    void addGlobalVariable(const std::string& name, std::shared_ptr<IR::IRType> type, std::shared_ptr<IR::IRConstant> value);  // 处理全局变量

    // --- 代码段生成 ---
    void emit(const std::string& instruction);  // 输出单条MIPS指令
    void emitLabel(const std::string& label);   // 输出标签

    // --- 函数处理 ---
    void generateFunction(std::shared_ptr<IR::NormalIRFunction> normalFunc);
    void generatePrintfImplementation();
    void generateGetintImplementation();

    // --- 指令翻译 ---
    // 每个NormalIRFunction的指令都有一个对应的visit方法

    // --- 内置纯函数翻译 ---
    // 这些方法被 visit(CallPureInst*) 调用
    // 返回结果存储的目标寄存器由调用者(visit CallPureInst)决定
    // 参数操作数的值需要先加载到寄存器中
    void translateBuiltinAdd(const std::string& destReg, const std::string& srcReg1, const std::string& srcReg2);
    void translateBuiltinSub(const std::string& destReg, const std::string& srcReg1, const std::string& srcReg2);
    void translateBuiltinMul(const std::string& destReg, const std::string& srcReg1, const std::string& srcReg2);  // 可能需要 $hi, $lo
    void translateBuiltinDiv(const std::string& destQuotReg, const std::string& destRemReg, const std::string& srcReg1, const std::string& srcReg2);
    void translateBuiltinAssign(const std::string& destReg, const std::string& srcReg);  // mov
    // ... 其他比较、逻辑运算 ...
    void translateBuiltinCompare(const std::string& destReg, const std::string& comparisonOp, const std::string& srcReg1, const std::string& srcReg2);  // slt, sle, sgt, sge, seq, sne

    // --- 寄存器管理和内存访问 ---
    // 简单策略：跟踪 IRVariable 到 MIPS 寄存器/栈位置的映射
    std::string getOperandRegister(std::shared_ptr<IR::IROperand> operand, bool needsLoading = true);  // 获取操作数所在的寄存器, needsLoading表示如果操作数在内存中，是否要加载到临时寄存器
    void loadToRegister(const std::string& reg, std::shared_ptr<IR::IROperand> operand);               // 将操作数加载到指定寄存器
    void storeFromRegister(const std::string& reg, std::shared_ptr<IR::IRVariable> var);               // 将寄存器值存回变量（如果变量在栈上）
    std::string allocateRegister(std::shared_ptr<IR::IRVariable> var = nullptr, bool forTemp = true);  // 分配一个寄存器 (简单策略)
    void freeRegister(const std::string& reg);                                                         // 释放一个寄存器

    std::string getLabelForGlobal(const std::string& varName);
    int getStackOffset(const std::string& varName);  // 获取变量在栈上的偏移
};

// 用于管理单个函数代码生成过程中的状态
class MipsFunctionContext {
   public:
    MipsFunctionContext(std::shared_ptr<IR::NormalIRFunction> func, MipsCodeGenerator* gen);

    std::shared_ptr<IR::NormalIRFunction> irFunction;
    MipsCodeGenerator* generator;

    int currentStackOffset;  // 当前栈顶相对于 $fp 的偏移（通常为负）
    int maxArgsPassed;       // 此函数调用其他函数时传递的最大参数数量 (用于预留栈空间)
    int frameSize;           // <<< ADDED frameSize HERE
    int totalLocalVarSize;   // Added to store size of local variables
    std::string epilogueLabel; // Added for jumping to epilogue from return statements

    // 变量位置映射：IRVariable名 -> (寄存器名 或 栈偏移量)
    // std::variant<std::string (reg), int (stack_offset)>
    // std::map<std::string, std::variant<std::string, int>> varLocations;
    std::map<std::string, VarLocation> varLocations;
    std::map<std::string, std::shared_ptr<IR::IRType>> varTypes;  // 存储变量类型，用于加载/存储

    // 寄存器分配信息
    std::set<std::string> usedRegisters;          // 当前已分配的寄存器
    std::map<std::string, std::string> varToReg;  // IRVariable名 -> 寄存器 (如果分配在寄存器中)
    std::map<std::string, std::string> regToVar;  // 寄存器 -> IRVariable名

    // MIPS寄存器列表 (可以根据需要调整)
    const std::vector<std::string> argumentRegs = {"$a0", "$a1", "$a2", "$a3"};
    static const std::vector<std::string> tempRegs;
    const std::vector<std::string> savedRegs = {"$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"};

    void allocateParameters();
    void allocateLocalsAndTemporaries();  // 可能在遍历指令时动态分配临时变量的栈空间
    void analyzeFunctionLayout();         // New method for pre-calculating layout

    int getVarStackOffset(const std::string& varName);
    std::string getVarRegister(const std::string& varName);  // 如果在寄存器中
    bool isVarInRegister(const std::string& varName);

    std::string reserveRegister(std::shared_ptr<IR::IRVariable> var = nullptr);  // 尝试分配一个临时寄存器
    void releaseRegister(const std::string& reg);

    std::string ensureOperandInRegister(std::shared_ptr<IR::IROperand> operand);  // 确保操作数在寄存器中，如果不在则加载
    void spillRegister(const std::string& reg);                                   // 如果寄存器需要被腾出，将其中的值存回栈

    void generatePrologue();
    void generateEpilogue();

    std::vector<std::pair<std::string, int>> paramNamesAndOffsetsForPrologue; // For storing $a0-$a3

    const std::vector<std::string> CALLEE_SAVED_REGS = {
        "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"
    };
    const std::vector<std::string> CALLER_SAVED_TEMP_REGS = {
        "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7"
    };
};
}  // namespace MipsCodeGenerator
