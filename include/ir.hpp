#pragma once

#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace IR {

class IRType;
class SimpleIRType;
class ArrayIRType;  // (用于普通函数)
class VoidIRType;   // (用于普通函数)
class LabelIRType;  // (用于普通函数)
class IROperand;
class IRConstant;
class IRVariable;
class IRLabelOperand;  // (用于普通函数)
class IRInstruction;
class PureIRFunction;
class NormalIRFunction;  // (结构基本不变)

//-----------------------------------------------------------------------------
// 1. IRTypes
//-----------------------------------------------------------------------------
class IRType {
   public:
    virtual ~IRType() = default;
    virtual std::string toString() const = 0;
    virtual bool equals(const IRType* other) const = 0;
};

enum class SimpleTypeKind {
    INTEGER
};

class SimpleIRType : public IRType {
   public:
    SimpleTypeKind kind;
    SimpleIRType(SimpleTypeKind k)
        : kind(k) {}
    std::string toString() const override {
        switch (kind) {
            case SimpleTypeKind::INTEGER:
                return "INTEGER";
            default:
                return "UNKNOWN_SIMPLE_TYPE";
        }
    }
    bool equals(const IRType* other) const override {
        if (auto otherSimple = dynamic_cast<const SimpleIRType*>(other)) {
            return kind == otherSimple->kind;
        }
        return false;
    }
};

class ArrayIRType : public IRType {
   public:
    std::shared_ptr<IRType> elementType;
    // SysY arrays often have their dimensions specified, but for a general IR,
    // just knowing it's an array of a certain element type might be enough.
    // Or you might want std::vector<int> dimensions;
    int numDimensions;  // Or a more complex dimension representation

    ArrayIRType(std::shared_ptr<IRType> elemType, int dims = 1)
        : elementType(elemType), numDimensions(dims) {}

    std::string toString() const override {
        std::string s = "ARRAY(dims=" + std::to_string(numDimensions) + ", type=";
        if (elementType) {
            s += elementType->toString();
        } else {
            s += "incomplete";
        }
        s += ")";
        return s;
    }

    bool equals(const IRType* other) const override {
        if (auto otherArray = dynamic_cast<const ArrayIRType*>(other)) {
            return numDimensions == otherArray->numDimensions &&
                   elementType && otherArray->elementType &&
                   elementType->equals(otherArray->elementType.get());
        }
        return false;
    }
};

class VoidIRType : public IRType {
   public:
    std::string toString() const override { return "VOID"; }
    bool equals(const IRType* other) const override {
        return dynamic_cast<const VoidIRType*>(other) != nullptr;
    }
};

class LabelIRType : public IRType {
   public:
    std::string toString() const override { return "LABEL"; }
    bool equals(const IRType* other) const override {
        return dynamic_cast<const LabelIRType*>(other) != nullptr;
    }
};

//-----------------------------------------------------------------------------
// 2. IROperands
//    Represents values that instructions operate on.
//    These are like "virtual registers" or SSA values if you take it further.
//-----------------------------------------------------------------------------

class IROperand {
   public:
    std::shared_ptr<IRType> type;
    virtual ~IROperand() = default;
    IROperand(std::shared_ptr<IRType> t)
        : type(t) {}
    virtual std::string toString() const = 0;
};

class IRConstant : public IROperand {
   public:
    // For now, only integer constants. Could be a variant for other types.
    int value;

    IRConstant(int val)
        : IROperand(std::make_shared<SimpleIRType>(SimpleTypeKind::INTEGER)), value(val) {}

    std::string toString() const override {
        return std::to_string(value);
    }
};

// Represents a named variable, parameter, or temporary result
class IRVariable : public IROperand {
   public:
    std::string name;  // e.g., "%t1", "param_x", "local_var_y"
    bool is_global; // Optional: could be useful
    bool is_const;  // To help with assignments
    std::shared_ptr<IRConstant> global_initializer_constant; // For global inits

    IRVariable(std::string n, std::shared_ptr<IRType> t, bool is_global = false, bool is_const = false)
        : IROperand(t), name(std::move(n)), is_global(is_global), is_const(is_const), global_initializer_constant(nullptr) {}

    std::string toString() const override {
        return name + ":" + (type ? type->toString() : "untyped");
    }
};

// Specific operand type for jump targets
class IRLabelOperand : public IROperand {
   public:
    std::string labelName;

    IRLabelOperand(std::string name)
        : IROperand(std::make_shared<LabelIRType>()), labelName(std::move(name)) {}

    std::string toString() const override {
        return "label " + labelName;
    }
};

//-----------------------------------------------------------------------------
// 3. IRInstructions
//-----------------------------------------------------------------------------

class IRInstruction {
   public:
    virtual ~IRInstruction() = default;
    virtual std::string toString() const = 0;
};

// === 指令: 仅用于 PureIRFunction 内部 ===

// 调用另一个纯函数 (内置或用户定义的)
class PureInternalCallInst : public IRInstruction {
   public:
    std::string calleePureFunctionName;
    std::vector<std::shared_ptr<IROperand>> arguments;  // 参数，可以是父纯函数的参数或局部变量
    std::vector<std::shared_ptr<IRVariable>> results;   // 结果，存储到父纯函数的新的局部变量中

    PureInternalCallInst(
        std::string callee,
        std::vector<std::shared_ptr<IROperand>> args,
        std::vector<std::shared_ptr<IRVariable>> res)
        : calleePureFunctionName(std::move(callee)), arguments(std::move(args)), results(std::move(res)) {}

    std::string toString() const override {
        std::string s = "";
        if (!results.empty()) {
            s += "(";
            for (size_t i = 0; i < results.size(); ++i) {
                s += results[i]->toString();
                if (i < results.size() - 1)
                    s += ", ";
            }
            s += ") = ";
        }
        s += "PURE_CALL " + calleePureFunctionName + "(";
        for (size_t i = 0; i < arguments.size(); ++i) {
            s += arguments[i]->toString();
            if (i < arguments.size() - 1)
                s += ", ";
        }
        s += ")";
        return s;
    }
};

// 从纯函数返回/指定输出值
class PureReturnInst : public IRInstruction {
   public:
    // 按顺序指定哪些操作数 (父纯函数的参数或局部变量) 作为此纯函数的输出
    std::vector<std::shared_ptr<IROperand>> outputValues;

    PureReturnInst(std::vector<std::shared_ptr<IROperand>> outputs)
        : outputValues(std::move(outputs)) {}

    std::string toString() const override {
        std::string s = "PURE_RETURN (";
        for (size_t i = 0; i < outputValues.size(); ++i) {
            s += outputValues[i]->toString();
            if (i < outputValues.size() - 1)
                s += ", ";
        }
        s += ")";
        return s;
    }
};

// === 指令: 仅用于 NormalIRFunction 内部 (与之前版本大部分相同) ===
// CallPureInst (现在很明确，这是从Normal函数调用Pure函数)
class CallPureInst : public IRInstruction {
   public:
    std::string pureFunctionName;
    std::vector<std::shared_ptr<IROperand>> arguments;
    std::vector<std::shared_ptr<IRVariable>> results;  // Variables to store results

    CallPureInst(
        std::string funcName,
        std::vector<std::shared_ptr<IROperand>> args,
        std::vector<std::shared_ptr<IRVariable>> res)
        : pureFunctionName(std::move(funcName)), arguments(std::move(args)), results(std::move(res)) {}

    std::string toString() const override {
        std::string s = "CALL_PURE " + pureFunctionName + "(";
        for (size_t i = 0; i < arguments.size(); ++i) {
            s += arguments[i]->toString();
            if (i < arguments.size() - 1)
                s += ", ";
        }
        s += ") -> (";
        for (size_t i = 0; i < results.size(); ++i) {
            s += results[i]->toString();
            if (i < results.size() - 1)
                s += ", ";
        }
        s += ")";
        return s;
    }
};

class LoadArrayInst : public IRInstruction {
   public:
    std::shared_ptr<IRVariable> destination;  // Where the loaded value goes
    std::shared_ptr<IROperand> arraySource;   // Variable holding the array
    std::vector<std::shared_ptr<IROperand>> indices;

    LoadArrayInst(
        std::shared_ptr<IRVariable> dest,
        std::shared_ptr<IROperand> src,
        std::vector<std::shared_ptr<IROperand>> idx)
        : destination(dest), arraySource(src), indices(std::move(idx)) {}

    std::string toString() const override {
        std::string s = destination->toString() + " = LOAD " + arraySource->toString();
        for (const auto& idx : indices) {
            s += "[" + idx->toString() + "]";
        }
        return s;
    }
};

class StoreArrayInst : public IRInstruction {
   public:
    std::shared_ptr<IROperand> arrayDestination;  // Variable holding the array
    std::vector<std::shared_ptr<IROperand>> indices;
    std::shared_ptr<IROperand> valueSource;  // Value to store

    StoreArrayInst(
        std::shared_ptr<IROperand> dest,
        std::vector<std::shared_ptr<IROperand>> idx,
        std::shared_ptr<IROperand> src)
        : arrayDestination(dest), indices(std::move(idx)), valueSource(src) {}

    std::string toString() const override {
        std::string s = "STORE " + arrayDestination->toString();
        for (const auto& idx : indices) {
            s += "[" + idx->toString() + "]";
        }
        s += ", " + valueSource->toString();
        return s;
    }
};

class LabelInst : public IRInstruction {
   public:
    std::string name;
    LabelInst(std::string n)
        : name(std::move(n)) {}
    std::string toString() const override { return name + ":"; }
};

class JumpInst : public IRInstruction {
   public:
    std::shared_ptr<IRLabelOperand> target;
    JumpInst(std::shared_ptr<IRLabelOperand> t)
        : target(t) {}
    std::string toString() const override { return "JUMP " + target->toString(); }
};

class CondJumpInst : public IRInstruction {
   public:
    std::shared_ptr<IROperand> condition;  // Must be of SimpleType::INTEGER
    std::shared_ptr<IRLabelOperand> trueTarget;
    std::shared_ptr<IRLabelOperand> falseTarget;

    CondJumpInst(
        std::shared_ptr<IROperand> cond,
        std::shared_ptr<IRLabelOperand> tt,
        std::shared_ptr<IRLabelOperand> ft)
        : condition(cond), trueTarget(tt), falseTarget(ft) {}

    std::string toString() const override {
        return "COND_JUMP " + condition->toString() +
               " ? " + trueTarget->toString() +
               " : " + falseTarget->toString();
    }
};

class CallNormalInst : public IRInstruction {
   public:
    std::string functionName;
    std::vector<std::shared_ptr<IROperand>> arguments;
    std::shared_ptr<IRVariable> resultDestination;
    bool hasResultDestination;

    CallNormalInst(
        std::string funcName,
        std::vector<std::shared_ptr<IROperand>> args,
        std::shared_ptr<IRVariable> resDest = nullptr)
        : functionName(std::move(funcName)), arguments(std::move(args)), resultDestination(resDest), hasResultDestination(resDest != nullptr) {}

    std::string toString() const override {
        std::string s = "";
        if (hasResultDestination && resultDestination) {
            s += resultDestination->toString() + " = ";
        }
        s += "CALL_NORMAL " + functionName + "(";
        for (size_t i = 0; i < arguments.size(); ++i) {
            s += arguments[i]->toString();
            if (i < arguments.size() - 1)
                s += ", ";
        }
        s += ")";
        return s;
    }
};

class ReturnInst : public IRInstruction {
   public:
    std::shared_ptr<IROperand> returnValue;
    bool hasReturnValue;

    ReturnInst(std::shared_ptr<IROperand> val = nullptr)
        : returnValue(val), hasReturnValue(val != nullptr) {}

    std::string toString() const override {
        if (hasReturnValue && returnValue) {
            return "RETURN " + returnValue->toString();
        } else {
            return "RETURN";
        }
    }
};

class AssignInst : public IRInstruction {
public:
    std::shared_ptr<IRVariable> dest;    // The variable being assigned to
    std::shared_ptr<IROperand> source; // The value or variable being assigned from

    AssignInst(std::shared_ptr<IRVariable> d, std::shared_ptr<IROperand> s)
        : dest(std::move(d)), source(std::move(s)) {}

    std::string toString() const override {
        return "ASSIGN " + (dest ? dest->name : "null_dest") + " = " + (source ? source->toString() : "null_src");
    }
};

//-----------------------------------------------------------------------------
// 4. IRFunction (Base, Pure, Normal) - PureIRFunction 结构大改
//-----------------------------------------------------------------------------

class IRFunctionBase {
   public:
    std::string name;
    virtual ~IRFunctionBase() = default;
    IRFunctionBase(std::string n)
        : name(std::move(n)) {}
    virtual std::string toString() const = 0;
};

class PureIRFunction : public IRFunctionBase {
   public:
    // 参数是带类型的 IRVariable
    std::vector<std::shared_ptr<IRVariable>> parameters;
    // 输出仅定义类型和数量，具体由 PureReturnInst 填充
    std::vector<std::shared_ptr<SimpleIRType>> outputTypes;

    // 纯函数内部的局部变量 (用于存储 PureInternalCallInst 的结果)
    std::map<std::string, std::shared_ptr<IRVariable>> locals;

    // 纯函数内部的指令列表
    std::vector<std::shared_ptr<IRInstruction>> instructions;

    bool isBuiltin;  // 是否是内置纯函数 (如 ADD, DIV)

    PureIRFunction(
        std::string n,
        std::vector<std::shared_ptr<IRVariable>> params,
        std::vector<std::shared_ptr<SimpleIRType>> outTypes,
        bool builtin = false)
        : IRFunctionBase(std::move(n)),
          parameters(std::move(params)),
          outputTypes(std::move(outTypes)),
          isBuiltin(builtin) {}

    void addLocal(std::shared_ptr<IRVariable> local_var) {
        if (local_var->type->toString() != "INTEGER") {  // 简陋的检查，应使用类型比较
                                                         // 抛出错误或处理: 纯函数局部变量必须是简单类型
        }
        locals[local_var->name] = local_var;
    }

    void addInstruction(std::shared_ptr<IRInstruction> inst) {
        // 可以在这里做检查，确保加入的指令是 PureInternalCallInst 或 PureReturnInst
        instructions.push_back(inst);
    }

    std::string toString() const override {
        std::string s = "PURE FUNCTION " + name + "(";
        for (size_t i = 0; i < parameters.size(); ++i) {
            s += parameters[i]->toString();
            if (i < parameters.size() - 1)
                s += ", ";
        }
        s += ") -> (";
        for (size_t i = 0; i < outputTypes.size(); ++i) {
            s += outputTypes[i]->toString();
            if (i < outputTypes.size() - 1)
                s += ", ";
        }
        s += ")";
        if (isBuiltin) {
            s += " [BUILTIN]";
        }
        s += "\n";

        if (!locals.empty()) {
            s += "  LOCALS:\n";
            for (const auto& pair : locals) {
                s += "    " + pair.second->toString() + "\n";
            }
        }

        if (!isBuiltin) {
            s += "  BODY:\n";
            for (const auto& inst : instructions) {
                s += "    " + inst->toString() + "\n";
            }
        }
        return s;
    }
};

class NormalIRFunction : public IRFunctionBase {
   public:
    std::vector<std::shared_ptr<IRVariable>> parameters;  // Named parameters
    std::shared_ptr<IRType> returnType;                   // Could be VoidIRType

    // Local variables declared within the function (excluding parameters).
    // Useful for symbol table management during IR generation.
    std::map<std::string, std::shared_ptr<IRVariable>> locals;

    std::vector<std::shared_ptr<IRInstruction>> instructions;  // Sequence of instructions

    NormalIRFunction(
        std::string n,
        std::vector<std::shared_ptr<IRVariable>> params,
        std::shared_ptr<IRType> retType)
        : IRFunctionBase(std::move(n)), parameters(std::move(params)), returnType(retType) {}

    void addInstruction(std::shared_ptr<IRInstruction> inst) {
        instructions.push_back(inst);
    }

    void addLocal(std::shared_ptr<IRVariable> local) {
        locals[local->name] = local;
    }

    std::string toString() const override {
        std::string s = "NORMAL FUNCTION " + name + "(";
        for (size_t i = 0; i < parameters.size(); ++i) {
            s += parameters[i]->toString();
            if (i < parameters.size() - 1)
                s += ", ";
        }
        s += ") -> " + returnType->toString() + "\n";

        if (!locals.empty()) {
            s += "  LOCALS:\n";
            for (const auto& pair : locals) {
                s += "    " + pair.second->toString() + "\n";
            }
        }

        s += "  BODY:\n";
        for (const auto& inst : instructions) {
            if (dynamic_cast<LabelInst*>(inst.get())) {
                s += "  " + inst->toString() + "\n";
            } else {
                s += "    " + inst->toString() + "\n";
            }
        }
        return s;
    }
};
//-----------------------------------------------------------------------------
// 5. IRProgram
//-----------------------------------------------------------------------------

class IRProgram {
   public:
    std::map<std::string, std::shared_ptr<PureIRFunction>> pureFunctions;
    std::map<std::string, std::shared_ptr<NormalIRFunction>> normalFunctions;
    // Could also have global variable declarations here

    // For string literals
    std::map<std::string, std::string> stringLiterals;  // label -> processed_string
    int stringLiteralCounter = 0;

    std::map<std::string, std::shared_ptr<IRVariable>> globalVariables; // Name to IRVariable

    IRProgram() = default;

    std::string addStringLiteral(const std::string& content) {
        // Check if identical string already exists to reuse label
        for (const auto& pair : stringLiterals) {
            if (pair.second == content) {
                return pair.first;
            }
        }
        std::string label = "_S" + std::to_string(stringLiteralCounter++);
        stringLiterals[label] = content;
        return label;
    }

    const std::map<std::string, std::string>& getStringLiteralTable() const {
        return stringLiterals;
    }

    void addPureFunction(std::shared_ptr<PureIRFunction> func) {
        pureFunctions[func->name] = func;
    }

    void addNormalFunction(std::shared_ptr<NormalIRFunction> func) {
        normalFunctions[func->name] = func;
    }

    void addGlobalVariable(const std::shared_ptr<IRVariable>& var) {
        if (var) {
            globalVariables[var->name] = var;
            var->is_global = true; // Mark it as global
        }
    }

    std::string toString() const {
        std::string s = "IR PROGRAM:\n\n";
        s += "--- GLOBAL VARIABLES ---\n";
        if (globalVariables.empty()) {
            s += "(none)\n";
        } else {
            for (const auto& pair : globalVariables) {
                s += "  VAR " + pair.second->toString();
                if (pair.second->is_const) {
                    s += " (const)";
                }
                if (pair.second->global_initializer_constant) {
                    s += " = " + pair.second->global_initializer_constant->toString();
                }
                s += "\n";
            }
        }
        s += "\n";

        s += "--- PURE FUNCTIONS ---\n";
        for (const auto& pair : pureFunctions) {
            s += pair.second->toString() + "\n";
        }
        s += "\n--- NORMAL FUNCTIONS ---\n";
        for (const auto& pair : normalFunctions) {
            s += pair.second->toString() + "\n";
        }
        s += "\n--- STRING LITERALS ---\n";
        for (const auto& pair : stringLiterals) {
            s += pair.first + ": \"" + pair.second + "\"\n";  // Basic escaping for output
        }
        return s;
    }
};

}  // namespace IR
