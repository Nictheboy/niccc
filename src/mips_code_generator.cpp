#include "mips_code_generator.hpp"
#include <variant>  // For MipsFunctionContext::varLocations
#include "ir.hpp"   // For IR::IRProgram, IR::NormalIRFunction, IR::ReturnInst, etc.

namespace MipsCodeGenerator {

// --- MipsCodeGenerator Class Implementation ---

MipsCodeGenerator::MipsCodeGenerator(std::ostream& out)
    : output(out), currentProgram(nullptr), currentFunctionContext(nullptr) {
    // Initialize dataSegment or other members if needed
    dataSegment = ".data\n";  // Initial minimal data segment
}

void MipsCodeGenerator::generateProgram(std::shared_ptr<IR::IRProgram> irProgram) {
    this->currentProgram = irProgram;
    // Emit .data section header (actual data added via addData methods)
    // output << dataSegment; // Output data segment at the end or as it's built

    // Emit .text section header
    emit(".text");
    emit(".globl _start");  // Program entry point

    // Generate the _start function
    emitLabel("_start");
    // Optionally, set up stack pointer for _start if it needs its own frame,
    // but for a simple call to main and then exit, it might not be strictly necessary
    // if main itself sets up its frame correctly and _start doesn't use much stack.
    // For simplicity, we assume _start doesn't need its own complex frame here.
    emit("jal main");  // Call main, return address is $ra, result in $v0
    emit("nop");       // Branch delay slot filler, often good practice

    // After main returns, its result is in $v0.
    // Move it to $a0 for the exit syscall.
    emit("move $a0, $v0");  // Move main's return value to $a0
    emit("li $v0, 17");     // Syscall code for exit2 (exit with status)
    emit("syscall");        // Exit program
    emit("nop");            // In case syscall has a delay or for alignment

    // Generate all other functions from the IR program (including main)
    for (const auto& pair : irProgram->normalFunctions) {
        std::shared_ptr<IR::NormalIRFunction> func = pair.second;
        generateFunction(func);
    }
    // Output the accumulated data segment at the end.
    output << "\n"
           << dataSegment;
}

void MipsCodeGenerator::addData(const std::string& label, const std::string& type, const std::string& value) {
    dataSegment += label + ": " + type + " " + value + "\n";
}

void MipsCodeGenerator::addGlobalVariable(const std::string& name, std::shared_ptr<IR::IRType> type) {
    // For .space or default initialized .word
    // SysY int is 4 bytes.
    // For minimal, we might not have global vars in testfile.txt
    // Example: addData(name, ".space", "4"); // For an int
}

void MipsCodeGenerator::addGlobalVariable(const std::string& name, std::shared_ptr<IR::IRType> type, std::shared_ptr<IR::IRConstant> value) {
    // For initialized .word
    // For minimal, we might not have global vars in testfile.txt
    // Example: if (auto int_const = std::dynamic_pointer_cast<IR::IRConstant>(value)) {
    //     addData(name, ".word", std::to_string(int_const->value));
    // }
}

void MipsCodeGenerator::emit(const std::string& instruction) {
    output << "\t" << instruction << std::endl;
}

void MipsCodeGenerator::emitLabel(const std::string& label) {
    output << label << ":" << std::endl;
}

void MipsCodeGenerator::generateFunction(std::shared_ptr<IR::NormalIRFunction> normalFunc) {
    MipsFunctionContext context(normalFunc, this);
    this->currentFunctionContext = &context;

    emitLabel(normalFunc->name);  // Function name as label
    context.generatePrologue();

    for (const auto& inst : normalFunc->instructions) {
        // Dispatch to visit methods based on instruction type
        if (auto returnInst = std::dynamic_pointer_cast<IR::ReturnInst>(inst)) {
            visit(returnInst);
        }
        // Add other instruction types as needed for minimal set:
        // else if (auto callPureInst = std::dynamic_pointer_cast<IR::CallPureInst>(inst)) { visit(callPureInst); }
        // else if (auto labelInst = std::dynamic_pointer_cast<IR::LabelInst>(inst)) { visit(labelInst); }
        // ... etc.
    }

    context.generateEpilogue();
    this->currentFunctionContext = nullptr;
}

// --- Instruction Visit Methods ---
void MipsCodeGenerator::visit(std::shared_ptr<IR::ReturnInst> inst) {
    if (inst->returnValue.has_value()) {
        auto operand = inst->returnValue.value();
        if (auto constant = std::dynamic_pointer_cast<IR::IRConstant>(operand)) {
            // For "return 0;", operand is IRConstant with value 0
            // Load constant into $v0
            emit("li $v0, " + std::to_string(constant->value));
        } else if (auto variable = std::dynamic_pointer_cast<IR::IRVariable>(operand)) {
            // Load variable's value into $v0
            // This requires register allocation/stack management implemented in MipsFunctionContext
            std::string reg = currentFunctionContext->ensureOperandInRegister(variable);
            emit("move $v0, " + reg);
            // If 'reg' was temporary, it might need to be released if ensureOperandInRegister doesn't handle it.
        }
        // Other operand types (e.g. from complex expressions) would be handled here
    }
    // The actual "jr $ra" is part of the epilogue
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::CallPureInst> inst) {
    // Minimal implementation:
    // 1. Get arguments into $a0-$a3 (or stack for more)
    // 2. Call the pure function's translator
    // 3. Get result from $v0 (or other regs) into destination IRVariable
}
void MipsCodeGenerator::visit(std::shared_ptr<IR::LoadArrayInst> inst) { /* ... */ }
void MipsCodeGenerator::visit(std::shared_ptr<IR::StoreArrayInst> inst) { /* ... */ }
void MipsCodeGenerator::visit(std::shared_ptr<IR::LabelInst> inst) {
    emitLabel(inst->name);
}
void MipsCodeGenerator::visit(std::shared_ptr<IR::JumpInst> inst) {
    emit("j " + inst->target->labelName);
}
void MipsCodeGenerator::visit(std::shared_ptr<IR::CondJumpInst> inst) {
    // Example: beqz, bnez. Requires condition to be evaluated into a register.
    // std::string cond_reg = currentFunctionContext->ensureOperandInRegister(inst->condition);
    // emit("beqz " + cond_reg + ", " + inst->falseTarget->labelName); // or other branch logic
    // emit("j " + inst->trueTarget->labelName);
}
void MipsCodeGenerator::visit(std::shared_ptr<IR::CallNormalInst> inst) {
    // Handle saving caller-saved registers, passing arguments, jal, restoring registers, getting result.
}

// --- Builtin Pure Function Translators ---
// These would be called by visit(CallPureInst*)
void MipsCodeGenerator::translateBuiltinAdd(const std::string& destReg, const std::string& srcReg1, const std::string& srcReg2) {
    emit("addu " + destReg + ", " + srcReg1 + ", " + srcReg2);
}
void MipsCodeGenerator::translateBuiltinSub(const std::string& destReg, const std::string& srcReg1, const std::string& srcReg2) {
    emit("subu " + destReg + ", " + srcReg1 + ", " + srcReg2);
}
// ... other builtins ...

// --- Register Management and Memory Access ---
// These are mostly delegated to MipsFunctionContext but might have stubs or direct calls here
std::string MipsCodeGenerator::getOperandRegister(std::shared_ptr<IR::IROperand> operand, bool needsLoading) {
    if (!currentFunctionContext)
        return "$zero";  // Should not happen
    return currentFunctionContext->ensureOperandInRegister(operand);
}

// --- MipsFunctionContext Class Implementation ---

MipsFunctionContext::MipsFunctionContext(std::shared_ptr<IR::NormalIRFunction> func, MipsCodeGenerator* gen)
    : irFunction(func), generator(gen), currentStackOffset(0), maxArgsPassed(0) {
    // Initialize varLocations, varTypes, usedRegisters, etc.
    // For minimal "return 0;", parameters and locals might not be complex.
    // Pre-allocate space for saved registers in currentStackOffset if not done in prologue calculation
}

void MipsFunctionContext::generatePrologue() {
    // Minimal prologue for a function like "int main() {return 0;}"
    // Stack frame:
    // SP_orig -> | ...          |
    //            | Saved $ra    | <-- currentStackOffset points here initially for saved regs
    //            | Saved $fp    |
    // SP_new ->  | Local Vars   |
    //            | Temps        |

    // Calculate frame size based on locals, temps, max args passed to other funcs, and saved regs.
    // For "return 0;", frame size might just be for $fp, $ra.
    int frameSize = 8;  // 4 bytes for $fp, 4 bytes for $ra (minimal)
                        // This would be calculated more dynamically in a full implementation.

    generator->emit("addiu $sp, $sp, -" + std::to_string(frameSize));  // Decrement SP
    currentStackOffset = -frameSize;                                   // Keep track of current offset from original SP

    // Save $fp and $ra
    // Note: offsets are relative to the NEW $sp
    generator->emit("sw $fp, " + std::to_string(frameSize - 4) + "($sp)");  // Save old $fp at top of new frame
    generator->emit("sw $ra, " + std::to_string(frameSize - 8) + "($sp)");  // Save $ra below $fp

    // Set up new $fp
    generator->emit("move $fp, $sp");

    // After prologue, currentStackOffset could be adjusted to point to the start of local var area
    // relative to the NEW $fp. For now, we've allocated space.
    // For local variable allocation: currentStackOffset -= size_of_local_var;
    // varLocations[varName] = currentStackOffset; (relative to $fp)
}

void MipsFunctionContext::generateEpilogue() {
    // Minimal epilogue for "int main() {return 0;}"
    // Assumes $v0 already has the return value (set by visit(ReturnInst))

    // Offsets are relative to current $fp
    int frameSize = 8;  // Must match prologue

    // Restore $ra and $fp
    generator->emit("lw $ra, " + std::to_string(frameSize - 8) + "($fp)");
    generator->emit("lw $fp, " + std::to_string(frameSize - 4) + "($fp)");

    // Deallocate stack frame
    generator->emit("addiu $sp, $sp, " + std::to_string(frameSize));

    // Return to caller
    generator->emit("jr $ra");
}

std::string MipsFunctionContext::ensureOperandInRegister(std::shared_ptr<IR::IROperand> operand) {
    // Minimal version for "return 0;" where operand is an IRConstant
    if (auto constant = std::dynamic_pointer_cast<IR::IRConstant>(operand)) {
        // For constants, we typically load them into a temporary register.
        // For "return 0;", this function might not even be called if visit(ReturnInst) handles it directly.
        // If it IS called, we need a temp register.
        std::string tempReg = reserveRegister(nullptr);  // Get any temp reg
        generator->emit("li " + tempReg + ", " + std::to_string(constant->value));
        // Note: The caller (e.g., visit(ReturnInst)) should be aware if this is a temp
        // and potentially releaseRegister(tempReg) afterwards if it's not $v0 or similar.
        return tempReg;
    } else if (auto variable = std::dynamic_pointer_cast<IR::IRVariable>(operand)) {
        // Look up variable in varLocations. If in register, return reg name.
        // If on stack, load it into a temporary register.
        // This is where a more complex register allocator would live.
        // For now, assume it's not needed for "return 0;" if handled by visit(ReturnInst).
        if (isVarInRegister(variable->name)) {
            return getVarRegister(variable->name);
        } else {
            // Load from stack
            int offset = getVarStackOffset(variable->name);
            std::string tempReg = reserveRegister(variable);  // Or just a general temp
            generator->emit("lw " + tempReg + ", " + std::to_string(offset) + "($fp)");
            return tempReg;
        }
    }
    return "$zero";  // Fallback, should not be reached for valid IR for "return 0;"
}

// --- Helper methods for MipsFunctionContext (mostly stubs for minimal version) ---
void MipsFunctionContext::allocateParameters() { /* ... */ }
void MipsFunctionContext::allocateLocalsAndTemporaries() { /* ... */ }

int MipsFunctionContext::getVarStackOffset(const std::string& varName) {
    if (varLocations.count(varName)) {
        if (std::holds_alternative<int>(varLocations[varName])) {
            return std::get<int>(varLocations[varName]);
        }
    }
    // Error or default
    return 0;
}

std::string MipsFunctionContext::getVarRegister(const std::string& varName) {
    if (varLocations.count(varName)) {
        if (std::holds_alternative<std::string>(varLocations[varName])) {
            return std::get<std::string>(varLocations[varName]);
        }
    }
    // Error or default
    return "";
}

bool MipsFunctionContext::isVarInRegister(const std::string& varName) {
    if (varLocations.count(varName)) {
        return std::holds_alternative<std::string>(varLocations[varName]);
    }
    return false;
}

std::string MipsFunctionContext::reserveRegister(std::shared_ptr<IR::IRVariable> var) {
    // Very simple strategy: find first available tempReg.
    // A real implementation needs to handle spilling, live ranges, etc.
    for (const std::string& reg : tempRegs) {
        if (usedRegisters.find(reg) == usedRegisters.end()) {
            usedRegisters.insert(reg);
            if (var) {  // If reserving for a specific variable
                varToReg[var->name] = reg;
                regToVar[reg] = var->name;
                varLocations[var->name] = reg;
            }
            return reg;
        }
    }
    // TODO: No free temp regs - need spilling or more regs.
    // For "return 0;", we might not need many. If ensureOperandInRegister is called for the constant 0,
    // it might use $v0 directly or one temp reg.
    // Fallback, should trigger error or spilling.
    return tempRegs[0];  // Highly problematic, just to compile.
}

void MipsFunctionContext::releaseRegister(const std::string& reg) {
    usedRegisters.erase(reg);
    if (regToVar.count(reg)) {
        std::string varName = regToVar[reg];
        varToReg.erase(varName);
        // varLocations might need to be updated if it was stored on stack after this.
        // For a simple temp, just freeing is enough.
        regToVar.erase(reg);
    }
}

void MipsFunctionContext::spillRegister(const std::string& reg) {
    // If reg holds a variable, store it back to its stack slot.
    // Minimal: not implemented yet.
}

}  // namespace MipsCodeGenerator