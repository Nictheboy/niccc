#include "mips_code_generator.hpp"
#include <iomanip>   // For std::setfill, std::setw with std::hex (not strictly needed for current escape)
#include <iostream>  // For std::cerr (debugging)
#include <sstream>   // For std::ostringstream in escapeStringForMipsAsm
#include <variant>   // For MipsFunctionContext::varLocations
#include "ir.hpp"    // For IR::IRProgram, IR::NormalIRFunction, IR::ReturnInst, etc.

namespace MipsCodeGenerator {

// Helper function to escape strings for MIPS .asciiz directives
static std::string escapeStringForMipsAsm(const std::string& s) {
    std::ostringstream oss;
    for (char c : s) {
        switch (c) {
            case '\n':
                oss << "\\n";
                break;
            case '\t':
                oss << "\\t";
                break;
            case '\"':
                oss << "\\\"";
                break;  // Escape the quote itself
            case '\\':
                oss << "\\\\";
                break;  // Escape the backslash itself
            // Add more escapes if needed (e.g., for non-printable ASCII)
            default:
                if (isprint(c)) {
                    oss << c;
                } else {
                    // Output non-printable characters as hex escapes, e.g., \x0A
                    // For simplicity now, we might just ignore or replace them
                    // For a robust solution, proper hex escaping is better.
                    // oss << "\\x" << std::hex << std::setw(2) << std::setfill('0') << (int)(unsigned char)c;
                    oss << ".";  // Replace non-printable with a dot for now
                }
                break;
        }
    }
    return oss.str();
}

// --- MipsCodeGenerator Class Implementation ---

MipsCodeGenerator::MipsCodeGenerator(std::ostream& out)
    : output(out), currentProgram(nullptr), currentFunctionContext(nullptr) {
    // dataSegment is initialized in generateProgram
}

void MipsCodeGenerator::generateProgram(std::shared_ptr<IR::IRProgram> irProgram) {
    this->currentProgram = irProgram;

    // Initialize dataSegment string but don't output it yet.
    dataSegment = ".data\n";
    dataSegment += "_newline: .asciiz \"\\n\"\n";

    // Populate string literals into dataSegment
    if (this->currentProgram) {
        const auto& stringLiterals = this->currentProgram->getStringLiteralTable();
        if (!stringLiterals.empty()) {
            dataSegment += "# String Literals\n";
            for (const auto& pair : stringLiterals) {
                const std::string& label = pair.first;
                const std::string& content = pair.second;
                dataSegment += label + ": .asciiz \"" + escapeStringForMipsAsm(content) + "\"\n";
            }
            dataSegment += "\n";
        }
    }

    // --- TEXT SEGMENT ---
    // Output .text segment first
    emit(".text");
    emit(".globl _start");
    emit(".globl main");

    // Generate the _start function (entry point) FIRST in .text segment
    emitLabel("_start");
    emit("jal main");
    emit("nop");
    emit("move $a0, $v0");
    emit("li $v0, 17");
    emit("syscall");

    // Generate printf implementation
    generatePrintfImplementation();

    // Generate all other functions from the IR program (including main)
    for (const auto& pair : irProgram->normalFunctions) {
        std::shared_ptr<IR::NormalIRFunction> func = pair.second;
        if (func->name != "_start") {
            generateFunction(func);
        }
    }

    // Output the accumulated data segment at the END.
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
        if (auto returnInst = std::dynamic_pointer_cast<IR::ReturnInst>(inst)) {
            visit(returnInst);
        } else if (auto labelInst = std::dynamic_pointer_cast<IR::LabelInst>(inst)) {
            visit(labelInst);
        } else if (auto jumpInst = std::dynamic_pointer_cast<IR::JumpInst>(inst)) {
            visit(jumpInst);
        } else if (auto callNormalInst = std::dynamic_pointer_cast<IR::CallNormalInst>(inst)) {
            visit(callNormalInst);
        }
        // Add other instruction types as needed
        // else if (auto callPureInst = std::dynamic_pointer_cast<IR::CallPureInst>(inst)) { visit(callPureInst); }
        // else if (auto condJumpInst = std::dynamic_pointer_cast<IR::CondJumpInst>(inst)) { visit(condJumpInst); }
    }

    context.generateEpilogue();
    this->currentFunctionContext = nullptr;
}

// --- Instruction Visit Methods ---
void MipsCodeGenerator::visit(std::shared_ptr<IR::ReturnInst> inst) {
    if (inst->returnValue.has_value()) {
        auto operand = inst->returnValue.value();
        // Ensure the operand is in a register (could be $v0 directly if simple enough)
        std::string reg = currentFunctionContext->ensureOperandInRegister(operand);
        if (reg != "$v0") {  // If ensureOperandInRegister used a temp reg
            emit("move $v0, " + reg);
        }
        // If ensureOperandInRegister directly put it into $v0 (e.g. for constants), no move needed.
        // This logic needs to be coordinated with ensureOperandInRegister's behavior.
        // For now, assume ensureOperandInRegister might use a temp, so move to $v0.
        // A more optimized ensureOperandInRegister could take a target register.

    } else {
        // Void return, $v0 can be anything, typically 0 by convention for main if not specified
        // but for other void functions, its value is not used.
        // emit("li $v0, 0"); // Optionally clear $v0 for void returns
    }
    // The actual "jr $ra" and stack cleanup is part of the function epilogue.
    // This visitor just ensures $v0 is set if there is a return value.
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
    emit("# --- Calling function: " + inst->functionName + " ---");

    // TODO: Save caller-saved registers ($t0-$t9) if they are live and not protected by callee.
    // For printf, it saves $s0-$s3 which it uses, so temps might be okay for now.

    // Argument passing (first 4 in $a0-$a3)
    int argCount = inst->arguments.size();
    for (int i = 0; i < argCount && i < 4; ++i) {
        std::shared_ptr<IR::IROperand> argOperand = inst->arguments[i];
        std::string argSrcReg = currentFunctionContext->ensureOperandInRegister(argOperand);
        std::string targetArgReg = "$a" + std::to_string(i);

        emit("move " + targetArgReg + ", " + argSrcReg);

        // If argSrcReg is a temporary register ($t0-$t9) and not also an argument register,
        // and the variable it holds (if any) isn't needed immediately after in that same reg,
        // it could be released. For simplicity, current `ensureOperandInRegister` always returns
        // a temp register ($tX) that it reserves. We might need to explicitly release it if the
        // register allocator doesn't handle this automatically after its use here.
        // Current simple allocator: reserveRegister finds first available.
        // Release it if it's from $t0-$t9 to make it available again.
        bool isTempReg = false;
        for (const std::string& tr : currentFunctionContext->tempRegs) {
            if (tr == argSrcReg) {
                isTempReg = true;
                break;
            }
        }
        if (isTempReg) {
            // currentFunctionContext->releaseRegister(argSrcReg); // Be careful with this, ensure it's safe.
            // For now, let's assume registers are precious but not over-releasing.
            // The simple reserveRegister will just find the next available one.
        }
    }

    // TODO: Pass arguments 5 and onwards on the stack (pushed in reverse order)
    // if (argCount > 4) { ... update currentFunctionContext->maxArgsPassed ... }

    emit("jal " + inst->functionName);
    emit("nop");  // Branch delay slot

    // TODO: Clean up stack if arguments were pushed.
    // TODO: Restore caller-saved registers.

    // Handle return value if one is expected
    if (inst->resultDestination.has_value()) {
        std::shared_ptr<IR::IRVariable> destVar = inst->resultDestination.value();
        // Result is in $v0. Assign it to the destination variable.
        // This might involve getting a register for destVar or storing to its stack slot.
        std::string destReg = currentFunctionContext->getVarRegister(destVar->name);  // Check if already in reg
        if (destReg.empty()) {                                                        // Not in a register, might be on stack or needs a new reg
            // A simple strategy: if destVar has a stack slot, store $v0 there.
            // Or, reserve a new register for it and move $v0 there.
            // For now, let's assume we want it in a register. If it's a local that needs to be stored,
            // subsequent code or a pass would handle that.
            destReg = currentFunctionContext->reserveRegister(destVar);  // This maps destVar to destReg
            emit("move " + destReg + ", $v0");
            emit("# Variable " + destVar->name + " (result of " + inst->functionName + ") now in " + destReg);
        } else {
            emit("move " + destReg + ", $v0");  // Already has a register, just update it
            emit("# Variable " + destVar->name + " (result of " + inst->functionName + ") updated in " + destReg);
        }
    }
    emit("# --- End of call to " + inst->functionName + " ---");
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
    : irFunction(func),
      generator(gen),
      currentStackOffset(0),
      maxArgsPassed(0),  // maxArgsPassed before frameSize
      frameSize(8),      // Initial base size for $ra, $fp
      totalLocalVarSize(0) {
    // Calculate space for local variables and assign their stack offsets.
    // Locals are at positive offsets from $fp, starting at 0($fp).
    int currentLocalOffset = 0;
    for (const auto& map_entry : irFunction->locals) {
        const std::string& varName = map_entry.first;
        // std::shared_ptr<IR::IRVariable> irVar = map_entry.second; // if needed for type/size

        // Assuming all locals are 4 bytes for now.
        // TODO: This needs to use IRType from map_entry.second to determine size.
        varLocations[varName] = currentLocalOffset;
        generator->emit("# Local var " + varName + " at " + std::to_string(currentLocalOffset) + "($fp)");
        currentLocalOffset += 4;  // TODO: Use actual size
    }
    this->totalLocalVarSize = currentLocalOffset;
    // Frame size will be finalized in analyzeFunctionLayout, which adds $ra/$fp space, maxArgsPassed, etc.
}

void MipsFunctionContext::generatePrologue() {
    // Frame size must have been calculated in constructor or earlier
    generator->emit("# Prologue for " + irFunction->name);
    generator->emit("addiu $sp, $sp, -" + std::to_string(frameSize));
    currentStackOffset = frameSize;  // $fp will point to $sp, so offsets from $fp will be positive

    // Save $fp and $ra. Offsets are relative to the NEW $sp.
    // $ra is at a higher address than $fp on the stack typically.
    // If currentStackOffset is frameSize (distance from new $sp to old $sp):
    // Saved $ra: frameSize - 4 from new $sp
    // Saved $fp: frameSize - 8 from new $sp
    generator->emit("sw $ra, " + std::to_string(frameSize - 4) + "($sp)");
    generator->emit("sw $fp, " + std::to_string(frameSize - 8) + "($sp)");

    generator->emit("move $fp, $sp");

    // After this, currentStackOffset in MipsFunctionContext could be used to mean
    // offset from $fp for allocating locals. Stack grows down from $fp.
    // E.g., first local at -4($fp), second at -8($fp) etc.
    // Or, keep offsets positive from $fp if $fp points to base of locals area.
    // Let's stick to standard: $fp is frame pointer, locals are at negative offsets from $fp.
    // $fp is now new_sp. Saved $fp is at 0($fp), Saved $ra is at 4($fp) if we use $fp as base of current frame for locals.
    // Let's re-evaluate. Standard MIPS: $sp moved, $fp points to old $sp + offset.
    // Simpler for now: $fp = new $sp. Saved $fp, $ra are at positive offsets from $fp.
    // Locals are at positive offsets too, below saved regs.
    // Example:
    // SP_new ($fp) -> | Local Var 1    | (e.g. 0($fp) or some base offset)
    //                | Local Var 2    |
    //                | ...            |
    //                | Saved $s0-$s7  | (if any)
    //                | Saved $fp      | (e.g. frameSize - 8 ($fp) )
    //                | Saved $ra      | (e.g. frameSize - 4 ($fp) )
    // SP_orig ---->  |                |

    // Parameter allocation (if on stack) and local variable allocation happens here
    // by assigning offsets relative to $fp. For now, this is basic.
    // Actual stack offsets for locals will be determined by allocateLocalsAndTemporaries
    // and then used in ensureOperandInRegister.
}

void MipsFunctionContext::generateEpilogue() {
    generator->emit("# Epilogue for " + irFunction->name);
    // $v0 should have the return value if any (set by visit(ReturnInst))

    // Restore $fp and $ra from where they were saved relative to current $fp
    generator->emit("lw $ra, " + std::to_string(frameSize - 4) + "($fp)");
    generator->emit("lw $fp, " + std::to_string(frameSize - 8) + "($fp)");

    generator->emit("addiu $sp, $sp, " + std::to_string(frameSize));
    generator->emit("jr $ra");
    generator->emit("nop");  // Branch delay slot
}

std::string MipsFunctionContext::ensureOperandInRegister(std::shared_ptr<IR::IROperand> operand) {
    if (!operand) {
        generator->emit("# Error: ensureOperandInRegister called with null operand");
        return "$zero";  // Should not happen
    }

    generator->emit("# Ensuring operand " + operand->toString() + " is in a register");

    if (auto constant = std::dynamic_pointer_cast<IR::IRConstant>(operand)) {
        std::string tempReg = reserveRegister();
        generator->emit("li " + tempReg + ", " + std::to_string(constant->value));
        return tempReg;
    } else if (auto variable = std::dynamic_pointer_cast<IR::IRVariable>(operand)) {
        if (isVarInRegister(variable->name)) {
            return getVarRegister(variable->name);
        } else {
            // Load from stack
            int offset = getVarStackOffset(variable->name);
            std::string tempReg = reserveRegister(variable);
            generator->emit("lw " + tempReg + ", " + std::to_string(offset) + "($fp)");
            return tempReg;
        }
    } else if (auto labelOp = std::dynamic_pointer_cast<IR::IRLabelOperand>(operand)) {
        // For string literals (labels), load their address
        std::string tempReg = reserveRegister();
        generator->emit("la " + tempReg + ", " + labelOp->labelName);
        return tempReg;
    }

    generator->emit("# Error: Unknown operand type in ensureOperandInRegister for " + operand->toString());
    return "$zero";  // Fallback for unknown operand types
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
        varLocations.erase(varName);  // Remove from varLocations if it was register mapped
        varToReg.erase(varName);
        regToVar.erase(reg);
    }
}

void MipsFunctionContext::spillRegister(const std::string& reg) {
    // If reg holds a variable, store it back to its stack slot.
    // Minimal: not implemented yet.
    generator->emit("# SPILL for register " + reg + " (not implemented)");
}

// Private method to be added to MipsCodeGenerator
void MipsCodeGenerator::generatePrintfImplementation() {
    emitLabel("printf");

    // Standard function prologue for printf
    emit("addi $sp, $sp, -20");  // Space for $ra, $fp, $s0, $s1, $s2 (5 words)
    emit("sw $ra, 16($sp)");
    emit("sw $fp, 12($sp)");
    emit("sw $s0, 8($sp)");  // $s0 will be format string pointer
    emit("sw $s1, 4($sp)");  // $s1 will be current char from format string / temp for arg (stores potential int arg)
    emit("sw $s2, 0($sp)");  // $s2 can be used to point to next arg ($a2, $a3, stack) if handling multiple % specifiers
    emit("move $fp, $sp");

    emit("move $s0, $a0");  // Copy format string address to $s0
    emit("move $s1, $a1");  // Copy first vararg ($a1) to $s1 (potential int for %d)

    emitLabel("printf_loop_start");
    emit("lb $t0, 0($s0)");              // Load current character of format string
    emit("beq $t0, $zero, printf_end");  // If char is null, end of string
    emit("nop");                         // Branch delay slot

    emit("li $t1, '%'");
    emit("bne $t0, $t1, printf_print_char_direct");  // If char is not '%', go print it
    emit("nop");                                     // Branch delay slot

    // Character is '%', check next character for specifier
    emit("addi $s0, $s0, 1");            // Move to next char in format string (specifier)
    emit("lb $t0, 0($s0)");              // Load specifier char
    emit("beq $t0, $zero, printf_end");  // Malformed: string ends with bare %. Jump to end.
    emit("nop");                         // Branch delay slot

    emit("li $t1, 'd'");
    emit("bne $t0, $t1, printf_handle_literal_percent");  // If not 'd', check if it's '%%'
    emit("nop");                                          // Branch delay slot

    // It is '%d'
    emit("move $a0, $s1");  // Move integer value (from original $a1, now in $s1) to $a0 for syscall
    emit("li $v0, 1");      // Syscall for print_integer
    emit("syscall");
    // TODO: Advance to next vararg ($s2 -> $a2, etc.) if we handle multiple %d.
    emit("j printf_loop_continue");  // Continue processing rest of format string
    emit("nop");                     // Branch delay slot

    emitLabel("printf_handle_literal_percent");
    emit("li $t1, '%'");
    emit("bne $t0, $t1, printf_unknown_specifier");  // If not '%%', it's an unknown specifier
    emit("nop");                                     // Branch delay slot
    // It is '%%'. $t0 (specifier char) is '%'. We want to print this character.
    // Fall through to printf_print_char_direct, which will print content of $t0.

    emitLabel("printf_print_char_direct");  // Corrected: No colon in argument
    emit("move $a0, $t0");                  // Char to print is in $t0
    emit("li $v0, 11");                     // Syscall for print_character
    emit("syscall");
    emit("j printf_loop_continue");  // JUMP TO CONTINUE LOOP
    emit("nop");                     // Branch delay slot

    emitLabel("printf_unknown_specifier");
    // Print the original '%' and then the unknown specifier char literally
    emit("li $a0, '%'");  // Load '%' to print
    emit("li $v0, 11");
    emit("syscall");
    emit("move $a0, $t0");  // $t0 has the unknown specifier char
    emit("li $v0, 11");
    emit("syscall");
    emit("j printf_loop_continue");  // JUMP TO CONTINUE LOOP
    emit("nop");                     // Branch delay slot

    emitLabel("printf_loop_continue");
    emit("addi $s0, $s0, 1");  // Move to next char in format string for next iteration
    emit("j printf_loop_start");
    emit("nop");  // Branch delay slot

    emitLabel("printf_end");
    // Standard function epilogue
    emit("lw $s2, 0($sp)");
    emit("lw $s1, 4($sp)");
    emit("lw $s0, 8($sp)");
    emit("lw $fp, 12($sp)");
    emit("lw $ra, 16($sp)");
    emit("addi $sp, $sp, 20");
    emit("jr $ra");
    emit("nop");  // Branch delay slot
}

}  // namespace MipsCodeGenerator