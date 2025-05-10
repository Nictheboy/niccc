#include "mips_code_generator.hpp"
#include <iomanip>   // For std::setfill, std::setw with std::hex (not strictly needed for current escape)
#include <iostream>  // For std::cerr (debugging)
#include <sstream>   // For std::ostringstream in escapeStringForMipsAsm
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

    // Initialize dataSegment string
    dataSegment = ".data\n";
    dataSegment += "_newline: .asciiz \"\\n\"\n"; // For printing newlines with printf

    // Populate global variables into dataSegment
    if (this->currentProgram && !this->currentProgram->globalVariables.empty()) {
        dataSegment += "# Global Variables\n";
        for (const auto& pair : this->currentProgram->globalVariables) {
            const std::string& varName = pair.first;
            std::shared_ptr<IR::IRVariable> var = pair.second;
            // Assuming all global ints are 4 bytes.
            // SysY doesn't have global arrays in the simple case like testfile.txt
            if (var->global_initializer_constant) { // Check if there's a constant initializer
                // For testfile.txt: int a = 2; const int c = 4;
                dataSegment += varName + ": .word " + std::to_string(var->global_initializer_constant->value) + "\n";
            } else {
                // For testfile.txt: int b; (uninitialized)
                dataSegment += varName + ": .space 4\n"; // Allocate 4 bytes for an uninitialized int
            }
        }
        dataSegment += "\n";
    }

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
    emit(".text");
    emit(".globl _start"); // Ensure _start is global if your entry point is _start
    emit(".globl main");   // Ensure main is global

    // Generate the _start function (entry point)
    emitLabel("_start");
    emit("jal main");
    emit("nop");          // Branch delay slot
    // After main returns, $v0 contains main's return value.
    // Standard exit sequence: move $v0 to $a0 for exit syscall, then syscall 17 (exit2)
    emit("move $a0, $v0"); // Argument for exit2 syscall is in $a0
    emit("li $v0, 17");   // Syscall code for exit2 (terminates with value)
    emit("syscall");      // Terminate program

    // Generate printf implementation
    generatePrintfImplementation();

    // Generate all other functions from the IR program
    if (irProgram) {
        for (const auto& pair : irProgram->normalFunctions) {
            std::shared_ptr<IR::NormalIRFunction> func = pair.second;
            // if (func->name != "_start") { // _start is special, already generated
            // Skip generating generic function code for "printf" as it has a custom implementation
            if (func->name == "printf") {
                continue;
            }
            generateFunction(func);
            // }
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

    emitLabel(normalFunc->name);
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
        } else if (auto assignInst = std::dynamic_pointer_cast<IR::AssignInst>(inst)) {
            visit(assignInst);
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
    if (!currentFunctionContext) { return; }
    if (inst->hasReturnValue && inst->returnValue) {
        auto operand = inst->returnValue;
        std::string reg = currentFunctionContext->ensureOperandInRegister(operand);
        if (reg != "$v0") {
            emit("move $v0, " + reg);
        }
        // If ensureOperandInRegister used a temp reg, it should be released.
        // currentFunctionContext->releaseRegister(reg); // Be careful if reg is $v0
    } else {
        // Void return, $v0 can be anything.
    }
    // Actual jr $ra and stack cleanup is in epilogue.
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::CallPureInst> inst) { /* ... placeholder ... */ }
void MipsCodeGenerator::visit(std::shared_ptr<IR::LoadArrayInst> inst) { /* ... placeholder ... */ }
void MipsCodeGenerator::visit(std::shared_ptr<IR::StoreArrayInst> inst) { /* ... placeholder ... */ }

void MipsCodeGenerator::visit(std::shared_ptr<IR::LabelInst> inst) {
    emitLabel(inst->name);
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::JumpInst> inst) {
    emit("j " + inst->target->labelName);
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::CondJumpInst> inst) {
    // ... placeholder ...
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::CallNormalInst> inst) {
    if (!currentFunctionContext) { return; }
    emit("# --- Calling function: " + inst->functionName + " ---");

    int argCount = inst->arguments.size();
    std::vector<std::string> arg_regs_used;
    for (int i = 0; i < argCount && i < 4; ++i) {
        std::shared_ptr<IR::IROperand> argOperand = inst->arguments[i];
        std::string argSrcReg = currentFunctionContext->ensureOperandInRegister(argOperand);
        arg_regs_used.push_back(argSrcReg); // Track for potential release
        std::string targetArgReg = "$a" + std::to_string(i);
        emit("move " + targetArgReg + ", " + argSrcReg);
    }

    // TODO: Pass arguments 5+ on stack

    emit("jal " + inst->functionName);
    emit("nop");  // Branch delay slot

    // Release temporary registers used for arguments after the call
    for(const std::string& reg : arg_regs_used) {
        currentFunctionContext->releaseRegister(reg);
    }

    if (inst->hasResultDestination && inst->resultDestination) {
        std::shared_ptr<IR::IRVariable> destVar = inst->resultDestination;
        // Result is in $v0. Store it to destVar.
        std::string dest_addr_reg; // For global vars
        if (currentProgram->globalVariables.count(destVar->name)) {
            dest_addr_reg = currentFunctionContext->reserveRegister(); 
            emit("la " + dest_addr_reg + ", " + destVar->name);
            emit("sw $v0, 0(" + dest_addr_reg + ")");
            currentFunctionContext->releaseRegister(dest_addr_reg);
        } else {
            int offset = currentFunctionContext->getVarStackOffset(destVar->name);
            emit("sw $v0, " + std::to_string(offset) + "($fp)");
        }
        emit("# Variable " + destVar->name + " (result of " + inst->functionName + ") stored from $v0");
    }
    emit("# --- End of call to " + inst->functionName + " ---");
}

// Implementation for AssignInst
void MipsCodeGenerator::visit(std::shared_ptr<IR::AssignInst> inst) {
    if (!currentFunctionContext) { 
        emit("# ERROR: AssignInst visited outside function context for " + inst->dest->name);
        return; 
    }
    emit("# AssignInst: " + inst->dest->name + " = " + inst->source->toString());

    std::string src_reg = currentFunctionContext->ensureOperandInRegister(inst->source);
    std::shared_ptr<IR::IRVariable> dest_var = inst->dest;

    if (currentProgram->globalVariables.count(dest_var->name)) {
        // Destination is a global variable
        std::string dest_addr_reg = currentFunctionContext->reserveRegister(); // Temp for address
        emit("la " + dest_addr_reg + ", " + dest_var->name);
        emit("sw " + src_reg + ", 0(" + dest_addr_reg + ")");
        currentFunctionContext->releaseRegister(dest_addr_reg);
        emit("# Stored to global " + dest_var->name);
    } else {
        // Destination is a local variable (or parameter on stack)
        int offset = currentFunctionContext->getVarStackOffset(dest_var->name);
        emit("sw " + src_reg + ", " + std::to_string(offset) + "($fp)");
        emit("# Stored to local " + dest_var->name + " at " + std::to_string(offset) + "($fp)");
    }

    currentFunctionContext->releaseRegister(src_reg); // Release register used for source
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
      currentStackOffset(0), // Will be set by prologue
      maxArgsPassed(0),      // For now, not handling stack-passed args beyond $a0-$a3
      frameSize(0),          // Will be calculated
      totalLocalVarSize(0) {

    // 1. Calculate total size needed for local variables.
    //    Parameters passed in $a0-$a3 might also be stored to stack if their regs are needed.
    //    For now, assume parameters are handled separately or copied to local slots if needed.
    //    The `irFunction->locals` list contains explicitly declared local variables.
    int currentLocalOffset = 0;
    if (irFunction) { // Check if irFunction is not null
        for (const auto& local_pair : irFunction->locals) {
            const auto& varName = local_pair.first;
            const auto& var = local_pair.second;
            if (varLocations.find(varName) == varLocations.end()) { // Don't re-process params
                varTypes[varName] = var->type;
                currentLocalOffset -= 4;
                // varLocations[varName] = currentLocalOffset; // Offset from $fp
                varLocations[varName] = VarLocation(currentLocalOffset);
            }
        }
    }
    this->totalLocalVarSize = currentLocalOffset;

    // 2. Calculate total frame size.
    // Base size for $ra and $fp = 8 bytes.
    // Add space for local variables.
    // Add space for arguments passed on stack by this function to its callees (maxArgsPassed * 4) - 0 for now.
    this->frameSize = this->totalLocalVarSize + 8; // + (maxArgsPassed * 4);
    generator->emit("# Function " + (irFunction ? irFunction->name : "null_func") + ": totalLocalVarSize = " + std::to_string(this->totalLocalVarSize) + ", frameSize = " + std::to_string(this->frameSize));


    // tempRegs initialization (already present in original code)
    // tempRegs = {"$t0", "$t1", ..., "$t9"}; // Ensure this is initialized somewhere, e.g. in MipsCodeGenerator or here
}

void MipsFunctionContext::generatePrologue() {
    if (!irFunction) return; // Should not happen if called correctly

    generator->emit("# Prologue for " + irFunction->name);
    generator->emit("addiu $sp, $sp, -" + std::to_string(frameSize)); // Decrement SP to allocate frame

    // Save $ra and $fp. $fp will point to the base of the allocated frame (new $sp).
    // Locals are at 0($fp), 4($fp), ...
    // Saved $fp is at totalLocalVarSize($fp)
    // Saved $ra is at totalLocalVarSize + 4 ($fp)
    // These offsets are relative to the NEW $sp before $fp is moved.
    // So, if $fp = new $sp:
    // totalLocalVarSize ($fp) is (frameSize - 8) from new $sp
    // totalLocalVarSize + 4 ($fp) is (frameSize - 4) from new $sp
    generator->emit("sw $ra, " + std::to_string(frameSize - 4) + "($sp)"); // Save $ra at top of frame
    generator->emit("sw $fp, " + std::to_string(frameSize - 8) + "($sp)"); // Save old $fp below $ra

    generator->emit("move $fp, $sp"); // $fp now points to the base of the current frame

    // currentStackOffset could track distance from $fp if needed for other things,
    // but for locals, varLocations stores their direct offset from $fp.
    // For arguments passed in $a0-$a3 that need to be saved to stack:
    // This is where you would sw $a0, offset($fp), etc. if you decide to save them.
    // For testfile.txt, main() has no parameters.
}

void MipsFunctionContext::generateEpilogue() {
    if (!irFunction) return;
    generator->emit("# Epilogue for " + irFunction->name);
    // $v0 should have the return value if any (set by visit(ReturnInst))

    // Restore $ra and $fp from their saved locations relative to current $fp
    // Using same logic as prologue for save locations:
    generator->emit("lw $ra, " + std::to_string(frameSize - 4) + "($fp)");
    generator->emit("lw $fp, " + std::to_string(frameSize - 8) + "($fp)");

    generator->emit("addiu $sp, $sp, " + std::to_string(frameSize)); // Deallocate frame
    generator->emit("jr $ra");
    generator->emit("nop");  // Branch delay slot
}

std::string MipsFunctionContext::ensureOperandInRegister(std::shared_ptr<IR::IROperand> operand) {
    if (!operand) {
        generator->emit("# Error: ensureOperandInRegister called with null operand");
        return "$zero"; // Should not happen
    }

    // Check if operand is already mapped to a register (e.g. if it was a recently stored temp)
    // This simple register allocator doesn't really do this; it always loads/creates.

    generator->emit("# Ensuring operand " + operand->toString() + " is in a register");

    if (auto constant = std::dynamic_pointer_cast<IR::IRConstant>(operand)) {
        std::string tempReg = reserveRegister(); // Reserve a general temp reg
        generator->emit("li " + tempReg + ", " + std::to_string(constant->value));
        return tempReg;
    } else if (auto variable = std::dynamic_pointer_cast<IR::IRVariable>(operand)) {
        // Check if it's a global variable first
        if (generator->currentProgram && generator->currentProgram->globalVariables.count(variable->name)) {
            std::string tempReg = reserveRegister(); // Register to hold the value
            generator->emit("la " + tempReg + ", " + variable->name);      // Load address of global
            generator->emit("lw " + tempReg + ", 0(" + tempReg + ")");    // Load word from that address
            generator->emit("# Loaded global var " + variable->name + " into " + tempReg);
            return tempReg;
        } else {
            // Must be a local variable or parameter on the stack
            if (varLocations.count(variable->name)) {
                int offset = getVarStackOffset(variable->name);
                std::string tempReg = reserveRegister(); // Reserve a general temp reg for the loaded value
                // varLocations[variable->name] = tempReg; // No, varLocations stores stack offset or reg if allocated
                generator->emit("lw " + tempReg + ", " + std::to_string(offset) + "($fp)");
                generator->emit("# Loaded local var " + variable->name + " from " + std::to_string(offset) + "($fp) into " + tempReg);
                return tempReg;
            } else {
                generator->emit("# Error: Local variable " + variable->name + " not found in varLocations during ensureOperandInRegister.");
                return "$zero"; // Error case
            }
        }
    } else if (auto labelOp = std::dynamic_pointer_cast<IR::IRLabelOperand>(operand)) {
        // For string literals (labels), load their address
        std::string tempReg = reserveRegister();
        generator->emit("la " + tempReg + ", " + labelOp->labelName);
        generator->emit("# Loaded address of label " + labelOp->labelName + " into " + tempReg);
        return tempReg;
    }

    generator->emit("# Error: Unknown operand type in ensureOperandInRegister for " + operand->toString());
    return "$zero"; // Fallback for unknown operand types
}

// --- Helper methods for MipsFunctionContext (mostly stubs for minimal version) ---
void MipsFunctionContext::allocateParameters() { /* ... */ }
void MipsFunctionContext::allocateLocalsAndTemporaries() { /* ... */ }

int MipsFunctionContext::getVarStackOffset(const std::string& varName) {
    if (varLocations.count(varName)) {
        const auto& loc = varLocations.at(varName);
        if (loc.isInt()) {
            return loc.getInt();
        }
    }
    generator->output << "# ERROR: Variable " << varName << " not found on stack or varLocations entry is not int!\n";
    return 0; // Default or error value
}

std::string MipsFunctionContext::getVarRegister(const std::string& varName) {
    if (varLocations.count(varName)) {
        const auto& loc = varLocations.at(varName);
        if (loc.isString()) { // Assuming string is register name
            return loc.getString();
        }
    }
    return ""; // Not in a register or not found
}

bool MipsFunctionContext::isVarInRegister(const std::string& varName) {
    if (varLocations.count(varName)) {
        return varLocations.at(varName).isString(); // Assuming string is register name
    }
    return false;
}

// Basic temporary register pool. Assumes $t0-$t7 are available.
// A more sophisticated allocator would be needed for complex functions.
const std::vector<std::string> MipsFunctionContext::tempRegs = {
    "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7"
    // Not using $t8, $t9 for now to keep it simple.
};

std::string MipsFunctionContext::reserveRegister(std::shared_ptr<IR::IRVariable> var_for_reg) {
    for (const std::string& reg : tempRegs) {
        if (usedRegisters.find(reg) == usedRegisters.end()) {
            usedRegisters.insert(reg);
            // if (var_for_reg) { // If we are reserving it for a specific IRVariable to keep it in reg
            //    varToReg[var_for_reg->name] = reg;
            //    regToVar[reg] = var_for_reg->name;
            //    varLocations[var_for_reg->name] = reg; // Update varLocations to map to register
            //    generator->emit("# Reserved register " + reg + " for variable " + var_for_reg->name);
            // } else {
                 generator->emit("# Reserved temporary register " + reg);
            // }
            return reg;
        }
    }
    generator->emit("# Error: No free temporary registers! Spilling needed (not implemented).");
    return tempRegs[0]; // Problematic: fallback, should implement spilling.
}

void MipsFunctionContext::releaseRegister(const std::string& reg) {
    if (reg.empty() || reg == "$zero" || reg == "$v0" || reg == "$a0" || reg == "$a1" || reg == "$a2" || reg == "$a3" || reg == "$fp" || reg == "$sp" || reg == "$ra") {
        // Do not release special purpose registers or empty strings
        generator->emit("# Attempted to release special/empty register " + reg + ". Ignoring.");
        return;
    }
    if (usedRegisters.erase(reg)) {
         generator->emit("# Released temporary register " + reg);
    } else {
         generator->emit("# Attempted to release register " + reg + " which was not marked as used.");
    }
    // If we were mapping vars to regs in varToReg/regToVar:
    // if (regToVar.count(reg)) {
    //    std::string varName = regToVar[reg];
    //    // If varLocations stored this register, it needs to revert to stack offset or be marked dirty.
    //    // For simplicity now, we assume varLocations primarily stores stack offsets for locals.
    //    varToReg.erase(varName);
    //    regToVar.erase(reg);
    // }
}


void MipsFunctionContext::spillRegister(const std::string& reg) {
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