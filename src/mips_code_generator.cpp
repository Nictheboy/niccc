#include "mips_code_generator.hpp"
#include <iomanip>   // For std::setfill, std::setw with std::hex (not strictly needed for current escape)
#include <iostream>  // For std::cerr (debugging)
#include <map>
#include <set>
#include <sstream>  // For std::ostringstream in escapeStringForMipsAsm
#include <variant>
#include "ir.hpp"  // For IR::IRProgram, IR::NormalIRFunction, IR::ReturnInst, etc.

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
    dataSegment += "_newline: .asciiz \"\\n\"\n";  // For printing newlines with printf

    // Populate global variables into dataSegment
    if (this->currentProgram && !this->currentProgram->globalVariables.empty()) {
        dataSegment += "# Global Variables\n";
        for (const auto& pair : this->currentProgram->globalVariables) {
            const std::string& varName = pair.first;
            std::shared_ptr<IR::IRVariable> var = pair.second;
            
            if (auto array_type = std::dynamic_pointer_cast<IR::ArrayIRType>(var->type)) {
                int element_size = 4; // Assuming int elements for now
                int total_elements = array_type->getTotalElementCount();
                if (total_elements <= 0) {
                    // This might happen if dimension parsing failed or was 0
                    dataSegment += "# ERROR: Array '" + varName + "' has invalid size: " + std::to_string(total_elements) + "\n";
                    dataSegment += varName + ": .space 4 # Fallback allocation for problematic array\n"; 
                } else {
                    dataSegment += varName + ": .space " + std::to_string(total_elements * element_size) + " # Array: " + std::to_string(total_elements) + " elements * " + std::to_string(element_size) + " bytes each\n";
                }
            } else if (var->global_initializer_constant) {  // Scalar with initializer
                dataSegment += varName + ": .word " + std::to_string(var->global_initializer_constant->value) + "\n";
            } else { // Scalar uninitialized or other non-array type
                dataSegment += varName + ": .space 4\n";  // Allocate 4 bytes for an uninitialized int/scalar
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
    emit(".globl _start");  // Ensure _start is global if your entry point is _start
    emit(".globl main");    // Ensure main is global

    // Generate the _start function (entry point)
    emitLabel("_start");
    emit("jal main");
    emit("nop");  // Branch delay slot
    // After main returns, $v0 contains main's return value.
    // Standard exit sequence: move $v0 to $a0 for exit syscall, then syscall 17 (exit2)
    emit("move $a0, $v0");  // Argument for exit2 syscall is in $a0
    emit("li $v0, 17");     // Syscall code for exit2 (terminates with value)
    emit("syscall");        // Terminate program

    // Generate printf implementation
    generatePrintfImplementation();

    // Generate getint implementation
    generateGetintImplementation();

    // Generate all other functions from the IR program
    if (irProgram) {
        for (const auto& pair : irProgram->normalFunctions) {
            std::shared_ptr<IR::NormalIRFunction> func = pair.second;
            // if (func->name != "_start") { // _start is special, already generated
            // Skip generating generic function code for "printf" and "getint" as they have custom implementations
            if (func->name == "printf" || func->name == "getint") {
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

    // Iterate directly over normalFunc->instructions
    for (const auto& inst : normalFunc->instructions) { 
        if (!inst) continue; // Safety check for null instruction
        
        // Assuming manual dispatch for now based on existing pattern:
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
        } else if (auto callPureInst = std::dynamic_pointer_cast<IR::CallPureInst>(inst)) {
            visit(callPureInst);
        } else if (auto condJumpInst = std::dynamic_pointer_cast<IR::CondJumpInst>(inst)) {
            visit(condJumpInst); 
        } else if (auto loadArrayInst = std::dynamic_pointer_cast<IR::LoadArrayInst>(inst)) {
            visit(loadArrayInst);
        } else if (auto storeArrayInst = std::dynamic_pointer_cast<IR::StoreArrayInst>(inst)) {
            visit(storeArrayInst);
        } else {
            emit("# Unhandled IRInstruction type in MipsCodeGenerator::generateFunction: " + inst->toString());
        }
    }

    context.generateEpilogue();
    this->currentFunctionContext = nullptr;
}

// --- Instruction Visit Methods ---
void MipsCodeGenerator::visit(std::shared_ptr<IR::ReturnInst> inst) {
    if (!currentFunctionContext) {
        return;
    }
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
    this->emit("j " + currentFunctionContext->epilogueLabel);
    this->emit("nop");
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::CallPureInst> inst) {
    if (!currentFunctionContext || !inst) {
        emit("# ERROR: CallPureInst visited outside function context or null instruction");
        return;
    }
    emit("# CallPureInst: " + inst->toString());

    std::shared_ptr<IR::IRVariable> dest_var = nullptr;
    bool has_dest = false;

    if (!inst->results.empty()) {
        dest_var = inst->results[0]; // For expressions, assume the first result is the one we want.
        has_dest = true;
    } else {
        // This case implies a pure function call that doesn't store its result anywhere, 
        // which might be unusual for the builtins we're handling in expressions.
        emit("# CallPureInst for " + inst->pureFunctionName + " has no result variables specified.");
        // We might still need to evaluate it if arguments have side effects (not typical for pure ops) or for other reasons.
        // For now, if no destination, we can't proceed with storing a result.
    }

    if (has_dest && !dest_var) {
        // This would mean inst->results was not empty, but results[0] was null.
        emit("# ERROR: CallPureInst has a result entry, but the IRVariable is null.");
        return;
    }

    std::string op1_reg, op2_reg;
    if (inst->arguments.size() >= 1) {
        op1_reg = currentFunctionContext->ensureOperandInRegister(inst->arguments[0]);
    } else {
        emit("# ERROR: CallPureInst expects at least one argument for " + inst->pureFunctionName);
        if(!op1_reg.empty() && op1_reg != "$zero") currentFunctionContext->releaseRegister(op1_reg); // op1_reg might be from a failed ensureOperand
        return;
    }

    if (inst->arguments.size() >= 2) {
        op2_reg = currentFunctionContext->ensureOperandInRegister(inst->arguments[1]);
    }

    std::string result_val_reg = "";

    if (has_dest && dest_var) { // Only reserve a register if we have a valid destination
        result_val_reg = currentFunctionContext->reserveRegister(); 
    } else if (has_dest && !dest_var) { // Should be caught by earlier check
         emit("# ERROR: CallPureInst has destination flag but dest_var is null (intermediate logic error)");
         currentFunctionContext->releaseRegister(op1_reg);
         if (!op2_reg.empty()) currentFunctionContext->releaseRegister(op2_reg);
         return;
    } 
    // If !has_dest, result_val_reg remains empty. Instructions below should handle this (e.g. not use it or error out if they need it)

    if (inst->pureFunctionName == "__builtin_add_int") {
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_add_int needs a destination register."); } else
        emit("addu " + result_val_reg + ", " + op1_reg + ", " + op2_reg);
    } else if (inst->pureFunctionName == "__builtin_sub_int") {
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_sub_int needs a destination register."); } else
        emit("subu " + result_val_reg + ", " + op1_reg + ", " + op2_reg);
    } else if (inst->pureFunctionName == "__builtin_mul_int") {
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_mul_int needs a destination register."); } else {
            emit("multu " + op1_reg + ", " + op2_reg);
            emit("mflo " + result_val_reg);
        }
    } else if (inst->pureFunctionName == "__builtin_div_int") { 
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_div_int needs a destination register."); } else {
            emit("div " + op1_reg + ", " + op2_reg);
            emit("mflo " + result_val_reg);
        }
    } else if (inst->pureFunctionName == "__builtin_mod_int") { 
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_mod_int needs a destination register."); } else {
            emit("div " + op1_reg + ", " + op2_reg);
            emit("mfhi " + result_val_reg);
        }
    } else if (inst->pureFunctionName == "__builtin_eq_int") { 
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_eq_int needs a destination register."); } else {
            // (op1 == op2) -> set $dest to 1 if $op1 == $op2, else 0
            emit("xor " + result_val_reg + ", " + op1_reg + ", " + op2_reg);
            emit("sltiu " + result_val_reg + ", " + result_val_reg + ", 1"); // Set to 1 if (op1_reg ^ op2_reg) is 0
        }
    } else if (inst->pureFunctionName == "__builtin_ne_int") {
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_ne_int needs a destination register."); } else {
            // (op1 != op2) -> set $dest to 1 if $op1 != $op2, else 0
            emit("xor " + result_val_reg + ", " + op1_reg + ", " + op2_reg);
            emit("sltu " + result_val_reg + ", $zero, " + result_val_reg); // Set to 1 if (op1_reg ^ op2_reg) is non-zero
        }
    } else if (inst->pureFunctionName == "__builtin_lt_int") { // op1 < op2
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_lt_int needs a destination register."); } else {
            emit("slt " + result_val_reg + ", " + op1_reg + ", " + op2_reg);
        }
    } else if (inst->pureFunctionName == "__builtin_le_int") { // op1 <= op2
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_le_int needs a destination register."); } else {
            // (op1 <= op2)  is !(op2 < op1)
            emit("slt " + result_val_reg + ", " + op2_reg + ", " + op1_reg);  // $result = op2 < op1
            emit("xori " + result_val_reg + ", " + result_val_reg + ", 1"); // $result = !(op2 < op1)
        }
    } else if (inst->pureFunctionName == "__builtin_gt_int") { // op1 > op2
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_gt_int needs a destination register."); } else {
            emit("slt " + result_val_reg + ", " + op2_reg + ", " + op1_reg); // (op1 > op2) is (op2 < op1)
        }
    } else if (inst->pureFunctionName == "__builtin_ge_int") { // op1 >= op2
        if (result_val_reg.empty()) { emit("# ERROR: __builtin_ge_int needs a destination register."); } else {
            // (op1 >= op2) is !(op1 < op2)
            emit("slt " + result_val_reg + ", " + op1_reg + ", " + op2_reg);  // $result = op1 < op2
            emit("xori " + result_val_reg + ", " + result_val_reg + ", 1"); // $result = !(op1 < op2)
        }
    } else {
        emit("# ERROR: Unknown pure function in CallPureInst: " + inst->pureFunctionName);
    }

    if (has_dest && dest_var && !result_val_reg.empty()) {
        if (this->currentProgram && this->currentProgram->globalVariables.count(dest_var->name)) {
            std::string dest_addr_reg = currentFunctionContext->reserveRegister();
            emit("la " + dest_addr_reg + ", " + dest_var->name);
            emit("sw " + result_val_reg + ", 0(" + dest_addr_reg + ")");
            currentFunctionContext->releaseRegister(dest_addr_reg);
            emit("# Stored result of pure call to global " + dest_var->name);
        } else {
            int offset = currentFunctionContext->getVarStackOffset(dest_var->name);
            emit("sw " + result_val_reg + ", " + std::to_string(offset) + "($fp)");
            emit("# Stored result of pure call to local " + dest_var->name + " at " + std::to_string(offset) + "($fp)");
        }
    }

    currentFunctionContext->releaseRegister(op1_reg);
    if (!op2_reg.empty()) {
        currentFunctionContext->releaseRegister(op2_reg);
    }
    if (!result_val_reg.empty()) {
        currentFunctionContext->releaseRegister(result_val_reg);
    }
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::LoadArrayInst> inst) {
    if (!currentFunctionContext || !inst || !inst->arraySource || inst->indices.empty() || !inst->destination) {
        emit("# ERROR: LoadArrayInst visited with null context, instruction, source, indices, or destination.");
        return;
    }
    emit("# LoadArrayInst: " + inst->toString());

    auto array_var = std::dynamic_pointer_cast<IR::IRVariable>(inst->arraySource);
    if (!array_var) {
        emit("# ERROR: LoadArrayInst source is not an IRVariable: " + inst->arraySource->toString());
        return;
    }

    // 1. Get base address of the array
    std::string base_addr_reg = currentFunctionContext->reserveRegister();
    bool is_global_array = this->currentProgram && this->currentProgram->globalVariables.count(array_var->name);

    if (is_global_array) {
        emit("la " + base_addr_reg + ", " + array_var->name);
        emit("# Loaded base address of global array '" + array_var->name + "' into " + base_addr_reg);
    } else {
        // Local array (not in testfile.txt, but for completeness)
        // Assuming local arrays are allocated on stack and base_addr_reg will get $fp + offset
        // This part needs more robust handling for local arrays if they are directly on stack.
        // For now, testfile.txt uses global arrays.
        // If local arrays were pointers to heap, this would be different.
        // If local arrays are on stack, varLocations should give its base stack offset.
        int offset = currentFunctionContext->getVarStackOffset(array_var->name);
        emit("addiu " + base_addr_reg + ", $fp, " + std::to_string(offset));
        emit("# Calculated base address of local array '" + array_var->name + "' (offset " + std::to_string(offset) + ") into " + base_addr_reg);
    }

    // 2. Get index value
    // Assuming 1D array for testfile.txt, so inst->indices[0]
    std::string index_val_reg = currentFunctionContext->ensureOperandInRegister(inst->indices[0]);
    emit("# Index value for '" + array_var->name + "' is in " + index_val_reg);

    // 3. Element size (assuming 4 bytes for int)
    int element_size = 4; 
    std::string element_size_reg = currentFunctionContext->reserveRegister();
    emit("li " + element_size_reg + ", " + std::to_string(element_size));

    // 4. Calculate offset_in_bytes = index_value * element_size
    std::string offset_bytes_reg = currentFunctionContext->reserveRegister();
    emit("multu " + index_val_reg + ", " + element_size_reg);
    emit("mflo " + offset_bytes_reg);
    emit("# Calculated offset_in_bytes (" + index_val_reg + " * " + std::to_string(element_size) + ") into " + offset_bytes_reg);

    // 5. Calculate final_address = base_address_reg + offset_in_bytes_reg
    std::string final_addr_reg = base_addr_reg; // Reuse base_addr_reg for final address to save a reg
    emit("addu " + final_addr_reg + ", " + base_addr_reg + ", " + offset_bytes_reg);
    emit("# Calculated final element address for '" + array_var->name + "[index]' into " + final_addr_reg);

    // 6. Load word into destination register
    // The destination variable (inst->destination) needs a register.
    // ensureOperandInRegister might not be right here if inst->destination is where we WANT to store.
    // We need to map inst->destination to a physical register if it's not already, or store it if it is.
    // For now, assuming inst->destination is a temp var that will be assigned a stack slot by MipsFunctionContext
    // and we load the value into a reserved register, then SW it to inst->destination's stack slot.

    std::string value_loaded_reg = currentFunctionContext->reserveRegister();
    emit("lw " + value_loaded_reg + ", 0(" + final_addr_reg + ")");
    emit("# Loaded value from '" + array_var->name + "[index]' into temp reg " + value_loaded_reg);

    // Store the loaded value into the actual destination variable (inst->destination)
    if (this->currentProgram && this->currentProgram->globalVariables.count(inst->destination->name)) {
        std::string dest_addr_reg_for_global = currentFunctionContext->reserveRegister();
        emit("la " + dest_addr_reg_for_global + ", " + inst->destination->name);
        emit("sw " + value_loaded_reg + ", 0(" + dest_addr_reg_for_global + ")");
        currentFunctionContext->releaseRegister(dest_addr_reg_for_global);
        emit("# Stored loaded array element to global destination '" + inst->destination->name + "'");
    } else {
        int dest_offset = currentFunctionContext->getVarStackOffset(inst->destination->name);
        emit("sw " + value_loaded_reg + ", " + std::to_string(dest_offset) + "($fp)");
        emit("# Stored loaded array element to local destination '" + inst->destination->name + "' at offset " + std::to_string(dest_offset));
    }

    // Release registers
    currentFunctionContext->releaseRegister(value_loaded_reg);
    currentFunctionContext->releaseRegister(offset_bytes_reg);
    currentFunctionContext->releaseRegister(element_size_reg);
    currentFunctionContext->releaseRegister(index_val_reg);
    currentFunctionContext->releaseRegister(base_addr_reg); // This was final_addr_reg
    emit("# --- End of LoadArrayInst for " + array_var->name + " ---");
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::StoreArrayInst> inst) {
    if (!currentFunctionContext || !inst || !inst->arrayDestination || inst->indices.empty() || !inst->valueSource) {
        emit("# ERROR: StoreArrayInst visited with null context, instruction, destination, indices, or source.");
        return;
    }
    emit("# StoreArrayInst: " + inst->toString());

    auto array_var = std::dynamic_pointer_cast<IR::IRVariable>(inst->arrayDestination);
    if (!array_var) {
        emit("# ERROR: StoreArrayInst destination is not an IRVariable: " + inst->arrayDestination->toString());
        return;
    }

    // 1. Get base address of the array
    std::string base_addr_reg = currentFunctionContext->reserveRegister();
    bool is_global_array = this->currentProgram && this->currentProgram->globalVariables.count(array_var->name);

    if (is_global_array) {
        emit("la " + base_addr_reg + ", " + array_var->name);
        emit("# Loaded base address of global array '" + array_var->name + "' into " + base_addr_reg);
    } else {
        // Local array (similar to LoadArrayInst, needs robust handling if not pointer)
        int offset = currentFunctionContext->getVarStackOffset(array_var->name);
        emit("addiu " + base_addr_reg + ", $fp, " + std::to_string(offset));
        emit("# Calculated base address of local array '" + array_var->name + "' (offset " + std::to_string(offset) + ") into " + base_addr_reg);
    }

    // 2. Get index value
    std::string index_val_reg = currentFunctionContext->ensureOperandInRegister(inst->indices[0]);
    emit("# Index value for '" + array_var->name + "' is in " + index_val_reg);

    // 3. Element size (assuming 4 bytes for int)
    int element_size = 4; 
    std::string element_size_reg = currentFunctionContext->reserveRegister();
    emit("li " + element_size_reg + ", " + std::to_string(element_size));

    // 4. Calculate offset_in_bytes = index_value * element_size
    std::string offset_bytes_reg = currentFunctionContext->reserveRegister();
    emit("multu " + index_val_reg + ", " + element_size_reg);
    emit("mflo " + offset_bytes_reg);
    emit("# Calculated offset_in_bytes (" + index_val_reg + " * " + std::to_string(element_size) + ") into " + offset_bytes_reg);

    // 5. Calculate final_address = base_address_reg + offset_in_bytes_reg
    std::string final_addr_reg = base_addr_reg; // Reuse base_addr_reg
    emit("addu " + final_addr_reg + ", " + base_addr_reg + ", " + offset_bytes_reg);
    emit("# Calculated final element address for '" + array_var->name + "[index]' into " + final_addr_reg);

    // 6. Get source value to store into a register
    std::string value_to_store_reg = currentFunctionContext->ensureOperandInRegister(inst->valueSource);
    emit("# Value to store in '" + array_var->name + "[index]' is in " + value_to_store_reg);

    // 7. Store word
    emit("sw " + value_to_store_reg + ", 0(" + final_addr_reg + ")");
    emit("# Stored value from " + value_to_store_reg + " into '" + array_var->name + "[index]'");

    // Release registers
    currentFunctionContext->releaseRegister(value_to_store_reg);
    currentFunctionContext->releaseRegister(offset_bytes_reg);
    currentFunctionContext->releaseRegister(element_size_reg);
    currentFunctionContext->releaseRegister(index_val_reg);
    currentFunctionContext->releaseRegister(base_addr_reg); // This was final_addr_reg
    emit("# --- End of StoreArrayInst for " + array_var->name + " ---");
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::LabelInst> inst) {
    emitLabel(inst->name);
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::JumpInst> inst) {
    emit("j " + inst->target->labelName);
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::CondJumpInst> inst) {
    if (!currentFunctionContext || !inst || !inst->condition || !inst->trueTarget || !inst->falseTarget) {
        emit("# ERROR: CondJumpInst visited with null context, instruction, condition, or target labels.");
        return;
    }
    emit("# CondJumpInst: if (" + inst->condition->toString() + ") goto " + inst->trueTarget->labelName + " else goto " + inst->falseTarget->labelName);

    std::string cond_reg = currentFunctionContext->ensureOperandInRegister(inst->condition);

    // inst->trueTarget is the label if condition is true (non-zero)
    // inst->falseTarget is the label if condition is false (zero)
    emit("bnez " + cond_reg + ", " + inst->trueTarget->labelName); // Branch if Not Equal to Zero
    emit("nop"); // Branch delay slot

    emit("j " + inst->falseTarget->labelName); // Jump to false target if condition was zero (didn't take bnez)
    emit("nop"); // Branch delay slot

    currentFunctionContext->releaseRegister(cond_reg);
    emit("# --- End of CondJumpInst ---");
}

void MipsCodeGenerator::visit(std::shared_ptr<IR::CallNormalInst> inst) {
    if (!currentFunctionContext) {
        return;
    }
    emit("# --- Calling function: " + inst->functionName + " ---");

    int argCount = inst->arguments.size();
    std::vector<std::string> arg_regs_used;
    for (int i = 0; i < argCount && i < 4; ++i) {
        std::shared_ptr<IR::IROperand> argOperand = inst->arguments[i];
        std::string argSrcReg = currentFunctionContext->ensureOperandInRegister(argOperand);
        arg_regs_used.push_back(argSrcReg);  // Track for potential release
        std::string targetArgReg = "$a" + std::to_string(i);
        emit("move " + targetArgReg + ", " + argSrcReg);
    }

    // TODO: Pass arguments 5+ on stack

    emit("jal " + inst->functionName);
    emit("nop");  // Branch delay slot

    // Release temporary registers used for arguments after the call
    for (const std::string& reg : arg_regs_used) {
        currentFunctionContext->releaseRegister(reg);
    }

    if (inst->hasResultDestination && inst->resultDestination) {
        std::shared_ptr<IR::IRVariable> destVar = inst->resultDestination;
        // Result is in $v0. Store it to destVar.
        std::string dest_addr_reg;  // For global vars
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
        std::string dest_addr_reg = currentFunctionContext->reserveRegister();  // Temp for address
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

    currentFunctionContext->releaseRegister(src_reg);  // Release register used for source
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
    : irFunction(func), generator(gen), totalLocalVarSize(0), frameSize(0) {
    if (!irFunction) {
        // Handle error or return, as context is invalid without a function
        return;
    }
    this->epilogueLabel = irFunction->name + "_epilogue"; // Initialize epilogue label

    // 1. Calculate total size needed for parameters (if saved to stack) and local variables.
    int currentLocalOffset = 0; // Negative offsets from $fp

    // Process parameters first: assign them stack slots and add to varLocations
    // These will be slots where $a0-$a3 (or stack-passed args) are stored by prologue.
    for (size_t i = 0; i < irFunction->parameters.size(); ++i) {
        const auto& param = irFunction->parameters[i];
        if (param) {
            currentLocalOffset -= 4; // Allocate 4 bytes for the parameter on stack
            varLocations[param->name] = VarLocation(currentLocalOffset);
            paramNamesAndOffsetsForPrologue.push_back({param->name, currentLocalOffset});
            generator->emit("# Parameter '" + param->name + "' assigned stack offset: " + std::to_string(currentLocalOffset) + "($fp)");
        }    
    }

    // Process local variables
    for (const auto& local_pair : irFunction->locals) {
        const auto& varName = local_pair.first;
        // const auto& var = local_pair.second; // var is IRVariable
        // Ensure we don't re-allocate space if a parameter name somehow clashes with a local
        // (though symbol table should prevent this from IRGenerator side)
        if (varLocations.find(varName) == varLocations.end()) { 
            currentLocalOffset -= 4; // Allocate 4 bytes for the local variable
            varLocations[varName] = VarLocation(currentLocalOffset);
            generator->emit("# Local var '" + varName + "' assigned stack offset: " + std::to_string(currentLocalOffset) + "($fp)");
        }
    }
    this->totalLocalVarSize = currentLocalOffset; // Will be <= 0

    // 2. Calculate total frame size.
    // Base size for $ra and $fp = 8 bytes.
    // Additional space for any callee-saved registers we decide to use might be added later.
    this->frameSize = -this->totalLocalVarSize + 8; // e.g. if totalLocalVarSize = -12 (3 vars/params), frameSize = 12 + 8 = 20
    generator->emit("# Function " + irFunction->name + ": totalLocalVarSize = " + std::to_string(totalLocalVarSize) + ", frameSize = " + std::to_string(frameSize));
}

void MipsFunctionContext::generatePrologue() {
    if (!irFunction)
        return; 

    generator->emit("# Prologue for " + irFunction->name);
    generator->emit("addiu $sp, $sp, -" + std::to_string(frameSize));
    generator->emit("sw $ra, " + std::to_string(frameSize - 4) + "($sp)");
    generator->emit("sw $fp, " + std::to_string(frameSize - 8) + "($sp)");
    generator->emit("addiu $fp, $sp, " + std::to_string(frameSize - 8));

    // Store incoming parameters ($a0-$a3) to their assigned stack slots
    for (size_t i = 0; i < paramNamesAndOffsetsForPrologue.size() && i < 4; ++i) {
        const auto& param_info = paramNamesAndOffsetsForPrologue[i];
        std::string arg_reg = "$a" + std::to_string(i);
        generator->emit("sw " + arg_reg + ", " + std::to_string(param_info.second) + "($fp)");
        generator->emit("# Stored " + arg_reg + " (param '" + param_info.first + "') to " + std::to_string(param_info.second) + "($fp)");
    }
    // TODO: Handle parameters passed on stack (5th and onwards)
}

void MipsFunctionContext::generateEpilogue() {
    if (!irFunction)
        return;
    generator->emitLabel(this->epilogueLabel); // Emit the epilogue label
    generator->emit("# Epilogue for " + irFunction->name);
    // $v0 should have the return value if any (set by visit(ReturnInst))

    // $fp currently points to where old $fp was saved.
    // Old $ra is 4 bytes above that location (higher address).
    generator->emit("lw $ra, 4($fp)");      // Restore $ra from [current $fp + 4]
    generator->emit("lw $fp, 0($fp)");      // Restore old $fp from [current $fp + 0]

    // Restore $sp to its value before this function's frame was allocated.
    // The $sp was decremented by frameSize in the prologue.
    generator->emit("addiu $sp, $sp, " + std::to_string(frameSize));  // Deallocate frame
    generator->emit("jr $ra");
    generator->emit("nop");  // Branch delay slot
}

std::string MipsFunctionContext::ensureOperandInRegister(std::shared_ptr<IR::IROperand> operand) {
    if (!operand) {
        generator->emit("# Error: ensureOperandInRegister called with null operand");
        return "$zero";  // Should not happen
    }

    // Check if operand is already mapped to a register (e.g. if it was a recently stored temp)
    // This simple register allocator doesn't really do this; it always loads/creates.

    generator->emit("# Ensuring operand " + operand->toString() + " is in a register");

    if (auto constant = std::dynamic_pointer_cast<IR::IRConstant>(operand)) {
        std::string tempReg = reserveRegister();  // Reserve a general temp reg
        generator->emit("li " + tempReg + ", " + std::to_string(constant->value));
        return tempReg;
    } else if (auto variable = std::dynamic_pointer_cast<IR::IRVariable>(operand)) {
        // Check if it's a global variable first
        if (generator->currentProgram && generator->currentProgram->globalVariables.count(variable->name)) {
            std::string tempReg = reserveRegister();                    // Register to hold the value
            generator->emit("la " + tempReg + ", " + variable->name);   // Load address of global
            generator->emit("lw " + tempReg + ", 0(" + tempReg + ")");  // Load word from that address
            generator->emit("# Loaded global var " + variable->name + " into " + tempReg);
            return tempReg;
        } else {
            // Must be a local variable or parameter on the stack
            if (varLocations.count(variable->name)) {
                int offset = getVarStackOffset(variable->name);
                std::string tempReg = reserveRegister();  // Reserve a general temp reg for the loaded value
                // varLocations[variable->name] = tempReg; // No, varLocations stores stack offset or reg if allocated
                generator->emit("lw " + tempReg + ", " + std::to_string(offset) + "($fp)");
                generator->emit("# Loaded local var " + variable->name + " from " + std::to_string(offset) + "($fp) into " + tempReg);
                return tempReg;
            } else {
                generator->emit("# Error: Local variable " + variable->name + " not found in varLocations during ensureOperandInRegister.");
                return "$zero";  // Error case
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
    return "$zero";  // Fallback for unknown operand types
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
    return 0;  // Default or error value
}

std::string MipsFunctionContext::getVarRegister(const std::string& varName) {
    if (varLocations.count(varName)) {
        const auto& loc = varLocations.at(varName);
        if (loc.isString()) {  // Assuming string is register name
            return loc.getString();
        }
    }
    return "";  // Not in a register or not found
}

bool MipsFunctionContext::isVarInRegister(const std::string& varName) {
    if (varLocations.count(varName)) {
        return varLocations.at(varName).isString();  // Assuming string is register name
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
    return tempRegs[0];  // Problematic: fallback, should implement spilling.
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

    // Prologue to save $a0-$a3 and setup arg processing registers
    emit("addi $sp, $sp, -32");  // $ra, $fp, $s0-$s4 (7 regs) -> make it 32 for 8 words alignment
    emit("sw $ra, 28($sp)");
    emit("sw $fp, 24($sp)");
    emit("sw $s0, 20($sp)"); // format string pointer ($a0)
    emit("sw $s1, 16($sp)"); // saved $a1 (first vararg)
    emit("sw $s2, 12($sp)"); // saved $a2 (second vararg)
    emit("sw $s3, 8($sp)");  // saved $a3 (third vararg)
    emit("sw $s4, 4($sp)");  // vararg counter (0 -> $s1, 1 -> $s2, 2 -> $s3)
    emit("move $fp, $sp");

    emit("move $s0, $a0");      // Format string pointer
    emit("move $s1, $a1");      // Save $a1
    emit("move $s2, $a2");      // Save $a2
    emit("move $s3, $a3");      // Save $a3
    emit("li $s4, 0");          // vararg_counter = 0 (use $s1 next)

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
    emit("move $t1, $s1");      // Default to arg1 ($s1) for $a0 of syscall
    emit("beq $s4, 1, printf_use_arg2"); // If counter == 1, use arg2 ($s2)
    emit("nop");
    emit("beq $s4, 2, printf_use_arg3"); // If counter == 2, use arg3 ($s3)
    emit("nop");
    // If counter is 0 ($s4 == 0), $t1 already has $s1. Go to print.
    emit("j printf_perform_print_int");
    emit("nop");

    emitLabel("printf_use_arg2");
    emit("move $t1, $s2");      // Use arg2 ($s2) for $a0 of syscall
    emit("j printf_perform_print_int");
    emit("nop");

    emitLabel("printf_use_arg3");
    emit("move $t1, $s3");      // Use arg3 ($s3) for $a0 of syscall
    // Fall through to printf_perform_print_int

    emitLabel("printf_perform_print_int");
    emit("move $a0, $t1");      // Integer to print is in $t1, move to $a0 for syscall
    emit("li $v0, 1");          // Syscall for print_integer
    emit("syscall");
    emit("addi $s4, $s4, 1");   // Increment vararg counter
    emit("j printf_loop_continue");
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
    // Restore $s0-$s4, $fp, $ra
    emit("lw $s4, 4($sp)");
    emit("lw $s3, 8($sp)");
    emit("lw $s2, 12($sp)");
    emit("lw $s1, 16($sp)");
    emit("lw $s0, 20($sp)");
    emit("lw $fp, 24($sp)");
    emit("lw $ra, 28($sp)");
    emit("addi $sp, $sp, 32");
    emit("jr $ra");
    emit("nop");  // Branch delay slot
}

// Private method to be added to MipsCodeGenerator for getint
void MipsCodeGenerator::generateGetintImplementation() {
    emitLabel("getint");

    // Standard function prologue for getint
    // For getint, it's very simple: no locals, no parameters to save.
    // Only need to save $ra if we were to make calls, but we only do a syscall.
    // However, to be safe and consistent with a general function structure:
    emit("addi $sp, $sp, -8");  // Space for $ra, $fp (2 words)
    emit("sw $ra, 4($sp)");
    emit("sw $fp, 0($sp)");
    emit("move $fp, $sp");

    // Syscall for read_integer (code 5)
    // The integer read is returned in $v0
    emit("li $v0, 5");
    emit("syscall");

    // Standard function epilogue
    // Result is already in $v0, no need to move it.
    emit("lw $fp, 0($sp)");
    emit("lw $ra, 4($sp)");
    emit("addi $sp, $sp, 8");
    emit("jr $ra");
    emit("nop"); // Branch delay slot
}

}  // namespace MipsCodeGenerator