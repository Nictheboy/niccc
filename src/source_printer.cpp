#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>
#include <stdexcept> // Required for std::runtime_error

#include "ir.hpp"
#include "mips_code_generator.hpp"
#include "common.hpp" // For CompilerError and IR::DataType

// Function to read file content
static std::string readFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error opening file: " << filename << std::endl;
        // Instead of exit, throw an exception to be caught by main
        throw std::runtime_error("Error opening file: " + filename);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

// Function to escape '%' characters for the MIPS printf function
static std::string escapePercentCharacters(const std::string& input) {
    std::string output;
    output.reserve(input.length());
    for (char c : input) {
        if (c == '%') {
            output += "%%";
        } else {
            output += c;
        }
    }
    return output;
}

int main(int argc, char* argv[]) {
    (void)argc; // Mark argc as unused
    (void)argv; // Mark argv as unused

    std::string input_filename = "testfile.txt";
    std::string output_mips_filename = "mips.txt";

    try {
        std::cout << "Reading source file: " << input_filename << std::endl;
        std::string source_content = readFile(input_filename);
        std::string escaped_source_content = escapePercentCharacters(source_content);

        std::cout << "Constructing IR program..." << std::endl;
        auto irProgram = std::make_shared<IR::IRProgram>();

        // Add the escaped source content as a string literal to the IR program.
        std::string source_string_label = irProgram->addStringLiteral(escaped_source_content);
        std::cout << "Added string literal with label: " << source_string_label << std::endl;

        // Create the main function in IR
        std::vector<std::shared_ptr<IR::IRVariable>> main_params; // main has no parameters
        auto main_return_type = std::make_shared<IR::SimpleIRType>(IR::SimpleTypeKind::INTEGER);
        auto main_ir_func = std::make_shared<IR::NormalIRFunction>("main", main_params, main_return_type);
        irProgram->addNormalFunction(main_ir_func);

        // Create IR operand for the string literal's label
        // IRLabelOperand constructor takes the label name.
        // The type for such an operand (a pointer to a string) is implicitly handled
        // or determined by the MIPS generator based on context.
        // The IRLabelOperand constructor itself assigns a LabelIRType.
        auto string_arg_op = std::make_shared<IR::IRLabelOperand>(source_string_label);

        // Create CallInst to printf
        std::vector<std::shared_ptr<IR::IROperand>> printf_args;
        printf_args.push_back(string_arg_op);
        // The MIPS `printf` does not return a value we use, so resultDestination is nullptr.
        auto call_printf = std::make_shared<IR::CallNormalInst>("printf", printf_args, nullptr);
        main_ir_func->addInstruction(call_printf);

        // Create ReturnInst for 'return 0'
        auto ret_val_op = std::make_shared<IR::IRConstant>(0); // IRConstant takes int value
        auto return_inst = std::make_shared<IR::ReturnInst>(ret_val_op);
        main_ir_func->addInstruction(return_inst);
        
        std::cout << "IR Program constructed." << std::endl;
        // Optional: Print IR for debugging
        // std::cout << "Generated IR:\n" << irProgram->toString() << std::endl;


        std::cout << "Generating MIPS code to " << output_mips_filename << "..." << std::endl;
        std::ofstream mips_output_file(output_mips_filename);
        if (!mips_output_file.is_open()) {
            std::cerr << "Error opening MIPS output file: " << output_mips_filename << std::endl;
            return EXIT_FAILURE;
        }

        MipsCodeGenerator::MipsCodeGenerator mips_gen(mips_output_file);
        mips_gen.generateProgram(irProgram);

        mips_output_file.close();
        std::cout << "MIPS code generated successfully to " << output_mips_filename << std::endl;

    } catch (const CompilerError& e) {
        std::cerr << "CompilerError: " << e.what() << std::endl;
        return EXIT_FAILURE;
    } catch (const std::exception& e) {
        std::cerr << "Standard exception: " << e.what() << std::endl;
        return EXIT_FAILURE;
    } catch (...) {
        std::cerr << "Unknown exception occurred." << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
} 