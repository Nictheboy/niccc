#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include "ir_generator.hpp"
#include "mips_code_generator.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

// Function to read file content
std::string readFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error opening file: " << filename << std::endl;
        exit(EXIT_FAILURE);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::cout << "Read file content: '" << buffer.str() << "'" << std::endl;
    return buffer.str();
}

// Simple and crude method to remove C-style comments
std::string removeComments(const std::string& input) {
    std::string result = input;
    
    // Remove multi-line comments /* */
    size_t pos = 0;
    while ((pos = result.find("/*", pos)) != std::string::npos) {
        size_t endPos = result.find("*/", pos + 2);
        if (endPos != std::string::npos) {
            result.erase(pos, endPos - pos + 2);
        } else {
            // If no closing */, remove everything from /* to end
            result.erase(pos);
            break;
        }
    }
    
    // Remove single-line comments //
    pos = 0;
    while ((pos = result.find("//", pos)) != std::string::npos) {
        size_t endPos = result.find('\n', pos);
        if (endPos != std::string::npos) {
            result.erase(pos, endPos - pos);
        } else {
            // If no newline, remove everything from // to end
            result.erase(pos);
            break;
        }
    }
    
    return result;
}

int main(int argc, char* argv[]) {
    (void)argc;  // Mark argc as unused
    (void)argv;  // Mark argv as unused
    std::string filename = "testfile.txt";
    std::string input = readFile(filename);
    
    // Remove C-style comments using simple and crude method
    input = removeComments(input);
    std::cout << "After removing comments: '" << input << "'" << std::endl;
    
    ScanContext scanContext(filename);

    // Create tokenizer and get definitions using the factory function
    auto tokenizerResult = Tokenizer::createTokenizer();
    auto tokenizer = tokenizerResult.first;
    auto tokenDefinitions = tokenizerResult.second;

    if (!tokenizer || !tokenDefinitions) {
        std::cerr << "Error: Could not create tokenizer or get definitions." << std::endl;
        return EXIT_FAILURE;
    }

    // Tokenize the input using the created tokenizer instance
    auto tokens = tokenizer->parse(input, filename);
    std::cout << "Tokenizer produced " << tokens->size() << " tokens" << std::endl;

    // Create grammar rules
    auto grammarRules = Parser::createGrammarRules();

    try {
        // Create parser, passing terminal definitions obtained from the tokenizer creation
        Parser::Parser parser(grammarRules, tokenDefinitions, scanContext);
        std::shared_ptr<AST::Node> astRoot = parser.parse(tokens);
        std::cout << "Parsing successful!" << std::endl;
        std::cout << "\nAST:" << std::endl;
        std::cout << astRoot->toString() << std::endl;

        IRGenerator::IRGenerator irGenerator;
        auto irProgram = irGenerator.generate(astRoot);
        std::cout << "IR Program:" << std::endl;
        std::cout << irProgram->toString() << std::endl;

        std::ofstream outputFile("mips.txt");
        std::cout << "Generating MIPS code..." << std::endl;
        MipsCodeGenerator::MipsCodeGenerator mipsCodeGenerator(outputFile);
        mipsCodeGenerator.generateProgram(irProgram);
        std::cout << "MIPS code generated successfully." << std::endl;
    } catch (const CompilerError& e) {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    } catch (const std::exception& e) {
        std::cerr << "Standard exception: " << e.what() << std::endl;
        return EXIT_FAILURE;
    } catch (...) {
        std::cerr << "Unknown exception occurred during parsing." << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
