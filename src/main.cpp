#include <fstream>
#include <iostream>
#include <memory>
#include "parser.hpp"
#include "tokenizer.hpp"

void traverseAST(std::shared_ptr<AST::Node> node, std::ofstream& outputFile) {
    if (auto terminal = std::dynamic_pointer_cast<AST::TerminalNode>(node)) {
        // Output token information
        outputFile << terminal->token->definition->name << " " << terminal->token->matched << std::endl;
    } else if (auto nonTerminal = std::dynamic_pointer_cast<AST::NonTerminalNode>(node)) {
        // First traverse all children
        for (const auto& child : nonTerminal->children) {
            traverseAST(child, outputFile);
        }
        
        // Output syntax component name if it's not one of the excluded types
        if (nonTerminal->name != "BlockItem" && 
            nonTerminal->name != "Decl" && 
            nonTerminal->name != "BType") {
            outputFile << "<" << nonTerminal->name << ">" << std::endl;
        }
    }
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <input_file>" << std::endl;
        return 1;
    }

    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cerr << "Failed to open file: " << argv[1] << std::endl;
        return 1;
    }
    std::cout << "Successfully opened file: " << argv[1] << std::endl;

    // Read file content
    std::string content((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    std::cout << "Read file content: '" << content << "'" << std::endl;

    // Create tokenizer
    auto tokenizer = Tokenizer::createTokenizer();
    std::cout << "Tokenizer input: '" << content << "'" << std::endl;

    // Tokenize
    ScanContext scanContext(argv[1]);
    auto tokens = tokenizer->parse(content, argv[1]);
    std::cout << "Tokenizer produced " << tokens->size() << " tokens" << std::endl;

    // Create parser
    auto rules = Parser::createGrammarRules();
    Parser::Parser parser(rules, scanContext);

    // Parse and get AST
    auto ast = parser.parse(tokens);
    if (ast) {
        // Open output file
        std::ofstream outputFile("output.txt");
        if (!outputFile.is_open()) {
            std::cerr << "Failed to open output file" << std::endl;
            return 1;
        }

        // Traverse AST and write output
        traverseAST(ast, outputFile);
        outputFile.close();
        std::cout << "Output written to output.txt" << std::endl;
    } else {
        std::cerr << "Failed to build AST" << std::endl;
        return 1;
    }

    return 0;
}
