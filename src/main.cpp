#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include "common.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

class SyntaxAnalyzer {
   private:
    std::shared_ptr<Tokenizer::Tokenizer> tokenizer;
    std::shared_ptr<Parser::Parser> parser;
    std::ofstream outputFile;
    ScanContext scanContext;

    void outputToken(const Tokenizer::Token& token) {
        outputFile << token.definition->name << " " << token.matched << std::endl;
    }

    void outputSyntaxComponent(const std::string& name) {
        if (name != "BlockItem" && name != "Decl" && name != "BType") {
            outputFile << "<" << name << ">" << std::endl;
        }
    }

   public:
    SyntaxAnalyzer()
        : scanContext("testfile.txt") {
        // Initialize tokenizer
        tokenizer = Tokenizer::createTokenizer();

        // Initialize parser
        auto grammarRules = Parser::createGrammarRules();
        parser = std::make_shared<Parser::Parser>(grammarRules, scanContext);

        // Open output file
        outputFile.open("output.txt");
        if (!outputFile.is_open()) {
            throw std::runtime_error("Failed to open output.txt");
        }
    }

    ~SyntaxAnalyzer() {
        if (outputFile.is_open()) {
            outputFile.close();
        }
    }

    void analyze() {
        try {
            // Tokenize the input
            auto tokens = tokenizer->parse(scanContext.getTokenValue(), scanContext.filename);

            // Parse the tokens
            parser->parse(tokens);

            // Output the final CompUnit
            outputSyntaxComponent("CompUnit");

        } catch (const CompilerError& e) {
            std::cerr << e.what() << std::endl;
            throw;
        }
    }
};

int main() {
    try {
        SyntaxAnalyzer analyzer;
        analyzer.analyze();
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
}
