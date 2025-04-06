#include <fstream>
#include <sstream>
#include <string>
#include "tokenizer_spec.hpp"

int main() {
    std::ifstream file("testfile.txt");
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string fileContent = buffer.str();
    auto tokenizer = createTokenizer();
    auto tokenList = tokenizer->parse(fileContent, "testfile.txt");
    std::ofstream outFile("output.txt", std::ios::trunc);
    for (auto& token : *tokenList) {
        if (token.definition->name == "WHITESPACE" || token.definition->name == "COMMENT")
            continue;
        ;
        outFile << token.definition->name << " " << token.matched << "\n";
    }
    outFile.close();
    return 0;
}
