#pragma once
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

struct ScanContext {
    std::string filename;
    int row, column;
    std::ifstream file;
    std::string currentToken;

    ScanContext(const std::string& filename)
        : filename(filename), row(1), column(1) {
        file.open(filename);
        if (!file.is_open()) {
            throw std::runtime_error("Failed to open file: " + filename);
        }
        std::cerr << "Successfully opened file: " << filename << std::endl;
    }

    bool next(char& c) {
        if (file.get(c)) {
            if (c == '\n') {
                row++;
                column = 1;
            } else {
                column++;
            }
            return true;
        }
        return false;
    }

    void appendToToken(char c) {
        currentToken += c;
    }

    std::string getTokenValue() {
        // Read the entire file into a string
        std::stringstream buffer;
        buffer << file.rdbuf();
        std::string content = buffer.str();
        std::cerr << "Read file content: '" << content << "'" << std::endl;
        return content;
    }
};

class CompilerError : public std::runtime_error {
   public:
    const std::string filename;
    const int row, column;
    const std::string message;
    CompilerError(ScanContext& context, std::string message)
        : std::runtime_error("error: " + message + " near " + context.filename + "(" + std::to_string(context.row) + ", " + std::to_string(context.column) + ")"),
          filename(context.filename),
          row(context.row),
          column(context.column),
          message(message) {};
};
