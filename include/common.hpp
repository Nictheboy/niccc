#pragma once
#include <stdexcept>
#include <string>

struct ScanContext {
    std::string filename;
    int row, column;
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
