#pragma once

#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>
#include "ir.hpp"  // For IR::IRVariable and IR::IRType

namespace IRGenerator {

// Renamed from SymbolEntry to SymbolInfo and added is_const
struct SymbolInfo {
    std::string name;
    std::shared_ptr<IR::IRVariable> variable; // Renamed from irVar for clarity
    std::shared_ptr<IR::IRType> ir_type;    // Renamed from type
    bool is_const;
    // Add other relevant symbol information if needed, like scope depth, etc.

    // Default constructor for not-found cases
    SymbolInfo() : variable(nullptr), ir_type(nullptr), is_const(false) {}

    SymbolInfo(std::string n, std::shared_ptr<IR::IRVariable> var, std::shared_ptr<IR::IRType> t, bool is_c)
        : name(std::move(n)), variable(std::move(var)), ir_type(std::move(t)), is_const(is_c) {}
};

class Scope {
   public:
    std::map<std::string, SymbolInfo> symbols; // Changed to store SymbolInfo
    Scope* parent;
    bool isFunctionScope;

    Scope(Scope* p = nullptr, bool isFunc = false)
        : parent(p), isFunctionScope(isFunc) {}

    // Updated to accept is_const and store full SymbolInfo
    bool add(const std::string& name, std::shared_ptr<IR::IRVariable> var, std::shared_ptr<IR::IRType> type, bool is_const) {
        if (symbols.count(name)) {
            return false; // Symbol redefined in current scope
        }
        symbols[name] = SymbolInfo(name, var, type, is_const);
        if (var) { // Pass constness to IRVariable as well for MIPS generator's direct use
            var->is_const = is_const;
        }
        return true;
    }

    // Updated to return SymbolInfo; returns a default-constructed SymbolInfo if not found
    SymbolInfo lookup(const std::string& name) {
        auto it = symbols.find(name);
        if (it != symbols.end()) {
            return it->second; // Return copy of SymbolInfo
        }
        if (parent) {
            return parent->lookup(name);
        }
        return SymbolInfo(); // Not found, return default (variable will be nullptr)
    }
};

class SymbolTable {
   public:
    SymbolTable() {
        // Create a global scope when the symbol table is constructed.
        // No need to explicitly manage globalScope unique_ptr separately if using scopesStack for all.
        enterScope(); // Initial global scope
    }

    void enterScope(bool isFunctionEntry = false) {
        Scope* parentScope = currentScope; // If currentScope is null, it's the first (global) scope
        // auto newScope = std::make_unique<Scope>(parentScope, isFunctionEntry);
        std::unique_ptr<Scope> newScope(new Scope(parentScope, isFunctionEntry));
        currentScope = newScope.get();
        scopesStack.push_back(std::move(newScope));
    }

    void leaveScope() {
        if (scopesStack.size() <= 1) { // Prevent leaving the initial global scope
            // Or throw std::runtime_error("Cannot leave the global scope.");
            return; 
        }
        scopesStack.pop_back();
        currentScope = scopesStack.empty() ? nullptr : scopesStack.back().get();
    }

    // Updated to accept is_const
    bool addSymbol(const std::string& name, std::shared_ptr<IR::IRVariable> var, std::shared_ptr<IR::IRType> type, bool is_const) {
        if (!currentScope) return false; // Should not happen if constructor ensures a scope
        return currentScope->add(name, var, type, is_const);
    }

    // Renamed from lookupSymbolEntry and changed return type to SymbolInfo
    SymbolInfo lookupSymbol(const std::string& name) {
        if (!currentScope) return SymbolInfo(); // Should not happen
        return currentScope->lookup(name);
    }
    
    // Kept for compatibility if MIPS generator directly uses it, but prefer lookupSymbol
    std::shared_ptr<IR::IRVariable> lookupVariable(const std::string& name) {
        SymbolInfo info = lookupSymbol(name);
        return info.variable; // Will be nullptr if not found
    }

    Scope* getCurrentScope() const { return currentScope; }

   private:
    // std::unique_ptr<Scope> globalScope; // Managed by scopesStack now
    Scope* currentScope = nullptr; // Initialize to nullptr, set by enterScope
    std::vector<std::unique_ptr<Scope>> scopesStack;
};

};  // namespace IRGenerator
