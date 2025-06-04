#pragma once

#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>
// #include <optional> // Removed for C++11 compatibility
#include "ir.hpp"  // For IR::IRVariable and IR::IRType

namespace IRGenerator {

struct SymbolInfo {
    std::string name;
    std::shared_ptr<IR::IRVariable> variable;
    std::shared_ptr<IR::IRType> ir_type;
    bool is_const;
    int const_value;         // Value for compile-time constants
    bool has_const_value;    // Flag to indicate if const_value is set

    SymbolInfo() : variable(nullptr), ir_type(nullptr), is_const(false), const_value(0), has_const_value(false) {}

    // Constructor for non-constant or constants without a compile-time known value
    SymbolInfo(std::string n, std::shared_ptr<IR::IRVariable> var, std::shared_ptr<IR::IRType> t, bool is_c)
        : name(std::move(n)), variable(std::move(var)), ir_type(std::move(t)), is_const(is_c), const_value(0), has_const_value(false) {}

    // Constructor for constant symbols with a known compile-time value
    SymbolInfo(std::string n, std::shared_ptr<IR::IRVariable> var, std::shared_ptr<IR::IRType> t, bool is_c, int val)
        : name(std::move(n)), variable(std::move(var)), ir_type(std::move(t)), is_const(is_c), const_value(val), has_const_value(true) {}
};

class Scope {
   public:
    std::map<std::string, SymbolInfo> symbols;
    Scope* parent;
    bool isFunctionScope;

    Scope(Scope* p = nullptr, bool isFunc = false)
        : parent(p), isFunctionScope(isFunc) {}

    bool add(const std::string& name, std::shared_ptr<IR::IRVariable> var, std::shared_ptr<IR::IRType> type, bool is_const, bool known_const_val = false, int const_val = 0) {
        if (symbols.count(name)) {
            return false; 
        }
        if (is_const && known_const_val) {
            symbols[name] = SymbolInfo(name, var, type, is_const, const_val);
        } else {
            symbols[name] = SymbolInfo(name, var, type, is_const);
        }
        if (var) { 
            var->is_const = is_const;
        }
        return true;
    }

    SymbolInfo lookup(const std::string& name) {
        auto it = symbols.find(name);
        if (it != symbols.end()) {
            return it->second; 
        }
        if (parent) {
            return parent->lookup(name);
        }
        return SymbolInfo(); 
    }
};

class SymbolTable {
   public:
    SymbolTable() {
        enterScope(); 
    }

    void enterScope(bool isFunctionEntry = false) {
        Scope* parentScope = currentScope; 
        std::unique_ptr<Scope> newScope(new Scope(parentScope, isFunctionEntry));
        currentScope = newScope.get();
        scopesStack.push_back(std::move(newScope));
    }

    void leaveScope() {
        if (scopesStack.size() <= 1) { 
            return; 
        }
        scopesStack.pop_back();
        currentScope = scopesStack.empty() ? nullptr : scopesStack.back().get();
    }

    bool addSymbol(const std::string& name, std::shared_ptr<IR::IRVariable> var, std::shared_ptr<IR::IRType> type, bool is_const, bool known_const_val = false, int const_val = 0) {
        if (!currentScope) return false; 
        return currentScope->add(name, var, type, is_const, known_const_val, const_val);
    }

    SymbolInfo lookupSymbol(const std::string& name) {
        if (!currentScope) return SymbolInfo(); 
        return currentScope->lookup(name);
    }
    
    std::shared_ptr<IR::IRVariable> lookupVariable(const std::string& name) {
        SymbolInfo info = lookupSymbol(name);
        return info.variable; 
    }

    Scope* getCurrentScope() const { return currentScope; }

   private:
    Scope* currentScope = nullptr; 
    std::vector<std::unique_ptr<Scope>> scopesStack;
};

};  // namespace IRGenerator
