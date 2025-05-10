#pragma once

#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>
#include "ir.hpp"  // For IR::IRVariable

namespace IRGenerator {

struct SymbolEntry {
    std::string name;
    std::shared_ptr<IR::IRVariable> irVar;
    std::shared_ptr<IR::IRType> type;  // 源语言类型或IR类型
    // 可以添加更多信息，如是否为常量、数组维度等
};

class Scope {
   public:
    std::map<std::string, SymbolEntry> symbols;
    Scope* parent;
    bool isFunctionScope;  // 标记是否为函数最外层作用域（用于参数）

    Scope(Scope* p = nullptr, bool isFunc = false)
        : parent(p), isFunctionScope(isFunc) {}

    bool add(const std::string& name, std::shared_ptr<IR::IRVariable> var, std::shared_ptr<IR::IRType> type) {
        if (symbols.count(name)) {
            // throw std::runtime_error("Symbol redefined: " + name); // 或者返回false
            return false;
        }
        symbols[name] = {name, var, type};
        return true;
    }

    SymbolEntry* lookup(const std::string& name) {
        auto it = symbols.find(name);
        if (it != symbols.end()) {
            return &it->second;
        }
        if (parent) {
            return parent->lookup(name);
        }
        return nullptr;
    }
};

class SymbolTable {
   public:
    SymbolTable() {
        globalScope = std::make_unique<Scope>(nullptr);
        currentScope = globalScope.get();
    }

    void enterScope(bool isFunctionEntry = false) {
        // isFunctionEntry 为 true 时，通常在处理函数参数后，进入函数体block前调用
        auto newScope = std::make_unique<Scope>(currentScope, isFunctionEntry);
        scopesStack.push_back(std::move(newScope));  // unique_ptr 移动所有权
        currentScope = scopesStack.back().get();
    }

    void leaveScope() {
        if (currentScope == globalScope.get()) {
            // throw std::runtime_error("Cannot leave global scope");
            return;  // Or handle error
        }
        currentScope = currentScope->parent;  // 指向上一个作用域
        scopesStack.pop_back();               // 移除当前作用域的unique_ptr，自动释放
    }

    bool addSymbol(const std::string& name, std::shared_ptr<IR::IRVariable> var, std::shared_ptr<IR::IRType> type) {
        return currentScope->add(name, var, type);
    }

    std::shared_ptr<IR::IRVariable> lookupVariable(const std::string& name) {
        SymbolEntry* entry = currentScope->lookup(name);
        return entry ? entry->irVar : nullptr;
    }

    SymbolEntry* lookupSymbolEntry(const std::string& name) {
        return currentScope->lookup(name);
    }

    Scope* getCurrentScope() const { return currentScope; }

   private:
    std::unique_ptr<Scope> globalScope;
    Scope* currentScope;
    std::vector<std::unique_ptr<Scope>> scopesStack;  // 使用 unique_ptr 管理作用域生命周期
};

};  // namespace IRGenerator
