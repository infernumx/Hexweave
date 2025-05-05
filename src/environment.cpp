#include "environment.hpp"
#include "common.hpp" // For RuntimeError, debug_log
#include <utility> // For std::move
#include <iostream> // For debug cerr

// Constructor for global scope
Environment::Environment() : enclosing(nullptr) {}

// Constructor for local scope
Environment::Environment(std::shared_ptr<Environment> enclosing_env)
    : enclosing(std::move(enclosing_env)) {}

// --- Private Helper to Find Variable Info ---
VariableInfo* Environment::findInfo(const std::string& name) {
    auto it = values.find(name);
    if (it != values.end()) {
        return &(it->second); // Found in current scope
    }
    if (enclosing != nullptr) {
        return enclosing->findInfo(name); // Recursively search upwards
    }
    return nullptr; // Not found
}
const VariableInfo* Environment::findInfo(const std::string& name) const {
     auto it = values.find(name);
    if (it != values.end()) {
        return &(it->second); // Found in current scope
    }
    if (enclosing != nullptr) {
        return enclosing->findInfo(name); // Recursively search upwards
    }
    return nullptr; // Not found
}


// Define a new variable in the *current* scope with type info.
void Environment::define(const std::string& name, const Value& value,
                         const Token& type_token,
                         const std::optional<Token>& map_key_type,
                         const std::optional<Token>& map_value_type,
                         const std::optional<Token>& list_element_type)
{
    if (values.count(name)) {
        debug_log << "Warning: Redefining variable '" << name << "' in the same scope (Line: " << type_token.line << ")." << std::endl;
    }

    // <<< Use the single comprehensive constructor >>>
    // Pass all optional types; the constructor handles defaults.
    VariableInfo info{value, type_token, map_key_type, map_value_type, list_element_type};

    // Use insert_or_assign to handle potential redefinition.
    values.insert_or_assign(name, std::move(info));
}

// Define without explicit type (e.g., for 'var' or built-ins, assume OBJ type)
void Environment::define(const std::string& name, const Value& value) {
     // Create a dummy OBJ token for the type specifier.
     Token obj_token(TokenType::OBJ, "obj", -1); // Line number -1 indicates internal definition
     if (values.count(name)) {
        // Allow redefinition for built-ins or var re-assignment in the same scope.
     }

     // <<< This call now matches the comprehensive constructor >>>
     // The last three std::nullopt arguments will match the optional parameters
     // with default values in the VariableInfo constructor.
     values.insert_or_assign(name, VariableInfo{value, obj_token, std::nullopt, std::nullopt, std::nullopt});
}


// Get a variable's value, searching up the scope chain.
Value Environment::get(const Token& name_token) {
    const VariableInfo* info = findInfo(name_token.lexeme);
    if (info) {
        return info->value;
    }
    throw RuntimeError("Undefined variable '" + name_token.lexeme + "'.", name_token.line);
}

// Get the full VariableInfo for type checking.
const VariableInfo& Environment::getInfo(const Token& name_token) {
     const VariableInfo* info = findInfo(name_token.lexeme);
     if (info) {
         return *info;
     }
     throw RuntimeError("Undefined variable '" + name_token.lexeme + "'.", name_token.line);
}


// Assign a value to an *existing* variable, searching up the scope chain.
void Environment::assign(const Token& name_token, const Value& value) {
     VariableInfo* info = findInfo(name_token.lexeme);
     if (info) {
         // TODO: Add const check: if (info->is_const) throw RuntimeError(...)
         info->value = value;
         return;
     }
     throw RuntimeError("Undefined variable '" + name_token.lexeme + "' for assignment.", name_token.line);
}

// Undefine a variable from the current scope only.
bool Environment::undefine(const std::string& name) {
    return values.erase(name) > 0;
}

// Get the enclosing environment.
std::shared_ptr<Environment> Environment::getEnclosing() const {
    return enclosing;
}
