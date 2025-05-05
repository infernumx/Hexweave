#ifndef ENVIRONMENT_HPP
#define ENVIRONMENT_HPP

#include "token.hpp"
#include "value.hpp" // <<< Include full definition BEFORE VariableInfo
#include <string>
#include <unordered_map>
#include <memory> // For shared_ptr
#include <optional> // For optional tokens
#include <tuple> // For map type info

// --- Forward Declarations ---
// class Value; // Included above

// Structure to hold variable info in the environment
struct VariableInfo {
    Value value; // The current value of the variable
    Token declared_type_token; // Primary type token (INT, MAP, LIST, OBJ, etc.)
    // Optional type tokens for complex types
    std::optional<Token> declared_map_key_type;
    std::optional<Token> declared_map_value_type;
    std::optional<Token> declared_list_element_type; // <<< ADDED for lists
    bool is_const = false; // For future 'const' keyword support

    // Default constructor
    VariableInfo() = default;

    // <<< REMOVED previous specific constructors >>>
    // VariableInfo(Value val, Token type, bool cnst = false) ...
    // VariableInfo(Value val, Token type, std::optional<Token> map_key, std::optional<Token> map_val, bool cnst = false) ...
    // VariableInfo(Value val, Token type, std::optional<Token> list_element, bool cnst = false) ...

    // <<< ADDED Comprehensive Constructor >>>
    // Handles all cases: primitives, maps, lists, obj
    VariableInfo(Value val, Token type,
                 std::optional<Token> map_key = std::nullopt,
                 std::optional<Token> map_val = std::nullopt,
                 std::optional<Token> list_element = std::nullopt,
                 bool cnst = false)
        : value(std::move(val)),
          declared_type_token(std::move(type)),
          declared_map_key_type(std::move(map_key)),
          declared_map_value_type(std::move(map_val)),
          declared_list_element_type(std::move(list_element)),
          is_const(cnst)
    {}
};


// Manages variable scopes and bindings using enclosing environments.
class Environment : public std::enable_shared_from_this<Environment> {
public:
    // Constructor for global scope (no enclosing environment)
    Environment();

    // Constructor for local scopes (enclosed within another environment)
    Environment(std::shared_ptr<Environment> enclosing);

    // Define a new variable in the current scope with its declared type info.
    // Takes optional type tokens for map keys/values and list elements.
    void define(const std::string& name, const Value& value,
                const Token& type_token, // The primary type token (e.g., INT, MAP, LIST)
                const std::optional<Token>& map_key_type = std::nullopt,
                const std::optional<Token>& map_value_type = std::nullopt,
                const std::optional<Token>& list_element_type = std::nullopt); // <<< ADDED

    // Define without explicit type (for 'var' or built-ins).
    // Implicitly uses OBJ type specifier internally.
    void define(const std::string& name, const Value& value);


    // Get the value of a variable, searching up the scope chain.
    // Throws RuntimeError if not found.
    Value get(const Token& name_token);

    // Get the full VariableInfo (including type) for type checking during assignment/calls.
    // Throws RuntimeError if not found.
    const VariableInfo& getInfo(const Token& name_token);


    // Assign a new value to an existing variable, searching up the scope chain.
    // Throws RuntimeError if not found. Type checking should happen *before* calling this.
    void assign(const Token& name_token, const Value& value);

    // Undefine a variable ONLY from the current scope.
    // Returns true if the variable was found and removed, false otherwise.
    bool undefine(const std::string& name);

    // Get the enclosing environment (nullptr for global scope).
    std::shared_ptr<Environment> getEnclosing() const;


private:
    std::shared_ptr<Environment> enclosing; // Pointer to the outer scope (nullptr for global)
    // Stores the variables defined in *this* specific scope.
    std::unordered_map<std::string, VariableInfo> values;

    // Helper to find variable info up the scope chain. Returns pointer to modify, or nullptr.
    VariableInfo* findInfo(const std::string& name);
    // Const version for read-only access.
    const VariableInfo* findInfo(const std::string& name) const;

};

#endif // ENVIRONMENT_HPP
