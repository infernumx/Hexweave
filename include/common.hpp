#ifndef COMMON_HPP
#define COMMON_HPP

#include <string>
#include <vector>
#include <memory> // For std::shared_ptr, std::unique_ptr
#include <variant> // For Value
#include <optional> // For optional return values
#include <stdexcept> // For exceptions
#include <fstream> // <<< Added for std::ofstream

// Forward declarations to avoid circular dependencies
class Value;
class Environment;
namespace AST {
    class Node; // Base AST Node
    // Add forward declarations for specific AST nodes as needed
    class Statement;
    class Expression;
}

// <<< Declare global debug log stream >>>
extern std::ofstream debug_log;

// --- Error Handling ---
class LangError : public std::runtime_error {
public:
    LangError(const std::string& message) : std::runtime_error(message) {}
};

class SyntaxError : public LangError {
public:
    SyntaxError(const std::string& message, int line = -1, int col = -1);
    // Add line/col information if available
    int line = -1;
    int col = -1;
};

class TypeError : public LangError {
public:
    TypeError(const std::string& message, int line = -1, int col = -1);
     // Add line/col information if available
    int line = -1;
    int col = -1;
};

class RuntimeError : public LangError {
public:
    RuntimeError(const std::string& message, int line = -1, int col = -1);
     // Add line/col information if available
    int line = -1;
    int col = -1;
};


#endif // COMMON_HPP
