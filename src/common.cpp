#include "common.hpp"
#include <string> // Make sure string is included

// --- Error Class Constructor Definitions ---

// Helper to build the error message string including line/col if available
// <<< REMOVED this helper as formatting is now done in main.cpp >>>
// std::string formatErrorMessage(const std::string& base_message, int line, int col) { ... }

SyntaxError::SyntaxError(const std::string& message, int line_num, int col_num)
    : LangError(message), // <<< Pass the raw message to the base class >>>
      line(line_num),
      col(col_num)
{}

TypeError::TypeError(const std::string& message, int line_num, int col_num)
    : LangError(message), // <<< Pass the raw message to the base class >>>
      line(line_num),
      col(col_num)
{}

RuntimeError::RuntimeError(const std::string& message, int line_num, int col_num)
    : LangError(message), // <<< Pass the raw message to the base class >>>
      line(line_num),
      col(col_num)
{}
