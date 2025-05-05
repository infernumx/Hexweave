#include "token.hpp"
#include <string>
#include <sstream> // For Token::toString
#include <variant> // For std::visit

// --- Function to convert TokenType to string ---
std::string tokenTypeToString(TokenType type) {
    switch(type) {
        // Single-character tokens
        case TokenType::LEFT_PAREN: return "LEFT_PAREN";
        case TokenType::RIGHT_PAREN: return "RIGHT_PAREN";
        case TokenType::LEFT_BRACE: return "LEFT_BRACE";
        case TokenType::RIGHT_BRACE: return "RIGHT_BRACE";
        case TokenType::LEFT_BRACKET: return "LEFT_BRACKET";
        case TokenType::RIGHT_BRACKET: return "RIGHT_BRACKET";
        case TokenType::COMMA: return "COMMA";
        case TokenType::DOT: return "DOT";
        case TokenType::MINUS: return "MINUS";
        case TokenType::PLUS: return "PLUS";
        case TokenType::SEMICOLON: return "SEMICOLON";
        case TokenType::SLASH: return "SLASH";
        case TokenType::STAR: return "STAR";
        case TokenType::PERCENT: return "PERCENT";
        case TokenType::PIPE: return "PIPE";
        case TokenType::COLON: return "COLON";

        // One or two character tokens
        case TokenType::BANG: return "BANG";
        case TokenType::BANG_EQUAL: return "BANG_EQUAL";
        case TokenType::EQUAL: return "EQUAL";
        case TokenType::EQUAL_EQUAL: return "EQUAL_EQUAL";
        case TokenType::GREATER: return "GREATER";
        case TokenType::GREATER_EQUAL: return "GREATER_EQUAL";
        case TokenType::LESS: return "LESS";
        case TokenType::LESS_EQUAL: return "LESS_EQUAL";

        // Literals
        case TokenType::IDENTIFIER: return "IDENTIFIER";
        case TokenType::STRING: return "STRING";
        case TokenType::NUMBER_INT: return "NUMBER_INT";
        case TokenType::NUMBER_FLOAT: return "NUMBER_FLOAT";
        case TokenType::INTERPOLATED_STRING: return "INTERPOLATED_STRING";

        // Keywords
        case TokenType::AND: return "AND";
        case TokenType::CLASS: return "CLASS";
        case TokenType::ELSE: return "ELSE";
        case TokenType::FALSE: return "FALSE";
        case TokenType::FN: return "FN";
        case TokenType::FOR: return "FOR";
        case TokenType::IF: return "IF";
        case TokenType::NIL: return "NIL";
        case TokenType::OR: return "OR";
        case TokenType::PRINT: return "PRINT";
        case TokenType::RETURN: return "RETURN";
        case TokenType::SUPER: return "SUPER";
        case TokenType::THIS: return "THIS";
        case TokenType::TRUE: return "TRUE";
        case TokenType::VAR: return "VAR";
        case TokenType::WHILE: return "WHILE";
        case TokenType::MAP: return "MAP";
        case TokenType::DELETE: return "DELETE";
        case TokenType::BREAK: return "BREAK";
        case TokenType::CONTINUE: return "CONTINUE";
        case TokenType::LIST: return "LIST";
        case TokenType::IMPORT: return "IMPORT"; // <<< ADDED IMPORT case

        // Type Keywords
        case TokenType::INT: return "INT";
        case TokenType::FLOAT: return "FLOAT";
        case TokenType::BOOL: return "BOOL";
        case TokenType::STRING_TYPE: return "STRING_TYPE";
        case TokenType::OBJ: return "OBJ";
        case TokenType::FUNCTION: return "FUNCTION";

        // Special
        case TokenType::END_OF_FILE: return "EOF";
        case TokenType::UNKNOWN: return "UNKNOWN";

        default: return "INVALID_TOKEN_TYPE";
    }
}

// --- Token::toString implementation ---
std::string Token::toString() const {
    std::string literal_str = "None";
    // Use std::visit to handle the variant literal type safely.
    std::visit([&literal_str](const auto& value) {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, std::string>) {
            literal_str = "\"" + value + "\""; // Add quotes around string literals
        } else if constexpr (std::is_same_v<T, int>) {
            literal_str = std::to_string(value);
        } else if constexpr (std::is_same_v<T, double>) {
            std::ostringstream oss; oss << value; literal_str = oss.str();
        } else if constexpr (std::is_same_v<T, bool>) {
            literal_str = value ? "true" : "false";
        }
        else if constexpr (std::is_same_v<T, InterpolatedStringData>) {
             std::stringstream ss;
             ss << "[";
             bool first = true;
             for(const auto& part : value) {
                 if (!first) ss << ", ";
                 first = false;
                 if (part.first == InterpolationPartType::LITERAL) {
                     ss << "LITERAL:\"" << part.second << "\"";
                 } else {
                     ss << "EXPR:\"" << part.second << "\"";
                 }
             }
             ss << "]";
             literal_str = ss.str();
        }
        // std::monostate (NilType) is handled by the default "None"
    }, literal);

    return tokenTypeToString(type) + " " + lexeme + " " + literal_str + " (Line " + std::to_string(line) + ")";
}
