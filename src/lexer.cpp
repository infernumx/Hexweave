#include "lexer.hpp"
#include "common.hpp" // For error reporting (optional), debug_log
#include "token.hpp"  // <<< Ensure TokenType::IMPORT is defined here
#include <iostream>   // For error reporting (can be replaced with better logging)
#include <stdexcept>  // For errors like unterminated string
#include <cctype>     // For isdigit, isalpha
#include <utility>    // For std::move
#include <vector>     // Included via header
#include <unordered_map> // Included via header
#include <sstream>    // For building strings in scanInterpolatedString

// Initialize the static keywords map
const std::unordered_map<std::string, TokenType> Lexer::keywords = {
    {"and",      TokenType::AND},
    {"class",    TokenType::CLASS},
    {"else",     TokenType::ELSE},
    {"false",    TokenType::FALSE},
    {"for",      TokenType::FOR},
    {"fn",       TokenType::FN},
    {"if",       TokenType::IF},
    {"nil",      TokenType::NIL},
    {"or",       TokenType::OR},
    {"print",    TokenType::PRINT},
    {"return",   TokenType::RETURN},
    {"super",    TokenType::SUPER},
    {"this",     TokenType::THIS},
    {"true",     TokenType::TRUE},
    {"var",      TokenType::VAR},
    {"while",    TokenType::WHILE},
    {"map",      TokenType::MAP},
    {"delete",   TokenType::DELETE},
    {"break",    TokenType::BREAK},
    {"continue", TokenType::CONTINUE},
    {"list",     TokenType::LIST},
    {"import",   TokenType::IMPORT}, // <<< ADDED 'import' keyword
    // Type keywords
    {"int",      TokenType::INT},
    {"float",    TokenType::FLOAT},
    {"bool",     TokenType::BOOL},
    {"string",   TokenType::STRING_TYPE},
    {"obj",      TokenType::OBJ},
    {"function", TokenType::FUNCTION} // Added FUNCTION keyword if it wasn't there
};


Lexer::Lexer(std::string source) : source(std::move(source)) {}

std::vector<Token> Lexer::scanTokens() {
    while (!isAtEnd()) {
        start = current;
        scanToken();
    }
    tokens.emplace_back(TokenType::END_OF_FILE, "", std::monostate{}, line);
    return tokens;
}

bool Lexer::isAtEnd() const {
    return current >= source.length();
}

char Lexer::advance() {
    if (!isAtEnd()) {
        return source[current++];
    }
    return '\0';
}

void Lexer::addToken(TokenType type) {
    addToken(type, std::monostate{});
}

// Overload to accept the variant type for literals
void Lexer::addToken(TokenType type, const TokenLiteral& literal) {
    if (start < 0 || start > static_cast<int>(source.length()) || current < start || current > static_cast<int>(source.length())) {
         debug_log << "Lexer Internal Error (Line " << line << "): Invalid indices for substring (start=" << start << ", current=" << current << ")" << std::endl;
         tokens.emplace_back(TokenType::UNKNOWN, "LEXER_INDEX_ERROR", std::monostate{}, line);
         return;
    }
    // The lexeme is always the raw text scanned from the source
    std::string text = source.substr(start, current - start);
    tokens.emplace_back(type, std::move(text), literal, line);
}


bool Lexer::match(char expected) {
    if (isAtEnd()) return false;
    if (source[current] != expected) return false;
    current++;
    return true;
}

char Lexer::peek() const {
    if (isAtEnd()) return '\0';
    return source[current];
}

char Lexer::peekNext() const {
    if (current + 1 >= source.length()) return '\0';
    return source[current + 1];
}

// Handles regular string literals "..." (including escapes)
void Lexer::scanString() {
    std::stringstream value_builder;
    while (peek() != '"' && !isAtEnd()) {
        char c = peek();
        if (c == '\\') {
            advance(); // Consume '\'
            if (isAtEnd()) { /* Error handled below */ break; }
            char escaped_char = advance();
            switch (escaped_char) {
                case '"':  value_builder << '"'; break;
                case '\\': value_builder << '\\'; break;
                // Add other escapes like \n, \t if desired
                default: value_builder << '\\' << escaped_char; break; // Keep invalid escapes literal
            }
        } else if (c == '\n') {
            line++; // Allow multi-line strings
            advance();
            value_builder << '\n'; // Add newline to value for regular strings
        } else {
            value_builder << c;
            advance();
        }
    }

    if (isAtEnd()) {
        std::cerr << "Lexer Error (Line " << line << "): Unterminated string." << std::endl;
        debug_log << "Lexer Error (Line " << line << "): Unterminated string." << std::endl;
        addToken(TokenType::UNKNOWN);
        return;
    }
    advance(); // Consume the closing ".
    addToken(TokenType::STRING, value_builder.str());
}

// Scans an interpolated string literal `...{expr}...`
void Lexer::scanInterpolatedString() {
    InterpolatedStringData parts;
    std::stringstream current_literal_part;
    int string_start_line = line; // Store line where the string starts

    while (peek() != '`' && !isAtEnd()) {
        char c = peek();

        if (c == '{') {
            // Check for escaped '{{'
            if (peekNext() == '{') {
                advance(); // Consume first {
                advance(); // Consume second {
                current_literal_part << '{'; // Add one literal brace
            } else {
                // Start of an expression part
                advance(); // Consume '{'

                // Add the preceding literal part (if any)
                if (current_literal_part.tellp() > 0) {
                    parts.emplace_back(InterpolationPartType::LITERAL, current_literal_part.str());
                    current_literal_part.str(""); // Reset for the next literal part
                    current_literal_part.clear(); // Clear state flags
                }

                // Scan the expression part (until matching '}')
                std::stringstream expression_part;
                int brace_nesting = 1; // Start at 1 for the opening brace we consumed
                int expr_start_line = line; // Track line where expression starts

                while (brace_nesting > 0 && !isAtEnd()) {
                    char expr_c = peek();
                    if (expr_c == '{') {
                        brace_nesting++;
                        expression_part << expr_c;
                        advance();
                    } else if (expr_c == '}') {
                        brace_nesting--;
                        if (brace_nesting == 0) {
                            advance(); // Consume the final '}'
                            break;     // Exit expression scanning
                        }
                        expression_part << expr_c;
                        advance();
                    } else if (expr_c == '\n') {
                        line++;
                        expression_part << expr_c; // Keep newlines within expression string
                        advance();
                    } else {
                        expression_part << expr_c;
                        advance();
                    }
                }

                if (brace_nesting > 0) {
                    // Unterminated expression within the string
                    std::cerr << "Lexer Error (Line " << expr_start_line << "): Unterminated expression inside interpolated string (missing '}')" << std::endl;
                    debug_log << "Lexer Error (Line " << expr_start_line << "): Unterminated expression inside interpolated string (missing '}')" << std::endl;
                    addToken(TokenType::UNKNOWN);
                    return;
                }

                // Add the expression part (as a raw string for now)
                parts.emplace_back(InterpolationPartType::EXPRESSION, expression_part.str());
            }
        } else if (c == '}') {
            // Check for escaped '}}'
            if (peekNext() == '}') {
                advance(); // Consume first }
                advance(); // Consume second }
                current_literal_part << '}'; // Add one literal brace
            } else {
                // Unescaped '}' outside an expression - treat as literal
                current_literal_part << c;
                advance();
            }
        } else if (c == '\n') {
            line++; // Allow multi-line interpolated strings
            current_literal_part << c; // Keep newline in literal part
            advance();
        } else {
            // Regular character, add to current literal part
            current_literal_part << c;
            advance();
        }
    }

    if (isAtEnd()) {
        std::cerr << "Lexer Error (Line " << string_start_line << "): Unterminated interpolated string." << std::endl;
        debug_log << "Lexer Error (Line " << string_start_line << "): Unterminated interpolated string." << std::endl;
        addToken(TokenType::UNKNOWN);
        return;
    }

    // Add any remaining literal part
    if (current_literal_part.tellp() > 0) {
        parts.emplace_back(InterpolationPartType::LITERAL, current_literal_part.str());
    }

    advance(); // Consume the closing '`'

    // Add the INTERPOLATED_STRING token with the structured data
    addToken(TokenType::INTERPOLATED_STRING, parts);
}

void Lexer::scanNumber() {
    bool is_float = false;
    while (isdigit(peek())) advance();
    if (peek() == '.' && isdigit(peekNext())) {
        is_float = true;
        advance(); // Consume the '.'
        while (isdigit(peek())) advance();
    }
    std::string num_str = source.substr(start, current - start);
    try {
        if (is_float) {
            addToken(TokenType::NUMBER_FLOAT, std::stod(num_str));
        } else {
            addToken(TokenType::NUMBER_INT, std::stoi(num_str));
        }
    } catch (const std::invalid_argument& ia) {
        std::cerr << "Lexer Error (Line " << line << "): Invalid number format: " << num_str << ". " << ia.what() << std::endl;
        debug_log << "Lexer Error (Line " << line << "): Invalid number format: " << num_str << ". " << ia.what() << std::endl;
        addToken(TokenType::UNKNOWN);
    } catch (const std::out_of_range& oor) {
        std::cerr << "Lexer Error (Line " << line << "): Number literal out of range: " << num_str << ". " << oor.what() << std::endl;
        debug_log << "Lexer Error (Line " << line << "): Number literal out of range: " << num_str << ". " << oor.what() << std::endl;
        addToken(TokenType::UNKNOWN);
    }
}

// Helper functions for identifier scanning
bool isAlpha(char c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'; }
bool isAlphaNumeric(char c) { return isAlpha(c) || isdigit(c); }

void Lexer::scanIdentifier() {
    while (isAlphaNumeric(peek())) advance();
    std::string text = source.substr(start, current - start);
    auto it = keywords.find(text);
    addToken(it == keywords.end() ? TokenType::IDENTIFIER : it->second);
}

void Lexer::scanBlockComment() {
    int nesting = 1;
    int comment_start_line = line; // Store line where comment started for error reporting
    while (nesting > 0 && !isAtEnd()) {
        if (peek() == '/' && peekNext() == '*') {
            advance(); advance();
            nesting++;
        } else if (peek() == '*' && peekNext() == '/') {
            advance(); advance();
            nesting--;
        } else if (peek() == '\n') {
            line++;
            advance();
        } else {
            advance();
        }
    }
    if (isAtEnd() && nesting > 0) {
        std::cerr << "Lexer Error (Line " << comment_start_line << "): Unterminated block comment." << std::endl;
        debug_log << "Lexer Error (Line " << comment_start_line << "): Unterminated block comment." << std::endl;
        // No token is added for comments
    }
}


// Scans the next token from the source string.
void Lexer::scanToken() {
    char c = advance(); // Consume the next character

    if (c == '\0' && isAtEnd()) return; // Should not happen if called correctly, but safe check

    switch (c) {
        // Single/double char tokens
        case '(': addToken(TokenType::LEFT_PAREN); break;
        case ')': addToken(TokenType::RIGHT_PAREN); break;
        case '{': addToken(TokenType::LEFT_BRACE); break;
        case '}': addToken(TokenType::RIGHT_BRACE); break;
        case '[': addToken(TokenType::LEFT_BRACKET); break;
        case ']': addToken(TokenType::RIGHT_BRACKET); break;
        case ',': addToken(TokenType::COMMA); break;
        case '.': addToken(TokenType::DOT); break;
        case '-': addToken(TokenType::MINUS); break;
        case '+': addToken(TokenType::PLUS); break;
        case ';': addToken(TokenType::SEMICOLON); break;
        case '*': addToken(TokenType::STAR); break;
        case '%': addToken(TokenType::PERCENT); break;
        case ':': addToken(TokenType::COLON); break;
        case '|': addToken(TokenType::PIPE); break;
        case '!': addToken(match('=') ? TokenType::BANG_EQUAL : TokenType::BANG); break;
        case '=': addToken(match('=') ? TokenType::EQUAL_EQUAL : TokenType::EQUAL); break;
        case '<': addToken(match('=') ? TokenType::LESS_EQUAL : TokenType::LESS); break;
        case '>': addToken(match('=') ? TokenType::GREATER_EQUAL : TokenType::GREATER); break;

        // Slash, comments
        case '/':
            if (match('/')) { // Single-line comment
                while (peek() != '\n' && !isAtEnd()) advance();
            } else if (match('*')) { // Block comment
                scanBlockComment();
            } else { // Division operator
                addToken(TokenType::SLASH);
            }
            break;

        // Whitespace
        case ' ':
        case '\r':
        case '\t':
            // Ignore whitespace.
            break;

        case '\n':
            line++;
            break;

        // String literals
        case '"': scanString(); break;
        case '`': scanInterpolatedString(); break;

        default:
            if (isdigit(c)) {
                scanNumber();
            } else if (isAlpha(c)) { // Handles identifiers and keywords
                scanIdentifier();
            } else {
                std::cerr << "Lexer Error (Line " << line << "): Unexpected character '" << c << "'" << std::endl;
                debug_log << "Lexer Error (Line " << line << "): Unexpected character '" << c << "'" << std::endl;
                addToken(TokenType::UNKNOWN); // Add an UNKNOWN token to signify the error point
            }
            break;
    }
}
