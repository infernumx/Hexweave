#ifndef LEXER_HPP
#define LEXER_HPP

#include "token.hpp" // Includes InterpolatedStringData definition
#include <string>
#include <vector>
#include <unordered_map>

class Lexer {
public:
    // Constructor: Takes the source code string
    Lexer(std::string source);

    // Scans the source code and returns a vector of tokens
    std::vector<Token> scanTokens();

private:
    const std::string source; // Source code string
    std::vector<Token> tokens; // List of tokens generated
    int start = 0;   // Start index of the current lexeme being scanned
    int current = 0; // Current index in the source string
    int line = 1;    // Current line number

    // Helper map for keywords
    static const std::unordered_map<std::string, TokenType> keywords;

    // Checks if the end of the source code has been reached
    bool isAtEnd() const;

    // Consumes the next character and advances the current pointer
    char advance();

    // Adds a token to the tokens list
    void addToken(TokenType type);

    // Adds a token with a specific literal value (updated variant type)
    void addToken(TokenType type, const std::variant<std::monostate, std::string, int, double, bool, InterpolatedStringData>& literal);

    // Checks if the current character matches the expected one
    bool match(char expected);

    // Looks at the current character without consuming it
    char peek() const;

    // Looks at the next character without consuming it
    char peekNext() const;

    // Handles scanning a regular string literal "..."
    void scanString();

    // <<< ADDED: Declaration for interpolated string scanning >>>
    // Handles scanning an interpolated string literal `...{expr}...`
    void scanInterpolatedString();
    // <<< END ADDED >>>

    // Handles scanning a number literal (int or float)
    void scanNumber();

    // Handles scanning an identifier or keyword
    void scanIdentifier();

    // Main scanning function for a single token
    void scanToken();

    // Helper for multi-line comments /* ... */
    void scanBlockComment();
};

#endif // LEXER_HPP
