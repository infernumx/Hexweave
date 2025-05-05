#ifndef PARSER_HPP
#define PARSER_HPP

#include "token.hpp" // Includes InterpolatedStringData definition
#include "ast.hpp"
#include "common.hpp" // For SyntaxError
#include <vector>
#include <memory> // For unique_ptr
#include <functional> // For std::function in helpers
#include <optional> // For optional tokens
#include <tuple> // For tuple return

// Parses a stream of tokens into an Abstract Syntax Tree (AST)
class Parser {
public:
    Parser(const std::vector<Token>& tokens);

    std::vector<std::unique_ptr<AST::Statement>> parse();

    // Method needed by Interpreter for interpolated strings
    std::unique_ptr<AST::Expression> expression();

    // Method needed by Interpreter for interpolated strings
    bool isAtEnd() const;

private:
    const std::vector<Token>& tokens;
    int current = 0;

    // --- Error Handling ---
    SyntaxError error(const Token& token, const std::string& message);
    void synchronize();

    // --- Helper Methods ---
    bool match(const std::vector<TokenType>& types);
    bool check(TokenType type) const;
    Token advance();
    // bool isAtEnd() const; // Definition is in .cpp, declaration is public
    Token peek() const;
    Token previous() const;
    Token consume(TokenType type, const std::string& message);


    // --- Grammar Rule Methods (Recursive Descent) ---
    std::unique_ptr<AST::Statement> declaration();
    std::unique_ptr<AST::Statement> varDeclaration();
    std::unique_ptr<AST::Statement> funDeclaration();
    std::unique_ptr<AST::FunctionStmt> functionBody(const std::string& kind);
    std::vector<std::pair<Token, Token>> parameters();
    std::unique_ptr<AST::Statement> statement();
    std::unique_ptr<AST::Statement> importStatement(); // <<< ADDED
    std::unique_ptr<AST::Statement> expressionStatement();
    std::unique_ptr<AST::Statement> printStatement();
    std::unique_ptr<AST::Statement> block();
    std::unique_ptr<AST::Statement> ifStatement();
    std::unique_ptr<AST::Statement> whileStatement();
    std::unique_ptr<AST::Statement> forStatement();
    std::unique_ptr<AST::Statement> returnStatement();
    std::unique_ptr<AST::Statement> deleteStatement();
    std::unique_ptr<AST::Statement> breakStatement();
    std::unique_ptr<AST::Statement> continueStatement();
    // std::unique_ptr<AST::Expression> expression(); // Declaration is public
    std::unique_ptr<AST::Expression> assignment();
    std::unique_ptr<AST::Expression> logicOr();
    std::unique_ptr<AST::Expression> logicAnd();
    std::unique_ptr<AST::Expression> equality();
    std::unique_ptr<AST::Expression> comparison();
    std::unique_ptr<AST::Expression> term();
    std::unique_ptr<AST::Expression> factor();
    std::unique_ptr<AST::Expression> unary();
    std::unique_ptr<AST::Expression> pipe();
    std::unique_ptr<AST::Expression> call();
    std::unique_ptr<AST::Expression> finishCall(std::unique_ptr<AST::Expression> callee);
    std::unique_ptr<AST::Expression> finishIndexAccess(std::unique_ptr<AST::Expression> collection);

    // primary -> NUMBER | STRING | INTERPOLATED_STRING | "true" | "false" | "nil" | IDENTIFIER
    //          | "(" expression ")" | mapLiteral | listLiteral ;
    std::unique_ptr<AST::Expression> primary();

    std::unique_ptr<AST::Expression> mapLiteral();
    std::unique_ptr<AST::Expression> listLiteral();

    Token parsePrimitiveTypeSpecifier();
    std::tuple<Token, std::optional<Token>, std::optional<Token>, std::optional<Token>> parseType();

    using OperandParserMemFn = std::unique_ptr<AST::Expression> (Parser::*)();
    template<typename OperatorFunc>
    static std::unique_ptr<AST::Expression> parseLeftAssocBinary(Parser* parser, OperatorFunc matchOperators, OperandParserMemFn parseOperand);
    template<typename OperatorFunc>
    static std::unique_ptr<AST::Expression> parseLeftAssocLogical(Parser* parser, OperatorFunc matchOperators, OperandParserMemFn parseOperand);

};

#endif // PARSER_HPP
