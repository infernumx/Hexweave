#ifndef INTERPRETER_HPP
#define INTERPRETER_HPP

#include "ast.hpp"
#include "value.hpp"
#include "environment.hpp"
#include "common.hpp"
#include <vector>
#include <memory>
#include <stdexcept> // For ReturnValue base class
#include <optional> // For optional tokens
#include <string> // For filename
#include <utility> // For std::move


// (Constructors and members as before)
class ReturnValue : public std::runtime_error {
public:
    Value value;
    int line;
    ReturnValue(Value val, int line_num = -1)
        : std::runtime_error("Return statement executed"), value(std::move(val)), line(line_num) {}
    ReturnValue(const ReturnValue&) = default;
    ReturnValue& operator=(const ReturnValue&) = default;
    ReturnValue(ReturnValue&&) = default;
    ReturnValue& operator=(ReturnValue&&) = default;
    virtual ~ReturnValue() noexcept override = default;
};
class BreakSignal : public std::runtime_error {
public:
    int line;
    BreakSignal(int line_num = -1) : std::runtime_error("Break statement executed"), line(line_num) {}
};
class ContinueSignal : public std::runtime_error {
public:
     int line;
    ContinueSignal(int line_num = -1) : std::runtime_error("Continue statement executed"), line(line_num) {}
};


// The Interpreter class walks the AST and executes the code.
class Interpreter : public AST::Visitor {
public:
    Interpreter();

    // Store source context for evaluating interpolated expressions
    void setSourceContext(const std::string& filename, const std::vector<std::string>& source_lines);

    void interpret(const std::vector<std::unique_ptr<AST::Statement>>& statements,
                   const std::string& filename,
                   const std::vector<std::string>& source_lines);

    void executeBlock(const std::vector<std::unique_ptr<AST::Statement>>& statements,
                      std::shared_ptr<Environment> block_environment);

    // --- Visitor Methods Implementation (Overrides from AST::Visitor) ---
    // Expressions
    Value visitLiteralExpr(AST::LiteralExpr& expr) override;
    Value visitInterpolatedStringExpr(AST::InterpolatedStringExpr& expr) override;
    Value visitVariableExpr(AST::VariableExpr& expr) override;
    Value visitAssignExpr(AST::AssignExpr& expr) override;
    Value visitBinaryExpr(AST::BinaryExpr& expr) override;
    Value visitUnaryExpr(AST::UnaryExpr& expr) override;
    Value visitLogicalExpr(AST::LogicalExpr& expr) override;
    Value visitCallExpr(AST::CallExpr& expr) override;
    Value visitMapLiteralExpr(AST::MapLiteralExpr& expr) override;
    Value visitMapAccessExpr(AST::MapAccessExpr& expr) override;
    Value visitListLiteralExpr(AST::ListLiteralExpr& expr) override;
    Value visitIndexExpr(AST::IndexExpr& expr) override;

    // Statements
    Value visitBlockStmt(AST::BlockStmt& stmt) override;
    Value visitExpressionStmt(AST::ExpressionStmt& stmt) override;
    Value visitPrintStmt(AST::PrintStmt& stmt) override;
    Value visitVarDeclStmt(AST::VarDeclStmt& stmt) override;
    Value visitIfStmt(AST::IfStmt& stmt) override;
    Value visitWhileStmt(AST::WhileStmt& stmt) override;
    Value visitForStmt(AST::ForStmt& stmt) override;
    Value visitFunctionStmt(AST::FunctionStmt& stmt) override;
    Value visitReturnStmt(AST::ReturnStmt& stmt) override;
    Value visitDeleteStmt(AST::DeleteStmt& stmt) override;
    Value visitBreakStmt(AST::BreakStmt& stmt) override;
    Value visitContinueStmt(AST::ContinueStmt& stmt) override;

    std::shared_ptr<Environment> getGlobalEnvironment() const;

    // Helper for type checking
    void checkType(const Value& value, const Token& type_specifier_token,
                   const std::optional<Token>& map_key_type,
                   const std::optional<Token>& map_value_type,
                   const std::optional<Token>& list_element_type,
                   const std::string& context, int line_num);
    void checkType(const Value& value, const Token& type_specifier_token,
                    const std::string& context, int line_num);

    ValueType getValueTypeFromToken(const Token& type_token);

    // <<< FIXED: Added optional context_msg parameter to match definition >>>
    void reportError(const LangError& error,
                     const std::string& filename,
                     const std::vector<std::string>& source_lines,
                     const std::string& context_msg = ""); // Added default value

private:
    std::shared_ptr<Environment> globals;
    std::shared_ptr<Environment> environment;
    bool had_error_ = false;

    // Store source context for error reporting within interpolated expressions
    std::string current_filename_ = "<unknown>";
    std::vector<std::string> current_source_lines_;

    Value evaluate(AST::Expression& expr);
    void execute(AST::Statement& stmt);
    bool isTruthy(const Value& value);
    void checkNumberOperand(const Token& op, const Value& operand);
    void checkNumberOperands(const Token& op, const Value& left, const Value& right);
    void checkStringOperand(const Token& op, const Value& operand);

    std::string getSourceLineInternal(const std::vector<std::string>& lines, int line_num);

};

#endif // INTERPRETER_HPP
