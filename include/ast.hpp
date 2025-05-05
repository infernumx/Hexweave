#ifndef AST_HPP
#define AST_HPP

#include "token.hpp" // Includes InterpolatedStringData definition
#include "value.hpp"
#include <vector>
#include <memory> // For unique_ptr
#include <string>
#include <map> // Not strictly needed, but common
#include <optional>
#include <tuple>
#include <utility> // For std::move

namespace AST {

class Visitor; // Forward declaration for the Visitor pattern

// --- Base Node ---
class Node {
public:
    virtual ~Node() = default;
    virtual Value accept(Visitor& visitor) = 0;
    int line = 0;
};

// --- Expressions ---
class Expression : public Node {};

class LiteralExpr : public Expression { /* ... as before ... */
public:
    Value value;
    LiteralExpr(Value val, int line_num) : value(std::move(val)) { this->line = line_num; }
    virtual Value accept(Visitor& visitor) override;
};
class InterpolatedStringExpr : public Expression { /* ... as before ... */
public:
    InterpolatedStringData parts;
    Token backtick_token;
    InterpolatedStringExpr(Token token, InterpolatedStringData data)
        : parts(std::move(data)), backtick_token(std::move(token)) { this->line = backtick_token.line; }
    virtual Value accept(Visitor& visitor) override;
};
class VariableExpr : public Expression { /* ... as before ... */
public:
    Token name;
    VariableExpr(Token name_token) : name(std::move(name_token)) { this->line = name.line; }
    virtual Value accept(Visitor& visitor) override;
};
class AssignExpr : public Expression { /* ... as before ... */
public:
    std::unique_ptr<Expression> target;
    Token equals_token;
    std::unique_ptr<Expression> value;
    AssignExpr(std::unique_ptr<Expression> target_expr, Token eq_token, std::unique_ptr<Expression> val_expr)
        : target(std::move(target_expr)), equals_token(std::move(eq_token)), value(std::move(val_expr)) { this->line = equals_token.line; }
    virtual Value accept(Visitor& visitor) override;
};
class BinaryExpr : public Expression { /* ... as before ... */
public:
    std::unique_ptr<Expression> left;
    Token op;
    std::unique_ptr<Expression> right;
    BinaryExpr(std::unique_ptr<Expression> left_expr, Token oper, std::unique_ptr<Expression> right_expr)
        : left(std::move(left_expr)), op(std::move(oper)), right(std::move(right_expr)) { this->line = op.line; }
    virtual Value accept(Visitor& visitor) override;
};
class UnaryExpr : public Expression { /* ... as before ... */
public:
    Token op;
    std::unique_ptr<Expression> right;
    UnaryExpr(Token oper, std::unique_ptr<Expression> right_expr)
        : op(std::move(oper)), right(std::move(right_expr)) { this->line = op.line; }
    virtual Value accept(Visitor& visitor) override;
};
class LogicalExpr : public Expression { /* ... as before ... */
public:
    std::unique_ptr<Expression> left;
    Token op;
    std::unique_ptr<Expression> right;
    LogicalExpr(std::unique_ptr<Expression> left_expr, Token oper, std::unique_ptr<Expression> right_expr)
        : left(std::move(left_expr)), op(std::move(oper)), right(std::move(right_expr)) { this->line = op.line; }
    virtual Value accept(Visitor& visitor) override;
};
class CallExpr : public Expression { /* ... as before ... */
public:
    std::unique_ptr<Expression> callee;
    Token paren;
    std::vector<std::unique_ptr<Expression>> arguments;
    CallExpr(std::unique_ptr<Expression> callee_expr, Token paren_token, std::vector<std::unique_ptr<Expression>> args)
        : callee(std::move(callee_expr)), paren(std::move(paren_token)), arguments(std::move(args)) { this->line = paren.line; }
    virtual Value accept(Visitor& visitor) override;
};
class MapLiteralExpr : public Expression { /* ... as before ... */
public:
    Token opening_brace;
    std::vector<std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>> elements;
    MapLiteralExpr(Token brace, std::vector<std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>> elems)
        : opening_brace(std::move(brace)), elements(std::move(elems)) { this->line = opening_brace.line; }
    virtual Value accept(Visitor& visitor) override;
};
class MapAccessExpr : public Expression { /* ... as before ... */
public:
    std::unique_ptr<Expression> map_expr;
    Token bracket; // '[' token
    std::unique_ptr<Expression> key_expr;
    MapAccessExpr(std::unique_ptr<Expression> map, Token brack, std::unique_ptr<Expression> key)
        : map_expr(std::move(map)), bracket(std::move(brack)), key_expr(std::move(key)) { this->line = bracket.line; }
    virtual Value accept(Visitor& visitor) override;
};
class ListLiteralExpr : public Expression { /* ... as before ... */
public:
    Token opening_bracket; // '[' token
    std::vector<std::unique_ptr<Expression>> elements;
    ListLiteralExpr(Token bracket, std::vector<std::unique_ptr<Expression>> elems)
        : opening_bracket(std::move(bracket)), elements(std::move(elems)) { this->line = opening_bracket.line; }
    virtual Value accept(Visitor& visitor) override;
};
class IndexExpr : public Expression { /* ... as before ... */
public:
    std::unique_ptr<Expression> collection; // The list/map being indexed
    Token bracket; // '[' token
    std::unique_ptr<Expression> index; // The index/key expression
    IndexExpr(std::unique_ptr<Expression> coll, Token brack, std::unique_ptr<Expression> idx)
        : collection(std::move(coll)), bracket(std::move(brack)), index(std::move(idx)) { this->line = bracket.line; }
    virtual Value accept(Visitor& visitor) override;
};


// --- Statements ---
class Statement : public Node {};

class BlockStmt : public Statement { /* ... as before ... */
public:
    std::vector<std::unique_ptr<Statement>> statements;
    BlockStmt(std::vector<std::unique_ptr<Statement>> stmts) : statements(std::move(stmts)) { if (!statements.empty() && statements.front()) this->line = statements.front()->line; }
    virtual Value accept(Visitor& visitor) override;
};
class ExpressionStmt : public Statement { /* ... as before ... */
public:
    std::unique_ptr<Expression> expression;
    ExpressionStmt(std::unique_ptr<Expression> expr) : expression(std::move(expr)) { if(expression) this->line = expression->line; }
    virtual Value accept(Visitor& visitor) override;
};
class PrintStmt : public Statement { /* ... as before ... */
public:
    Token keyword;
    std::vector<std::unique_ptr<Expression>> expressions;
    PrintStmt(Token kw, std::vector<std::unique_ptr<Expression>> exprs) : keyword(std::move(kw)), expressions(std::move(exprs)) { this->line = keyword.line; }
    virtual Value accept(Visitor& visitor) override;
};

class VarDeclStmt : public Statement {
public:
    Token name;
    std::optional<Token> type_specifier_token;
    std::optional<Token> map_key_type_token;
    std::optional<Token> map_value_type_token;
    std::optional<Token> list_element_type_token;
    std::unique_ptr<Expression> initializer; // Initializer is now always last

    // <<< FIX: Keep ONLY the comprehensive constructor >>>
    VarDeclStmt(Token name_token, std::optional<Token> type_tok,
                 std::optional<Token> map_key = std::nullopt,
                 std::optional<Token> map_val = std::nullopt,
                 std::optional<Token> list_elem = std::nullopt,
                 std::unique_ptr<Expression> init_expr = nullptr) // Initializer is last argument
        : name(std::move(name_token)),
          type_specifier_token(std::move(type_tok)),
          map_key_type_token(std::move(map_key)),
          map_value_type_token(std::move(map_val)),
          list_element_type_token(std::move(list_elem)),
          initializer(std::move(init_expr)) // Assign initializer here
          { this->line = name.line; }

     // <<< REMOVED redundant constructors >>>
     // VarDeclStmt(Token name_token, Token map_kw_tok, Token key_type, Token val_type, std::unique_ptr<Expression> init_expr) ...
     // VarDeclStmt(Token name_token, Token list_kw_tok, std::optional<Token> element_type, std::unique_ptr<Expression> init_expr) ...

    virtual Value accept(Visitor& visitor) override;
};

class IfStmt : public Statement { /* ... as before ... */
public:
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> then_branch;
    std::unique_ptr<Statement> else_branch;
    IfStmt(std::unique_ptr<Expression> cond, std::unique_ptr<Statement> then_b, std::unique_ptr<Statement> else_b)
        : condition(std::move(cond)), then_branch(std::move(then_b)), else_branch(std::move(else_b)) { if(condition) this->line = condition->line; }
    virtual Value accept(Visitor& visitor) override;
};
class WhileStmt : public Statement { /* ... as before ... */
public:
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> body;
    WhileStmt(std::unique_ptr<Expression> cond, std::unique_ptr<Statement> b)
        : condition(std::move(cond)), body(std::move(b)) { if(condition) this->line = condition->line; }
    virtual Value accept(Visitor& visitor) override;
};
class ForStmt : public Statement { /* ... as before ... */
public:
    Token variable_name;
    Token variable_type;
    std::unique_ptr<Expression> range_start;
    std::unique_ptr<Expression> range_end;
    std::unique_ptr<Statement> body;
    Token keyword;
     ForStmt(Token kw, Token var_type, Token var_name, std::unique_ptr<Expression> start, std::unique_ptr<Expression> end, std::unique_ptr<Statement> b)
        : variable_name(std::move(var_name)), variable_type(std::move(var_type)), range_start(std::move(start)), range_end(std::move(end)), body(std::move(b)), keyword(std::move(kw)) { this->line = keyword.line; }
    virtual Value accept(Visitor& visitor) override;
};


class FunctionStmt : public Statement {
public:
    Token name;
    std::vector<std::pair<Token, Token>> params; // Pair: <type_token, name_token>
    Token return_type;
    std::optional<Token> map_return_key_type_token;
    std::optional<Token> map_return_value_type_token;
    std::optional<Token> list_return_element_type_token;
    std::unique_ptr<BlockStmt> body; // Body is now always last

     // <<< FIX: Keep ONLY the comprehensive constructor >>>
     FunctionStmt(Token name_token, std::vector<std::pair<Token, Token>> parameters,
                  Token ret_type,
                  std::optional<Token> map_key = std::nullopt,
                  std::optional<Token> map_val = std::nullopt,
                  std::optional<Token> list_elem = std::nullopt,
                  std::unique_ptr<BlockStmt> func_body = nullptr) // Body is last argument
        : name(std::move(name_token)),
          params(std::move(parameters)),
          return_type(std::move(ret_type)),
          map_return_key_type_token(std::move(map_key)),
          map_return_value_type_token(std::move(map_val)),
          list_return_element_type_token(std::move(list_elem)),
          body(std::move(func_body)) // Assign body here
          { this->line = name.line; }

    // <<< REMOVED redundant constructors >>>
    // FunctionStmt(Token name_token, std::vector<std::pair<Token, Token>> parameters, Token map_kw_tok, Token key_type, Token val_type, std::unique_ptr<BlockStmt> func_body) ...
    // FunctionStmt(Token name_token, std::vector<std::pair<Token, Token>> parameters, Token list_kw_tok, std::optional<Token> element_type, std::unique_ptr<BlockStmt> func_body) ...

    virtual Value accept(Visitor& visitor) override;
};

class ReturnStmt : public Statement { /* ... as before ... */
public:
    Token keyword;
    std::unique_ptr<Expression> value;
    ReturnStmt(Token kw, std::unique_ptr<Expression> val_expr) : keyword(std::move(kw)), value(std::move(val_expr)) { this->line = keyword.line; }
    virtual Value accept(Visitor& visitor) override;
};
class DeleteStmt : public Statement { /* ... as before ... */
public:
    Token keyword;
    std::unique_ptr<Expression> expression;
    DeleteStmt(Token kw, std::unique_ptr<Expression> expr) : keyword(std::move(kw)), expression(std::move(expr)) { this->line = keyword.line; }
    virtual Value accept(Visitor& visitor) override;
};
class BreakStmt : public Statement { /* ... as before ... */
public:
    Token keyword;
    BreakStmt(Token kw) : keyword(std::move(kw)) { this->line = keyword.line; }
    virtual Value accept(Visitor& visitor) override;
};
class ContinueStmt : public Statement { /* ... as before ... */
public:
    Token keyword;
    ContinueStmt(Token kw) : keyword(std::move(kw)) { this->line = keyword.line; }
    virtual Value accept(Visitor& visitor) override;
};


// --- Visitor Interface ---
class Visitor {
public:
    virtual ~Visitor() = default;

    // Expressions
    virtual Value visitLiteralExpr(LiteralExpr& expr) = 0;
    virtual Value visitInterpolatedStringExpr(InterpolatedStringExpr& expr) = 0;
    virtual Value visitVariableExpr(VariableExpr& expr) = 0;
    virtual Value visitAssignExpr(AssignExpr& expr) = 0;
    virtual Value visitBinaryExpr(BinaryExpr& expr) = 0;
    virtual Value visitUnaryExpr(UnaryExpr& expr) = 0;
    virtual Value visitLogicalExpr(LogicalExpr& expr) = 0;
    virtual Value visitCallExpr(CallExpr& expr) = 0;
    virtual Value visitMapLiteralExpr(MapLiteralExpr& expr) = 0;
    virtual Value visitMapAccessExpr(MapAccessExpr& expr) = 0;
    virtual Value visitListLiteralExpr(ListLiteralExpr& expr) = 0;
    virtual Value visitIndexExpr(IndexExpr& expr) = 0;

    // Statements
    virtual Value visitBlockStmt(BlockStmt& stmt) = 0;
    virtual Value visitExpressionStmt(ExpressionStmt& stmt) = 0;
    virtual Value visitPrintStmt(PrintStmt& stmt) = 0;
    virtual Value visitVarDeclStmt(VarDeclStmt& stmt) = 0;
    virtual Value visitIfStmt(IfStmt& stmt) = 0;
    virtual Value visitWhileStmt(WhileStmt& stmt) = 0;
    virtual Value visitForStmt(ForStmt& stmt) = 0;
    virtual Value visitFunctionStmt(FunctionStmt& stmt) = 0;
    virtual Value visitReturnStmt(ReturnStmt& stmt) = 0;
    virtual Value visitDeleteStmt(DeleteStmt& stmt) = 0;
    virtual Value visitBreakStmt(BreakStmt& stmt) = 0;
    virtual Value visitContinueStmt(ContinueStmt& stmt) = 0;
};

} // namespace AST

#endif // AST_HPP
