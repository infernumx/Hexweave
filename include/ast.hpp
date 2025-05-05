#ifndef AST_HPP
#define AST_HPP

#include "token.hpp" // Includes InterpolationPartType and InterpolatedStringData
#include "value.hpp" // Needed for LiteralExpr value
#include <vector>
#include <memory> // For unique_ptr
#include <optional> // For optional tokens in VarDeclStmt, FunctionStmt

// --- Forward Declarations ---
namespace AST {
    struct Expression;
    struct LiteralExpr;
    struct InterpolatedStringExpr; // <<< ADDED
    struct VariableExpr;
    struct AssignExpr;
    struct BinaryExpr;
    struct UnaryExpr;
    struct LogicalExpr;
    struct CallExpr;
    struct MapLiteralExpr;
    struct MapAccessExpr;
    struct ListLiteralExpr;
    struct IndexExpr;

    struct Statement;
    struct BlockStmt;
    struct ExpressionStmt;
    struct PrintStmt;
    struct VarDeclStmt;
    struct IfStmt;
    struct WhileStmt;
    struct ForStmt;
    struct FunctionStmt;
    struct ReturnStmt;
    struct DeleteStmt;
    struct BreakStmt;
    struct ContinueStmt;
    struct ImportStmt; // <<< ADDED ImportStmt

    // Visitor Interface (forward declaration)
    class Visitor;
} // namespace AST


// --- Visitor Pattern Interface ---
namespace AST {
    class Visitor {
    public:
        virtual ~Visitor() = default;

        // Expressions
        virtual Value visitLiteralExpr(LiteralExpr& expr) = 0;
        virtual Value visitInterpolatedStringExpr(InterpolatedStringExpr& expr) = 0; // <<< ADDED
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
        virtual Value visitImportStmt(ImportStmt& stmt) = 0; // <<< ADDED ImportStmt visit method
    };
} // namespace AST


// --- Base AST Node Classes ---
namespace AST {
    struct Node {
        int line = -1; // Default line number
        virtual ~Node() = default;
        virtual Value accept(Visitor& visitor) = 0;
    };

    struct Expression : public Node {};
    struct Statement : public Node {};

} // namespace AST


// --- Expression Node Definitions ---
namespace AST {

    struct LiteralExpr : public Expression {
        Value value;
        LiteralExpr(Value val, int line_num) : value(std::move(val)) { line = line_num; }
        Value accept(Visitor& visitor) override { return visitor.visitLiteralExpr(*this); }
    };

    // <<< ADDED InterpolatedStringExpr Definition >>>
    struct InterpolatedStringExpr : public Expression {
        Token token; // The original INTERPOLATED_STRING token
        InterpolatedStringData parts; // The vector of literal/expression string parts from the lexer

        InterpolatedStringExpr(Token tok, InterpolatedStringData pts)
            : token(std::move(tok)), parts(std::move(pts)) { line = token.line; }

        Value accept(Visitor& visitor) override { return visitor.visitInterpolatedStringExpr(*this); }
    };
    // <<< END ADDED >>>


    struct VariableExpr : public Expression {
        Token name;
        VariableExpr(Token n) : name(std::move(n)) { line = name.line; }
        Value accept(Visitor& visitor) override { return visitor.visitVariableExpr(*this); }
    };

    struct AssignExpr : public Expression {
        std::unique_ptr<Expression> target; // Can be VariableExpr, IndexExpr, etc.
        Token equals_token; // The '=' token for line number info
        std::unique_ptr<Expression> value;
        AssignExpr(std::unique_ptr<Expression> t, Token eq, std::unique_ptr<Expression> v)
            : target(std::move(t)), equals_token(std::move(eq)), value(std::move(v)) { line = equals_token.line; }
        Value accept(Visitor& visitor) override { return visitor.visitAssignExpr(*this); }
    };

    struct BinaryExpr : public Expression {
        std::unique_ptr<Expression> left;
        Token op;
        std::unique_ptr<Expression> right;
        BinaryExpr(std::unique_ptr<Expression> l, Token o, std::unique_ptr<Expression> r)
            : left(std::move(l)), op(std::move(o)), right(std::move(r)) { line = op.line; }
        Value accept(Visitor& visitor) override { return visitor.visitBinaryExpr(*this); }
    };

    struct UnaryExpr : public Expression {
        Token op;
        std::unique_ptr<Expression> right;
        UnaryExpr(Token o, std::unique_ptr<Expression> r)
            : op(std::move(o)), right(std::move(r)) { line = op.line; }
        Value accept(Visitor& visitor) override { return visitor.visitUnaryExpr(*this); }
    };

    struct LogicalExpr : public Expression {
        std::unique_ptr<Expression> left;
        Token op; // AND or OR
        std::unique_ptr<Expression> right;
        LogicalExpr(std::unique_ptr<Expression> l, Token o, std::unique_ptr<Expression> r)
            : left(std::move(l)), op(std::move(o)), right(std::move(r)) { line = op.line; }
        Value accept(Visitor& visitor) override { return visitor.visitLogicalExpr(*this); }
    };

    struct CallExpr : public Expression {
        std::unique_ptr<Expression> callee;
        Token paren; // The '(' token for line number info
        std::vector<std::unique_ptr<Expression>> arguments;
        CallExpr(std::unique_ptr<Expression> c, Token p, std::vector<std::unique_ptr<Expression>> args)
            : callee(std::move(c)), paren(std::move(p)), arguments(std::move(args)) { line = paren.line; }
        Value accept(Visitor& visitor) override { return visitor.visitCallExpr(*this); }
    };

    struct MapLiteralExpr : public Expression {
        Token opening_brace;
        std::vector<std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>> elements;
        MapLiteralExpr(Token brace, std::vector<std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>> elems)
            : opening_brace(std::move(brace)), elements(std::move(elems)) { line = opening_brace.line; }
        Value accept(Visitor& visitor) override { return visitor.visitMapLiteralExpr(*this); }
    };

    // Note: MapAccessExpr might be deprecated in favor of IndexExpr for maps
    struct MapAccessExpr : public Expression {
        std::unique_ptr<Expression> map_expr;
        Token bracket; // '[' token
        std::unique_ptr<Expression> key_expr;
        MapAccessExpr(std::unique_ptr<Expression> map, Token b, std::unique_ptr<Expression> key)
            : map_expr(std::move(map)), bracket(std::move(b)), key_expr(std::move(key)) { line = bracket.line; }
        Value accept(Visitor& visitor) override { return visitor.visitMapAccessExpr(*this); }
    };

    struct ListLiteralExpr : public Expression {
        Token opening_bracket;
        std::vector<std::unique_ptr<Expression>> elements;
        ListLiteralExpr(Token bracket, std::vector<std::unique_ptr<Expression>> elems)
            : opening_bracket(std::move(bracket)), elements(std::move(elems)) { line = opening_bracket.line; }
        Value accept(Visitor& visitor) override { return visitor.visitListLiteralExpr(*this); }
    };

    struct IndexExpr : public Expression {
        std::unique_ptr<Expression> collection; // List or Map expression
        Token bracket; // '[' token
        std::unique_ptr<Expression> index; // Integer for list, any valid key for map
        IndexExpr(std::unique_ptr<Expression> coll, Token b, std::unique_ptr<Expression> idx)
            : collection(std::move(coll)), bracket(std::move(b)), index(std::move(idx)) { line = bracket.line; }
        Value accept(Visitor& visitor) override { return visitor.visitIndexExpr(*this); }
    };

} // namespace AST


// --- Statement Node Definitions ---
namespace AST {

    struct BlockStmt : public Statement {
        std::vector<std::unique_ptr<Statement>> statements;
        BlockStmt(std::vector<std::unique_ptr<Statement>> stmts) : statements(std::move(stmts)) {}
        Value accept(Visitor& visitor) override { return visitor.visitBlockStmt(*this); }
    };

    struct ExpressionStmt : public Statement {
        std::unique_ptr<Expression> expression;
        ExpressionStmt(std::unique_ptr<Expression> expr) : expression(std::move(expr)) { if(expression) line = expression->line; }
        Value accept(Visitor& visitor) override { return visitor.visitExpressionStmt(*this); }
    };

    struct PrintStmt : public Statement {
        Token keyword; // The 'print' token
        std::vector<std::unique_ptr<Expression>> expressions;
        PrintStmt(Token kw, std::vector<std::unique_ptr<Expression>> exprs)
            : keyword(std::move(kw)), expressions(std::move(exprs)) { line = keyword.line; }
        Value accept(Visitor& visitor) override { return visitor.visitPrintStmt(*this); }
    };

    struct VarDeclStmt : public Statement {
        Token name;
        std::optional<Token> type_specifier_token; // e.g., INT, FLOAT, MAP, LIST, FUNCTION, OBJ
        std::optional<Token> map_key_type_token;   // Only if type_specifier_token is MAP
        std::optional<Token> map_value_type_token; // Only if type_specifier_token is MAP
        std::optional<Token> list_element_type_token; // Only if type_specifier_token is LIST
        std::unique_ptr<Expression> initializer; // Can be nullptr if no initializer
        VarDeclStmt(Token n, std::optional<Token> type_tok,
                    std::optional<Token> key_tok, std::optional<Token> val_tok, std::optional<Token> list_elem_tok,
                    std::unique_ptr<Expression> init)
            : name(std::move(n)), type_specifier_token(std::move(type_tok)),
              map_key_type_token(std::move(key_tok)), map_value_type_token(std::move(val_tok)), list_element_type_token(std::move(list_elem_tok)),
              initializer(std::move(init)) { line = name.line; }
        Value accept(Visitor& visitor) override { return visitor.visitVarDeclStmt(*this); }
    };

    struct IfStmt : public Statement {
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> then_branch;
        std::unique_ptr<Statement> else_branch; // Can be nullptr
        IfStmt(std::unique_ptr<Expression> cond, std::unique_ptr<Statement> then_b, std::unique_ptr<Statement> else_b)
            : condition(std::move(cond)), then_branch(std::move(then_b)), else_branch(std::move(else_b)) { if(condition) line = condition->line; }
        Value accept(Visitor& visitor) override { return visitor.visitIfStmt(*this); }
    };

    struct WhileStmt : public Statement {
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> body;
        WhileStmt(std::unique_ptr<Expression> cond, std::unique_ptr<Statement> b)
            : condition(std::move(cond)), body(std::move(b)) { if(condition) line = condition->line; }
        Value accept(Visitor& visitor) override { return visitor.visitWhileStmt(*this); }
    };

    struct ForStmt : public Statement {
        Token keyword; // 'for' token
        Token variable_type; // Currently must be INT
        Token variable_name;
        std::unique_ptr<Expression> range_start;
        std::unique_ptr<Expression> range_end;
        std::unique_ptr<Statement> body;
        ForStmt(Token kw, Token var_type, Token var_name, std::unique_ptr<Expression> start, std::unique_ptr<Expression> end, std::unique_ptr<Statement> b)
            : keyword(std::move(kw)), variable_type(std::move(var_type)), variable_name(std::move(var_name)),
              range_start(std::move(start)), range_end(std::move(end)), body(std::move(b)) { line = keyword.line; }
        Value accept(Visitor& visitor) override { return visitor.visitForStmt(*this); }
    };

    struct FunctionStmt : public Statement {
        Token name;
        std::vector<std::pair<Token, Token>> params; // List of <type_token, name_token>
        Token return_type_token; // Primary return type (e.g., INT, MAP, LIST, FUNCTION, OBJ, NIL)
        std::optional<Token> map_return_key_type_token;   // Only if return_type_token is MAP
        std::optional<Token> map_return_value_type_token; // Only if return_type_token is MAP
        std::optional<Token> list_return_element_type_token; // Only if return_type_token is LIST
        std::unique_ptr<BlockStmt> body;
        FunctionStmt(Token n, std::vector<std::pair<Token, Token>> p,
                     Token ret_type, std::optional<Token> ret_key, std::optional<Token> ret_val, std::optional<Token> ret_list_elem,
                     std::unique_ptr<BlockStmt> b)
            : name(std::move(n)), params(std::move(p)),
              return_type_token(std::move(ret_type)), map_return_key_type_token(std::move(ret_key)),
              map_return_value_type_token(std::move(ret_val)), list_return_element_type_token(std::move(ret_list_elem)),
              body(std::move(b)) { line = name.line; }
        Value accept(Visitor& visitor) override { return visitor.visitFunctionStmt(*this); }
    };

    struct ReturnStmt : public Statement {
        Token keyword; // 'return' token
        std::unique_ptr<Expression> value; // Can be nullptr for return;
        ReturnStmt(Token kw, std::unique_ptr<Expression> val)
            : keyword(std::move(kw)), value(std::move(val)) { line = keyword.line; }
        Value accept(Visitor& visitor) override { return visitor.visitReturnStmt(*this); }
    };

    struct DeleteStmt : public Statement {
        Token keyword; // 'delete' token
        std::unique_ptr<Expression> expression; // VariableExpr or IndexExpr
        DeleteStmt(Token kw, std::unique_ptr<Expression> expr)
            : keyword(std::move(kw)), expression(std::move(expr)) { line = keyword.line; }
        Value accept(Visitor& visitor) override { return visitor.visitDeleteStmt(*this); }
    };

    struct BreakStmt : public Statement {
        Token keyword;
        BreakStmt(Token kw) : keyword(std::move(kw)) { line = keyword.line; }
        Value accept(Visitor& visitor) override { return visitor.visitBreakStmt(*this); }
    };

    struct ContinueStmt : public Statement {
        Token keyword;
        ContinueStmt(Token kw) : keyword(std::move(kw)) { line = keyword.line; }
        Value accept(Visitor& visitor) override { return visitor.visitContinueStmt(*this); }
    };

    // <<< ADDED ImportStmt Definition >>>
    struct ImportStmt : public Statement {
        Token keyword; // 'import' token
        Token filename_token; // STRING token containing the path

        ImportStmt(Token kw, Token fname)
            : keyword(std::move(kw)), filename_token(std::move(fname)) { line = keyword.line; }

        Value accept(Visitor& visitor) override { return visitor.visitImportStmt(*this); }
    };
    // <<< END ADDED >>>

} // namespace AST


#endif // AST_HPP
