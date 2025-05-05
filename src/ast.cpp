#include "ast.hpp"
#include "interpreter.hpp" // Include visitor implementation header

// Constructors are defined directly in the header (ast.hpp)

namespace AST {

// --- Define accept methods for AST nodes ---
Value LiteralExpr::accept(Visitor& visitor) { return visitor.visitLiteralExpr(*this); }
// <<< ADDED accept for InterpolatedStringExpr >>>
Value InterpolatedStringExpr::accept(Visitor& visitor) { return visitor.visitInterpolatedStringExpr(*this); }
// <<< END ADDED >>>
Value VariableExpr::accept(Visitor& visitor) { return visitor.visitVariableExpr(*this); }
Value AssignExpr::accept(Visitor& visitor) { return visitor.visitAssignExpr(*this); }
Value BinaryExpr::accept(Visitor& visitor) { return visitor.visitBinaryExpr(*this); }
Value UnaryExpr::accept(Visitor& visitor) { return visitor.visitUnaryExpr(*this); }
Value LogicalExpr::accept(Visitor& visitor) { return visitor.visitLogicalExpr(*this); }
Value CallExpr::accept(Visitor& visitor) { return visitor.visitCallExpr(*this); }
Value MapLiteralExpr::accept(Visitor& visitor) { return visitor.visitMapLiteralExpr(*this); }
Value MapAccessExpr::accept(Visitor& visitor) { return visitor.visitMapAccessExpr(*this); }
Value ListLiteralExpr::accept(Visitor& visitor) { return visitor.visitListLiteralExpr(*this); }
Value IndexExpr::accept(Visitor& visitor) { return visitor.visitIndexExpr(*this); }

Value BlockStmt::accept(Visitor& visitor) { return visitor.visitBlockStmt(*this); }
Value ExpressionStmt::accept(Visitor& visitor) { return visitor.visitExpressionStmt(*this); }
Value PrintStmt::accept(Visitor& visitor) { return visitor.visitPrintStmt(*this); }
Value VarDeclStmt::accept(Visitor& visitor) { return visitor.visitVarDeclStmt(*this); }
Value IfStmt::accept(Visitor& visitor) { return visitor.visitIfStmt(*this); }
Value WhileStmt::accept(Visitor& visitor) { return visitor.visitWhileStmt(*this); }
Value ForStmt::accept(Visitor& visitor) { return visitor.visitForStmt(*this); }
Value FunctionStmt::accept(Visitor& visitor) { return visitor.visitFunctionStmt(*this); }
Value ReturnStmt::accept(Visitor& visitor) { return visitor.visitReturnStmt(*this); }
Value DeleteStmt::accept(Visitor& visitor) { return visitor.visitDeleteStmt(*this); }
Value BreakStmt::accept(Visitor& visitor) { return visitor.visitBreakStmt(*this); }
Value ContinueStmt::accept(Visitor& visitor) { return visitor.visitContinueStmt(*this); }

} // namespace AST
