#include "interpreter.hpp"
#include "common.hpp" // Include for debug_log and error types
#include "lexer.hpp"  // Need Lexer to re-tokenize expressions
#include "parser.hpp" // Need Parser to re-parse expressions
#include "token.hpp"  // <<< ADDED: Include header defining InterpolationPartType
#include <iostream>
#include <cmath>     // For std::fabs, std::fmod
#include <stdexcept>
#include <utility> // For std::move
#include <memory> // For make_shared
#include <string> // For std::stoi, std::stod
#include <limits> // For numeric_limits
#include <typeinfo> // For typeid (used in error reporting)
#include <vector>
#include <sstream> // For building interpolated string result

// --- Interpolation Types (Assume defined in token.hpp or similar) ---
// <<< REMOVED: Duplicate definition of InterpolationPartType and InterpolatedStringData >>>
// enum class InterpolationPartType { LITERAL, EXPRESSION };
// using InterpolatedStringData = std::vector<std::pair<InterpolationPartType, std::string>>;
// --- End Interpolation Types ---


Interpreter::Interpreter() : globals(std::make_shared<Environment>()), environment(globals) {
    // Register Native Functions
    globals->define("assert", Value(std::make_shared<NativeAssert>()));
    globals->define("len", Value(std::make_shared<NativeLen>()));
    globals->define("type", Value(std::make_shared<NativeType>()));
    globals->define("input", Value(std::make_shared<NativeInput>()));
    globals->define("cast", Value(std::make_shared<NativeCast>()));
    globals->define("append", Value(std::make_shared<NativeListAppend>()));
    globals->define("remove", Value(std::make_shared<NativeListRemove>()));
}

// Store source context for error reporting in interpolated expressions
void Interpreter::setSourceContext(const std::string& filename, const std::vector<std::string>& source_lines) {
    current_filename_ = filename;
    current_source_lines_ = source_lines;
}

std::shared_ptr<Environment> Interpreter::getGlobalEnvironment() const {
    return globals;
}

// --- Error Reporting Helper ---
std::string Interpreter::getSourceLineInternal(const std::vector<std::string>& lines, int line_num) {
     if (line_num >= 1 && static_cast<size_t>(line_num) <= lines.size()) {
        return lines[static_cast<size_t>(line_num) - 1];
    }
    return "<Source line not available>";
}

// <<< FIXED: Removed default argument from definition >>>
void Interpreter::reportError(const LangError& error,
                              const std::string& filename, // Use provided filename
                              const std::vector<std::string>& source_lines, // Use provided lines
                              const std::string& context_msg) { // Default argument is only in header
    had_error_ = true;
    std::string error_type = "Error";
    int line = -1;
    if (const RuntimeError* re = dynamic_cast<const RuntimeError*>(&error)) { error_type = "Runtime Error"; line = re->line; }
    else if (const TypeError* te = dynamic_cast<const TypeError*>(&error)) { error_type = "Type Error"; line = te->line; }
    else if (const SyntaxError* se = dynamic_cast<const SyntaxError*>(&error)) { error_type = "Syntax Error"; line = se->line; }

    std::cerr << filename << ":" << (line != -1 ? std::to_string(line) : "?") << " | " << error_type << context_msg << ": " << error.what() << std::endl;
    if (line != -1) {
        std::cerr << "  " << getSourceLineInternal(source_lines, line) << std::endl;
    }
    std::cerr.flush();

    debug_log << filename << ":" << (line != -1 ? std::to_string(line) : "?") << " | " << error_type << context_msg << ": " << error.what() << std::endl;
    if (line != -1) {
        debug_log << "  " << getSourceLineInternal(source_lines, line) << std::endl;
    }
    debug_log.flush();
}


// --- Main Execution Logic ---
void Interpreter::interpret(const std::vector<std::unique_ptr<AST::Statement>>& statements,
                           const std::string& filename,
                           const std::vector<std::string>& source_lines)
{
    // Store context before starting interpretation
    setSourceContext(filename, source_lines);

    had_error_ = false;
    debug_log << ">>> Interpreter::interpret starting execution. Processing " << statements.size() << " statements." << std::endl;
    try {
        for (const auto& statement : statements) {
            if (statement) {
                 execute(*statement);
            } else {
                 debug_log << " (NULL statement pointer encountered!)" << std::endl;
            }
            // Optional: Stop execution immediately on first error
            // if (had_error_) break;
        }
    } catch (const LangError& error) {
        // Catch errors propagating from execute/evaluate
        reportError(error, current_filename_, current_source_lines_);
    } catch (const std::exception& e) {
        std::cerr << current_filename_ << ":? | System Error: " << e.what() << std::endl;
        debug_log << current_filename_ << ":? | System Error: " << e.what() << std::endl;
        had_error_ = true;
    } catch (...) {
         std::cerr << current_filename_ << ":? | Unknown Error Occurred during interpretation." << std::endl;
         debug_log << current_filename_ << ":? | Unknown Error Occurred during interpretation." << std::endl;
         had_error_ = true;
    }
     debug_log << ">>> Interpreter::interpret finished execution loop." << std::endl;
}


void Interpreter::execute(AST::Statement& stmt) {
    stmt.accept(*this);
}


Value Interpreter::evaluate(AST::Expression& expr) {
    return expr.accept(*this);
}

void Interpreter::executeBlock(const std::vector<std::unique_ptr<AST::Statement>>& statements,
                               std::shared_ptr<Environment> block_environment) {
    std::shared_ptr<Environment> previous_environment = this->environment;
    struct EnvironmentGuard { Interpreter& i; std::shared_ptr<Environment> o; EnvironmentGuard(Interpreter& interp, std::shared_ptr<Environment> orig) : i(interp), o(std::move(orig)) {} ~EnvironmentGuard() { i.environment = o; } } guard(*this, previous_environment);
    this->environment = std::move(block_environment);
    for (const auto& statement : statements) {
         if (statement) execute(*statement);
    }
}

// --- Visitor Implementations for Expressions ---
Value Interpreter::visitLiteralExpr(AST::LiteralExpr& expr) { return expr.value; }

// visitInterpolatedStringExpr Implementation
Value Interpreter::visitInterpolatedStringExpr(AST::InterpolatedStringExpr& expr) {
    std::stringstream result_builder;
    int original_line = expr.line; // Line where the backtick string started

    for (const auto& part : expr.parts) {
        // <<< FIXED: Use fully qualified name for enum value >>>
        if (part.first == InterpolationPartType::LITERAL) {
            // Append literal parts directly
            result_builder << part.second;
        } else { // InterpolationPartType::EXPRESSION
            const std::string& expr_string = part.second;
            if (expr_string.empty()) {
                // Handle empty {} - append empty string.
                continue;
            }

            // --- Re-Lex, Re-Parse, Evaluate the expression string ---
            try {
                debug_log << "Interpolating expression: {" << expr_string << "}" << std::endl;

                // 1. Lex the expression string
                // We need to lex the expression in isolation.
                // Add a newline for EOF handling, but don't include it in error context.
                Lexer expr_lexer(expr_string);
                std::vector<Token> expr_tokens = expr_lexer.scanTokens();

                // Remove the EOF token added by the lexer.
                if (!expr_tokens.empty() && expr_tokens.back().type == TokenType::END_OF_FILE) {
                    expr_tokens.pop_back();
                }

                if (expr_tokens.empty()) {
                     // Throw error if the expression part was empty or only whitespace
                     throw RuntimeError("Empty expression found within interpolated string.", original_line);
                }

                // 2. Parse the expression tokens.
                // Create a temporary parser for these tokens.
                Parser expr_parser(expr_tokens);

                // Attempt to parse a single expression using the parser's expression() method.
                std::unique_ptr<AST::Expression> parsed_expr = expr_parser.expression();

                // Check if parsing succeeded and if the parser consumed all tokens.
                // If not all tokens were consumed, it means there was extra stuff after the expression.
                if (!parsed_expr) {
                    // If expression() returned nullptr, an error likely occurred during parsing.
                    // The parser should have thrown, but check just in case.
                    throw RuntimeError("Invalid expression found within interpolated string (parsing failed).", original_line);
                }
                if (!expr_parser.isAtEnd()) {
                    // If the parser didn't reach the end of the temporary token list,
                    // it means there was more than just a single valid expression.
                     throw RuntimeError("Invalid syntax within interpolated expression (extra tokens found after expression).", original_line);
                }


                // 3. Evaluate the parsed expression in the *current* interpreter environment
                Value result_value = evaluate(*parsed_expr);

                // 4. Convert the result to string and append
                result_builder << result_value.toString();

            } catch (const LangError& interp_error) {
                 // Report error using the main context, adding a note about interpolation
                 std::string context_msg = " (within interpolated string starting on line " + std::to_string(original_line) + ")";
                 reportError(interp_error, current_filename_, current_source_lines_, context_msg);
                 // Stop building the string on error and rethrow to halt execution.
                 throw;
            } catch (const std::exception& std_err) {
                 // Handle unexpected C++ errors during re-parse/eval
                 throw RuntimeError(std::string("Internal error during string interpolation: ") + std_err.what(), original_line);
            }
        }
    }

    return Value(result_builder.str());
}


Value Interpreter::visitVariableExpr(AST::VariableExpr& expr) { try { return environment->get(expr.name); } catch (const RuntimeError& /*e*/) { throw; } }
Value Interpreter::visitAssignExpr(AST::AssignExpr& expr) { Value assigned_value = evaluate(*expr.value); int assignment_line = expr.equals_token.line; if (AST::VariableExpr* var_target = dynamic_cast<AST::VariableExpr*>(expr.target.get())) { try { const VariableInfo& var_info = environment->getInfo(var_target->name); checkType(assigned_value, var_info.declared_type_token, var_info.declared_map_key_type, var_info.declared_map_value_type, var_info.declared_list_element_type, "assignment to variable '" + var_target->name.lexeme + "'", assignment_line); environment->assign(var_target->name, assigned_value); } catch (const RuntimeError& /*e*/) { throw; } catch (const TypeError& /*e*/) { throw; } } else if (AST::IndexExpr* index_target = dynamic_cast<AST::IndexExpr*>(expr.target.get())) { Value collection_val = evaluate(*(index_target->collection)); Value index_val = evaluate(*(index_target->index)); if (collection_val.isList()) { if (!index_val.isInt()) { throw RuntimeError("List index for assignment must be an integer.", index_target->bracket.line); } int index = index_val.getInt(); try { auto& list_data = collection_val.getListData(); size_t size = list_data.size(); int effective_index = index; if (index < 0) { effective_index = static_cast<int>(size) + index; } if (effective_index < 0 || static_cast<size_t>(effective_index) >= size) { throw RuntimeError("List index out of bounds (" + std::to_string(index) + " for list of size " + std::to_string(size) + ").", index_target->bracket.line); } list_data[static_cast<size_t>(effective_index)] = assigned_value; } catch (const std::runtime_error& e) { throw RuntimeError(e.what(), index_target->bracket.line); } catch (const TypeError& e) { throw TypeError(e.what(), index_target->bracket.line); } } else if (collection_val.isMap()) { if (index_val.isMap() || index_val.isList() || index_val.isFunction() || index_val.isObject() || index_val.isNil()) { throw TypeError("Invalid type used as map key in assignment: " + valueTypeToString(index_val.getType()) + ".", index_target->bracket.line); } std::optional<Token> expected_key_type_token = std::nullopt; std::optional<Token> expected_value_type_token = std::nullopt; if (AST::VariableExpr* map_var_expr = dynamic_cast<AST::VariableExpr*>(index_target->collection.get())) { try { const VariableInfo& map_var_info = environment->getInfo(map_var_expr->name); if (map_var_info.declared_type_token.type == TokenType::MAP) { expected_key_type_token = map_var_info.declared_map_key_type; expected_value_type_token = map_var_info.declared_map_value_type; } else if (map_var_info.declared_type_token.type != TokenType::OBJ) { throw TypeError("Variable '" + map_var_expr->name.lexeme + "' is not a map or obj, cannot perform indexed assignment.", map_var_expr->line); } } catch (const RuntimeError&) { throw; } } if (expected_key_type_token) { try { checkType(index_val, *expected_key_type_token, "map key", index_target->bracket.line); } catch (const TypeError& e) { throw; } } if (expected_value_type_token) { try { checkType(assigned_value, *expected_value_type_token, "assignment to map element", assignment_line); } catch (const TypeError& e) { throw TypeError(std::string(e.what()) + " for key " + index_val.toString() + ".", assignment_line); } } try { collection_val.getMapData()[index_val] = assigned_value; } catch (const std::runtime_error& e) { throw RuntimeError(e.what(), index_target->bracket.line); } catch (const TypeError& e) { throw TypeError(e.what(), index_target->bracket.line); } } else { throw RuntimeError("Cannot perform indexed assignment '[]' on type '" + valueTypeToString(collection_val.getType()) + "'.", index_target->bracket.line); } } else if (AST::MapAccessExpr* map_access_target = dynamic_cast<AST::MapAccessExpr*>(expr.target.get())) { Value map_val = evaluate(*(map_access_target->map_expr)); if (!map_val.isMap()) { throw RuntimeError("Cannot perform indexed assignment on non-map type (using MapAccessExpr).", map_access_target->bracket.line); } Value key_val = evaluate(*(map_access_target->key_expr)); try { map_val.getMapData()[key_val] = assigned_value; } catch (const std::runtime_error& e) { throw RuntimeError(e.what(), map_access_target->bracket.line); } catch (const TypeError& e) { throw TypeError(e.what(), map_access_target->bracket.line); } debug_log << "Warning: Using deprecated MapAccessExpr for assignment at line " << expr.equals_token.line << std::endl; } else { throw RuntimeError("Invalid assignment target.", expr.equals_token.line); } return assigned_value; }
Value Interpreter::visitLogicalExpr(AST::LogicalExpr& expr) { Value left = evaluate(*expr.left); if (expr.op.type == TokenType::OR) { if (isTruthy(left)) return left; } else { if (!isTruthy(left)) return left; } return evaluate(*expr.right); }
Value Interpreter::visitUnaryExpr(AST::UnaryExpr& expr) { Value right = evaluate(*expr.right); switch (expr.op.type) { case TokenType::MINUS: checkNumberOperand(expr.op, right); if (right.isInt()) return Value(-right.getInt()); if (right.isFloat()) return Value(-right.getFloat()); throw RuntimeError("Internal error: Operand is number but not int or float.", expr.op.line); case TokenType::BANG: return Value(!isTruthy(right)); default: throw RuntimeError("Invalid unary operator.", expr.op.line); } return Value(); }
Value Interpreter::visitBinaryExpr(AST::BinaryExpr& expr) { Value left = evaluate(*expr.left); Value right = evaluate(*expr.right); try { switch (expr.op.type) { case TokenType::MINUS: checkNumberOperands(expr.op, left, right); if (left.isInt() && right.isInt()) return Value(left.getInt() - right.getInt()); return Value(left.asNumber() - right.asNumber()); case TokenType::PLUS: if (left.isNumber() && right.isNumber()) { if (left.isInt() && right.isInt()) return Value(left.getInt() + right.getInt()); return Value(left.asNumber() + right.asNumber()); } if (left.isString() && right.isString()) { return Value(left.getString() + right.getString()); } if (left.isString()) { return Value(left.getString() + right.toString()); } if (right.isString()) { return Value(left.toString() + right.getString()); } throw RuntimeError("Operands must be two numbers or at least one string for '+'.", expr.op.line); case TokenType::SLASH: checkNumberOperands(expr.op, left, right); if (right.asNumber() == 0.0) throw RuntimeError("Division by zero.", expr.op.line); return Value(left.asNumber() / right.asNumber()); case TokenType::STAR: checkNumberOperands(expr.op, left, right); if (left.isInt() && right.isInt()) return Value(left.getInt() * right.getInt()); return Value(left.asNumber() * right.asNumber()); case TokenType::PERCENT: checkNumberOperands(expr.op, left, right); if (right.asNumber() == 0.0) throw RuntimeError("Modulo by zero.", expr.op.line); if (left.isInt() && right.isInt()) { if (right.getInt() == 0) throw RuntimeError("Modulo by zero.", expr.op.line); return Value(left.getInt() % right.getInt()); } return Value(std::fmod(left.asNumber(), right.asNumber())); case TokenType::GREATER:       return Value(left > right); case TokenType::GREATER_EQUAL: return Value(left >= right); case TokenType::LESS:          return Value(left < right); case TokenType::LESS_EQUAL:    return Value(left <= right); case TokenType::BANG_EQUAL:    return Value(left != right); case TokenType::EQUAL_EQUAL:   return Value(left == right); default: throw RuntimeError("Invalid binary operator.", expr.op.line); } } catch (const TypeError& e) { throw TypeError(std::string(e.what()), expr.op.line); } catch (const RuntimeError& /*e*/) { throw; } return Value(); }
Value Interpreter::visitCallExpr(AST::CallExpr& expr) { Value callee_value = evaluate(*expr.callee); if (!callee_value.isCallable()) { throw RuntimeError("Can only call functions.", expr.paren.line); } std::shared_ptr<Callable> function = callee_value.getFunction(); std::vector<Value> arguments; arguments.reserve(expr.arguments.size()); for (const auto& arg_expr : expr.arguments) { arguments.push_back(evaluate(*arg_expr)); } int expected_arity = function->arity(); if (expected_arity != -1 && arguments.size() != static_cast<size_t>(expected_arity)) { throw RuntimeError("Expected " + std::to_string(expected_arity) + " arguments but got " + std::to_string(arguments.size()) + ".", expr.paren.line); } try { if (auto* lang_func = dynamic_cast<LangFunction*>(function.get())) { if (!lang_func->declaration) throw RuntimeError("Internal error: Function declaration is null.", expr.paren.line); if (lang_func->declaration->params.size() != arguments.size()) throw RuntimeError("Internal error: Parameter count mismatch during call.", expr.paren.line); for (size_t i = 0; i < arguments.size(); ++i) { const Token& param_type_token = lang_func->declaration->params[i].first; const Token& param_name_token = lang_func->declaration->params[i].second; checkType(arguments[i], param_type_token, std::nullopt, std::nullopt, std::nullopt, "argument '" + param_name_token.lexeme + "'", expr.paren.line); } } return function->call(*this, arguments); } catch (const TypeError& te) { throw TypeError(te.what(), expr.paren.line); } catch (const RuntimeError& re) { throw RuntimeError(re.what(), expr.paren.line); } return Value(); }
Value Interpreter::visitMapLiteralExpr(AST::MapLiteralExpr& expr) { auto map_data = std::make_shared<Value::MapDataType>(); for (const auto& pair : expr.elements) { Value key = evaluate(*pair.first); Value value = evaluate(*pair.second); if (key.isMap() || key.isList() || key.isFunction() || key.isObject() || key.isNil()) { throw TypeError("Invalid type used as map key in literal: " + valueTypeToString(key.getType()) + ".", expr.opening_brace.line); } try { map_data->emplace(std::move(key), std::move(value)); } catch(const TypeError& e) { throw TypeError(std::string(e.what()) + " used as map key.", expr.opening_brace.line); } catch (const std::runtime_error& e) { throw RuntimeError(std::string("Error creating map literal: ") + e.what(), expr.opening_brace.line); } } return Value(std::move(map_data)); }
Value Interpreter::visitMapAccessExpr(AST::MapAccessExpr& expr) { debug_log << "Warning: Visiting potentially deprecated MapAccessExpr at line " << expr.bracket.line << std::endl; Value map_val = evaluate(*expr.map_expr); if (!map_val.isMap()) { throw RuntimeError("Cannot perform indexed access '[]' on non-map type (using MapAccessExpr).", expr.bracket.line); } Value key_val = evaluate(*expr.key_expr); try { const auto& map_data = map_val.getMapData(); auto it = map_data.find(key_val); if (it != map_data.end()) { return it->second; } else { return Value(); } } catch (const std::runtime_error& e) { throw RuntimeError(e.what(), expr.bracket.line); } catch (const TypeError& e) { throw TypeError(e.what(), expr.bracket.line); } return Value(); }
Value Interpreter::visitListLiteralExpr(AST::ListLiteralExpr& expr) { auto list_data = std::make_shared<Value::ListDataType>(); list_data->reserve(expr.elements.size()); for (const auto& element_expr : expr.elements) { list_data->push_back(evaluate(*element_expr)); } return Value(std::move(list_data)); }
Value Interpreter::visitIndexExpr(AST::IndexExpr& expr) { Value collection_val = evaluate(*expr.collection); Value index_val = evaluate(*expr.index); if (collection_val.isList()) { if (!index_val.isInt()) { throw RuntimeError("List index must be an integer.", expr.bracket.line); } int index = index_val.getInt(); try { const auto& list_data = collection_val.getListData(); size_t size = list_data.size(); int effective_index = index; if (index < 0) { effective_index = static_cast<int>(size) + index; } if (effective_index < 0 || static_cast<size_t>(effective_index) >= size) { return Value(); } return list_data[static_cast<size_t>(effective_index)]; } catch (const std::runtime_error& e) { throw RuntimeError(e.what(), expr.bracket.line); } catch (const TypeError& e) { throw TypeError(e.what(), expr.bracket.line); } } else if (collection_val.isMap()) { if (index_val.isMap() || index_val.isList() || index_val.isFunction() || index_val.isObject() || index_val.isNil()) { throw TypeError("Invalid type used as map key: " + valueTypeToString(index_val.getType()) + ".", expr.bracket.line); } std::optional<Token> expected_key_type_token = std::nullopt; if (AST::VariableExpr* map_var_expr = dynamic_cast<AST::VariableExpr*>(expr.collection.get())) { try { const VariableInfo& map_var_info = environment->getInfo(map_var_expr->name); if (map_var_info.declared_type_token.type == TokenType::MAP) { expected_key_type_token = map_var_info.declared_map_key_type; } else if (map_var_info.declared_type_token.type != TokenType::OBJ) { throw TypeError("Variable '" + map_var_expr->name.lexeme + "' is not a map or obj, cannot perform indexed access.", map_var_expr->line); } } catch (const RuntimeError&) { throw; } } if(expected_key_type_token) { try { checkType(index_val, *expected_key_type_token, "map key", expr.bracket.line); } catch (const TypeError& e) { throw; } } try { const auto& map_data = collection_val.getMapData(); auto it = map_data.find(index_val); if (it != map_data.end()) { return it->second; } else { return Value(); } } catch (const std::runtime_error& e) { throw RuntimeError(e.what(), expr.bracket.line); } catch (const TypeError& e) { throw TypeError(e.what(), expr.bracket.line); } } else { throw RuntimeError("Cannot perform indexed access '[]' on type '" + valueTypeToString(collection_val.getType()) + "'.", expr.bracket.line); } return Value(); }


// --- Visitor Implementations for Statements ---
Value Interpreter::visitExpressionStmt(AST::ExpressionStmt& stmt) { evaluate(*stmt.expression); return Value(); }
Value Interpreter::visitPrintStmt(AST::PrintStmt& stmt) { bool first = true; int arg_index = 0; for (const auto& expr : stmt.expressions) { if (!expr) { std::cerr << "!!!! Internal Error: NULL expression found in print statement at line " << stmt.line << std::endl; continue; } if (!first) { std::cout << " "; } first = false; Value value = evaluate(*expr); std::cout << value.toString(); arg_index++; } std::cout << std::endl; return Value(); }
Value Interpreter::visitVarDeclStmt(AST::VarDeclStmt& stmt) { Value initial_value; bool is_map_decl = stmt.type_specifier_token && stmt.type_specifier_token->type == TokenType::MAP; bool is_list_decl = stmt.type_specifier_token && stmt.type_specifier_token->type == TokenType::LIST; if (is_map_decl && !stmt.initializer) { initial_value = Value(std::make_shared<Value::MapDataType>()); } else if (is_list_decl && !stmt.initializer) { initial_value = Value(std::make_shared<Value::ListDataType>()); } if (stmt.initializer) { initial_value = evaluate(*stmt.initializer); } if (stmt.type_specifier_token) { try { checkType(initial_value, *stmt.type_specifier_token, stmt.map_key_type_token, stmt.map_value_type_token, stmt.list_element_type_token, "initialization of variable '" + stmt.name.lexeme + "'", stmt.name.line); environment->define(stmt.name.lexeme, initial_value, *stmt.type_specifier_token, stmt.map_key_type_token, stmt.map_value_type_token, stmt.list_element_type_token); } catch (const TypeError& e) { throw TypeError(e.what(), stmt.name.line); } } else { if (!stmt.initializer) { throw RuntimeError("Variable declaration using 'var' requires an initializer.", stmt.name.line); } environment->define(stmt.name.lexeme, initial_value); } return Value(); }
Value Interpreter::visitBlockStmt(AST::BlockStmt& stmt) { executeBlock(stmt.statements, std::make_shared<Environment>(environment)); return Value(); }
Value Interpreter::visitIfStmt(AST::IfStmt& stmt) { Value cond = evaluate(*stmt.condition); if (isTruthy(cond)) { execute(*stmt.then_branch); } else if (stmt.else_branch) { execute(*stmt.else_branch); } return Value(); }
Value Interpreter::visitWhileStmt(AST::WhileStmt& stmt) { while (isTruthy(evaluate(*stmt.condition))) { try { execute(*stmt.body); } catch (const BreakSignal& ) { break; } catch (const ContinueSignal& ) { continue; } catch (const ReturnValue& ) { throw; } } return Value(); }
Value Interpreter::visitForStmt(AST::ForStmt& stmt) { Value start_val = evaluate(*stmt.range_start); Value end_val = evaluate(*stmt.range_end); if (!start_val.isInt() || !end_val.isInt()) throw RuntimeError("For loop range bounds must be integers.", stmt.keyword.line); int start_int = start_val.getInt(); int end_int = end_val.getInt(); if (getValueTypeFromToken(stmt.variable_type) != ValueType::INT) throw TypeError("For loop variable type must be 'int' for integer range.", stmt.variable_type.line); for (int i = start_int; i < end_int; ++i) { auto loop_env = std::make_shared<Environment>(environment); loop_env->define(stmt.variable_name.lexeme, Value(i), stmt.variable_type); std::shared_ptr<Environment> previous_env = this->environment; struct EnvironmentGuard { Interpreter& i; std::shared_ptr<Environment> o; EnvironmentGuard(Interpreter& interp, std::shared_ptr<Environment> orig) : i(interp), o(std::move(orig)) {} ~EnvironmentGuard() { i.environment = o; } } guard(*this, previous_env); this->environment = loop_env; try { execute(*stmt.body); } catch (const BreakSignal& ) { break; } catch (const ContinueSignal& ) { continue; } catch (const ReturnValue& ) { throw; } } return Value(); }
Value Interpreter::visitFunctionStmt(AST::FunctionStmt& stmt) { auto function = std::make_shared<LangFunction>(&stmt, environment); Token func_type_token(TokenType::FUNCTION, "function", std::monostate{}, stmt.name.line); environment->define(stmt.name.lexeme, Value(function), func_type_token, stmt.map_return_key_type_token, stmt.map_return_value_type_token, stmt.list_return_element_type_token); return Value(); }
Value Interpreter::visitReturnStmt(AST::ReturnStmt& stmt) { Value value; if (stmt.value) { value = evaluate(*stmt.value); } throw ReturnValue(std::move(value), stmt.keyword.line); }
Value Interpreter::visitDeleteStmt(AST::DeleteStmt& stmt) { if (AST::VariableExpr* var_expr = dynamic_cast<AST::VariableExpr*>(stmt.expression.get())) { if (!environment->undefine(var_expr->name.lexeme)) { throw RuntimeError("Cannot delete variable '" + var_expr->name.lexeme + "' as it is not defined in the current scope.", var_expr->line); } } else if (AST::IndexExpr* index_expr = dynamic_cast<AST::IndexExpr*>(stmt.expression.get())) { Value collection_val = evaluate(*(index_expr->collection)); Value index_val = evaluate(*(index_expr->index)); if (collection_val.isList()) { if (!index_val.isInt()) { throw RuntimeError("List index for delete must be an integer.", index_expr->bracket.line); } int index = index_val.getInt(); try { auto& list_data = collection_val.getListData(); size_t size = list_data.size(); int effective_index = index; if (index < 0) { effective_index = static_cast<int>(size) + index; } if (effective_index < 0 || static_cast<size_t>(effective_index) >= size) { throw RuntimeError("List index out of bounds for delete (" + std::to_string(index) + " for list of size " + std::to_string(size) + ").", index_expr->bracket.line); } list_data.erase(list_data.begin() + effective_index); } catch (const std::runtime_error& e) { throw RuntimeError(e.what(), index_expr->bracket.line); } catch (const TypeError& e) { throw TypeError(e.what(), index_expr->bracket.line); } } else if (collection_val.isMap()) { if (index_val.isMap() || index_val.isList() || index_val.isFunction() || index_val.isObject() || index_val.isNil()) { throw TypeError("Invalid type used as map key for delete: " + valueTypeToString(index_val.getType()) + ".", index_expr->bracket.line); } try { size_t erased_count = collection_val.getMapData().erase(index_val); if (erased_count == 0) { } } catch (const std::runtime_error& e) { throw RuntimeError(e.what(), index_expr->bracket.line); } catch (const TypeError& e) { throw TypeError(e.what(), index_expr->bracket.line); } } else { throw RuntimeError("Cannot perform indexed delete '[]' on type '" + valueTypeToString(collection_val.getType()) + "'.", index_expr->bracket.line); } } else if (AST::MapAccessExpr* map_access = dynamic_cast<AST::MapAccessExpr*>(stmt.expression.get())) { Value map_val = evaluate(*(map_access->map_expr)); if (!map_val.isMap()) { throw RuntimeError("Cannot perform delete on non-map type using '[]' (MapAccessExpr).", map_access->bracket.line); } Value key_val = evaluate(*(map_access->key_expr)); try { map_val.getMapData().erase(key_val); } catch (const std::runtime_error& e) { throw RuntimeError(e.what(), map_access->bracket.line); } catch (const TypeError& e) { throw TypeError(e.what(), map_access->bracket.line); } debug_log << "Warning: Using deprecated MapAccessExpr for delete at line " << stmt.keyword.line << std::endl; } else { throw RuntimeError("Operand for 'delete' must be a variable or an indexed element access.", stmt.keyword.line); } return Value(); }
Value Interpreter::visitBreakStmt(AST::BreakStmt& stmt) { throw BreakSignal(stmt.keyword.line); }
Value Interpreter::visitContinueStmt(AST::ContinueStmt& stmt) { throw ContinueSignal(stmt.keyword.line); }


// --- Helpers ---
bool Interpreter::isTruthy(const Value& value) { return value.isTruthy(); }
void Interpreter::checkNumberOperand(const Token& op, const Value& operand) { if (!operand.isNumber()) throw TypeError("Operand must be a number.", op.line); }
void Interpreter::checkNumberOperands(const Token& op, const Value& left, const Value& right) { if (!left.isNumber() || !right.isNumber()) throw TypeError("Operands must be numbers.", op.line); }
void Interpreter::checkStringOperand(const Token& op, const Value& operand) { if (!operand.isString()) throw TypeError("Operand must be a string.", op.line); }
ValueType Interpreter::getValueTypeFromToken(const Token& type_token) { switch(type_token.type) { case TokenType::INT: return ValueType::INT; case TokenType::FLOAT: return ValueType::FLOAT; case TokenType::BOOL: return ValueType::BOOL; case TokenType::STRING_TYPE: return ValueType::STRING; case TokenType::OBJ: return ValueType::OBJECT; case TokenType::NIL: return ValueType::NIL; case TokenType::MAP: return ValueType::MAP; case TokenType::LIST: return ValueType::LIST; case TokenType::FUNCTION: return ValueType::FUNCTION; default: throw RuntimeError("Invalid type specifier token encountered: " + tokenTypeToString(type_token.type) , type_token.line); } }
void Interpreter::checkType(const Value& value_to_check, const Token& expected_type_token, const std::optional<Token>& expected_map_key_type_token, const std::optional<Token>& expected_map_value_type_token, const std::optional<Token>& expected_list_element_type_token, const std::string& context, int line_num) { ValueType expected_primary_type = getValueTypeFromToken(expected_type_token); ValueType actual_type = value_to_check.getType(); if (actual_type == ValueType::NIL) { if (expected_primary_type == ValueType::NIL || expected_primary_type == ValueType::OBJECT || expected_primary_type == ValueType::MAP || expected_primary_type == ValueType::FUNCTION || expected_primary_type == ValueType::LIST) { return; } else { throw TypeError("Cannot assign 'nil' to type '" + tokenTypeToString(expected_type_token.type) + "' in " + context + ".", line_num); } } bool primary_type_mismatch = false; if (expected_primary_type == ValueType::FLOAT && actual_type == ValueType::INT) { /* Allowed */ } else if (expected_primary_type == ValueType::FUNCTION && (actual_type == ValueType::FUNCTION || actual_type == ValueType::NATIVE_FUNCTION)) { /* Allowed */ } else if (actual_type != expected_primary_type) { if (expected_primary_type != ValueType::OBJECT) { primary_type_mismatch = true; } } if (primary_type_mismatch) { throw TypeError("Type mismatch for " + context + ". Expected '" + valueTypeToString(expected_primary_type) + "' but got '" + valueTypeToString(actual_type) + "'.", line_num); } if (expected_primary_type == ValueType::MAP && expected_map_key_type_token && expected_map_value_type_token && !value_to_check.isNil()) { if (!value_to_check.isMap()) { throw TypeError("Type mismatch for " + context + ". Expected 'map' but got '" + valueTypeToString(actual_type) + "'.", line_num); } Token key_type_token_for_check = expected_map_key_type_token.value(); Token value_type_token_for_check = expected_map_value_type_token.value(); const auto& map_data = value_to_check.getMapData(); for (const auto& pair : map_data) { checkType(pair.first, key_type_token_for_check, std::nullopt, std::nullopt, std::nullopt, context + " map key", line_num); checkType(pair.second, value_type_token_for_check, std::nullopt, std::nullopt, std::nullopt, context + " map value for key " + pair.first.toString(), line_num); } } else if (expected_primary_type == ValueType::LIST && expected_list_element_type_token && !value_to_check.isNil()) { if (!value_to_check.isList()) { throw TypeError("Type mismatch for " + context + ". Expected 'list' but got '" + valueTypeToString(actual_type) + "'.", line_num); } Token element_type_token_for_check = expected_list_element_type_token.value(); const auto& list_data = value_to_check.getListData(); int element_index = 0; for (const auto& element : list_data) { checkType(element, element_type_token_for_check, std::nullopt, std::nullopt, std::nullopt, context + " list element at index " + std::to_string(element_index), line_num); element_index++; } } }
void Interpreter::checkType(const Value& value, const Token& type_specifier_token, const std::string& context, int line_num) { checkType(value, type_specifier_token, std::nullopt, std::nullopt, std::nullopt, context, line_num); }
