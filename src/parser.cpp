#include "parser.hpp"
#include "common.hpp" // Include for debug_log and error types
#include "token.hpp"  // Ensure this includes InterpolationPartType and InterpolatedStringData
#include <iostream> // Keep for user-facing errors (like parse errors)
#include <utility> // For std::move
#include <vector>
#include <optional> // For optional tokens in parseType
#include <tuple> // For tuple return
#include <exception> // For std::exception
#include <variant> // Needed for token literal

// --- Interpolation Types (Assume defined in token.hpp or similar) ---
// enum class InterpolationPartType { LITERAL, EXPRESSION };
// using InterpolatedStringData = std::vector<std::pair<InterpolationPartType, std::string>>;
// --- End Interpolation Types ---


Parser::Parser(const std::vector<Token>& tokens) : tokens(tokens) {}

// --- Main Parsing Logic ---
std::vector<std::unique_ptr<AST::Statement>> Parser::parse() {
    std::vector<std::unique_ptr<AST::Statement>> statements;
    while (!isAtEnd()) { // <<< Uses isAtEnd()
        if (peek().type == TokenType::END_OF_FILE) { break; }
        std::unique_ptr<AST::Statement> decl = nullptr;
        int current_before_decl = current;
        try {
            decl = declaration();
            if (decl) {
                statements.push_back(std::move(decl));
            } else {
                 // Avoid infinite loops if declaration() fails without advancing
                 if (!isAtEnd() && current == current_before_decl && peek().type != TokenType::END_OF_FILE) { // <<< Uses isAtEnd()
                     debug_log << "Warning: Parser::parse declaration() returned nullptr and did not advance. Forcing advance past token: " << peek().toString() << std::endl;
                     advance(); // Force progress
                 }
            }
        } catch (const SyntaxError& error) {
            // Report user-facing syntax errors
            std::cerr << "[Line " << error.line << "] Parse Error: " << error.what() << std::endl;
            debug_log << "DEBUG: Parser::parse caught SyntaxError. Synchronizing..." << std::endl;
            synchronize(); // Attempt to recover and continue parsing
            debug_log << "DEBUG: Parser::parse synchronized. Current token: " << peek().toString() << std::endl;
        } catch (const std::runtime_error& rt_error) {
             // Report internal errors during parsing
             std::cerr << "[Internal Parse Error] " << rt_error.what() << std::endl;
             debug_log << "DEBUG: Parser::parse caught runtime_error. Synchronizing..." << std::endl;
             synchronize(); // Attempt to recover
             debug_log << "DEBUG: Parser::parse synchronized. Current token: " << peek().toString() << std::endl;
        } catch (const std::exception& std_ex) {
             // Catch other standard exceptions
             std::cerr << "!!!! Parser::parse caught std::exception: " << std_ex.what() << ". Current token: " << peek().toString() << ". Synchronizing..." << std::endl;
             debug_log << "!!!! DEBUG: Parser::parse caught std::exception: " << std_ex.what() << ". Current token: " << peek().toString() << ". Synchronizing..." << std::endl;
             synchronize();
             debug_log << "!!!! DEBUG: Parser::parse synchronized after std::exception. Current token: " << peek().toString() << std::endl;
        }
        catch (...) {
             // Catch any other unknown exceptions
             std::cerr << "!!!! Parser::parse caught UNKNOWN EXCEPTION type. Current token: " << peek().toString() << ". Synchronizing..." << std::endl;
             debug_log << "!!!! DEBUG: Parser::parse caught UNKNOWN EXCEPTION type. Current token: " << peek().toString() << ". Synchronizing..." << std::endl;
             synchronize();
             debug_log << "!!!! DEBUG: Parser::parse synchronized after unknown exception. Current token: " << peek().toString() << std::endl;
        }
    }
    return statements;
}

// --- Grammar Rule Implementations ---

// declaration -> funDeclaration | varDeclaration | statement ;
std::unique_ptr<AST::Statement> Parser::declaration() {
    try {
        if (match({TokenType::FN})) {
            return funDeclaration();
        }
        // Check for type keywords OR 'var' to start a variable declaration
        if (check(TokenType::INT) || check(TokenType::FLOAT) || check(TokenType::BOOL) ||
            check(TokenType::STRING_TYPE) || check(TokenType::OBJ) || check(TokenType::NIL) ||
            check(TokenType::MAP) || check(TokenType::LIST) || check(TokenType::FUNCTION) ||
            check(TokenType::VAR))
        {
            return varDeclaration();
        }
        // If at EOF, return nullptr cleanly
        if (check(TokenType::END_OF_FILE)) {
            return nullptr;
        }
        // Otherwise, parse as a regular statement
        return statement();
    } catch (...) {
        // Re-throw caught exceptions (like SyntaxError) to be handled by parse() loop
        throw;
    }
    // Should not be reached if logic is correct, but prevents compiler warnings
    return nullptr;
}

// varDeclaration -> ( "var" IDENTIFIER "=" expression | type IDENTIFIER ( "=" expression )? ) ";" ;
std::unique_ptr<AST::Statement> Parser::varDeclaration() {
    Token name_tok(TokenType::UNKNOWN, "", std::monostate{}, -1); // Initialize with a default
    std::unique_ptr<AST::Expression> initializer = nullptr;
    std::unique_ptr<AST::Statement> result = nullptr;

    if (match({TokenType::VAR})) {
         // Untyped 'var' declaration (must have initializer)
         name_tok = consume(TokenType::IDENTIFIER, "Expect variable name after 'var'.");
         consume(TokenType::EQUAL, "Expect '=' initializer for 'var' declaration.");
         initializer = expression(); // Parse the initializer expression
         if (!initializer) return nullptr; // Error handled by expression() or consume()
         consume(TokenType::SEMICOLON, "Expect ';' after variable declaration.");

         // Create the AST node for untyped 'var'
         // Pass nullopt for type_tok and all complex type tokens. Initializer is last.
         result = std::make_unique<AST::VarDeclStmt>(name_tok, std::nullopt,
                                                     std::nullopt, std::nullopt, std::nullopt,
                                                     std::move(initializer));
         result->line = name_tok.line; // Set line number for error reporting

    } else {
        // Typed declaration (initializer is optional)
        auto [type_token, key_type_token, value_type_token, list_element_token] = parseType(); // Parse the type specifier(s)
        name_tok = consume(TokenType::IDENTIFIER, "Expect variable name after type specifier.");

        if (match({TokenType::EQUAL})) { // Check for optional initializer
            initializer = expression();
            if (!initializer) return nullptr; // Error handled by expression() or consume()
        }
        consume(TokenType::SEMICOLON, "Expect ';' after variable declaration.");

        // Create the AST node for typed declaration
        // Pass all parsed type tokens. Initializer is last (can be nullptr).
        result = std::make_unique<AST::VarDeclStmt>(name_tok, type_token,
                                                    key_type_token, value_type_token, list_element_token,
                                                    std::move(initializer));
        result->line = name_tok.line; // Set line number
    }
    return result;
}


// funDeclaration -> "fn" functionBody ;
std::unique_ptr<AST::Statement> Parser::funDeclaration() {
    // Delegate to functionBody, passing "function" as the kind for error messages
    return functionBody("function");
}

// Helper for parsing function structure (used by funDeclaration)
// functionBody -> IDENTIFIER "(" parameters? ")" ":" type "{" block "}" ;
std::unique_ptr<AST::FunctionStmt> Parser::functionBody(const std::string& kind) {
     Token name = consume(TokenType::IDENTIFIER, "Expect " + kind + " name.");
     int func_line = name.line; // Store line number early

     consume(TokenType::LEFT_PAREN, "Expect '(' after " + kind + " name.");
     std::vector<std::pair<Token, Token>> params; // Stores pairs of <type_token, name_token>
     if (!check(TokenType::RIGHT_PAREN)) { // Check if parameters exist
         params = parameters(); // Parse parameters if present
     }
     consume(TokenType::RIGHT_PAREN, "Expect ')' after parameters.");

     consume(TokenType::COLON, "Expect ':' before " + kind + " return type.");
     // Parse the return type (can be simple or complex like map/list)
     auto [return_type_token, return_key_type, return_val_type, return_list_element] = parseType();

     consume(TokenType::LEFT_BRACE, "Expect '{' before " + kind + " body.");
     // Parse the function body as a block statement
     auto body_stmt_base = block();
     if (!body_stmt_base) {
         // If block() returns nullptr (e.g., due to error during parsing inside block), throw error.
         throw error(previous(), "Expected function body block after '{'.");
     }

     // Safely cast the base Statement pointer to a BlockStmt pointer
     AST::BlockStmt* block_ptr = dynamic_cast<AST::BlockStmt*>(body_stmt_base.get());
     if (!block_ptr) {
         // This should ideally not happen if block() returns a valid BlockStmt, but check just in case.
         throw error(previous().type == TokenType::LEFT_BRACE ? previous() : peek(), "Function body must be a block statement.");
     }
     // Release ownership from the unique_ptr returned by block()
     body_stmt_base.release();
     // Create a new unique_ptr managing the raw BlockStmt pointer
     auto body_block_ptr = std::unique_ptr<AST::BlockStmt>(block_ptr);

     // Create the Function Statement AST node
     // Pass all parsed type tokens. Body is last.
     auto func_stmt = std::make_unique<AST::FunctionStmt>(name, params, return_type_token,
                                                          return_key_type, return_val_type, return_list_element,
                                                          std::move(body_block_ptr));
     func_stmt->line = func_line; // Set the line number for the function statement
     return func_stmt;
}

// parameters -> type IDENTIFIER ( "," type IDENTIFIER )* ;
std::vector<std::pair<Token, Token>> Parser::parameters() {
    std::vector<std::pair<Token, Token>> params;
    do {
        if (params.size() >= 255) {
            // Enforce maximum parameter limit
            error(peek(), "Cannot have more than 255 parameters.");
            // Note: error() throws, so no need to return here.
        }
        // Parse the type for the current parameter
        auto [param_type_token, key_type, val_type, list_elem] = parseType();
        // Disallow 'nil' as an explicit parameter type
        if (param_type_token.type == TokenType::NIL) {
             throw error(param_type_token, "Function parameters cannot be explicitly declared as type 'nil'. Use 'obj' or a specific type.");
        }
        // Consume the parameter name
        Token param_name = consume(TokenType::IDENTIFIER, "Expect parameter name.");
        // Store the type token and name token as a pair
        params.emplace_back(std::move(param_type_token), std::move(param_name));
    } while (match({TokenType::COMMA})); // Continue parsing if a comma is found
    return params;
}

// statement -> printStatement | breakStatement | continueStatement | block | ifStatement | whileStatement | forStatement | returnStatement | deleteStatement | expressionStatement ;
std::unique_ptr<AST::Statement> Parser::statement() {
    if (match({TokenType::PRINT})) return printStatement();
    if (match({TokenType::BREAK})) return breakStatement();
    if (match({TokenType::CONTINUE})) return continueStatement();
    if (match({TokenType::LEFT_BRACE})) { // Start of a block
        // Note: block() returns std::unique_ptr<AST::Statement>, but it's dynamically a BlockStmt
        return block();
    }
    if (match({TokenType::IF})) return ifStatement();
    if (match({TokenType::WHILE})) return whileStatement();
    if (match({TokenType::FOR})) return forStatement();
    if (match({TokenType::RETURN})) return returnStatement();
    if (match({TokenType::DELETE})) return deleteStatement();

    // If none of the above match, assume it's an expression statement
    return expressionStatement();
}

// expressionStatement -> expression ";" ;
std::unique_ptr<AST::Statement> Parser::expressionStatement() {
    auto expr = expression(); // Parse the expression first
    if (!expr) return nullptr; // Propagate error if expression parsing failed
    int expr_line = expr->line; // Store line number from expression
    consume(TokenType::SEMICOLON, "Expect ';' after expression statement.");
    // Wrap the expression in an ExpressionStmt AST node
    auto expr_stmt = std::make_unique<AST::ExpressionStmt>(std::move(expr));
    expr_stmt->line = expr_line; // Assign line number
    return expr_stmt;
}

// printStatement -> "print" "(" ( expression ( "," expression )* )? ")" ";" ;
std::unique_ptr<AST::Statement> Parser::printStatement() {
    Token keyword = previous(); // The 'print' token
    int keyword_line = keyword.line;
    consume(TokenType::LEFT_PAREN, "Expect '(' after 'print'.");
    std::vector<std::unique_ptr<AST::Expression>> expressions;
    if (!check(TokenType::RIGHT_PAREN)) { // Check if there are arguments
        do {
            auto expr = expression(); // Parse each argument expression
            if (!expr) return nullptr; // Propagate error
            expressions.push_back(std::move(expr));
        } while (match({TokenType::COMMA})); // Continue if comma is found
    }
    consume(TokenType::RIGHT_PAREN, "Expect ')' after print arguments.");
    consume(TokenType::SEMICOLON, "Expect ';' after print statement.");
    // Create the PrintStmt AST node
    auto print_stmt = std::make_unique<AST::PrintStmt>(keyword, std::move(expressions));
    print_stmt->line = keyword_line;
    return print_stmt;
}

// block -> "{" declaration* "}" ;
std::unique_ptr<AST::Statement> Parser::block() {
    std::vector<std::unique_ptr<AST::Statement>> statements;
    int start_line = previous().line; // Line of the opening '{'

    // Continue parsing declarations/statements until '}' or EOF is encountered
    while (!check(TokenType::RIGHT_BRACE) && !isAtEnd()) { // <<< Uses isAtEnd()
        auto decl = declaration(); // Parse one statement or declaration
        if(decl) {
            statements.push_back(std::move(decl));
        } else {
            // Handle cases where declaration() might return nullptr without error (e.g., EOF reached unexpectedly)
            // or if an error occurred and was synchronized within declaration().
            if (!isAtEnd() && !check(TokenType::RIGHT_BRACE)) { // <<< Uses isAtEnd()
                 debug_log << "Warning: Null statement encountered inside block at line " << peek().line << ". Attempting to continue." << std::endl;
                 // Avoid getting stuck if declaration() fails but doesn't advance or throw
                 // This might happen if synchronize() recovers poorly. Consider advancing here if needed.
            }
        }
    }

    consume(TokenType::RIGHT_BRACE, "Expect '}' after block.");
    // Create the BlockStmt AST node
    auto block_stmt = std::make_unique<AST::BlockStmt>(std::move(statements));
    block_stmt->line = start_line;
    // Return as a base Statement pointer (caller might need to cast if BlockStmt features are needed)
    return block_stmt;
}

// ifStatement -> "if" "(" expression ")" statement ( "else" statement )? ;
std::unique_ptr<AST::Statement> Parser::ifStatement() {
    Token if_token = previous(); // The 'if' token
    int if_line = if_token.line;
    consume(TokenType::LEFT_PAREN, "Expect '(' after 'if'.");
    auto condition = expression(); // Parse the condition
    if (!condition) return nullptr;
    consume(TokenType::RIGHT_PAREN, "Expect ')' after if condition.");

    auto thenBranch = statement(); // Parse the 'then' branch (a single statement)
    if (!thenBranch) return nullptr;

    std::unique_ptr<AST::Statement> elseBranch = nullptr;
    if (match({TokenType::ELSE})) { // Check for an 'else' branch
        elseBranch = statement(); // Parse the 'else' branch
        if (!elseBranch) return nullptr;
    }

    // Create the IfStmt AST node
    auto if_stmt = std::make_unique<AST::IfStmt>(std::move(condition), std::move(thenBranch), std::move(elseBranch));
    if_stmt->line = if_line;
    return if_stmt;
}

// whileStatement -> "while" "(" expression ")" statement ;
std::unique_ptr<AST::Statement> Parser::whileStatement() {
    Token while_token = previous(); // The 'while' token
    int while_line = while_token.line;
    consume(TokenType::LEFT_PAREN, "Expect '(' after 'while'.");
    auto condition = expression(); // Parse the loop condition
    if (!condition) return nullptr;
    consume(TokenType::RIGHT_PAREN, "Expect ')' after while condition.");

    auto body = statement(); // Parse the loop body (a single statement)
    if (!body) return nullptr;

    // Create the WhileStmt AST node
    auto while_stmt = std::make_unique<AST::WhileStmt>(std::move(condition), std::move(body));
    while_stmt->line = while_line;
    return while_stmt;
}

// forStatement -> "for" "int" IDENTIFIER ":" "(" expression "," expression ")" statement ;
std::unique_ptr<AST::Statement> Parser::forStatement() {
    Token keyword = previous(); // The 'for' token
    int for_line = keyword.line;

    // Expect 'int' type specifier for the loop variable
    Token var_type = parsePrimitiveTypeSpecifier();
    if (var_type.type != TokenType::INT) {
        throw error(var_type, "For loop variable must be of type 'int'.");
    }

    Token var_name = consume(TokenType::IDENTIFIER, "Expect loop variable name.");
    consume(TokenType::COLON, "Expect ':' after loop variable.");
    consume(TokenType::LEFT_PAREN, "Expect '(' for range start.");
    auto range_start = expression(); // Parse the start expression of the range
    if (!range_start) return nullptr;
    consume(TokenType::COMMA, "Expect ',' separating range start and end.");
    auto range_end = expression(); // Parse the end expression of the range
    if (!range_end) return nullptr;
    consume(TokenType::RIGHT_PAREN, "Expect ')' after range end.");

    auto body = statement(); // Parse the loop body
    if (!body) return nullptr;

    // Create the ForStmt AST node
    auto for_stmt = std::make_unique<AST::ForStmt>(keyword, var_type, var_name, std::move(range_start), std::move(range_end), std::move(body));
    for_stmt->line = for_line;
    return for_stmt;
}

// returnStatement -> "return" expression? ";" ;
std::unique_ptr<AST::Statement> Parser::returnStatement() {
    Token keyword = previous(); // The 'return' token
    int return_line = keyword.line;
    std::unique_ptr<AST::Expression> value = nullptr;
    if (!check(TokenType::SEMICOLON)) { // Check if there's a return value
        value = expression(); // Parse the return value expression
        // If expression() returned null AND we are not at a semicolon, it's an invalid expression.
        // Note: expression() returning null might also mean a valid parse error occurred within it.
        if (!value && !isAtEnd() && !check(TokenType::SEMICOLON)) { // <<< Uses isAtEnd()
             throw error(peek(), "Invalid expression after 'return'.");
        }
    }
    consume(TokenType::SEMICOLON, "Expect ';' after return value.");
    // Create the ReturnStmt AST node (value can be nullptr)
    auto return_stmt = std::make_unique<AST::ReturnStmt>(keyword, std::move(value));
    return_stmt->line = return_line;
    return return_stmt;
}

// deleteStatement -> "delete" ( variable | indexExpr | mapAccessExpr ) ";" ;
std::unique_ptr<AST::Statement> Parser::deleteStatement() {
    Token keyword = previous(); // The 'delete' token
    int delete_line = keyword.line;
    // Parse the expression operand for delete
    auto expr_to_delete = expression();
    if (!expr_to_delete) return nullptr;

    // Validate that the expression is a valid target for deletion
    // It must be something that refers to a deletable location (variable, list index, map key)
    if (!(dynamic_cast<AST::VariableExpr*>(expr_to_delete.get()) ||
          dynamic_cast<AST::IndexExpr*>(expr_to_delete.get()) ||
          dynamic_cast<AST::MapAccessExpr*>(expr_to_delete.get()))) // Assuming MapAccessExpr exists for map['key']
    {
        throw error(keyword, "Operand for 'delete' must be a variable or an indexed element access.");
    }

    consume(TokenType::SEMICOLON, "Expect ';' after delete expression.");
    // Create the DeleteStmt AST node
    auto delete_stmt = std::make_unique<AST::DeleteStmt>(keyword, std::move(expr_to_delete));
    delete_stmt->line = delete_line;
    return delete_stmt;
}

// breakStatement -> "break" ";" ;
std::unique_ptr<AST::Statement> Parser::breakStatement() {
    Token keyword = previous(); // The 'break' token
    consume(TokenType::SEMICOLON, "Expect ';' after 'break'.");
    auto break_stmt = std::make_unique<AST::BreakStmt>(keyword);
    break_stmt->line = keyword.line;
    return break_stmt;
}

// continueStatement -> "continue" ";" ;
std::unique_ptr<AST::Statement> Parser::continueStatement() {
    Token keyword = previous(); // The 'continue' token
    consume(TokenType::SEMICOLON, "Expect ';' after 'continue'.");
    auto continue_stmt = std::make_unique<AST::ContinueStmt>(keyword);
    continue_stmt->line = keyword.line;
    return continue_stmt;
}


// --- Expression Parsing ---
// expression -> assignment ;
std::unique_ptr<AST::Expression> Parser::expression() { // <<< Definition is public
    return assignment(); // Start parsing from the lowest precedence level (assignment)
}

// assignment -> ( call "." IDENTIFIER | call "[" expression "]" ) "=" assignment | logicOr ;
// Note: This implementation only handles simple variable assignment (IDENTIFIER = assignment)
// and index/map assignment (call[index] = assignment). Property assignment (call.IDENTIFIER = assignment) is not handled here.
std::unique_ptr<AST::Expression> Parser::assignment() {
    auto expr = logicOr(); // Parse the left-hand side (could be a variable, index access, etc.)
    if (!expr) return nullptr;

    if (match({TokenType::EQUAL})) { // Check if it's an assignment operation
        Token equals = previous(); // The '=' token
        int equals_line = equals.line;
        auto value = assignment(); // Recursively parse the right-hand side (allows chained assignments like a = b = 5)
        if (!value) return nullptr;

        // Check if the left-hand side is a valid assignment target (l-value)
        if (dynamic_cast<AST::VariableExpr*>(expr.get()) ||
            dynamic_cast<AST::IndexExpr*>(expr.get()) ||
            dynamic_cast<AST::MapAccessExpr*>(expr.get())) // Assuming MapAccessExpr exists
        {
            // Create an AssignExpr AST node
            auto assign_expr = std::make_unique<AST::AssignExpr>(std::move(expr), equals, std::move(value));
            assign_expr->line = equals_line;
            return assign_expr; // Return the assignment expression
        }

        // If the left side is not a valid target, report an error
        error(equals, "Invalid assignment target."); // Throws SyntaxError
        return nullptr; // Should not be reached due to throw
    }

    return expr; // If no '=', return the parsed left-hand side expression
}

// Template helper for parsing left-associative binary operators
template<typename OperatorFunc>
std::unique_ptr<AST::Expression> Parser::parseLeftAssocBinary(Parser* parser, OperatorFunc matchOperators, OperandParserMemFn parseOperand) {
    auto expr = (parser->*parseOperand)(); // Parse the left operand
    if (!expr) return nullptr;

    // While the next token matches one of the operators for this precedence level
    while (parser->match(matchOperators())) {
        Token op = parser->previous(); // Get the operator token
        int op_line = op.line;
        auto right = (parser->*parseOperand)(); // Parse the right operand
        if (!right) return nullptr;
        // Combine the left expression, operator, and right expression into a new BinaryExpr node
        auto binary_expr = std::make_unique<AST::BinaryExpr>(std::move(expr), op, std::move(right));
        binary_expr->line = op_line;
        // The new binary expression becomes the left operand for the next iteration (left-associativity)
        expr = std::move(binary_expr);
    }
    return expr; // Return the final expression tree for this level
}

// Template helper for parsing left-associative logical operators (similar to binary, but creates LogicalExpr)
template<typename OperatorFunc>
std::unique_ptr<AST::Expression> Parser::parseLeftAssocLogical(Parser* parser, OperatorFunc matchOperators, OperandParserMemFn parseOperand) {
    auto expr = (parser->*parseOperand)(); // Parse the left operand
    if (!expr) return nullptr;

    while (parser->match(matchOperators())) {
        Token op = parser->previous(); // Get the logical operator token (AND, OR)
        int op_line = op.line;
        auto right = (parser->*parseOperand)(); // Parse the right operand
        if (!right) return nullptr;
        // Create a LogicalExpr node
        auto logical_expr = std::make_unique<AST::LogicalExpr>(std::move(expr), op, std::move(right));
        logical_expr->line = op_line;
        expr = std::move(logical_expr); // Update expr for left-associativity
    }
    return expr;
}


// logicOr -> logicAnd ( "or" logicAnd )* ;
std::unique_ptr<AST::Expression> Parser::logicOr() {
    // Use the template helper for 'or' operator, parsing 'logicAnd' as operands
    return parseLeftAssocLogical(this, []{ return std::vector<TokenType>{TokenType::OR}; }, &Parser::logicAnd);
}

// logicAnd -> equality ( "and" equality )* ;
std::unique_ptr<AST::Expression> Parser::logicAnd() {
    // Use the template helper for 'and' operator, parsing 'equality' as operands
    return parseLeftAssocLogical(this, []{ return std::vector<TokenType>{TokenType::AND}; }, &Parser::equality);
}

// equality -> comparison ( ( "!=" | "==" ) comparison )* ;
std::unique_ptr<AST::Expression> Parser::equality() {
    // Use the template helper for '==' and '!=', parsing 'comparison' as operands
    return parseLeftAssocBinary(this, []{ return std::vector<TokenType>{TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL}; }, &Parser::comparison);
}

// comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// Note: This implementation only parses one comparison operator.
// Chaining like a < b < c is parsed as (a < b) < c, which might not be intended semantically.
std::unique_ptr<AST::Expression> Parser::comparison() {
    auto expr = term(); // Parse the left operand (term precedence)
    if (!expr) return nullptr;

    // Check for ONE comparison operator
    if (match({TokenType::GREATER, TokenType::GREATER_EQUAL, TokenType::LESS, TokenType::LESS_EQUAL})) {
        Token op = previous(); // Get the operator
        auto right = term(); // Parse the right operand
        if (!right) return nullptr;
        // Create the BinaryExpr node for the comparison
        auto binary_expr = std::make_unique<AST::BinaryExpr>(std::move(expr), op, std::move(right));
        binary_expr->line = op.line;
        expr = std::move(binary_expr);
        // 'break;' was removed here to correctly allow parsing the rest of the expression after a comparison
        // However, this still parses a < b < c as (a < b) < c. Proper chaining requires different logic.
    }
    // If no comparison operator is found, just return the parsed term expression.
    return expr;
}


// term -> factor ( ( "-" | "+" ) factor )* ;
std::unique_ptr<AST::Expression> Parser::term() {
    // Use the template helper for '+' and '-', parsing 'factor' as operands
    return parseLeftAssocBinary(this, []{ return std::vector<TokenType>{TokenType::MINUS, TokenType::PLUS}; }, &Parser::factor);
}

// factor -> unary ( ( "/" | "*" | "%" ) unary )* ;
std::unique_ptr<AST::Expression> Parser::factor() {
    // Use the template helper for '*', '/', '%', parsing 'unary' as operands
    return parseLeftAssocBinary(this, []{ return std::vector<TokenType>{TokenType::SLASH, TokenType::STAR, TokenType::PERCENT}; }, &Parser::unary);
}

// unary -> ( "!" | "-" ) unary | pipe ;
std::unique_ptr<AST::Expression> Parser::unary() {
    if (match({TokenType::BANG, TokenType::MINUS})) { // Check for unary operators '!' or '-'
        Token op = previous(); // Get the operator token
        int op_line = op.line;
        auto right = unary(); // Recursively parse the operand (which is also a unary expression)
        if (!right) return nullptr;
        // Create the UnaryExpr AST node
        auto unary_expr = std::make_unique<AST::UnaryExpr>(op, std::move(right));
        unary_expr->line = op_line;
        return unary_expr;
    }
    // If no unary operator, parse the next higher precedence level (pipe)
    return pipe();
}

// pipe -> call ( "|" primary )* ; // Changed right operand to primary for simplicity
std::unique_ptr<AST::Expression> Parser::pipe() {
    auto expr = call(); // Parse the initial left-hand side expression (a call or primary)
    if (!expr) return nullptr;

    while (match({TokenType::PIPE})) { // While we find pipe operators '|'
        Token pipe_token = previous();
        int pipe_line = pipe_token.line;

        // The right-hand side of a pipe is expected to be something callable (like a function name/variable)
        // We parse it as 'primary' for simplicity, assuming it resolves to a callable entity at runtime.
        auto right_func_expr = primary();
        if (!right_func_expr) return nullptr; // Error parsing the function expression

        // Construct the arguments for the implicit call: the result of the left side is the first argument.
        std::vector<std::unique_ptr<AST::Expression>> arguments;
        arguments.push_back(std::move(expr)); // Move the previous expression result as the first argument

        // Create a CallExpr AST node representing the pipe operation.
        // The 'callee' is the expression on the right of the pipe.
        // We use a dummy LEFT_PAREN token for the CallExpr constructor.
        Token dummy_paren(TokenType::LEFT_PAREN, "(", std::monostate{}, pipe_line); // Create a placeholder '(' token
        auto call_expr = std::make_unique<AST::CallExpr>(std::move(right_func_expr), dummy_paren, std::move(arguments));
        call_expr->line = pipe_line; // Set line number

        // The result of this pipe operation becomes the left-hand side for the next potential pipe.
        expr = std::move(call_expr);
    }

    return expr; // Return the final expression after all pipes are processed
}


// call -> primary ( "(" arguments? ")" | "[" expression "]" )* ;
// arguments -> expression ( "," expression )* ;
std::unique_ptr<AST::Expression> Parser::call() {
    auto expr = primary(); // Start by parsing a primary expression (literal, variable, grouping, etc.)
    if (!expr) return nullptr;

    // Loop to handle chained calls or index accesses like func()() or list[0][1]
    while (true) {
        if (match({TokenType::LEFT_PAREN})) { // If we see '(', it's a function call
            expr = finishCall(std::move(expr)); // Pass the parsed primary as the callee
            if (!expr) return nullptr; // Propagate error from finishCall
        } else if (match({TokenType::LEFT_BRACKET})) { // If we see '[', it's an index access
            expr = finishIndexAccess(std::move(expr)); // Pass the parsed primary as the collection being indexed
            if (!expr) return nullptr; // Propagate error from finishIndexAccess
        } else {
            break; // No more calls or index accesses, exit the loop
        }
    }
    return expr; // Return the final expression (potentially a CallExpr or IndexExpr)
}

// Helper to finish parsing a function call after the '(' has been matched
std::unique_ptr<AST::Expression> Parser::finishCall(std::unique_ptr<AST::Expression> callee) {
    std::vector<std::unique_ptr<AST::Expression>> arguments;
    Token paren = previous(); // The '(' token that was just matched
    int paren_line = paren.line;

    if (!check(TokenType::RIGHT_PAREN)) { // Check if there are arguments inside the parentheses
        do {
            if (arguments.size() >= 255) {
                error(peek(), "Cannot have more than 255 arguments."); // Enforce limit
                // error() throws, so we don't need to return nullptr explicitly
            }
            auto arg_expr = expression(); // Parse each argument expression
            if (!arg_expr) return nullptr; // Propagate error
            arguments.push_back(std::move(arg_expr));
        } while (match({TokenType::COMMA})); // Continue parsing arguments if a comma is found
    }

    consume(TokenType::RIGHT_PAREN, "Expect ')' after arguments.");
    // Create the CallExpr AST node
    auto call_expr = std::make_unique<AST::CallExpr>(std::move(callee), paren, std::move(arguments));
    call_expr->line = paren_line; // Set line number to the '('
    return call_expr;
}

// Helper to finish parsing an index access after the '[' has been matched
std::unique_ptr<AST::Expression> Parser::finishIndexAccess(std::unique_ptr<AST::Expression> collection) {
    Token bracket = previous(); // The '[' token that was just matched
    int bracket_line = bracket.line;

    auto index_expr = expression(); // Parse the expression inside the brackets
    if (!index_expr) return nullptr; // Propagate error

    consume(TokenType::RIGHT_BRACKET, "Expect ']' after index expression.");
    // Create the IndexExpr AST node (assuming this AST node exists)
    // It takes the collection being indexed, the '[' token, and the index expression
    auto index_node = std::make_unique<AST::IndexExpr>(std::move(collection), bracket, std::move(index_expr));
    index_node->line = bracket_line; // Set line number to the '['
    return index_node;
}


// primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER | mapLiteral | listLiteral | INTERPOLATED_STRING ;
std::unique_ptr<AST::Expression> Parser::primary() {
    int current_line = peek().line; // Get line number early for literals

    if (match({TokenType::FALSE})) return std::make_unique<AST::LiteralExpr>(Value(false), current_line);
    if (match({TokenType::TRUE})) return std::make_unique<AST::LiteralExpr>(Value(true), current_line);
    if (match({TokenType::NIL})) return std::make_unique<AST::LiteralExpr>(Value(), current_line); // Assuming Value() constructs nil

    if (match({TokenType::NUMBER_INT})) {
        // Ensure the token's literal variant holds an int before accessing
        if (std::holds_alternative<int>(previous().literal)) {
            return std::make_unique<AST::LiteralExpr>(Value(std::get<int>(previous().literal)), current_line);
        } else {
            // This indicates an internal inconsistency (lexer tagged as INT but literal is wrong type)
            error(previous(), "Internal error: Expected int literal.");
            return nullptr; // error() throws, but return nullptr for safety
        }
    }
    if (match({TokenType::NUMBER_FLOAT})) {
        // Ensure the token's literal variant holds a double
        if (std::holds_alternative<double>(previous().literal)) {
            return std::make_unique<AST::LiteralExpr>(Value(std::get<double>(previous().literal)), current_line);
        } else {
            error(previous(), "Internal error: Expected float literal.");
            return nullptr;
        }
    }
    if (match({TokenType::STRING})) {
        // Ensure the token's literal variant holds a string
        if (std::holds_alternative<std::string>(previous().literal)) {
            return std::make_unique<AST::LiteralExpr>(Value(std::get<std::string>(previous().literal)), current_line);
        } else {
            error(previous(), "Internal error: Expected string literal.");
            return nullptr;
        }
    }

    // --- Handling Interpolated Strings ---
    if (match({TokenType::INTERPOLATED_STRING})) {
        Token token = previous(); // The INTERPOLATED_STRING token itself
        int interp_line = token.line; // Line where the string starts

        // Check if the token's literal holds the expected vector of pairs
        if (std::holds_alternative<InterpolatedStringData>(token.literal)) {
            // Move the vector of pairs from the token's literal variant.
            // The AST::InterpolatedStringExpr constructor needs to accept this vector.
            // The Interpreter will handle evaluating the string parts at runtime.
            auto interp_data = std::get<InterpolatedStringData>(std::move(token.literal)); // Get the vector

            // Create the AST node, passing the token and the vector of parts
            auto interp_expr = std::make_unique<AST::InterpolatedStringExpr>(token, std::move(interp_data));
            interp_expr->line = interp_line; // Use line from token
            return interp_expr;

        } else {
            // If the token is INTERPOLATED_STRING but doesn't hold the right data type, it's an internal error.
            error(token, "Internal error: Expected vector<pair<...>> data from lexer for interpolated string.");
            return nullptr;
        }
    }
    // --- End Interpolated String Handling ---


    if (match({TokenType::IDENTIFIER})) {
        Token name_token = previous(); // The identifier token
        // Create a VariableExpr AST node
        auto var_expr = std::make_unique<AST::VariableExpr>(name_token);
        var_expr->line = name_token.line; // Set line number
        return var_expr;
    }

    if (match({TokenType::LEFT_PAREN})) { // Grouping expression
        auto expr = expression(); // Parse the expression inside the parentheses
        if (!expr) return nullptr; // Propagate error
        consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.");
        // Return the inner expression directly (parentheses only affect precedence)
        return expr;
    }

    if (match({TokenType::LEFT_BRACE})) { // Map literal starts with '{'
        return mapLiteral();
    }
    if (match({TokenType::LEFT_BRACKET})) { // List literal starts with '['
        return listLiteral();
    }


    // If none of the above primary expressions match, it's a syntax error
    throw error(peek(), "Expect expression.");
}

// mapLiteral -> "{" ( expression ":" expression ( "," expression ":" expression )* )? "}" ;
std::unique_ptr<AST::Expression> Parser::mapLiteral() {
    Token opening_brace = previous(); // The '{' token
    int brace_line = opening_brace.line;
    std::vector<std::pair<std::unique_ptr<AST::Expression>, std::unique_ptr<AST::Expression>>> elements;

    if (!check(TokenType::RIGHT_BRACE)) { // Check if the map is not empty
        do {
            auto key_expr = expression(); // Parse the key expression
            if (!key_expr) return nullptr;
            consume(TokenType::COLON, "Expect ':' after map key expression.");
            auto value_expr = expression(); // Parse the value expression
            if (!value_expr) return nullptr;
            // Store the key-value pair
            elements.emplace_back(std::move(key_expr), std::move(value_expr));
        } while (match({TokenType::COMMA})); // Continue if a comma separates pairs
    }

    consume(TokenType::RIGHT_BRACE, "Expect '}' after map elements.");
    // Create the MapLiteralExpr AST node
    auto map_lit = std::make_unique<AST::MapLiteralExpr>(opening_brace, std::move(elements));
    map_lit->line = brace_line;
    return map_lit;
}

// listLiteral -> "[" ( expression ( "," expression )* )? "]" ;
std::unique_ptr<AST::Expression> Parser::listLiteral() {
    Token opening_bracket = previous(); // The '[' token
    int bracket_line = opening_bracket.line;
    std::vector<std::unique_ptr<AST::Expression>> elements;

    if (!check(TokenType::RIGHT_BRACKET)) { // Check if the list is not empty
        do {
            auto element_expr = expression(); // Parse list element expression
            if (!element_expr) return nullptr;
            elements.push_back(std::move(element_expr));
        } while (match({TokenType::COMMA})); // Continue if comma separates elements
    }

    consume(TokenType::RIGHT_BRACKET, "Expect ']' after list elements.");
    // Create the ListLiteralExpr AST node
    auto list_lit = std::make_unique<AST::ListLiteralExpr>(opening_bracket, std::move(elements));
    list_lit->line = bracket_line;
    return list_lit;
}

// --- Type Parsing ---

// Parses only primitive types (used in map keys, for loops)
Token Parser::parsePrimitiveTypeSpecifier() {
    if (match({TokenType::INT, TokenType::FLOAT, TokenType::BOOL, TokenType::STRING_TYPE, TokenType::OBJ, TokenType::NIL})) {
        return previous(); // Return the matched type token
    }
    // If none match, throw an error
    throw error(peek(), "Expect primitive type specifier (int, float, bool, string, obj, nil).");
}

// Parses a full type specifier, including map<K,V> and list<T>
// Returns: <primary_token, optional<key_token>, optional<value_token>, optional<list_element_token>>
std::tuple<Token, std::optional<Token>, std::optional<Token>, std::optional<Token>> Parser::parseType() {
    if (match({TokenType::MAP})) {
        Token map_token = previous(); // The 'map' token
        consume(TokenType::LEFT_BRACKET, "Expect '[' after 'map'.");
        // Key type MUST be a primitive type
        Token key_type = parsePrimitiveTypeSpecifier();
        consume(TokenType::COMMA, "Expect ',' separating map key and value types.");
        // Value type can be any type (primitive, map, list, function)
        auto [value_primary_type, value_map_key, value_map_val, value_list_elem] = parseType();
        // Warn about nested complex types within map values (currently parsed but maybe not fully used later)
        if (value_map_key || value_list_elem) {
            debug_log << "Warning: Nested map/list types in map value type at line " << value_primary_type.line << " are parsed but may not be fully utilized yet." << std::endl;
        }
        consume(TokenType::RIGHT_BRACKET, "Expect ']' after map value type.");
        // Return map token, key token, value's primary token, and nullopt for list element
        return {map_token, key_type, value_primary_type, std::nullopt};

    } else if (match({TokenType::LIST})) {
        Token list_token = previous(); // The 'list' token
        std::optional<Token> element_type_token = std::nullopt;
        // Optional element type specification within brackets []
        if (match({TokenType::LEFT_BRACKET})) {
            // Parse the element type (can be any type)
            auto [elem_primary_type, elem_map_key, elem_map_val, elem_list_elem] = parseType();
            // Warn about nested complex types within list elements
             if (elem_map_key || elem_list_elem) {
                 debug_log << "Warning: Nested map/list types in list element type at line " << elem_primary_type.line << " are parsed but may not be fully utilized yet." << std::endl;
             }
            element_type_token = elem_primary_type; // Store the primary token of the element type
            consume(TokenType::RIGHT_BRACKET, "Expect ']' after list element type.");
        }
        // Return list token, nullopt for map keys/values, and the optional element type token
        return {list_token, std::nullopt, std::nullopt, element_type_token};

    } else {
        // Handle primitive types and 'function' type
        if (match({TokenType::INT, TokenType::FLOAT, TokenType::BOOL, TokenType::STRING_TYPE, TokenType::OBJ, TokenType::NIL, TokenType::FUNCTION})) {
            Token primary_type = previous(); // Get the matched type token
            // Return the primary type token, nullopt for all complex type parts
            return {primary_type, std::nullopt, std::nullopt, std::nullopt};
        } else {
            // If no valid type specifier is found
            throw error(peek(), "Expect type specifier (int, float, bool, string, obj, nil, map, list, function).");
        }
    }
}


// --- Utility Methods ---

// Checks if the current token matches any of the given types. If so, consumes it and returns true.
bool Parser::match(const std::vector<TokenType>& types) {
    for (TokenType type : types) {
        if (check(type)) { // Check if current token matches
            advance(); // Consume the token
            return true; // Match found
        }
    }
    return false; // No match found
}

// Checks if the current token is of the given type without consuming it.
bool Parser::check(TokenType type) const {
    if (isAtEnd()) return false; // Cannot check if at the end // <<< Uses isAtEnd()
    return peek().type == type;
}

// Consumes the current token and returns the *previous* token.
Token Parser::advance() {
    if (!isAtEnd()) current++; // Move to the next token // <<< Uses isAtEnd()
    return previous(); // Return the token that was just consumed
}

// <<< ADDED: Definition for isAtEnd >>>
// Checks if the parser has reached the end of the token stream.
bool Parser::isAtEnd() const {
    // Check if index is out of bounds OR if the current token is EOF
    // Note: We check >= size() because the 'tokens' vector might be empty.
    // peek() handles the case where current == tokens.size() by returning EOF.
    return current >= static_cast<int>(tokens.size()) || peek().type == TokenType::END_OF_FILE;
}
// <<< END ADDED >>>


// Returns the current token without consuming it.
Token Parser::peek() const {
    // Ensure current is within bounds before accessing tokens[current]
    if (current >= static_cast<int>(tokens.size())) {
        // Return a synthetic EOF token if past the end
        // Ensure line number is reasonable, maybe use last token's line or -1
        int last_line = tokens.empty() ? -1 : tokens.back().line;
        return Token(TokenType::END_OF_FILE, "", std::monostate{}, last_line);
    }
    // Cast current to size_t only when known to be valid index
    return tokens[static_cast<size_t>(current)];
}

// Returns the most recently consumed token.
Token Parser::previous() const {
    if (current == 0 || tokens.empty()) {
        // Handle edge case of being at the beginning
        return Token(TokenType::UNKNOWN, "", std::monostate{}, -1); // Or some other default
    }
    size_t prev_index = static_cast<size_t>(current) - 1;
    // Boundary check (shouldn't be necessary if current > 0 and not empty, but safe)
    if (prev_index >= tokens.size()) {
         return Token(TokenType::UNKNOWN, "", std::monostate{}, -1); // Should not happen
    }
    return tokens[prev_index];
}

// Checks if the current token is of the expected type. If yes, consumes it. If no, throws a SyntaxError.
Token Parser::consume(TokenType type, const std::string& message) {
    if (check(type)) return advance(); // Consume and return if type matches
    // If type doesn't match, throw a syntax error
    throw error(peek(), message);
}

// Creates and returns a SyntaxError object (intended to be thrown).
SyntaxError Parser::error(const Token& token, const std::string& message) {
    std::string full_message;
    if (token.type == TokenType::END_OF_FILE) {
        full_message = message + " at end";
    } else {
        // Include the problematic lexeme in the error message
        full_message = message + " at '" + token.lexeme + "'";
    }
    // Return the error object, including the line number
    return SyntaxError(full_message, token.line);
    // Note: This function *creates* the error; the caller must *throw* it.
}

// Error recovery: Advance tokens until a likely statement boundary or synchronization point is found.
void Parser::synchronize() {
    advance(); // Consume the token that caused the error

    while (!isAtEnd()) { // <<< Uses isAtEnd()
        // Stop synchronizing if the previous token was a semicolon (likely end of a statement)
        if (previous().type == TokenType::SEMICOLON) {
             debug_log << "DEBUG: Synchronize stopped after SEMICOLON." << std::endl;
             return;
        }

        // Check if the *current* token looks like the start of a new statement/declaration
        switch (peek().type) {
            case TokenType::CLASS: // Add other relevant keywords
            case TokenType::FN:
            case TokenType::VAR:
            case TokenType::INT: // Type keywords
            case TokenType::FLOAT:
            case TokenType::BOOL:
            case TokenType::STRING_TYPE:
            case TokenType::OBJ:
            case TokenType::MAP:
            case TokenType::LIST:
            case TokenType::FUNCTION:
            case TokenType::FOR: // Statement keywords
            case TokenType::IF:
            case TokenType::WHILE:
            case TokenType::PRINT:
            case TokenType::RETURN:
            case TokenType::DELETE:
            case TokenType::BREAK:
            case TokenType::CONTINUE:
                // Found a potential recovery point, stop synchronizing.
                debug_log << "DEBUG: Synchronize found recovery token: " << peek().toString() << std::endl;
                return;
            default:
                // Keep consuming tokens if not a recovery point
                break;
        }
        advance(); // Move to the next token
    }
    // Reached end of file while synchronizing
    debug_log << "DEBUG: Synchronize reached EOF." << std::endl;
}
