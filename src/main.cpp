#include "lexer.hpp"
#include "parser.hpp"
#include "interpreter.hpp"
#include "ast.hpp"
#include "common.hpp" // Includes fstream and declares debug_log
#include <iostream>
#include <fstream> // Included via common.hpp, but good practice here too
#include <sstream>
#include <vector>
#include <memory>
#include <string>
#include <exception>

// Define the global debug log stream
std::ofstream debug_log;

// --- Function Prototypes ---
void run(const std::string& filename, const std::string& source, const std::vector<std::string>& source_lines, Interpreter& interpreter);
void runFile(const std::string& path, Interpreter& interpreter);
void runPrompt(Interpreter& interpreter);

// Helper to get source line (adjusting for 0-based index)
std::string getSourceLine(const std::vector<std::string>& lines, int line_num) {
    if (line_num >= 1 && static_cast<size_t>(line_num) <= lines.size()) {
        return lines[static_cast<size_t>(line_num) - 1];
    }
    return "<Source line not available>";
}


// --- Main Entry Point ---
int main(int argc, char* argv[]) {
    // Open the debug log file
    debug_log.open("debug_log.txt", std::ios::out | std::ios::trunc); // Overwrite existing log
    if (!debug_log.is_open()) {
        std::cerr << "FATAL: Could not open debug_log.txt for writing." << std::endl;
        return 1; // Indicate failure
    }
    debug_log << "--- Debug Log Started ---" << std::endl;


    // Create the interpreter instance once
    Interpreter interpreter;

    if (argc > 2) {
        std::cout << "Usage: interpreter [script]" << std::endl;
        debug_log << "Usage error: Incorrect number of arguments." << std::endl; // Log usage error
        return 64; // Exit code indicating incorrect command line usage
    } else if (argc == 2) {
        // Run script from file
        runFile(argv[1], interpreter);
    } else {
        // Run interactive prompt (REPL)
        runPrompt(interpreter);
    }

    debug_log << "--- Debug Log Finished ---" << std::endl; // Log end
    debug_log.close(); // Close the log file
    return 0; // Success
}

// --- Execution Functions ---

// Executes a given source code string
void run(const std::string& filename, const std::string& source, const std::vector<std::string>& source_lines, Interpreter& interpreter) {
    debug_log << "===== Running Source: " << filename << " =====" << std::endl; // Log start
    // 1. Lexing
    debug_log << "--- Lexing ---" << std::endl; // Log phase
    Lexer lexer(source);
    std::vector<Token> tokens = lexer.scanTokens();
    debug_log << "Lexing complete. " << tokens.size() << " tokens found." << std::endl; // Log result

    // 2. Parsing
    debug_log << "--- Parsing ---" << std::endl; // Log phase
    Parser parser(tokens);
    std::vector<std::unique_ptr<AST::Statement>> statements = parser.parse(); // Parser logs its own debug

    debug_log << "DEBUG main::run: Parser returned " << statements.size() << " statements." << std::endl;


    // Check for parser errors (indicated by nullptrs)
    bool parse_error = false;
    for(size_t i = 0; i < statements.size(); ++i) {
        if (!statements[i]) {
            parse_error = true;
             debug_log << "DEBUG main::run: Found NULL statement pointer at index " << i << "." << std::endl; // Log null pointer
            // Don't necessarily stop, interpreter loop will skip nulls
        }
    }
    // Report if parser had issues *before* interpretation
    if (parse_error) {
         debug_log << "WARNING: Parsing produced null statements. Interpretation may be incomplete." << std::endl;
    }
    debug_log << "Parsing complete." << std::endl; // Log result

    // 3. Interpretation
    debug_log << "--- Interpreting ---" << std::endl; // Log phase
    debug_log << "DEBUG main::run: Passing " << statements.size() << " statements to interpreter." << std::endl;

    // <<< FIX: Re-add try...catch block around interpret call >>>
    try {
        interpreter.interpret(statements, filename, source_lines);
    } catch (const LangError& error) {
        // Catch the first language error and report it
        interpreter.reportError(error, filename, source_lines);
        // Execution stops here
    } catch (const std::exception& e) { // Catch other standard exceptions
        std::cerr << filename << ":? | System Error: " << e.what() << std::endl;
        debug_log << filename << ":? | System Error: " << e.what() << std::endl;
    } catch (...) { // Catch any non-standard exceptions
         std::cerr << filename << ":? | Unknown Error Occurred during interpretation." << std::endl;
         debug_log << filename << ":? | Unknown Error Occurred during interpretation." << std::endl;
    }
    // <<< End Fix >>>

    debug_log << "===== Run Finished: " << filename << " =====" << std::endl; // Log end

}

// Reads source code from a file and runs it
void runFile(const std::string& path, Interpreter& interpreter) {
    std::ifstream file(path);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open file '" << path << "'" << std::endl; // Keep user-facing error on cerr
        debug_log << "Error: Could not open file '" << path << "'" << std::endl; // Log file error
        return;
    }

    std::string line;
    std::vector<std::string> source_lines;
    std::stringstream source_buffer;

    while (std::getline(file, line)) {
        source_lines.push_back(line);
        source_buffer << line << '\n';
    }
    file.close();

    run(path, source_buffer.str(), source_lines, interpreter);
}

// Runs an interactive Read-Eval-Print Loop (REPL)
void runPrompt(Interpreter& interpreter) {
    std::string line;
    std::vector<std::string> current_line_vec(1);
    int line_num = 0;

    std::cout << "> "; // Keep user prompt on cout
    while (std::getline(std::cin, line)) {
         if (line.empty()) break;

         line_num++;
         current_line_vec[0] = line;

        run("<stdin>", line + "\n", current_line_vec, interpreter);

        std::cout << "> "; // Keep user prompt on cout
    }
    std::cout << std::endl;
}
