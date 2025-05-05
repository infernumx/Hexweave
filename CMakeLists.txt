# Minimum CMake version required
cmake_minimum_required(VERSION 3.10)

# Project name
project(CustomLangInterpreter CXX)

# Require C++17 standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF) # Prefer not to use compiler-specific extensions

# Define the executable
# Ensure ALL source files are listed correctly
add_executable(interpreter
    src/main.cpp
    src/ast.cpp         # <<< Definition source file
    src/common.cpp
    src/environment.cpp
    src/interpreter.cpp
    src/lexer.cpp
    src/parser.cpp
    src/token.cpp
    src/value.cpp       # <<< Definition source file
)

# Specify include directories
target_include_directories(interpreter PUBLIC include)

# Optional: Enable warnings (recommended)
if(MSVC)
    target_compile_options(interpreter PRIVATE /W4) # Enable high warning level for MSVC
    # Consider adding /WX to treat warnings as errors during debugging
    # target_compile_options(interpreter PRIVATE /WX)
else()
    # Assume GCC/Clang
    target_compile_options(interpreter PRIVATE -Wall -Wextra -Wpedantic)
    # target_compile_options(interpreter PRIVATE -Werror) # Treat warnings as errors
endif()

# Print configuration summary
message(STATUS "Compiler: ${CMAKE_CXX_COMPILER_ID}")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Install prefix: ${CMAKE_INSTALL_PREFIX}")
