# Hexweave Language

Hexweave is a dynamically-typed scripting language implemented in C++. This repository contains the source code for its interpreter.

## Current Features

* **Variables:**
    * Declaration using `var identifier = value;` (type inferred from value).
    * Declaration with explicit types: `int i = 10;`, `string s;`, `list[int] nums = [1, 2];`, `map[string, float] data = {"pi": 3.14};`, etc.
    * Supported types: `int`, `float`, `bool`, `string`, `list`, `map`, `function`, `obj` (generic object/any type), `nil`.
    * Assignment: `identifier = new_value;` (type checking enforced if variable was declared with an explicit type).
    * Deletion: `delete identifier;`, `delete list[index];`, `delete map[key];`
* **Data Types:**
    * **Numbers:** `int` (e.g., `10`, `-5`), `float` (e.g., `3.14`, `-0.5`).
    * **Booleans:** `true`, `false`.
    * **Nil:** `nil` (representing absence of value).
    * **Strings:**
        * Literal strings: `"Hello, world!"` (supports `\"` and `\\` escapes).
        * Interpolated strings: `` `Hello {name + "!"}, value is {calculate()} `` (expressions inside `{...}` are evaluated, `{{` and `}}` produce literal braces).
    * **Lists:** Ordered collections, e.g., `[1, "two", true, [3]]`. Accessed via integer index (0-based, negative indices allowed): `myList[0]`, `myList[-1]`.
    * **Maps:** Key-value pairs, e.g., `{"name": "Hex", "version": 0.1, 1: true}`. Keys can be `int`, `float`, `bool`, `string`. Accessed via key: `myMap["name"]`, `myMap[1]`.
* **Operators:**
    * Arithmetic: `+`, `-`, `*`, `/`, `%` (integers and floats). `+` also concatenates strings.
    * Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`.
    * Logical: `and`, `or` (short-circuiting).
    * Unary: `-` (negation), `!` (logical NOT).
    * Pipe: `|` (passes result of left expression as first argument to function on right).
    * Index Access: `[]` (for lists and maps).
    * Assignment: `=`.
* **Control Flow:**
    * Conditional: `if (condition) { ... } else if (condition) { ... } else { ... }`
    * While Loop: `while (condition) { ... }`
    * For Loop (Integer Range): `for int i : (start, end) { ... }` (iterates `i` from `start` up to, but not including, `end`).
    * Loop Control: `break;`, `continue;`
* **Functions:**
    * Declaration: `fn functionName(type1 param1, type2 param2) : returnType { ... return value; }`
    * Return type specification is mandatory.
    * Parameters require type specification.
    * Function calls: `functionName(arg1, arg2)`
    * Return statement: `return value;` or `return;` (returns `nil`).
* **Input/Output:**
    * `print(arg1, arg2, ...);` (prints arguments separated by spaces, followed by newline).
* **Comments:**
    * Single-line: `// This is a comment`
    * Multi-line: `/* This is a \n block comment */` (supports nesting).
* **Native Functions:**
    * `assert(condition)`: Throws runtime error if condition is false.
    * `len(string|list|map)`: Returns length of string, list, or map.
    * `type(value)`: Returns string representation of the value's type (e.g., "int", "string", "list").
    * `input(prompt_string)`: Reads a line of text from standard input.
    * `cast(value, type_string)`: Attempts to cast value to the specified type ("int", "float", "string", "bool").
    * `append(list, value)`: Appends value to the end of a list.
    * `remove(list, index)`: Removes element at the given index from a list.
* **Error Handling:** Runtime errors (type mismatches, division by zero, index out of bounds, etc.) and parse errors are reported with line numbers.

## Planned Features

* **Import System:** `import "filename.hex";` (Work in Progress)
* Classes & Objects (`class` keyword exists but is not yet implemented).
* More built-in functions.

## Building & Running

*(Add instructions here on how to build the interpreter using CMake/Make/VS etc. and how to run a .hex file)*

