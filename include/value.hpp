#ifndef VALUE_HPP
#define VALUE_HPP

#include "common.hpp" // Include common first
#include <string>
#include <vector> // <<< Need vector for ListDataType
#include <variant>
#include <memory> // For shared_ptr
#include <functional> // For std::function, std::hash
#include <iostream> // For printing
#include <unordered_map>

// --- Forward Declarations ---
class Interpreter;
class Environment;
namespace AST { class FunctionStmt; }
class Value;

// --- Custom Hasher for Value ---
// Needed if Values are used as keys in unordered_map (like our MapDataType)
struct ValueHash { std::size_t operator()(const Value& val) const; };

// --- Custom Equality Comparator for Value ---
// Needed if Values are used as keys in unordered_map
struct ValueEqual { bool operator()(const Value& lhs, const Value& rhs) const; };


// --- Type Enum ---
// <<< ADDED LIST >>>
enum class ValueType { NIL, BOOL, INT, FLOAT, STRING, OBJECT, FUNCTION, MAP, LIST, NATIVE_FUNCTION };
// Declaration of the conversion function (defined in value.cpp)
std::string valueTypeToString(ValueType type);

// --- Callable Interface ---
// Abstract base class for things that can be called (functions)
class Callable {
public:
    virtual ~Callable() = default;
    // Returns the number of arguments the callable expects, or -1 for variadic.
    virtual int arity() const = 0;
    // Executes the callable's logic.
    virtual Value call(Interpreter& interpreter, const std::vector<Value>& arguments) = 0;
    // Returns a string representation (e.g., "<fn name>", "<native fn assert>").
    virtual std::string toString() const = 0;
};

// --- Language Function Representation ---
// Represents a user-defined function in Hexweave.
class LangFunction : public Callable {
public:
    AST::FunctionStmt* declaration; // Pointer to the AST node defining the function
    std::shared_ptr<Environment> closure; // Environment captured when the function was defined
    LangFunction(AST::FunctionStmt* decl, std::shared_ptr<Environment> closure_env);
    int arity() const override;
    Value call(Interpreter& interpreter, const std::vector<Value>& arguments) override;
    std::string toString() const override;
};

// --- Native Assert Function ---
class NativeAssert : public Callable {
public:
    int arity() const override;
    Value call(Interpreter& interpreter, const std::vector<Value>& arguments) override;
    std::string toString() const override;
};

// --- Native Len Function ---
class NativeLen : public Callable {
public:
    int arity() const override;
    Value call(Interpreter& interpreter, const std::vector<Value>& arguments) override;
    std::string toString() const override;
};

// --- Native Type Function ---
class NativeType : public Callable {
public:
    int arity() const override;
    Value call(Interpreter& interpreter, const std::vector<Value>& arguments) override;
    std::string toString() const override;
};

// --- Native Input Function Declaration ---
class NativeInput : public Callable {
public:
    int arity() const override;
    Value call(Interpreter& interpreter, const std::vector<Value>& arguments) override;
    std::string toString() const override;
};

// --- Native Cast Function Declaration ---
class NativeCast : public Callable {
public:
    int arity() const override;
    Value call(Interpreter& interpreter, const std::vector<Value>& arguments) override;
    std::string toString() const override;
};

// --- Native List Append Function Declaration ---
class NativeListAppend : public Callable {
public:
    int arity() const override;
    Value call(Interpreter& interpreter, const std::vector<Value>& arguments) override;
    std::string toString() const override;
};

// --- Native List Remove Function Declaration ---
class NativeListRemove : public Callable {
public:
    int arity() const override;
    Value call(Interpreter& interpreter, const std::vector<Value>& arguments) override;
    std::string toString() const override;
};


// --- Value Class ---
// Represents any value in the Hexweave language.
// Uses std::variant for type-safe storage of different underlying types.
class Value {
public:
    // Type aliases for clarity
    using NilType = std::monostate; // Represents nil
    using BoolType = bool;
    using IntType = int;
    using FloatType = double;
    using StringType = std::string;
    using FunctionType = std::shared_ptr<Callable>; // Can hold LangFunction or NativeFunction
    using ObjectType = std::shared_ptr<Value>; // For future complex objects (placeholder)
    using MapDataType = std::unordered_map<Value, Value, ValueHash, ValueEqual>; // Underlying map storage
    using MapType = std::shared_ptr<MapDataType>; // Shared ptr to map data
    // <<< ADDED List types >>>
    using ListDataType = std::vector<Value>;         // Underlying list storage
    using ListType = std::shared_ptr<ListDataType>; // Shared ptr to list data


    // Storage variant holds one of the possible types.
    // <<< ADDED ListType to variant >>>
    std::variant< NilType, BoolType, IntType, FloatType, StringType, ObjectType, FunctionType, MapType, ListType > data;

    // Constructors
    Value(); // Defaults to nil
    Value(bool val);
    Value(int val);
    Value(double val);
    Value(const char* val); // Convenience for string literals
    Value(std::string val);
    Value(std::shared_ptr<Callable> func);
    explicit Value(ObjectType obj_ptr); // Explicit to avoid accidental conversion
    explicit Value(MapType map_ptr);   // Explicit
    explicit Value(ListType list_ptr); // <<< ADDED List constructor (explicit)
    // Default copy/move constructors/assignment operators are sufficient
    // because the variant handles the underlying types correctly (including shared_ptr).
    Value(const Value&) = default;
    Value& operator=(const Value&) = default;
    Value(Value&&) = default;
    Value& operator=(Value&&) = default;
    ~Value(); // Default destructor is fine


    // Type Checking Methods
    ValueType getType() const; // Returns the ValueType enum
    bool isNil() const;
    bool isBool() const;
    bool isInt() const;
    bool isFloat() const;
    bool isNumber() const; // Checks if Int or Float
    bool isString() const;
    bool isObject() const; // Placeholder for future objects
    bool isFunction() const; // Checks if FunctionType (Lang or Native)
    bool isCallable() const; // Synonym for isFunction currently
    bool isMap() const;
    bool isList() const; // <<< ADDED isList check

    // Truthiness Check (used in if/while conditions)
    bool isTruthy() const;

    // Accessors (throw TypeError if type doesn't match)
    BoolType getBool() const;
    IntType getInt() const;
    FloatType getFloat() const;
    StringType getString() const;
    FunctionType getFunction() const;
    ObjectType getObject() const;
    MapType getMap() const; // Returns the shared_ptr to the map data
    MapDataType& getMapData(); // Returns a reference to the underlying map (throws if null/wrong type)
    const MapDataType& getMapData() const; // Const version
    ListType getList() const; // <<< ADDED List accessor (returns shared_ptr)
    ListDataType& getListData(); // <<< ADDED List data accessor (returns reference, throws)
    const ListDataType& getListData() const; // <<< ADDED const List data accessor
    Value& getObjectValue(); // Placeholder for accessing underlying object
    const Value& getObjectValue() const; // Placeholder

    // Conversion Helper (throws TypeError if not number)
    double asNumber() const;

    // String Representation (used for printing, concatenation)
    std::string toString() const;

    // Comparison Operators (defined in value.cpp)
    // Implement comparison logic based on type.
    bool operator==(const Value& other) const;
    bool operator!=(const Value& other) const;
    bool operator<(const Value& other) const;
    bool operator<=(const Value& other) const;
    bool operator>(const Value& other) const;
    bool operator>=(const Value& other) const;

private:
    // Helper template for equality comparison (used internally by operator==)
    template <typename T>
    bool compareEquality(const Value& other) const;
};

// --- Output Stream Operator ---
// Allows printing Values directly using std::cout << value;
std::ostream& operator<<(std::ostream& os, const Value& value);


#endif // VALUE_HPP
