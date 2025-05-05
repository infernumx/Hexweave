#include "value.hpp"
#include "environment.hpp"
#include "interpreter.hpp"
#include "ast.hpp"
#include <stdexcept>
#include <sstream>
#include <cmath>     // For std::fabs, std::fmod
#include <limits>    // For numeric_limits
#include <vector>
#include <iostream> // For debug cerr, std::cout in input()
#include <string>   // For std::stoi, stod
#include <memory>   // For std::shared_ptr comparison

// --- ValueHash Implementation ---
std::size_t ValueHash::operator()(const Value& val) const {
    // Use std::visit to apply a lambda based on the actual type held by the variant.
    return std::visit([](const auto& v) -> std::size_t {
        using T = std::decay_t<decltype(v)>;
        // Use std::hash for basic types that have standard hash implementations.
        if constexpr (std::is_same_v<T, Value::NilType>) {
            return 0; // Hash nil consistently to 0.
        } else if constexpr (std::is_same_v<T, Value::BoolType> ||
                             std::is_same_v<T, Value::IntType> ||
                             std::is_same_v<T, Value::FloatType> ||
                             std::is_same_v<T, Value::StringType>) {
            return std::hash<T>{}(v);
        } else if constexpr (std::is_same_v<T, Value::FunctionType> ||
                             std::is_same_v<T, Value::ObjectType> ||
                             std::is_same_v<T, Value::MapType> ||
                             std::is_same_v<T, Value::ListType>) { // <<< ADDED ListType
            // For shared_ptr types (functions, objects, maps, lists), hash the pointer address.
            return std::hash<std::shared_ptr<void>>{}(std::static_pointer_cast<void>(v));
        }
        return 0; // Should not happen if all variant types are handled.
    }, val.data);
}

// --- ValueEqual Implementation ---
bool ValueEqual::operator()(const Value& lhs, const Value& rhs) const {
    return lhs == rhs;
}


// --- ValueType to String ---
std::string valueTypeToString(ValueType type) {
    switch (type) {
        case ValueType::NIL: return "nil";
        case ValueType::BOOL: return "bool";
        case ValueType::INT: return "int";
        case ValueType::FLOAT: return "float";
        case ValueType::STRING: return "string";
        case ValueType::OBJECT: return "obj"; // Placeholder type name
        case ValueType::FUNCTION: return "function";
        case ValueType::MAP: return "map";
        case ValueType::LIST: return "list"; // <<< ADDED LIST case
        case ValueType::NATIVE_FUNCTION: return "native_function";
        default: return "unknown";
    }
}

// --- Value Methods ---

// Constructor/Destructor Definitions
Value::Value() : data(NilType{}) {}
Value::Value(bool val) : data(val) {}
Value::Value(int val) : data(val) {}
Value::Value(double val) : data(val) {}
Value::Value(const char* val) : data(std::string(val)) {}
Value::Value(std::string val) : data(std::move(val)) {}
Value::Value(std::shared_ptr<Callable> func) : data(std::move(func)) {}
Value::Value(ObjectType obj_ptr) : data(std::move(obj_ptr)) {}
Value::Value(MapType map_ptr) : data(std::move(map_ptr)) {}
Value::Value(ListType list_ptr) : data(std::move(list_ptr)) {}
Value::~Value() = default;


// --- Type Checking Methods ---
ValueType Value::getType() const {
    return std::visit([](const auto& v) -> ValueType {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, NilType>) return ValueType::NIL;
        else if constexpr (std::is_same_v<T, BoolType>) return ValueType::BOOL;
        else if constexpr (std::is_same_v<T, IntType>) return ValueType::INT;
        else if constexpr (std::is_same_v<T, FloatType>) return ValueType::FLOAT;
        else if constexpr (std::is_same_v<T, StringType>) return ValueType::STRING;
        else if constexpr (std::is_same_v<T, ObjectType>) return ValueType::OBJECT;
        else if constexpr (std::is_same_v<T, FunctionType>) {
            if (v && dynamic_cast<LangFunction*>(v.get())) return ValueType::FUNCTION;
            else return ValueType::NATIVE_FUNCTION;
        }
        else if constexpr (std::is_same_v<T, MapType>) return ValueType::MAP;
        else if constexpr (std::is_same_v<T, ListType>) return ValueType::LIST;
        else return ValueType::NIL;
    }, data);
}

bool Value::isNil() const { return std::holds_alternative<NilType>(data); }
bool Value::isBool() const { return std::holds_alternative<BoolType>(data); }
bool Value::isInt() const { return std::holds_alternative<IntType>(data); }
bool Value::isFloat() const { return std::holds_alternative<FloatType>(data); }
bool Value::isNumber() const { return isInt() || isFloat(); }
bool Value::isString() const { return std::holds_alternative<StringType>(data); }
bool Value::isObject() const { return std::holds_alternative<ObjectType>(data); }
bool Value::isFunction() const { return std::holds_alternative<FunctionType>(data); }
bool Value::isCallable() const { return isFunction(); }
bool Value::isMap() const { return std::holds_alternative<MapType>(data); }
bool Value::isList() const { return std::holds_alternative<ListType>(data); }


bool Value::isTruthy() const {
    if (isNil()) return false;
    if (isBool()) return std::get<BoolType>(data);
    if (isString() && std::get<StringType>(data).empty()) return false;
    if (isInt() && std::get<IntType>(data) == 0) return false;
    if (isFloat() && std::fabs(std::get<FloatType>(data)) < std::numeric_limits<double>::epsilon()) return false;
    return true;
}

// --- Accessors ---
Value::BoolType Value::getBool() const { return std::get<BoolType>(data); }
Value::IntType Value::getInt() const { return std::get<IntType>(data); }
Value::FloatType Value::getFloat() const { return std::get<FloatType>(data); }
Value::StringType Value::getString() const { return std::get<StringType>(data); }
Value::FunctionType Value::getFunction() const { return std::get<FunctionType>(data); }
Value::ObjectType Value::getObject() const { return std::get<ObjectType>(data); }
Value::MapType Value::getMap() const { return std::get<MapType>(data); }

Value::MapDataType& Value::getMapData() {
    if (!isMap()) throw TypeError("Cannot get map data from non-map type '" + valueTypeToString(getType()) + "'.");
    MapType& ptr = std::get<MapType>(data);
    if (!ptr) throw RuntimeError("Internal error: Map value pointer is null.");
    return *ptr;
}
const Value::MapDataType& Value::getMapData() const {
    if (!isMap()) throw TypeError("Cannot get map data from non-map type '" + valueTypeToString(getType()) + "'.");
    const MapType& ptr = std::get<MapType>(data);
    if (!ptr) throw RuntimeError("Internal error: Map value pointer is null.");
    return *ptr;
}
Value::ListType Value::getList() const {
     if (!isList()) throw TypeError("Cannot get list from non-list type '" + valueTypeToString(getType()) + "'.");
    return std::get<ListType>(data);
}
Value::ListDataType& Value::getListData() {
    if (!isList()) throw TypeError("Cannot get list data from non-list type '" + valueTypeToString(getType()) + "'.");
    ListType& ptr = std::get<ListType>(data);
    if (!ptr) throw RuntimeError("Internal error: List value pointer is null.");
    return *ptr;
}
const Value::ListDataType& Value::getListData() const {
    if (!isList()) throw TypeError("Cannot get list data from non-list type '" + valueTypeToString(getType()) + "'.");
    const ListType& ptr = std::get<ListType>(data);
    if (!ptr) throw RuntimeError("Internal error: List value pointer is null.");
    return *ptr;
}

Value& Value::getObjectValue() {
    if (!isObject()) throw TypeError("Cannot get object value from non-object type '" + valueTypeToString(getType()) + "'.");
    ObjectType& ptr = std::get<ObjectType>(data);
    if (!ptr) throw RuntimeError("Internal error: Object value pointer is null.");
    return *ptr;
}
const Value& Value::getObjectValue() const {
    if (!isObject()) throw TypeError("Cannot get object value from non-object type '" + valueTypeToString(getType()) + "'.");
    const ObjectType& ptr = std::get<ObjectType>(data);
    if (!ptr) throw RuntimeError("Internal error: Object value pointer is null.");
    return *ptr;
}

double Value::asNumber() const {
    if (isInt()) return static_cast<double>(getInt());
    if (isFloat()) return getFloat();
    throw TypeError("Value of type '" + valueTypeToString(getType()) + "' cannot be converted to a number.");
}

std::string Value::toString() const {
    std::ostringstream oss;
    std::visit([&oss, this](const auto& v) {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, NilType>) { oss << "nil"; }
        else if constexpr (std::is_same_v<T, BoolType>) { oss << (v ? "true" : "false"); }
        else if constexpr (std::is_same_v<T, IntType>) { oss << v; }
        else if constexpr (std::is_same_v<T, FloatType>) { oss << v; }
        else if constexpr (std::is_same_v<T, StringType>) { oss << v; }
        else if constexpr (std::is_same_v<T, ObjectType>) {
            if (v) oss << "obj(" << v->toString() << ")";
            else oss << "obj(null_ptr)";
        } else if constexpr (std::is_same_v<T, FunctionType>) {
            if (v) oss << v->toString();
            else oss << "<null function>";
        } else if constexpr (std::is_same_v<T, MapType>) {
            if (!v) { oss << "{<null map>}"; return; }
            oss << "{";
            bool first = true;
            for (const auto& pair : *v) {
                if (!first) { oss << ", "; } first = false;
                if (pair.first.isString()) { oss << "\"" << pair.first.getString() << "\""; }
                else { oss << pair.first.toString(); }
                oss << ": ";
                if (pair.second.isString()) { oss << "\"" << pair.second.getString() << "\""; }
                else if (pair.second.isMap() && pair.second.getMap().get() == v.get()) { oss << "{...}"; }
                else { oss << pair.second.toString(); }
            }
            oss << "}";
        }
        else if constexpr (std::is_same_v<T, ListType>) {
            if (!v) { oss << "[<null list>]"; return; }
            oss << "[";
            bool first = true;
            for (const auto& element : *v) {
                if (!first) { oss << ", "; } first = false;
                if (element.isString()) { oss << "\"" << element.getString() << "\""; }
                else if (element.isList() && element.getList().get() == v.get()) { oss << "[...]"; }
                else { oss << element.toString(); }
            }
            oss << "]";
        }
        else { oss << "<unknown value type>"; }
    }, data);
    return oss.str();
}

// --- Comparison Operators ---
bool Value::operator==(const Value& other) const {
    if (data.index() != other.data.index()) {
        if (this->isNumber() && other.isNumber()) {
            constexpr double epsilon = 1e-9;
            return std::fabs(this->asNumber() - other.asNumber()) < epsilon;
        }
        return false;
    }
    return std::visit([&](const auto& self_val) -> bool {
        const auto& other_val = std::get<std::decay_t<decltype(self_val)>>(other.data);
        using T = std::decay_t<decltype(self_val)>;
        if constexpr (std::is_same_v<T, NilType>) { return true; }
        else if constexpr (std::is_same_v<T, BoolType>) { return self_val == other_val; }
        else if constexpr (std::is_same_v<T, IntType>) { return self_val == other_val; }
        else if constexpr (std::is_same_v<T, FloatType>) { constexpr double epsilon = 1e-9; return std::fabs(self_val - other_val) < epsilon; }
        else if constexpr (std::is_same_v<T, StringType>) { return self_val == other_val; }
        else if constexpr (std::is_same_v<T, FunctionType>) { return self_val == other_val; }
        else if constexpr (std::is_same_v<T, ObjectType>) { if (!self_val && !other_val) return true; if (!self_val || !other_val) return false; return self_val == other_val; }
        else if constexpr (std::is_same_v<T, MapType>) { if (!self_val && !other_val) return true; if (!self_val || !other_val) return false; return self_val == other_val; }
        else if constexpr (std::is_same_v<T, ListType>) {
             if (!self_val && !other_val) return true;
             if (!self_val || !other_val) return false;
             // Deep comparison for lists
             if (self_val->size() != other_val->size()) return false;
             for (size_t i = 0; i < self_val->size(); ++i) {
                 if ((*self_val)[i] != (*other_val)[i]) return false;
             }
             return true;
        }
        else { return false; }
    }, data);
}
bool Value::operator!=(const Value& other) const { return !(*this == other); }
bool Value::operator<(const Value& other) const {
    if (this->isNumber() && other.isNumber()) { return this->asNumber() < other.asNumber(); }
    if (this->isString() && other.isString()) { return this->getString() < other.getString(); }
    throw TypeError("Operands must be numbers or strings for operator '<'.");
}
bool Value::operator<=(const Value& other) const {
    if (this->isNumber() && other.isNumber()) { return this->asNumber() <= other.asNumber(); }
    if (this->isString() && other.isString()) { return this->getString() <= other.getString(); }
    throw TypeError("Operands must be numbers or strings for operator '<='.");
}
bool Value::operator>(const Value& other) const {
     if (this->isNumber() && other.isNumber()) { return this->asNumber() > other.asNumber(); }
    if (this->isString() && other.isString()) { return this->getString() > other.getString(); }
    throw TypeError("Operands must be numbers or strings for operator '>'.");
}
bool Value::operator>=(const Value& other) const {
     if (this->isNumber() && other.isNumber()) { return this->asNumber() >= other.asNumber(); }
    if (this->isString() && other.isString()) { return this->getString() >= other.getString(); }
    throw TypeError("Operands must be numbers or strings for operator '>='.");
}

// --- Output Stream Operator ---
std::ostream& operator<<(std::ostream& os, const Value& value) {
    os << value.toString();
    return os;
}


// --- LangFunction Methods ---
LangFunction::LangFunction(AST::FunctionStmt* decl, std::shared_ptr<Environment> closure_env)
    : declaration(decl), closure(std::move(closure_env)) {}

int LangFunction::arity() const {
    if (!declaration) return 0;
    return static_cast<int>(declaration->params.size());
}

Value LangFunction::call(Interpreter& interpreter, const std::vector<Value>& arguments) {
    auto environment = std::make_shared<Environment>(closure);
    if (!declaration) { throw RuntimeError("Internal error: Calling null function declaration."); }
    for (size_t i = 0; i < declaration->params.size(); ++i) {
        environment->define(declaration->params[i].second.lexeme, arguments[i]);
    }
    try {
        interpreter.executeBlock(declaration->body->statements, environment);
    } catch (const ReturnValue& returnValue) {
        try {
            interpreter.checkType(returnValue.value, declaration->return_type,
                                  declaration->map_return_key_type_token,
                                  declaration->map_return_value_type_token,
                                  declaration->list_return_element_type_token,
                                  "return value of function '" + declaration->name.lexeme + "'",
                                  returnValue.line);
        } catch (const TypeError& type_error) {
            throw TypeError(type_error.what(), returnValue.line);
        }
        return returnValue.value;
    }
    Value implicit_return_value;
    try {
         interpreter.checkType(implicit_return_value, declaration->return_type,
                               declaration->map_return_key_type_token,
                               declaration->map_return_value_type_token,
                               declaration->list_return_element_type_token,
                               "implicit return from function '" + declaration->name.lexeme + "'",
                               declaration->name.line);
    } catch (const TypeError& type_error) {
         throw TypeError(type_error.what(), declaration->name.line);
    }
    return implicit_return_value;
}

std::string LangFunction::toString() const {
    if (!declaration) return "<invalid fn>";
    return "<fn " + declaration->name.lexeme + ">";
}

// --- NativeAssert Implementation ---
int NativeAssert::arity() const { return 1; }
Value NativeAssert::call(Interpreter& /*interpreter*/, const std::vector<Value>& arguments) {
    if (!arguments[0].isTruthy()) {
        throw RuntimeError("Assertion failed.", -1);
    }
    return Value();
}
std::string NativeAssert::toString() const { return "<native fn assert>"; }

// --- NativeLen Implementation ---
int NativeLen::arity() const { return 1; }
Value NativeLen::call(Interpreter& /*interpreter*/, const std::vector<Value>& arguments) {
    const Value& arg = arguments[0];
    if (arg.isString()) {
        return Value(static_cast<int>(arg.getString().length()));
    } else if (arg.isMap()) {
        try { return Value(static_cast<int>(arg.getMapData().size())); }
        catch (const std::runtime_error& e) { throw RuntimeError(e.what(), -1); }
    } else if (arg.isList()) {
        try { return Value(static_cast<int>(arg.getListData().size())); }
        catch (const std::runtime_error& e) { throw RuntimeError(e.what(), -1); }
    } else {
        throw TypeError("Object of type '" + valueTypeToString(arg.getType()) + "' has no len().", -1);
    }
}
std::string NativeLen::toString() const { return "<native fn len>"; }

// --- NativeType Implementation ---
int NativeType::arity() const { return 1; }
Value NativeType::call(Interpreter& /*interpreter*/, const std::vector<Value>& arguments) {
    const Value& arg = arguments[0];
    ValueType type = arg.getType();
    std::string type_name = valueTypeToString(type);
    return Value(type_name);
}
std::string NativeType::toString() const { return "<native fn type>"; }

// --- NativeInput Implementation ---
int NativeInput::arity() const { return -1; }
Value NativeInput::call(Interpreter& /*interpreter*/, const std::vector<Value>& arguments) {
    if (arguments.size() > 1) {
        throw RuntimeError("input() takes 0 or 1 arguments (" + std::to_string(arguments.size()) + " given).", -1);
    }
    if (arguments.size() == 1) {
        const Value& prompt = arguments[0];
        if (!prompt.isString()) {
            throw TypeError("input() prompt must be a string.", -1);
        }
        std::cout << prompt.getString();
        std::cout.flush();
    }
    std::string line;
    if (!std::getline(std::cin, line)) {
        if (std::cin.eof()) { return Value(); }
        else { throw RuntimeError("Error reading input.", -1); }
    }
    return Value(line);
}
std::string NativeInput::toString() const { return "<native fn input>"; }

// --- NativeCast Implementation ---
int NativeCast::arity() const { return 2; }
Value NativeCast::call(Interpreter& /*interpreter*/, const std::vector<Value>& arguments) {
    const Value& value_to_cast = arguments[0];
    const Value& type_name_val = arguments[1];
    if (!type_name_val.isString()) { throw TypeError("Target type for cast() must be a string (e.g., \"int\", \"float\").", -1); }
    std::string target_type_name = type_name_val.getString();
    ValueType source_type = value_to_cast.getType();
    ValueType target_type;
    if (target_type_name == "int") target_type = ValueType::INT;
    else if (target_type_name == "float") target_type = ValueType::FLOAT;
    else if (target_type_name == "string") target_type = ValueType::STRING;
    else if (target_type_name == "bool") target_type = ValueType::BOOL;
    else if (target_type_name == "nil") target_type = ValueType::NIL;
    else { throw RuntimeError("Invalid target type string '" + target_type_name + "' for cast().", -1); }
    switch (target_type) {
        case ValueType::INT:
            if (source_type == ValueType::INT) return value_to_cast;
            if (source_type == ValueType::FLOAT) return Value(static_cast<int>(value_to_cast.getFloat()));
            if (source_type == ValueType::BOOL) return Value(value_to_cast.getBool() ? 1 : 0);
            if (source_type == ValueType::STRING) { try { return Value(std::stoi(value_to_cast.getString())); } catch (const std::exception&) { throw RuntimeError("Cannot cast string '" + value_to_cast.getString() + "' to int.", -1); } }
            break;
        case ValueType::FLOAT:
            if (source_type == ValueType::FLOAT) return value_to_cast;
            if (source_type == ValueType::INT) return Value(static_cast<double>(value_to_cast.getInt()));
            if (source_type == ValueType::BOOL) return Value(value_to_cast.getBool() ? 1.0 : 0.0);
            if (source_type == ValueType::STRING) { try { return Value(std::stod(value_to_cast.getString())); } catch (const std::exception&) { throw RuntimeError("Cannot cast string '" + value_to_cast.getString() + "' to float.", -1); } }
            break;
        case ValueType::BOOL: return Value(value_to_cast.isTruthy());
        case ValueType::STRING: return Value(value_to_cast.toString());
        case ValueType::NIL: if (source_type == ValueType::NIL) return Value(); break;
        default: break;
    }
    throw TypeError("Cannot cast type '" + valueTypeToString(source_type) + "' to '" + target_type_name + "'.", -1);
}
std::string NativeCast::toString() const { return "<native fn cast>"; }


// --- NativeListAppend Implementation ---
int NativeListAppend::arity() const { return 2; } // list, element
Value NativeListAppend::call(Interpreter& /*interpreter*/, const std::vector<Value>& arguments) {
    // <<< FIX: Get a non-const *copy* of the first argument Value >>>
    // We need to modify the list, but the arguments vector is const.
    // Getting a copy allows us to call non-const methods on it.
    // The modification will affect the original list because the
    // underlying data is managed by a shared_ptr.
    Value list_val = arguments[0];
    const Value& element_to_add = arguments[1];

    if (!list_val.isList()) {
        throw TypeError("First argument to append() must be a list, got " + valueTypeToString(list_val.getType()) + ".", -1);
    }
    try {
        // <<< Now list_val is non-const, so getListData() returns non-const ref >>>
        auto& list_data = list_val.getListData();
        // TODO: Add type checking if list has declared element type
        list_data.push_back(element_to_add); // This should now work
        return Value(); // Append returns nil
    } catch (const std::runtime_error& e) {
        throw RuntimeError(e.what(), -1);
    }
}
std::string NativeListAppend::toString() const { return "<native fn append>"; }


// --- NativeListRemove Implementation ---
int NativeListRemove::arity() const { return 2; } // list, index
Value NativeListRemove::call(Interpreter& /*interpreter*/, const std::vector<Value>& arguments) {
    // <<< FIX: Get a non-const *copy* of the first argument Value >>>
    Value list_val = arguments[0];
    const Value& index_val = arguments[1];

    if (!list_val.isList()) {
        throw TypeError("First argument to remove() must be a list, got " + valueTypeToString(list_val.getType()) + ".", -1);
    }
     if (!index_val.isInt()) {
        throw TypeError("Second argument (index) to remove() must be an integer, got " + valueTypeToString(index_val.getType()) + ".", -1);
    }

    int index = index_val.getInt();
    try {
        // <<< Now list_val is non-const, so getListData() returns non-const ref >>>
        auto& list_data = list_val.getListData();
        size_t size = list_data.size();

        // Handle negative indices.
        int effective_index = index;
        if (index < 0) {
            effective_index = static_cast<int>(size) + index;
        }

        // Bounds check.
        if (effective_index < 0 || static_cast<size_t>(effective_index) >= size) {
             throw RuntimeError("List index out of bounds for remove() (" + std::to_string(index) +
                                " for list of size " + std::to_string(size) + ").", -1);
        }

        // Erase element using iterator arithmetic and return the removed value.
        Value removed_value = list_data[static_cast<size_t>(effective_index)]; // Copy value before erasing
        list_data.erase(list_data.begin() + effective_index); // This should now work
        return removed_value;

    } catch (const std::runtime_error& e) {
         throw RuntimeError(e.what(), -1);
    }
}
std::string NativeListRemove::toString() const { return "<native fn remove>"; }
