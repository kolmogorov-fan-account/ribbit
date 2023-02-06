#include <type_traits>
#include <utility>
#include <array>

namespace p2098 {
    template<typename, template<typename...> typename>
    struct is_specialization_of : std::false_type {
    };

    template<template<typename...> typename Primary, typename... Args>
    struct is_specialization_of<Primary<Args...>, Primary> : std::true_type {
    };

    template<typename T, template<typename...> typename Primary>
    inline constexpr bool is_specialization_of_v = is_specialization_of<T, Primary>::value;
}

/// Consider the type system of Java.
/// - A small, closed set of primitive types (byte, short, int, ...).
/// - An open set of user-defined types (object).
///
/// C++ system is similar in some ways.
/// - There is also a small, closed set of primitive types (char, std::int32_t, ...).
/// - There is also an open set of user-defined types (typename).
///
/// It is also very different in other ways. First, there is no automatic conversion (boxing/unboxing) between
/// primitives and user-defined types. Fortunately, we can still do it manually with wrappers, which are already
/// provided in the standard library. Let's give them shorter names.

template<char c>
using Char = std::integral_constant<char, c>;

template<int i>
using Int = std::integral_constant<int, i>;

template<std::size_t s>
using Size = std::integral_constant<std::size_t, s>;

/// Like Java, values must be given a type when declared. However, in C++, there is a syntactical distinction when
/// accessing non-primitive values. Whether a member m in Java is a primitive or an object, we can access it with o.m.
/// In C++, we access a primitive with o::m and a user-defined type with typename o::m (C++ keywords are often weird).
/// Since typename is kind of long for something so common, lets abbreviate it with $, since it is a kind of sigil like
/// those found in Perl or PHP.

#define $ typename

/// Let's take a closer look at "decode_92", a function that decodes base 92 integers from Ribbit bytecode.
///
/// In C++, you declare functions with the struct keyword. This is an unusual keyword for functions and I think it comes
/// from the fact that functions in C++ are "structured", unlike functions in C which can contain gotos. Another
/// seemingly strange keyword is "template", to introduce the list of parameters. However, this one makes a lot of sense
/// if you think of this list as a "template" for the actual arguments!
///
/// Finally, in C++ return values have a name and they are introduced with "static constexpr" for primitives or "using"
/// for non primitives. In this case, this is not only to be different from other languages, but allows functions to
/// have multiple return values! We will see examples later on.

template<char c>
struct decode_92 {
    static constexpr char value = c < 35 ? 57 : c - 35;
};

/// For functions with a single return value, it is conventional to provide a shortcut to avoid naming the return value
/// every time.

template<char c>
inline constexpr char decode_92_v = decode_92<c>::value;

static_assert(decode_92_v<'!'> == 57);
static_assert(decode_92_v<'#'> == 0);

/// Now that we have seen how to write functions, let's look at data structures. The reader might be surprised to
/// discover that there is no built-in way to define them (like a "class" declaration in Java). Theoretical computer
/// science is usually useless, but once in a while it can teach us a useful trick. In this case, it is what's called
/// "Church encoding" and it shows that data structures can be encoded by functions. In most languages, using this
/// encoding is very unwieldy, but it is not so bad in C++. In most languages, a data structure becomes a function that
/// take a callback with the values of its fields. In our case, we can leverage multiple return values to access
/// multiple fields directly.
///
/// Here is a possible encoding of a linked list, which will be useful later.

struct Nil {
};

template<$ H, $ T>
struct Cons {
    using Head = H;
    using Tail = T;
};

template<$ T>
concept $List = std::same_as<T, Nil> || p2098::is_specialization_of_v<T, Cons>;

//template<typename List>
//using head = typename List::Head;
//
//template<typename List>
//using tail = typename List::Tail;
//
//template<char get(std::size_t), typename indices>
//struct list_of_string;
//
//template<char get(std::size_t)>
//struct list_of_string<get, std::index_sequence<>> {
//    using List = Nil;
//};
//
//template<char get(std::size_t), std::size_t head, std::size_t ... tail>
//struct list_of_string<get, std::index_sequence<head, tail ...>> {
//    using Head = Char<get(head)>;
//    using Tail = typename list_of_string<get, std::index_sequence<tail ...>>::List;
//
//    using List = Cons<Head, Tail>;
//};
//
//#define STR(s)                                                                 \
//    typename list_of_string<                                                     \
//        [] (std::size_t i) constexpr { return (s)[i]; },                       \
//        std::make_index_sequence<sizeof(s) - 1> /* -1 because of the null.  */ \
//    >::List
//
//static_assert(std::is_same_v<STR("allo"), Cons<Char<'a'>, Cons<Char<'l'>, Cons<Char<'l'>, Cons<Char<'o'>, Nil>>>>>);
//
//using Source = STR(");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y");

template<std::size_t Size>
struct CharSequence {
    std::array<char, Size - 1> data_;

    template<std::size_t... Indices>
    constexpr CharSequence(std::index_sequence<Indices...>, char const (&data)[Size]) : data_{data[Indices]...} {}

    constexpr CharSequence(char const (&data)[Size]) : CharSequence(std::make_index_sequence<Size - 1>{}, data) {}
};

template<CharSequence Chars, std::size_t... Indices>
constexpr auto
make_source_details(std::index_sequence<Indices...>) -> std::integer_sequence<char, Chars.data_[Indices]...> {
    return {};
}

template<CharSequence Chars>
struct MakeSourceImpl {
    using Seq = decltype(make_source_details<Chars>(std::make_index_sequence<Chars.data_.size()>{}));
};

template<CharSequence Chars>
using MakeSource = MakeSourceImpl<Chars>::Seq;

template<char... Chars>
using Source = std::integer_sequence<char, Chars...>;

template<typename T, char... Chars>
concept $Source = std::same_as<T, Source<Chars...>>;

using Input = MakeSource<");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y">;

template<$, $, $>
struct Rib {
};

using NIL = Rib<Int<0>, Int<0>, Int<5>>;
using FALSE = Rib<Int<1>, Int<0>, Int<5>>;
using TRUE = Rib<Int<2>, Int<0>, Int<5>>;

/// Decodes a variable-length, base 46 size_t.
template<std::size_t acc, char... source>
struct decode_46_impl;

template<std::size_t acc, char head, char... tail> requires (decode_92_v<head> < 46)
struct decode_46_impl<acc, head, tail...> {
    static constexpr int Value = acc * 46 + decode_92_v<head>;
    using Rest = Source<tail...>;
};

template<std::size_t acc, char head, char... tail> requires (decode_92_v<head> >= 46)
struct decode_46_impl<acc, head, tail...> {
    using Loop = decode_46_impl<acc * 46 + decode_92_v<head> - 46, tail...>;

    static constexpr int Value = Loop::Value;
    using Rest = $ Loop::Rest;
};

template<char... source>
struct decode_46 {
    using Loop = decode_46_impl<0, source...>;

    static constexpr int Value = Loop::Value;
    using Rest = $ Loop::Rest;
};

template<std::size_t i>
struct Symbol {
    using Value = Size<i>;
};

template<std::size_t i>
struct Slot {
    using Value = Size<i>;
};

struct OpCall {
    static constexpr char opcode_min = 23;
    static constexpr char s = 20;

    template<std::size_t i>
    using ArgKind = Symbol<i>;

    template<std::size_t i>
    using AltArgKind = Slot<i>;
};

struct OpSet {
    static constexpr char opcode_min = 56;
    static constexpr char s = 0;

    template<std::size_t i>
    using ArgKind = Slot<i>;

    template<std::size_t i>
    using AltArgKind = Symbol<i>;
};

struct OpGet {
    static constexpr char opcode_min = 59;
    static constexpr char s = 10;

    template<std::size_t i>
    using ArgKind = Slot<i>;

    template<std::size_t i>
    using AltArgKind = Symbol<i>;
};

struct OpConst {
    static constexpr char opcode_min = 72;
    static constexpr char s = 11;

    template<std::size_t i>
    using ArgKind = Size<i>;

    template<std::size_t i>
    using AltArgKind = Symbol<i>;
};

struct OpIf {
};

constexpr char opcode_jump_short_min = 0;
constexpr char opcode_jump_short_max = 19;
constexpr char opcode_jump_variable_length = 20;
constexpr char opcode_jump_slot = 21;

template<$ Source, $ Stack>
struct decode_program;

template<char byte, $ OpCode>
concept is_short_encoding = requires(decode_92<byte> code) {
    requires
    (code.value >= OpCode::opcode_min) &&
    (code.value < OpCode::opcode_min + OpCode::s);
};

template<typename OpCode, $ Stack, char head, char... tail>
struct decode_short {
    static constexpr auto i = decode_92_v<head> - OpCode::opcode_min;
    using Op = Rib<OpCode, $ OpCode::template ArgKind<i>, $ Stack::Head>;

    using Program = $ decode_program<Cons<Op, $ Stack::Tail>, tail...>::Program;
};

template<char byte, $ OpCode>
concept is_variable_length_encoding = (decode_92_v<byte> == OpCode::opcode_min + OpCode::s);

template<$ OpCode, $ Stack, char... source>
struct decode_variable_length {
    using Arg = decode_46<source...>;
    using Op = Rib<OpCode, $ OpCode::template ArgKind<Arg::Value>, $ Stack::Head>;

    using Program = $ decode_program<Cons<Op, $ Stack::Tail>, $ >::Program;
};

template<char byte, typename OpCode>
concept is_alternative_encoding = requires(decode_92<byte> code) {
    requires
    (code.value == OpCode::opcode_min + OpCode::s + 1) ||
    (code.value == OpCode::opcode_min + OpCode::s + 2);
};

template<typename OpCode, char h, typename T, typename Stack>
struct decode_alternative {
    using Arg = decode_46<T, decode_92_v<h> - (OpCode::opcode_min + OpCode::s + 1)>;
    using Op = Rib<OpCode, typename OpCode::template AltArgKind<Arg::Value>, head<Stack>>;

    using Program = typename decode_program<typename Arg::Rest, Cons<Op, tail<Stack>>>::Program;
};

template<typename H>
struct decode_program<Nil, Cons<H, Nil>> {
    using Program = H;
};

/// Short encoding of jumps.
template<char h, typename T, typename Stack> requires
(decode_92_v<h> >= opcode_jump_short_min)
&& (decode_92_v<h> <= opcode_jump_short_max)
struct decode_program<Cons<Char<h>, T>, Stack> {
    static constexpr auto i = decode_92_v<h> - opcode_jump_short_min;
    using Op = Rib<OpCall, Symbol<i>, Int<0>>;

    using Program = typename decode_program<T, Cons<Op, Stack>>::Program;
};

/// Variable encoding of jumps
template<typename T, typename Stack>
struct decode_program<Cons<Char<opcode_jump_variable_length>, T>, Stack> {
    using Arg = decode_46<T>;
    using Op = Rib<OpCall, Symbol<Arg::Value>, Int<0>>;

    using Program = typename decode_program<typename Arg::Rest, Cons<Op, Stack>>::Program;
};

/// Jump to a slot
template<char h, typename T, typename Stack> requires
(decode_92_v<h> == opcode_jump_slot)
|| (decode_92_v<h> == opcode_jump_slot + 1)
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Arg = decode_46<T, h == opcode_jump_slot ? 0 : 1>;
    using Op = Rib<OpCall, Slot<Arg::Value>, Int<0>>;

    using Program = typename decode_program<typename Arg::Rest, Cons<Op, Stack>>::Program;
};

template<char h, typename T, typename Stack> requires is_short_encoding<h, OpCall>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_short<OpCall, h, T, Stack>::Program;
};

template<char h, typename T, typename Stack> requires is_variable_length_encoding<h, OpCall>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_variable_length<OpCall, T, Stack>::Program;
};

template<char h, typename T, typename Stack> requires is_alternative_encoding<h, OpCall>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_alternative<OpCall, h, T, Stack>::Program;
};

template<char h, typename T, typename Stack> requires is_variable_length_encoding<h, OpSet>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_variable_length<OpSet, T, Stack>::Program;
};

template<char h, typename T, typename Stack> requires is_alternative_encoding<h, OpSet>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_alternative<OpSet, h, T, Stack>::Program;
};

template<char h, typename T, typename Stack> requires is_short_encoding<h, OpGet>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_short<OpGet, h, T, Stack>::Program;
};

template<char h, typename T, typename Stack> requires is_variable_length_encoding<h, OpGet>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_variable_length<OpGet, T, Stack>::Program;
};

template<char h, typename T, typename Stack> requires is_alternative_encoding<h, OpGet>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_alternative<OpGet, h, T, Stack>::Program;
};

template<char h, typename T, typename Stack> requires is_short_encoding<h, OpConst>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_short<OpConst, h, T, Stack>::Program;
};

template<char h, typename T, typename Stack> requires is_variable_length_encoding<h, OpConst>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_variable_length<OpConst, T, Stack>::Program;
};

template<char h, typename T, typename Stack> requires is_alternative_encoding<h, OpConst>
struct decode_program<Cons<Char<h>, T>, Stack> {
    using Program = typename decode_alternative<OpConst, h, T, Stack>::Program;
};

using HelloProgram = typename decode_program<Source, Nil>::Program;

int main() {
    return 0;
}
