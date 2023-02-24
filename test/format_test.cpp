#include <ktl/fmt/format.hpp>

#include "platform.h"

using namespace ktl;
using namespace ktl::fmt::literals;

template<fmt::fixed_string FmtStr>
static void print();

template<fmt::fixed_string Str, std::integral T, fmt::detail::int_base Base>
struct from_chars {
    static constexpr auto value = []() -> std::optional<T> {
        T v;
        auto res = fmt::detail::from_chars<Base>(std::begin(Str.value), std::end(Str.value), v);
        if (res.ec != std::errc {}) {
            return {};
        }
        return v;
    }();
};

template<
    fmt::fixed_string Str,
    std::integral T,
    fmt::detail::int_base Base = fmt::detail::int_base::dec>
static constexpr auto from_chars_v = from_chars<Str, T, Base>::value;

auto main() -> int {
    static_assert(from_chars_v<"100", int> == 100);
    static_assert(from_chars_v<"-340404", int> == -340404);
    static_assert(from_chars_v<"-12a", int> == -12);
    static_assert(!from_chars_v<"+12a", int>.has_value());
    static_assert(!from_chars_v<"a100", int>.has_value());

    static_assert(*fmt::detail::field_count("format {} is {{ }} {:} valid \n{}") == 8);
    static_assert(*fmt::detail::field_count("{{}}") == 2);
    static_assert(*fmt::detail::field_count("a{{{:} }}") == 3);
    static_assert(*fmt::detail::field_count("{{ {:} }}") == 4);
    static_assert(*fmt::detail::field_count("a{{ {:.<5.5s} }}") == 4);
    static_assert(!fmt::detail::field_count("{"));
    static_assert(!fmt::detail::field_count("}"));
    static_assert(!fmt::detail::field_count("{}}"));
    static_assert(!fmt::detail::field_count("Hi}{"));
    static_assert(*fmt::detail::field_count("Hi}}{{") == 2);
    static_assert(*fmt::detail::field_count("{1002:*^-#0{20}.{30}Ld}") == 1);
    static_assert(*fmt::detail::field_count("{1002:*^-#0{20}.22Ld}") == 1);
    static_assert(*fmt::detail::field_count("{1002:*^-#0{}.22Ld}") == 1);

    static_assert(
        std::string_view {"{:*^{}} {:+}"_f.format_to_array<"this"_s, 10, 100>().data()}
        == "***this*** +100");

    std::array<char, 64> arr;
    fmt::detail::array_buffer ab {arr};
    int a = 10;
    int b = 20;
    auto res = "{:*^{}} {:+}"_f.format(ab, a, 10, b);
    if (res) {
        arr[res->formatted_len()] = 0;
        write(arr.data());
        write("\n");
    } else {
        write("none\n");
    }

    return 0;
}