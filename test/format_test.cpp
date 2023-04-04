#include <bit>

#include <ktl/access.hpp>
#include <ktl/fmt/format.hpp>
#include <ktl/span.hpp>

#include "platform.h"

using namespace ktl::fmt;
using namespace ktl::fmt::literals;

template<typename CharT, ktl::usize N>
constexpr auto view(const std::array<CharT, N>& arr) -> ktl::basic_string_view<CharT> {
    ktl::basic_string_view view = {arr.begin(), arr.end()};
    auto len = view.find('\0');
    return len == ktl::string_view::npos ? view : view.substr(0, len);
}

template<const_string RawFmtStr, typename... Args>
constexpr auto check(const char* exp, const Args&... args) -> unsigned {
    std::array<char, 1024> buf;  // NOLINT
    fixed_buffer fb {buf.begin(), buf.end()};
    auto res = format<RawFmtStr>(fb, args...);

    check_(res, "must not be error");
    check_(*res, "must not complete");

    ktl::at(buf, res->formatted_len()) = '\0';
    if (ktl::basic_string_view str = buf.data(); str != exp) {
        write("failure: `");
        write(str.data());
        write("` != `");
        write(exp);
        write("`\n");
        return 1;
    }

    return 0;
}

auto main() -> int {
    // NOLINTBEGIN(*-magic-numbers)
    static_assert("{{}}"_f.format<>() == "{}");
    static_assert("{{ {:} }}"_f.format<"1"_cs>() == "{ 1 }");
    static_assert("a{{ {:.<5s} }}"_f.format<"1"_cs>() == "a{ 1.... }");
    static_assert("a{{ {:*^5s} }}"_f.format<"1"_cs>() == "a{ **1** }");
    static_assert("{0:+#0{1}d}"_f.format<100, 10>() == "+000000100");
    static_assert(
        "{:*^{}} {:+#08X} test string `{:3s}`. {} {:d}"_f
            .format<"this"_cs, 10, 100, "str"_cs, false, true>()
        == "***this*** +0X00064 test string `str`. false 1");

    unsigned ret = 0;

    ret |= check<"{:*^{}} {:+}">("****10**** +20", 10, 10, 20);
    ret |= check<"{}">("0x0", nullptr);
    ret |= check<"{:-^14}">("--0xdeadbeef--", std::bit_cast<void*>(0xdeadbeefULL));

    // NOLINTEND(*-magic-numbers)

    return static_cast<int>(ret);
}