#include <ktl/assert.hpp>
#include <ktl/fmt/format.hpp>
#include <ktl/string_view.hpp>

using namespace ktl;
using namespace ktl::fmt::literals;
using namespace ktl::literals;

[[nodiscard("a pure function")]] constexpr auto
count_substrings(ktl::string_view hive, ktl::string_view const bee) -> usize {
    if (hive.empty() or bee.empty())
        return 0U;

    usize buzz {};
    while (bee.size() <= hive.size()) {
        const auto pos = hive.find(bee);
        if (pos == ktl::string_view::npos)
            break;
        ++buzz;
        hive.remove_prefix(pos + bee.size());
    }
    return buzz;
}

static constexpr std::array remove_suffix_test_arr = {'a', 'b', 'c', 'd', '\0', '\0', '\0'};
static constexpr auto remove_suffix_test() {
    ktl::string_view v {remove_suffix_test_arr.data(), remove_suffix_test_arr.size()};
    auto trim_pos = v.find('\0');

    if (trim_pos != string_view::npos)
        v.remove_suffix(v.size() - trim_pos);

    return v;
}

auto main() -> int {
    // Remove prefix test
    constexpr auto hive {"bee buzz bee buzz bee"};
    static_assert(count_substrings(hive, "bee") == 3);

    constexpr auto str = [] {
        auto str = "   trim me"_sv;
        str.remove_prefix(std::min(str.find_first_not_of(" "), str.size()));
        return str;
    }();
    static_assert(str == "trim me");

    // Remove suffix test
    static_assert(remove_suffix_test() == "abcd");
    static_assert(remove_suffix_test().size() == 4);

    // Copy test
    {
        constexpr string_view source {"ABCDEF"};
        constexpr auto dest1 = [&] {
            std::array<char, 8> dest {};
            check_(source.copy(dest.data(), 4), "");
            return dest;
        }();
        static_assert(string_view {dest1.data()} == "ABCD");

        constexpr auto dest2 = [&] {
            std::array<char, 8> dest {};
            check_(source.copy(dest.data(), 4, 1), "");
            return dest;
        }();
        static_assert(string_view {dest2.data()} == "BCDE");

        constexpr auto dest3 = [&] {
            std::array<char, 8> dest {};
            check_(source.copy(dest.data(), 42, 2), "");
            return dest;
        }();
        static_assert(string_view {dest3.data()} == "CDEF");

        constexpr auto dest4 = [&] {
            std::array<char, 8> dest = {};
            return source.copy(dest.data(), 1, 666);
        }();
        static_assert(dest4.error() == Error::IndexOutOfBounds);
    }

    // substr test
    {
        using count_t = usize;
        using pos_t = usize;
        constexpr string_view data {"ABCDEF"};

        static_assert(*data.substr() == "ABCDEF");
        static_assert(data.substr(pos_t(1)) == "BCDEF");
        static_assert(data.substr(pos_t(2), count_t(3)) == "CDE");
        static_assert(data.substr(pos_t(4), count_t(42)) == "EF");
        static_assert(data.substr(pos_t(666), count_t(1)).error() == Error::IndexOutOfBounds);
    }

    // Compare
    {
        static_assert("abc"_sv.compare("abcd"_sv) < 0);
        static_assert("abcd"_sv.compare("abc"_sv) > 0);
        static_assert("abc"_sv.compare("abc"_sv) == 0);
        static_assert(""_sv.compare(""_sv) == 0);
        static_assert("abc"_sv.compare(4, 0, "abc"_sv).error() == Error::IndexOutOfBounds);
    }

    // starts_with
    {
        static_assert("https://cppreference.com"_sv.starts_with("http"_sv));
        static_assert(!"https://cppreference.com"_sv.starts_with("ftp"_sv));
        static_assert("C++20"_sv.starts_with('C'));
        static_assert(!"C++20"_sv.starts_with('J'));
        static_assert("string_view"_sv.starts_with("string"));
        static_assert(!"string_view"_sv.starts_with("String"));
    }

    // ends_with
    {
        // (1) bool ends_with( basic_string_view sv ) const noexcept;
        static_assert(string_view("https://cppreference.com").ends_with(".com"_sv));
        static_assert(!string_view("https://cppreference.com").ends_with(".org"_sv));

        // (2) bool ends_with( CharT c ) const noexcept;
        static_assert(string_view("C++20").ends_with('0'));
        static_assert(!string_view("C++20").ends_with('3'));

        // (3) bool ends_with( const CharT* s ) const;
        static_assert(string_view("string_view").ends_with("view"));
        static_assert(!string_view("string_view").ends_with("View"));
    }

    // contains
    {
        // bool contains(basic_string_view x) const noexcept;
        static_assert("https://cppreference.com"_sv.contains("cpp"_sv));
        static_assert(!"https://cppreference.com"_sv.contains("java"_sv));

        // bool contains(CharT x) const noexcept;
        static_assert("C++23"_sv.contains('+'));
        static_assert(!"C++23"_sv.contains('-'));

        // bool contains(const CharT* x) const;
        static_assert(string_view("basic_string_view").contains("string"));
        static_assert(!string_view("basic_string_view").contains("String"));
    }

    // find
    {
        constexpr auto str {" long long int;"_sv};

        // NOLINTBEGIN
        // clang-format off
        static_assert(
            1 == str.find("long"_sv)            && "<- find(v , pos = 0)" &&
            6 == str.find("long"_sv, 2)         && "<- find(v , pos = 2)" &&
            0 == str.find(' ')                 && "<- find(ch, pos = 0)" &&
            2 == str.find('o', 1)              && "<- find(ch, pos = 1)" &&
            2 == str.find("on")                && "<- find(s , pos = 0)" &&
            6 == str.find("long double", 5, 4) && "<- find(s , pos = 5, count = 4)"
        );
        // clang-format on
        // NOLINTEND
    }

    // rfind
    {
        constexpr auto N = string_view::npos;
        // NOLINTBEGIN
        // clang-format off
        static_assert(true
            && (6 == "AB AB AB"_sv.rfind("AB"_sv))
            && (6 == "AB AB AB"_sv.rfind("ABCD", N, 2))
            && (3 == "AB AB AB"_sv.rfind("AB", 5))
            && (2 == "B AB AB "_sv.rfind("AB", 2))
            && (N == "B AB AB "_sv.rfind("AB", 1))
            && (5 == "B AB AB "_sv.rfind('A'))
            && (4 == "AB AB AB"_sv.rfind('B', 4))
            && (N == "AB AB AB"_sv.rfind('C'))
            && (N == "AB AB AB"_sv.rfind('C', 100))
        );
        // clang-format on
        // NOLINTEND
    }

    // find_first_of
    {
        constexpr auto N = string_view::npos;

        auto is_white_space = [](const char c) noexcept {
            return " \t\n\f\r\v"_sv.find_first_of(c) != N;
        };

        // NOLINTBEGIN
        // clang-format off
        static_assert(
            1 == "alignas"_sv.find_first_of("klmn"_sv) &&
            //   └─────────────────────────┘
            N == "alignof"_sv.find_first_of("wxyz"_sv) &&
            //
            3 == "concept"_sv.find_first_of("bcde"_sv, /* pos= */ 1) &&
            //     └───────────────────────┘
            N == "consteval"_sv.find_first_of("oxyz"_sv, /* pos= */ 2) &&
            //
            6 == "constexpr"_sv.find_first_of('x') &&
            //        └─────────────────────┘
            N == "constinit"_sv.find_first_of('x') &&
            //
            6 == "const_cast"_sv.find_first_of('c', /* pos= */ 4) &&
            //        └──────────────────────┘
            N == "continue"_sv.find_first_of('c', /* pos= */ 42) &&
            //
            5 == "co_await"_sv.find_first_of("cba", /* pos= */ 4) &&
            //       └───────────────────────┘
            7 == "decltype"_sv.find_first_of("def", /* pos= */ 2, /* count= */ 2) &&
            //         └────────────────────┘
            N == "decltype"_sv.find_first_of("def", /* pos= */ 2, /* count= */ 1) &&
            //
            is_white_space(' ') && is_white_space('\r') && !is_white_space('\a') &&
            //
            N == "decltype"_sv.find_first_of("def", /* pos= */ 200, /* count= */ 1)
        );
        // clang-format on
        // NOLINTEND
    }

    // find_last_of
    {
        constexpr auto N = string_view::npos;
        // NOLINTBEGIN
        // clang-format off
        static_assert(
            5 == "delete"_sv.find_last_of("cdef"_sv) &&
            //       └────────────────────┘
            N == "double"_sv.find_last_of("fghi"_sv) &&
            //
            0 == "else"_sv.find_last_of("bcde"_sv, 2 /* pos [0..2]: "els" */) &&
            //  └────────────────────────┘
            N == "explicit"_sv.find_last_of("abcd"_sv, 4 /* pos [0..4]: "expli" */) &&
            //
            3 == "extern"_sv.find_last_of('e') &&
            //     └────────────────────┘
            N == "false"_sv.find_last_of('x') &&
            //
            0 == "inline"_sv.find_last_of('i', 2 /* pos [0..2]: "inl" */) &&
            //  └───────────────────────┘
            N == "mutable"_sv.find_last_of('a', 2 /* pos [0..2]: "mut" */) &&
            //
            3 == "namespace"_sv.find_last_of("cdef", 3 /* pos [0..3]: "name" */, 3 /* "cde" */)
            &&
            //     └─────────────────────────┘
            N == "namespace"_sv.find_last_of("cdef", 3 /* pos [0..3]: "name" */, 2 /* "cd" */)
        );
        // clang-format on
        // NOLINTEND
    }

    // find_first_not_of
    {
        // NOLINTBEGIN
        // clang-format off
        static_assert(2 == "BCDEF"_sv.find_first_not_of("ABC"));
                        //    ^
        static_assert(4 == "BCDEF"_sv.find_first_not_of("ABC", 4));
                        //      ^
        static_assert(1 == "BCDEF"_sv.find_first_not_of('B'));
                        //   ^
        static_assert(3 == "BCDEF"_sv.find_first_not_of('D', 2));
                        //     ^
        static_assert(string_view::npos == "BCDEF"_sv.find_first_not_of('D', 200));
        // clang-format on
        // NOLINTEND
    }

    // find_last_not_of
    {
        // NOLINTBEGIN
        // clang-format off
        static_assert(1 == "BCDEF"_sv.find_last_not_of("DEF"));
                        //   ^
        static_assert(2 == "BCDEFG"_sv.find_last_not_of("EFG", 3));
                        //    ^
        static_assert(2 == "ABBA"_sv.find_last_not_of('A'));
                        //    ^
        static_assert(1 == "ABBA"_sv.find_last_not_of('A', 1));
                        //   ^
        // clang-format on
        // NOLINTEND
    }

    return 0;
}