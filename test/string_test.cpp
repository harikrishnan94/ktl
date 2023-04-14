#include <ktl/fixed_string.hpp>
#include <ktl/static_string.hpp>
#include <ktl/string.hpp>
#include <ktl/string_view.hpp>

#include "allocator.hpp"
#include "input_iterator.hpp"

using namespace ktl;

static void sstring_assign_test();
static void sstring_append_test();
static void sstring_insert_test();
static void sstring_replace_test();
static void sstring_erase_test();
static void sstring_substr_test();
static void sstring_copy_test();
static void sstring_compare_test();
static void sstring_find_test();
static void sstring_operators_test();
static void fstring_test();

static void sstring_test() {
    static_assert(ASAN_ENABLED ? true : sizeof(static_string<3>) == sizeof(char) * 4);
    static_assert(ASAN_ENABLED ? true : std::is_trivially_destructible_v<static_string<3>>);

    static_assert(make_static_string("012").size() == 3);
    static_assert(make_static_string<4>("abc").size() == 3);
    static_assert(make_static_string<4>("abc").capacity() == 4);
    static_assert(make_static_string<4>("abc")[0] == 'a');

    // Size and element access
    {
        constexpr auto str = [] {
            static_string<3> str;
            check_(str.push_back('a'), "push_back failed");
            check_(str.push_back('b'), "push_back failed");
            return str;
        }();
        static_assert(str.size() == 2);
        static_assert(str.capacity() == str.max_size() + 1);
        static_assert(str[0] == 'a');
        static_assert(str[1] == 'b');
        static_assert(str.at(1) == 'b');
        static_assert(str.at(2) == unexpected(Error::IndexOutOfBounds));
        static_assert(str.data()[str.size()] == '\0');
        static_assert(str.c_str() == string_view {"ab"});
    }

    // Push back
    [[maybe_unused]] static constinit auto push_back = [] {
        static_string<2> str;
        check_(str.push_back('a').has_value(), "push_back must succeed");
        check_(!str.push_back('b').has_value(), "push_back must fail");
        return str.size();
    }();

    // Pop back
    [[maybe_unused]] static constinit auto pop_back = [] {
        auto str = make_static_string("12");
        str.pop_back();
        check_(str.back() == '1', "pop_back must remove last char");
        str.pop_back();
        check_(str.empty(), "must be empty after pop_back");
        return str.size();
    }();

    // clear
    [[maybe_unused]] static constinit auto clear = [] {
        auto str = make_static_string("str");
        str.clear();
        check_(str.empty(), "must be empty after clear");
        return str.size();
    }();

    // Resize
    [[maybe_unused]] static constinit auto resize = [] {
        auto str = make_static_string("hello");
        check_(str.resize(1), "resize must succeed");
        check_(str.resize(2, 'x'), "resize must succeed");
        check_(str.front() == 'h', "must contain `filled_value` after resize");
        check_(str.resize(3, 'x'), "resize must succeed");
        check_(str.back() == 'x', "must contain `filled_value` after resize");
        check_(!str.resize(6), "resize over capacity must fail");
        return str.size();
    }();

    sstring_assign_test();
    sstring_append_test();
    sstring_insert_test();
    sstring_replace_test();
    sstring_erase_test();
    sstring_substr_test();
    sstring_copy_test();
    sstring_compare_test();
    sstring_find_test();
    sstring_operators_test();
}

void sstring_assign_test() {
    [[maybe_unused]] static constinit auto cstr = [] {
        static_string<4> str;

        check_(str.assign("123"), "assigning len elements must pass");
        check_(str.front() == '1', "");
        check_(str[1] == '2', "");
        check_(str.back() == '3', "");
        check_(!str.assign("1234"), "assigning more than len elements must fail");
        check_(str.front() == '1', "");
        check_(str[1] == '2', "");
        check_(str.back() == '3', "");

        check_(str.assign("123", 1), "");
        check_(str == "1"_sv, "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto fill = [] {
        static_string<4> str;

        check_(str.assign(2, 'x'), "assigning len elements must pass");
        check_(str.front() == 'x', "");
        check_(str[1] == 'x', "");
        check_(str.back() == 'x', "");
        check_(!str.assign(4, 'a'), "assigning more than len elements must fail");
        check_(str.front() == 'x', "");
        check_(str[1] == 'x', "");
        check_(str.back() == 'x', "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto other_str = [] {
        static_string<4> str;
        auto str2 = make_static_string("hel");

        check_(!str.assign(make_static_string("helo")), "assigning larger string must fail");
        check_(str.empty(), "failed assign must not modify str");
        check_(str.assign(str2), "assigning valid string must pass");
        check_(str == "hel", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto other_substr = [] {
        static_string<4> str;
        auto str2 = make_static_string("hel");

        check_(str.assign(str2, 1, 2), "assigning valid string must pass");
        check_(str == "el", "");

        check_(!str.assign(str2, 4, 3), "assigning invalid substr must fail");
        check_(str == "el", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto input_it = [] {
        static_string<4> str;
        auto str2 = make_static_string("hel");

        check_(
            str.assign(InputIterator {str2.begin()}, InputIterator {str2.end()}),
            "assigning valid string must pass");
        check_(str == "hel", "");

        check_(str.assign(str2.begin(), str2.end()), "assigning valid string must pass");
        check_(str == "hel", "");

        auto str3 = make_static_string("large string");
        check_(
            !str.assign(InputIterator {str3.begin()}, InputIterator {str3.end()}),
            "assigning large string must fail");
        check_(str == "hel", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto ilist = [] {
        static_string<4> str;

        check_(str.assign({'h', 'e', 'l'}), "assigning valid ilist must pass");
        check_(str == "hel", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto view = [] {
        static_string<4> str;

        check_(str.assign("hel"_sv), "assigning valid string_view must pass");
        check_(str == "hel", "");

        check_(str.assign("hel"_sv, 2, 2), "assigning valid string_view must pass");
        check_(str == "l", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto str = [] {
        static_string<4> str;
        auto str2 = make_static_string("hel");

        check_(str.assign(str2, 0), "assigning valid string_view must pass");
        check_(str == basic_string_view {str2}, "");

        check_(str.assign(str2, 2, 2), "assigning valid string_view must pass");
        check_(str == "l", "");

        return str.empty();
    }();
}

void sstring_append_test() {
    [[maybe_unused]] static constinit auto cstr = [] {
        static_string<4> str;

        check_(str.append("123"), "appending len elements must pass");
        check_(str == "123", "");
        check_(!str.append("1234"), "appending more than len elements must fail");
        str.pop_back();
        check_(str.append("3"), "appending len elements must pass");
        check_(str == "123", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto fill = [] {
        static_string<4> str;

        check_(str.append(2, 'x'), "appending len elements must pass");
        check_(str == "xx", "");
        check_(str.append(1, 'a'), "appending more than len elements must fail");
        check_(str == "xxa", "");
        check_(!str.append(2, 'b'), "appending more than len elements must fail");
        check_(str == "xxa", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto other_str = [] {
        static_string<4> str;
        auto str2 = make_static_string("hel");

        check_(!str.append(make_static_string("helo")), "appending larger string must fail");
        check_(str.empty(), "failed append must not modify str");
        check_(str.append(str2), "appending valid string must pass");
        check_(str == "hel", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto other_substr = [] {
        static_string<4> str;
        auto str2 = make_static_string("hel");

        check_(str.append(str2, 1, 2), "appending valid string must pass");
        check_(str == "el", "");

        check_(!str.append(str2, 4, 3), "appending invalid substr must fail");
        check_(str == "el", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto input_it = [] {
        static_string<4> str;
        auto str2 = make_static_string("hel");

        check_(
            str.append(InputIterator {str2.begin()}, InputIterator {str2.end()}),
            "appending valid string must pass");
        check_(str == "hel", "");

        check_(!str.append(str2.begin(), str2.end()), "appending more data must fail");
        check_(str == "hel", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto ilist = [] {
        static_string<4> str;

        check_(str.append({'h', 'e', 'l'}), "appending valid ilist must pass");
        check_(str == "hel", "");

        return str.empty();
    }();

    [[maybe_unused]] static constinit auto view = [] {
        static_string<4> str;

        check_(str.append("hel"_sv), "appending valid string_view must pass");
        check_(str == "hel", "");

        check_(str.append("hel"_sv, 2, 2), "appending valid string_view must pass");
        check_(str == "l", "");

        return str.empty();
    }();
}

void sstring_insert_test() {
    [[maybe_unused]] static constinit auto sanity_test = [] {
        constexpr auto FinalString = "Exemplar is an:== example string."_sv;
        auto s = make_static_string<FinalString.size() + 1>("xmplr");

        check_(s.insert(0, 1, 'E'), "");
        check_("Exmplr" == s, "");

        check_(s.insert(2, "e"), "");
        check_("Exemplr" == s, "");

        check_(s.insert(6, make_static_string("a")), "");
        check_("Exemplar" == s, "");

        check_(s.insert(8, make_static_string(" is an example string."), 0, 14), "");
        check_("Exemplar is an example" == s, "");

        check_(s.insert(s.cbegin() + basic_string_view {s}.find_first_of('n') + 1, ':'), "");
        check_("Exemplar is an: example" == s, "");

        check_(s.insert(s.cbegin() + basic_string_view {s}.find_first_of(':') + 1, 2, '='), "");
        check_("Exemplar is an:== example" == s, "");

        {
            const auto seq = " string"_sv;
            check_(
                s.insert(
                    s.begin() + basic_string_view {s}.find_last_of('e') + 1,
                    std::begin(seq),
                    std::end(seq)),
                "");
            check_("Exemplar is an:== example string" == s, "");
        }

        check_(s.insert(s.cbegin() + basic_string_view {s}.find_first_of('g') + 1, {'.'}), "");
        check_(FinalString == s, "");

        s.clear();
        check_(s.insert(0, FinalString, 0), "");
        check_(FinalString == s, "");

        return s.empty();
    }();

    [[maybe_unused]] static constinit auto fill = [] {
        auto s = make_static_string<4>("1");

        check_(s.insert(0, 2, '2'), "");
        check_(s == "221", "");

        check_(s.assign("1"), "");
        check_(s.insert(1, 2, '2'), "");
        check_(s == "122", "");

        check_(!s.insert(1, 2, '2'), "");
        check_(s == "122", "");

        return s.empty();
    }();

    [[maybe_unused]] static constinit auto cstr = [] {
        auto s = make_static_string<4>("1");

        check_(s.insert(0, "22", 2), "");
        check_(s == "221", "");

        check_(s.assign("1"), "");
        check_(s.insert(1, "22", 1), "");
        check_(s == "12", "");

        check_(!s.insert(1, "22"), "");
        check_(s == "12", "");

        return s.empty();
    }();

    [[maybe_unused]] static constinit auto input_it = [] {
        auto s = make_static_string<4>("1");
        auto sv = "22"_sv;

        check_(s.insert(s.end(), InputIterator {sv.begin()}, InputIterator {sv.end()}), "");
        return s.empty();
    }();

    [[maybe_unused]] static constinit auto ilist = [] {
        auto s = make_static_string<4>("1");

        check_(s.insert(s.end(), {'2', '2'}), "");
        return s.empty();
    }();

    [[maybe_unused]] static constinit auto sv = [] {
        auto s = make_static_string<4>("1");
        auto sv = "22"_sv;

        check_(s.insert(1, sv, 0), "");
        return s.empty();
    }();
}

void sstring_replace_test() {
    [[maybe_unused]] static constinit auto sanity = [] {
        auto replace_all = []<typename String>(String& inout, string_view what, string_view with) {
            usize count {};
            for (typename String::size_type pos {}; inout.npos
                 != (pos = basic_string_view {inout}.find(what.data(), pos, what.length()));
                 pos += with.length(), ++count) {
                check_(inout.replace(pos, what.length(), with.data(), with.length()), "");
            }

            return count;
        };

        auto remove_all = [&]<typename String>(String& inout, string_view what) {
            return replace_all(inout, what, "");
        };

        auto str = make_static_string("The quick brown fox jumps over the lazy dog.");

        check_(str.replace(str.begin() + 10, str.begin() + 15, "red"), "");  // (5)
        check_(str.replace(str.begin(), str.begin() + 3, 1, 'A'), "");  // (6)
        check_(str == "A quick red fox jumps over the lazy dog.", "");

        constexpr auto FinalString = "http: httphttp: http:"_sv;
        auto str2 = make_static_string<FinalString.size() + 1>("ftp: ftpftp: ftp:");

        auto count = replace_all(str2, "ftp", "http");
        check_(count == 4, "");
        check_(str2 == "http: httphttp: http:", "");

        count = replace_all(str2, "ftp", "http");
        check_(count == 0, "");
        check_(str2 == "http: httphttp: http:", "");

        count = remove_all(str2, "http");
        check_(count == 4, "");
        check_(str2 == ": : :", "");

        return str2.size();
    }();

    [[maybe_unused]] static constinit auto ind_sv = [] {
        auto str = make_static_string<15>("100, {}, 300");

        check_(str.replace(str.begin() + 5, str.begin() + 7, "200"_sv), "");
        check_(str == "100, 200, 300", "");

        check_(str.replace(5, 3, "02000"_sv, 1, 3), "");
        check_(str == "100, 200, 300", "");

        check_(str.replace(10, 3, "{}"), "");
        check_(str == "100, 200, {}", "");

        return str.size();
    }();

    [[maybe_unused]] static constinit auto fill = [] {
        auto str = make_static_string<5>("1");

        check_(str.replace(0, 1, 2, '2'), "");
        check_(str == "22", "");
        check_(!str.replace(0, 2, 8, 'x'), "");
        check_(str == "22", "");
        check_(str.replace(str.begin(), str.end(), 1, 'x'), "");
        check_(str == "x", "");

        return str.size();
    }();

    [[maybe_unused]] static constinit auto ilist = [] {
        auto str = make_static_string<5>("1");

        check_(str.replace(str.begin(), str.end(), {'2', '2'}), "");
        check_(str == "22", "");
        check_(!str.replace(str.begin(), str.end(), {'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x'}), "");
        check_(str == "22", "");
        check_(str.replace(str.begin(), str.end(), {'x'}), "");
        check_(str == "x", "");

        return str.size();
    }();

    [[maybe_unused]] static constinit auto ip_it = [] {
        auto str = make_static_string<4>("MI");
        auto csk = "CSK"_sv;

        check_(
            str.replace(
                str.begin(),
                str.end(),
                InputIterator {csk.begin()},
                InputIterator {csk.end()}),
            "");
        check_(str == csk, "");

        check_(str.replace(str.begin(), str.end(), csk.begin(), csk.end()), "");
        check_(str == csk, "");

        return str.size();
    }();

    [[maybe_unused]] static constinit auto str = [] {
        auto str = make_static_string<4>("MI");
        auto csk = make_static_string("CSK");

        check_(str.replace(str.begin(), str.end(), csk), "");
        check_(str == csk, "");

        check_(str.replace(0, str.length(), csk, 0), "");
        check_(str == csk, "");

        return str.size();
    }();

    [[maybe_unused]] static constinit auto cstr = [] {
        auto str = make_static_string<4>("MI");
        const auto* csk = "CSK";

        check_(str.replace(str.begin(), str.end(), csk), "");
        check_(str == csk, "");

        check_(str.replace(0, str.length(), csk, std::char_traits<char>::length(csk)), "");
        check_(str == csk, "");

        return str.size();
    }();
}

void sstring_erase_test() {
    [[maybe_unused]] static constinit auto _ = [] {
        auto str = make_static_string("11223");

        check_(erase(str, '3') == 1, "");
        check_(str == "1122", "");

        check_(erase_if(str, [](auto c) { return c == '1'; }) == 2, "");
        check_(str == "22", "");

        check_(*str.erase(str.begin()) == '2', "");

        return str.size();
    }();
}

void sstring_substr_test() {
    [[maybe_unused]] static constinit auto _ = [] {
        auto str = make_static_string("12334");
        auto res = str.substr(0, 3);

        check_(res, "");

        basic_static_string ss {*res};
        check_(ss == "123", "");
        check_(ss.substr(ss.size())->view().empty(), "");
        check_(!ss.substr(ss.size() + 1), "");

        check_(**std::move(ss).substr(1, 4) == "23", "");

        return str.size();
    }();
}

void sstring_copy_test() {
    [[maybe_unused]] static constinit auto _ = [] {
        auto str = make_static_string("12334");
        std::array<char, 4> dest {};

        check_(str.copy(dest.data(), 2, 3), "");
        check_(!str.copy(dest.data(), 2, 6), "");

        return str.size();
    }();
}

void sstring_compare_test() {
    [[maybe_unused]] static constinit auto _ = [] {
        auto rajni = make_static_string("Rajni");
        auto kamal = make_static_string("Kamal");

        check_(*rajni.compare(kamal) > 0, "");
        check_(*rajni.compare(1, 4, kamal) > 0, "");
        check_(*kamal.compare(1, 4, rajni) > 0, "");
        check_(*kamal.compare(rajni.c_str()) < 0, "");
        check_(*kamal.compare(1, 2, rajni.c_str()) > 0, "");
        check_(*kamal.compare(1, 2, rajni.c_str(), 2) > 0, "");
        check_(*rajni.compare(basic_string_view {kamal}) > 0, "");
        check_(*rajni.compare(1, 4, basic_string_view {kamal}) > 0, "");
        check_(*rajni.compare(1, 4, basic_string_view {kamal}, 2, 4) < 0, "");

        return rajni.empty() || kamal.empty();
    }();
}

void sstring_find_test() {
    // starts_with
    {
        constexpr auto str = make_static_string("hello world");

        static_assert(str.starts_with("hello"));
        static_assert(!str.starts_with("goodbye"));
        static_assert(str.starts_with('h'));
        static_assert(!str.starts_with('x'));
    }

    // ends_with
    {
        constexpr auto str = make_static_string("hello world");

        static_assert(str.ends_with("world"));
        static_assert(!str.ends_with("goodbye"));
        static_assert(str.ends_with('d'));
        static_assert(!str.ends_with('x'));
    }

    // contains
    {
        constexpr auto str = make_static_string("hello world");

        static_assert(str.contains("lo wor"));
        static_assert(!str.contains("goodbye"));
        static_assert(str.contains('w'));
        static_assert(!str.contains('x'));
    }

    // find
    {
        // constexpr auto str {11_ss};
        constexpr basic_static_string str {" long long int;"};

        // NOLINTBEGIN
        static_assert(
            1 == str.find("long"_sv) && "<- find(v , pos = 0)" && 6 == str.find("long"_sv, 2)
            && "<- find(v , pos = 2)" && 0 == str.find(' ') && "<- find(ch, pos = 0)"
            && 2 == str.find('o', 1) && "<- find(ch, pos = 1)" && 2 == str.find("on")
            && "<- find(s , pos = 0)" && 6 == str.find("long double", 5, 4)
            && "<- find(s , pos = 5, count = 4)");
        // NOLINTEND
    }

    // rfind
    {
        constexpr auto N = static_string<1>::npos;
        // NOLINTBEGIN
        static_assert(
            true && (6 == make_static_string("AB AB AB").rfind("AB"_sv))
            && (6 == make_static_string("AB AB AB").rfind("ABCD", N, 2))
            && (3 == make_static_string("AB AB AB").rfind("AB", 5))
            && (2 == make_static_string("B AB AB ").rfind("AB", 2))
            && (N == make_static_string("B AB AB ").rfind("AB"_sv, 1))
            && (5 == make_static_string("B AB AB ").rfind('A'))
            && (4 == make_static_string("AB AB AB").rfind('B', 4))
            && (N == make_static_string("AB AB AB").rfind('C'))
            && (N == make_static_string("AB AB AB").rfind('C', 100)));
        // NOLINTEND
    }

    // find_first_of
    {
        constexpr auto N = static_string<1>::npos;

        auto is_white_space = [](const char c) noexcept {
            return make_static_string(" \t\n\f\r\v").find_first_of(c) != N;
        };

        // NOLINTBEGIN
        static_assert(
            1 == make_static_string("alignas").find_first_of(make_static_string("klmn"))
            && N == make_static_string("alignof").find_first_of(make_static_string("wxyz"))
            && 3
                == make_static_string("concept").find_first_of(
                    make_static_string("bcde"),
                    /* pos= */ 1)
            && N == make_static_string("consteval").find_first_of("oxyz"_sv, /* pos= */ 2)
            && 6 == make_static_string("constexpr").find_first_of('x')
            && N == make_static_string("constinit").find_first_of('x')
            && 6 == make_static_string("const_cast").find_first_of('c', /* pos= */ 4)
            && N == make_static_string("continue").find_first_of('c', /* pos= */ 42)
            && 5 == make_static_string("co_await").find_first_of("cba", /* pos= */ 4)
            && 7
                == make_static_string("decltype").find_first_of("def", /* pos= */ 2, /* count= */ 2)
            && N
                == make_static_string("decltype").find_first_of("def", /* pos= */ 2, /* count= */ 1)
            && is_white_space(' ') && is_white_space('\r') && !is_white_space('\a')
            && N
                == make_static_string("decltype")
                       .find_first_of("def", /* pos= */ 200, /* count= */ 1));
        // NOLINTEND
    }

    // find_last_of
    {
        constexpr auto N = static_string<1>::npos;
        // NOLINTBEGIN
        static_assert(
            5 == make_static_string("delete").find_last_of("cdef"_sv)
            && N == make_static_string("double").find_last_of("fghi"_sv)
            && 0 == make_static_string("else").find_last_of("bcde"_sv, 2 /* pos [0..2]: "els" */)
            && N == make_static_string("explicit").find_last_of("abcd", 4 /* pos [0..4]: "expli" */)
            && 3 == make_static_string("extern").find_last_of('e')
            && N == make_static_string("false").find_last_of('x')
            && 0 == make_static_string("inline").find_last_of('i', 2 /* pos [0..2]: "inl" */)
            && N == make_static_string("mutable").find_last_of('a', 2 /* pos [0..2]: "mut" */)
            && 3
                == make_static_string("namespace")
                       .find_last_of("cdef", 3 /* pos [0..3]: "name" */, 3 /* "cde" */)
            && N
                == make_static_string("namespace")
                       .find_last_of("cdef", 3 /* pos [0..3]: "name" */, 2 /* "cd" */));
        // NOLINTEND
    }

    // find_first_not_of
    {
        // NOLINTBEGIN
        static_assert(2 == make_static_string("BCDEF").find_first_not_of("ABC"));
        static_assert(4 == make_static_string("BCDEF").find_first_not_of("ABC"_sv, 4));
        static_assert(1 == make_static_string("BCDEF").find_first_not_of('B'));
        static_assert(3 == make_static_string("BCDEF").find_first_not_of('D', 2));
        static_assert(
            static_string<1>::npos == make_static_string("BCDEF").find_first_not_of('D', 200));
        // NOLINTEND
    }

    // find_last_not_of
    {
        // NOLINTBEGIN
        static_assert(1 == make_static_string("BCDEF").find_last_not_of("DEF_sv"));
        static_assert(2 == make_static_string("BCDEFG").find_last_not_of("EFG", 3));
        static_assert(2 == make_static_string("ABBA").find_last_not_of('A'));
        static_assert(1 == make_static_string("ABBA").find_last_not_of('A', 1));
        // NOLINTEND
    }
}

void sstring_operators_test() {
    [[maybe_unused]] static constinit auto op_plus_eq = [] {
        auto str = make_static_string<10>("1222");

        check_(**(str += "44") == "122244", "");
        check_((str += "4444444") == unexpected(Error::BufferFull), "");

        return str.size();
    }();

    [[maybe_unused]] static constinit auto three_way = [] {
        auto rajni = make_static_string("Rajni");
        auto kamal = make_static_string("Kamal");

        check_(rajni > kamal, "");
        check_(kamal < rajni.c_str(), "");

        return rajni.empty() || kamal.empty();
    }();
}

void fstring_test() {
    [[maybe_unused]] static constinit auto _ = [] {
        std::array<char, 4> arr {};
        usize len = 1;
        fixed_string str {arr, len};

        check_(str.push_back('x'), "");
        check_(str.append("yy"), "");
        check_(str == "xyy", "");

        std::array<char, 4> arr2 {};
        usize len2 = 1;
        fixed_string str2 {arr2, len2};

        check_(str2.assign("yx"), "");

        check_(str < str2, "");

        std::swap(str, str2);
        check_(str == "yx", "");
        check_(str2 == "xyy", "");

        return str.size();
    }();
}

template<allocator_for<char> Allocator>
constexpr void sanity_test() {
    auto str = *make_string<Allocator>("a");

    auto find_and_replace =
        [](auto& str, const char* pattern, const char* replacement) -> expected<usize, Error> {
        usize count = 0;
        usize pos = 0;
        while (true) {
            auto ind = str.find(pattern, pos);
            if (ind == str.npos) {
                break;
            }
            TryV(str.replace(ind, std::char_traits<char>::length(pattern), replacement));
            pos = ind + std::char_traits<char>::length(replacement);
            count++;
        }
        return count;
    };
    auto find_and_remove = [](auto& str, const char* pattern) -> usize {
        usize count = 0;
        usize pos = 0;
        while (true) {
            pos = str.find(pattern, pos);
            if (pos == str.npos) {
                break;
            }
            str.erase(pos, std::char_traits<char>::length(pattern));
            count++;
        }
        return count;
    };

    check_(str == "a", "");
    check_(str.insert(0, "bb"), "");
    check_(str == "bba", "");
    check_(str.insert(str.length(), "cc"), "");
    check_(str == "bbacc", "");
    check_(str += " operation", "");
    check_(str == "bbacc operation", "");
    auto s = clone(str);
    check_(s, "");
    check_(str.insert(0, *s), "");
    check_(str.insert(str.size(), *s), "");

    check_(find_and_replace(str, "bbacc", "Hello,") == 3U, "");
    check_(find_and_replace(str, "operation", "World! ") == 3U, "");
    check_(str == "Hello, World! Hello, World! Hello, World! ", "");
    check_(find_and_replace(str, "operation", "World!") == 0U, "");
    check_(find_and_replace(str, "!", "!") == 3U, "");
    check_(str == "Hello, World! Hello, World! Hello, World! ", "");

    check_(find_and_remove(str, "Hello") == 3, "");
    check_(str == ", World! , World! , World! ", "");
    check_(find_and_remove(str, "World") == 3, "");
    check_(str == ", ! , ! , ! ", "");
    erase(str, ',');
    erase(str, ' ');
    erase(str, '!');
    check_(str.empty(), "");

    check_(str.shrink_to_fit(), "");
    if (std::is_constant_evaluated()) {
        check_(str.capacity() == 1, "");
    } else {
        check_(str.capacity() == sizeof(str), "");
    }

    check_(str += "Hello World", "");
    check_(str == "Hello World", "");
}

void string_test() {
    [[maybe_unused]] static constinit auto const_test = [] {
        sanity_test<ConstAllocator<char>>();
        return true;
    }();

    // Short string test
    sanity_test<BumpAllocator<char>>();
}

auto main() -> int {
    string_test();
    sstring_test();
    fstring_test();
    return 0;
}