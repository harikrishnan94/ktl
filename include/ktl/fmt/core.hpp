#pragma once

#include <algorithm>
#include <array>
#include <iterator>
#include <optional>
#include <variant>

#include <ktl/access.hpp>
#include <ktl/assert.hpp>
#include <ktl/detail/charconv.hpp>
#include <ktl/detail/preproc.hpp>
#include <ktl/error.hpp>
#include <ktl/expected.hpp>
#include <ktl/static_string.hpp>
#include <ktl/utility.hpp>

namespace ktl::fmt {
// Result type of `format` routine.
class Result {
  public:
    constexpr Result(usize formatted_len, bool is_complete) :
        m_formatted_len(formatted_len),
        m_is_complete(is_complete) {}

    // Returns whether format ran thru completetion, without any error.
    [[nodiscard]] constexpr auto complete() const noexcept -> bool {
        return m_is_complete;
    }
    // Returns `format_complete`.
    constexpr operator bool() const noexcept {  // NOLINT(hicpp-explicit-conversions)
        return complete();
    }

    // Length of the formatted string.
    [[nodiscard]] constexpr auto len() const noexcept -> usize {
        return m_formatted_len;
    }

  private:
    usize m_formatted_len;
    bool m_is_complete;
};

template<typename CharT, std::output_iterator<CharT> OI>
struct buffer_view {
    OI out;
    usize len;
};

// `string_buffer` concept is used to model a static/dynamic buffer - holding the formatted result
// string.
// Any type that can be used as `string_buffer` must provide the following functions
// - `reserve`: should guarantee availability of `0..=len` chars by returning the output
// iterator to the start of the region. If the returned `len` is lesser than the requested length,
// only the `returned len` chars are filled and further calls to `reserve` are not made.
// - `putback`: called when only lesser amount of chars than requested was used. Implementations
// must use this information to avoid gaps in the formatted string.
template<typename SB>
concept string_buffer = requires(SB a, usize len) {
    { !std::is_void_v<typename SB::char_type> };
    { a.reserve(len) };
    { a.putback(len) };
};
template<typename SB, typename CharT>
concept string_buffer_of = string_buffer<SB> && std::same_as<CharT, typename SB::char_type>;

// Reusable context, abstracting any `string_buffer`.
// Provides common operations for formatting strings.
// Like int, pointer and string formatting routines.
template<string_buffer SB>
class FormatContext;

// `formatter` API is used by FormatContext to convert a value of type `T` to `string`.
// Implementations need not worry about how memory/space for format string is obtained, as it is
// provided by FormatContext.
// Used defined types need to specialize the `formatter` class for able to be used in `format` API.
template<typename CharT, typename T>
struct formatter {};
// {
//      template<typename FormatContext>
//      constexpr auto format(FormatContext& ctx, const fmt_spec_t &fs, const T& val)
//      noexcept -> expected<bool, Error> {
//          Implementation ...
//      }
// };

namespace detail {
    struct parse_error {
        const char* reason;
        usize pos;
        bool is_eob;
    };

    // NOLINTBEGIN(hicpp-explicit-conversions, misc-non-private-member-variables-in-classes)
    template<typename T>
    struct optional {
        constexpr optional() = default;
        constexpr optional(std::nullopt_t /*nullopt*/) {}
        constexpr optional(T value) : m_has_value {true}, m_value {value} {}

        constexpr explicit operator bool() const {
            return m_has_value;
        }

        [[nodiscard]] constexpr auto value() const -> const auto& {
            if (!m_has_value) {
                abort_("optional is empty");
            }
            return m_value;
        }
        [[nodiscard]] constexpr auto value() -> auto& {
            if (!m_has_value) {
                abort_("optional is empty");
            }
            return m_value;
        }

        bool m_has_value {false};
        T m_value {};
    };

    struct unexpected_t {
    } constexpr unexpected;

    template<typename T, typename E>
    struct expected {
        constexpr expected() : expected {T {}} {}

        template<std::convertible_to<T> U>
        constexpr expected(U ok) : m_tag(Ok), m_ok {ok} {}

        constexpr expected(unexpected_t /*unexpected*/, E err) : m_tag {Error}, m_err {err} {}

        constexpr auto operator*() const -> const auto& {
            check_(m_tag == Ok, "dereferencing unexpected");
            return m_ok.value();
        }

        [[nodiscard]] constexpr auto value() const -> const auto& {
            if (m_tag != Ok) {
                abort_("expected contains error");
            }
            return m_ok.value();
        }

        [[nodiscard]] constexpr auto error() const -> const auto& {
            check_(m_tag == Error, "expected doesn't contain an error");
            return m_err.value();
        }

        constexpr operator bool() const {
            return m_tag == Ok;
        }

        enum Tag { Ok, Error };

        Tag m_tag = {};
        optional<T> m_ok = {};
        optional<E> m_err = {};
    };

    template<typename E>
    struct expected<void, E> {
        constexpr expected() = default;
        constexpr expected(unexpected_t /*unexected*/, E err) : m_err(err) {}

        [[nodiscard]] constexpr auto error() const -> const auto& {
            return m_err.value();
        }

        constexpr operator bool() const {
            return !m_err;
        }

        optional<E> m_err = {};
    };

    // NOLINTBEGIN(cppcoreguidelines-macro-usage,*-macro-parentheses)

#define TryEVT(e, tmp_name) \
    auto&& tmp_name = (e); \
    if (!(tmp_name)) \
        return {detail::unexpected, (tmp_name).error()};

#define TryEV(e) TryEVT(e, CONCAT(tmp__, __LINE__))

#define TryET(varname, e, tmp_name) \
    auto&& tmp_name = (e); \
    if (!(tmp_name)) \
        return {detail::unexpected, (tmp_name).error()}; \
    auto&& varname = *std::move((tmp_name))

#define TryE(varname, e) TryET(varname, e, CONCAT(tmp__, __LINE__))

    // NOLINTEND(cppcoreguidelines-macro-usage,*-macro-parentheses)

    struct range_t {
        usize start;
        usize end;  // Inclusive
    };

    struct literal_t {
        range_t range;
    };

    struct argument_id_t {
        range_t range;
    };

    struct simple_replacement_t {
        constexpr simple_replacement_t() = default;

        constexpr simple_replacement_t(u64 argid) : m_argid {argid} {}

        [[nodiscard]] constexpr auto has_argid() const -> bool {
            return static_cast<bool>(m_argid);
        }

        [[nodiscard]] constexpr auto argid() const -> u64 {
            check_(has_argid(), "");
            return m_argid.value();
        }

        constexpr void set_argid(u64 argid) {
            m_argid = argid;
        }

        optional<u64> m_argid;
    };

    struct direct_or_replacement {
        enum Tag { Replacement, Direct };

        // Constructs Replacement
        constexpr direct_or_replacement() = default;
        constexpr direct_or_replacement(u64 direct) : m_tag {Direct}, m_direct {direct} {}

        constexpr direct_or_replacement(simple_replacement_t replacement) :
            m_replacement {replacement} {}

        [[nodiscard]] constexpr auto has_replacement() const -> bool {
            return m_tag == Replacement;
        }
        [[nodiscard]] constexpr auto has_direct() const -> bool {
            return m_tag == Direct;
        }

        [[nodiscard]] constexpr auto direct() const -> u64 {
            check_(has_direct(), "");
            return m_direct.value();
        }
        [[nodiscard]] constexpr auto replacement() const -> const simple_replacement_t& {
            check_(has_replacement(), "");
            return m_replacement.value();
        }
        [[nodiscard]] constexpr auto replacement() -> simple_replacement_t& {
            check_(has_replacement(), "");
            return m_replacement.value();
        }

        Tag m_tag = Replacement;
        optional<simple_replacement_t> m_replacement = simple_replacement_t {};
        optional<u64> m_direct;
    };

    struct fill_and_align_t {
        enum align_t { start = '<', end = '>', center = '^' };

        static constexpr auto default_fill = ' ';
        static constexpr auto default_align = align_t::end;

        char fill;
        align_t align;
    };

    enum class sign_t { plus = '+', minus = '-', space = ' ' };

    enum class type_t {
        // string
        string = 's',
        escape = '?',

        // integer
        binary = 'b',
        upper_binary = 'B',
        char_ = 'c',
        decimal = 'd',
        octal = 'o',
        hex = 'x',
        upper_hex = 'X',

        // pointer
        pointer = 'p',

        // floating precision
        f_hex = 'a',
        f_upper_hex = 'A',
        scientific = 'e',
        upper_scientific = 'E',
        fixed = 'f',
        upper_fixed = 'F',
        general = 'g',
        upper_general = 'G',
    };

    struct fmt_spec_t {
        optional<fill_and_align_t> fill_and_align;
        optional<sign_t> sign;
        bool use_alternative_form = false;
        bool zero_pad = false;
        optional<direct_or_replacement> width;
        optional<direct_or_replacement> precision;
        bool locale_specific = false;
        optional<type_t> type;
    };

    struct replacement_t {
        simple_replacement_t value;
        optional<fmt_spec_t> fmt_spec;
    };

    struct field_t {
        enum Tag { Literal, Replacement };

        constexpr field_t() = default;
        constexpr field_t(literal_t literal) : m_tag {Literal}, m_literal {literal} {}
        constexpr field_t(replacement_t replacement) :
            m_tag {Replacement},
            m_replacement {replacement} {}

        [[nodiscard]] constexpr auto has_literal() const {
            return m_tag == Literal;
        }
        [[nodiscard]] constexpr auto has_replacement() const {
            return m_tag == Replacement;
        }

        [[nodiscard]] constexpr auto literal() const -> const literal_t& {
            check_(has_literal(), "");
            return m_literal.value();
        }
        [[nodiscard]] constexpr auto replacement() const -> const auto& {
            check_(has_replacement(), "");
            return m_replacement.value();
        }
        [[nodiscard]] constexpr auto replacement() -> auto& {
            check_(has_replacement(), "");
            return m_replacement.value();
        }

        Tag m_tag = {};
        optional<literal_t> m_literal;
        optional<replacement_t> m_replacement;
    };

    template<usize N>
    struct format_string_t {
        using iterator_t = field_t*;
        using const_iterator_t = const field_t*;

        // NOLINTNEXTLINE(*-dynamic-static-initializers)
        static constexpr auto FieldCount = N;

        [[nodiscard]] constexpr auto begin() -> iterator_t {
            return std::begin(m_fields);
        }
        [[nodiscard]] constexpr auto begin() const -> const_iterator_t {
            return std::begin(m_fields);
        }

        [[nodiscard]] constexpr auto end() -> iterator_t {
            return std::end(m_fields);
        }
        [[nodiscard]] constexpr auto end() const -> const_iterator_t {
            return std::end(m_fields);
        }

        constexpr auto operator[](usize I) const -> const field_t& {
            check_(I < FieldCount, "");
            return m_fields[I];
        }

        constexpr auto operator[](usize I) -> field_t& {
            check_(I < FieldCount, "");
            return m_fields[I];
        }

        field_t m_fields[N];  // NOLINT(*-avoid-c-arrays)
    };
    // NOLINTEND(hicpp-explicit-conversions, misc-non-private-member-variables-in-classes)

    template<typename CharT>
    class Parser {
      public:
        constexpr explicit Parser(basic_string_view<CharT> fmt_str) : m_fmt_str {fmt_str} {}

        constexpr auto parse(std::invocable<field_t> auto&& take) -> expected<void, parse_error> {
            while (m_cur_pos < m_fmt_str.length()) {
                switch (m_cur_state) {
                    case Empty: {
                        TryE(literal, parse_string_literal());
                        if (literal) {
                            take(literal.value());
                        }
                    } break;

                    case InsideReplacement: {
                        TryE(repl, parse_replacement());
                        take(repl);
                        m_cur_state = Empty;
                    } break;
                }
            }

            if (m_cur_state != Empty) {
                return {
                    unexpected,
                    parse_error {
                        .reason = "unexpected end of buffer found",
                        .pos = m_cur_pos,
                        .is_eob = true}};
            }

            // Success
            return {};
        }

      private:
        constexpr auto parse_replacement() -> expected<replacement_t, parse_error> {
            replacement_t replacement;

            TryE(arg_id, parse_integer());
            if (arg_id) {
                replacement.value = simple_replacement_t {arg_id.value()};
            }

            TryE(fmt_spec, parse_fmt_spec());
            if (fmt_spec) {
                replacement.fmt_spec = fmt_spec;
            }

            TryE(c, peek_one());
            if (c != '}') {
                return {
                    unexpected,
                    parse_error {
                        .reason = "expected '}', but found something else",
                        .pos = m_cur_pos,
                        .is_eob = false}};
            }

            ++m_cur_pos;
            return replacement;
        }

        // Returns a range of chars, which are part of string literal.
        // Read's until end of buffer or end of string literal.
        // Positions the `m_cur_pos` one past the end of the range.
        // Returns an empty range if m_fmt_str doesn't start with string literals.
        constexpr auto parse_string_literal() -> expected<optional<literal_t>, parse_error> {
            auto start = m_cur_pos;

            if (start == m_fmt_str.length())
                return {};

            char prv = '\0';
            for (; m_cur_pos < m_fmt_str.length(); ++m_cur_pos) {
                auto c = at(m_fmt_str, m_cur_pos);

                // End of literal range
                if ((c == '}' && prv == '}') || (c == '{' && prv == '{')) {
                    auto end = m_cur_pos - 1;
                    ++m_cur_pos;
                    return literal_t {{start, end}};
                }

                // Found astray '}'
                if (prv == '}' && c != '}') {
                    return {
                        unexpected,
                        parse_error {
                            .reason = "unexpected '}'",
                            .pos = m_cur_pos,
                            .is_eob = false}};
                }

                // End of literal range
                if (prv == '{') {
                    m_cur_state = InsideReplacement;
                    if (m_cur_pos - 1 != start) {
                        return literal_t {{
                            .start = start,
                            .end = m_cur_pos - 2,
                        }};
                    }
                    return {};
                }

                prv = c;
            }

            if (prv == '{') {
                return {
                    unexpected,
                    parse_error {.reason = "unmatched '{'", .pos = m_cur_pos, .is_eob = true}};
            }
            if (prv == '}') {
                return {
                    unexpected,
                    parse_error {.reason = "unmatched '}'", .pos = m_cur_pos, .is_eob = true}};
            }

            return literal_t {{
                .start = start,
                .end = m_cur_pos - 1,
            }};
        }

        constexpr auto parse_fmt_spec() -> expected<optional<fmt_spec_t>, parse_error> {
            TryE(c, peek_one());
            if (c != ':') {
                return {};
            }

            ++m_cur_pos;

            fmt_spec_t fmt_spec;

            TryE(fill_and_align, parse_fill_and_align());
            if (fill_and_align) {
                fmt_spec.fill_and_align = fill_and_align;
            }

            TryE(sign, peek_one());
            if (sign == '+' || sign == '-' || sign == ' ') {
                ++m_cur_pos;
                fmt_spec.sign = sign_t {sign};
            }

            TryE(pound, peek_one());
            if (pound == '#') {
                ++m_cur_pos;
                fmt_spec.use_alternative_form = true;
            }

            TryE(zero, peek_one());
            if (zero == '0') {
                ++m_cur_pos;
                fmt_spec.zero_pad = true;
            }

            TryE(width, parse_width());
            if (width) {
                fmt_spec.width = width;
            }

            TryE(precision, parse_precision());
            if (precision) {
                fmt_spec.precision = precision;
            }

            TryE(locale, peek_one());
            if (locale == 'L') {
                ++m_cur_pos;
                fmt_spec.locale_specific = true;
            }

            TryE(type, parse_type());
            if (type) {
                fmt_spec.type = type;
            }

            return fmt_spec;
        }

        constexpr auto parse_fill_and_align() -> expected<optional<fill_and_align_t>, parse_error> {
            TryE(first, peek_one());
            auto second = [&]() -> optional<char> {
                // Peek next
                if (m_cur_pos + 1 == m_fmt_str.length())
                    return {};
                return at(m_fmt_str, m_cur_pos + 1);
            }();

            switch (auto align = static_cast<fill_and_align_t::align_t>(first)) {
                case fill_and_align_t::start:
                case fill_and_align_t::end:
                case fill_and_align_t::center:
                    ++m_cur_pos;
                    return fill_and_align_t {
                        .fill = fill_and_align_t::default_fill,
                        .align = align};
            }

            if (first == '{' || first == '}' || !second)
                return {};

            switch (auto align = static_cast<fill_and_align_t::align_t>(second.value())) {
                case fill_and_align_t::start:
                case fill_and_align_t::end:
                case fill_and_align_t::center:
                    m_cur_pos += 2;
                    return fill_and_align_t {.fill = first, .align = align};
            }

            return {};
        }

        constexpr auto parse_width() -> expected<optional<direct_or_replacement>, parse_error> {
            TryE(c, peek_one());
            if (c == '{') {
                ++m_cur_pos;
                TryE(width, parse_sub_replacement());

                TryE(c, peek_one());
                if (c != '}') {
                    return {
                        unexpected,
                        parse_error {
                            .reason = "expected '}', but found something else.",
                            .pos = m_cur_pos,
                            .is_eob = false}};
                }

                ++m_cur_pos;
                return width;
            }

            TryE(i, parse_integer());
            if (i) {
                if (i.value() > std::numeric_limits<u64>::max()) {
                    return {
                        unexpected,
                        parse_error {
                            .reason = "width must fit in u64",
                            .pos = m_cur_pos,
                            .is_eob = false}};
                }
                return direct_or_replacement {static_cast<u64>(i.value())};
            }

            return {};
        }

        constexpr auto parse_precision() -> expected<optional<direct_or_replacement>, parse_error> {
            TryE(c, peek_one());
            if (c != '.') {
                return {};
            }

            ++m_cur_pos;
            TryE(ch, peek_one());
            if (ch == '{') {
                ++m_cur_pos;
                TryE(precision, parse_sub_replacement());

                TryE(c, peek_one());
                if (c != '}') {
                    return {
                        unexpected,
                        parse_error {
                            .reason = "expected '}', but found something else.",
                            .pos = m_cur_pos,
                            .is_eob = false}};
                }

                ++m_cur_pos;
                return precision;
            }

            TryE(i, parse_integer());
            if (i) {
                if (i.value() > std::numeric_limits<u64>::max()) {
                    return {
                        unexpected,
                        parse_error {
                            .reason = "precision must fit in u64",
                            .pos = m_cur_pos,
                            .is_eob = false}};
                }
                return direct_or_replacement {static_cast<u64>(i.value())};
            }

            return {
                unexpected,
                parse_error {
                    .reason = "expected precison specifier, but none found",
                    .pos = m_cur_pos,
                    .is_eob = false}};
        }

        constexpr auto parse_type() -> expected<optional<type_t>, parse_error> {
            TryE(c, peek_one());
            auto t = static_cast<type_t>(c);
            switch (t) {
                case type_t::string:
                case type_t::escape:
                case type_t::binary:
                case type_t::upper_binary:
                case type_t::char_:
                case type_t::decimal:
                case type_t::octal:
                case type_t::hex:
                case type_t::upper_hex:
                case type_t::pointer:
                case type_t::f_hex:
                case type_t::f_upper_hex:
                case type_t::scientific:
                case type_t::upper_scientific:
                case type_t::fixed:
                case type_t::upper_fixed:
                case type_t::general:
                case type_t::upper_general:
                    ++m_cur_pos;
                    return t;

                default:
                    return {};
            }
        }

        // Positions `m_cur_pos` at the end of replacement field.
        // This is typically '}'.
        constexpr auto parse_sub_replacement() -> expected<direct_or_replacement, parse_error> {
            TryE(field, ([&]() -> expected<direct_or_replacement, parse_error> {
                     TryE(c, peek_one());
                     if (c == '}') {
                         return direct_or_replacement {};
                     }

                     TryE(argid, parse_integer());
                     if (argid) {
                         return direct_or_replacement {simple_replacement_t {argid.value()}};
                     }

                     return {
                         unexpected,
                         parse_error {
                             .reason =
                                 "unexpected char found while parsing width/precision replacement",
                             .pos = m_cur_pos,
                             .is_eob = false}};
                 }()));

            return field;
        }

        // Returns an integer parsing the decimal integer digits.
        // Positions the `m_cur_pos` one past the end of the range.
        // Returns a nullopt if m_fmt_str doesn't start with an integer.
        // Return an error, when end-of-buffer is reached.
        constexpr auto parse_integer() -> expected<optional<u64>, parse_error> {
            if (m_cur_pos == m_fmt_str.length()) {
                return {
                    unexpected,
                    parse_error {
                        .reason = "unexpected end of buffer while parsing integer",
                        .pos = m_cur_pos,
                        .is_eob = true}};
            }

            auto start = m_cur_pos;

            for (; m_cur_pos < m_fmt_str.length(); ++m_cur_pos) {
                auto c = at(m_fmt_str, m_cur_pos);
                if (c < '0' || c > '9') {
                    break;
                }
            }

            if (m_cur_pos == m_fmt_str.length()) {
                return {
                    unexpected,
                    parse_error {
                        .reason = "unexpected end of buffer while parsing integer",
                        .pos = m_cur_pos,
                        .is_eob = true}};
            }

            if (m_cur_pos == start) {
                return {};
            }

            u64 val = 0;
            [[maybe_unused]] auto res =
                from_chars<int_base::dec>(&m_fmt_str[start], &m_fmt_str[m_cur_pos], val);
            assert(res.ec == std::errc {});

            return val;
        }

        // Returns the char at current position.
        // `m_cur_pos` is unchanged.
        // Return an error, when end-of-buffer is reached.
        constexpr auto peek_one() -> expected<char, parse_error> {
            if (m_cur_pos == m_fmt_str.length()) {
                return {
                    unexpected,
                    parse_error {
                        .reason = "unexpected end of buffer",
                        .pos = m_cur_pos,
                        .is_eob = true}};
            }

            return at(m_fmt_str, m_cur_pos);
        }

        enum State {
            // State when the parser is outside any literal/replacement fields.
            // Expects a literal/replacement field.
            Empty,
            // State when the parser is inside a replacement field.
            InsideReplacement
        };

        basic_string_view<CharT> m_fmt_str;
        usize m_cur_pos = 0;
        State m_cur_state = Empty;
    };

    template<typename CharT>
    constexpr auto field_count(basic_string_view<CharT> fmt_str) -> expected<usize, parse_error> {
        detail::Parser parser(fmt_str);

        usize count = 0;
        TryEV(parser.parse([&count](auto /* field */) { count++; }));

        return count;
    }

    template<usize N>
    // NOLINTNEXTLINE(*-cognitive-complexity)
    constexpr auto rewrite(const format_string_t<N>& FmtStr) -> optional<format_string_t<N>> {
        bool has_argument_id = false;
        bool first_replacement = true;
        u32 arg_count = 0;
        auto rewritten_fmt_str = FmtStr;

        for (auto& field : rewritten_fmt_str) {
            if (!field.has_replacement()) {
                continue;
            }

            auto& rep = field.replacement();

            if (rep.value.has_argid()) {
                if (first_replacement) {
                    has_argument_id = true;
                } else if (!has_argument_id) {
                    return {};
                }
            } else {
                if (first_replacement) {
                    has_argument_id = false;
                } else if (has_argument_id) {
                    return {};
                }
                rep.value.set_argid(arg_count);
            }

            first_replacement = false;
            arg_count++;

            if (rep.fmt_spec) {
                auto& fmt_spec = rep.fmt_spec.value();
                if (fmt_spec.width && fmt_spec.width.value().has_replacement()) {
                    if (!fmt_spec.width.value().replacement().has_argid()) {
                        fmt_spec.width.value().replacement().set_argid(arg_count);
                    }
                    arg_count++;
                }
                if (fmt_spec.precision && fmt_spec.precision.value().has_replacement()) {
                    if (!fmt_spec.precision.value().replacement().has_argid()) {
                        fmt_spec.precision.value().replacement().set_argid(arg_count);
                    }
                    arg_count++;
                }
            }
        }

        return rewritten_fmt_str;
    }

    template<const_string FmtStr, usize N = detail::field_count(FmtStr.view()).value()>
    constexpr auto parse() -> detail::expected<detail::format_string_t<N>, parse_error> {
        detail::Parser parser(FmtStr.view());
        detail::format_string_t<N> fmt_str;

        usize i = 0;
        TryEV(parser.parse([&](auto field) { fmt_str.m_fields[i++] = std::move(field); }));

        if (auto new_fmt_str = detail::rewrite(fmt_str)) {
            return new_fmt_str.value();
        }
        return {
            detail::unexpected,
            parse_error {.reason = "cannot rewrite format string", .pos = 0, .is_eob = false}};
    }

#undef TryE
#undef TryET
#undef TryEV
#undef TryEVT

    template<const_string FmtStr, usize N = detail::field_count(FmtStr.view()).value()>
    constexpr auto parse_and_check() -> detail::format_string_t<N> {
        constexpr auto fmt_str = parse<FmtStr>();
        static_assert(fmt_str, "cannot compile format string");
        return *fmt_str;
    }

    template<typename S, typename CharT>
    concept string_type =
        std::ranges::range<S> && std::convertible_to<std::ranges::range_value_t<S>, CharT>;

    template<typename I>
    concept int_type = std::integral<I>;

    template<typename ArgT, typename CharT>
    constexpr auto check_type(replacement_t rep) -> bool {
        static_assert(!std::floating_point<ArgT>, "floating point types are not supported");
        if (rep.fmt_spec) {
            auto& fmt_spec = rep.fmt_spec.value();
            if (fmt_spec.type) {
                switch (fmt_spec.type.value()) {
                    case type_t::string:
                        return string_type<ArgT, CharT> || std::is_same_v<ArgT, bool>;

                    case type_t::escape:
                        return string_type<ArgT, CharT> || std::is_same_v<ArgT, char>;

                    case type_t::binary:
                    case type_t::upper_binary:
                    case type_t::decimal:
                    case type_t::octal:
                    case type_t::hex:
                    case type_t::upper_hex:
                        return int_type<ArgT>;

                    case type_t::char_:
                        return int_type<ArgT> && !std::is_same_v<ArgT, bool>;

                    case type_t::pointer:
                        return std::is_pointer_v<ArgT>;

                    default:
                        abort_("floating point types are not supported");
                }
            }
        }

        return true;
    }

    template<typename Arg>
    using add_const_pointer_t = std::add_pointer_t<std::add_const_t<Arg>>;

    template<typename Arg>
    using remove_const_pointer_t = std::remove_const_t<std::remove_pointer_t<Arg>>;

    template<typename... Args>
    class FmtArgs {
      public:
        using tuple_type = std::tuple<add_const_pointer_t<Args>...>;

        constexpr explicit FmtArgs(const Args&... args) : m_data {std::make_tuple(&args...)} {}

        template<usize I, typename FmtArgs>
        constexpr friend auto get(FmtArgs&& args) noexcept -> const auto&;

      private:
        tuple_type m_data;
    };

    template<usize I, typename FmtArgs>
    constexpr auto get(FmtArgs&& args) noexcept -> const auto& {
        return *std::get<I>(std::forward<FmtArgs>(args).m_data);
    }

    template<const_string FmtStr, field_t F, typename... Args>
    struct formatter_base {
        using FmtArgs = class FmtArgs<Args...>;
        using char_type = typename std::decay_t<decltype(FmtStr)>::char_type;

        template<string_buffer_of<char_type> SB>
        [[nodiscard]] static constexpr auto
        format(FormatContext<SB>& ctx, const FmtArgs& args) noexcept -> ktl::expected<bool, Error> {
            if constexpr (F.has_literal()) {
                const auto* begin = &FmtStr.value[F.m_literal.value().range.start];
                const auto* end = &FmtStr.value[F.m_literal.value().range.end] + 1;

                (void)args;
                return ctx.Write(begin, end);
            } else {
                static_assert(
                    F.replacement().value.argid() < std::tuple_size_v<typename FmtArgs::tuple_type>,
                    "argument_id exceeds argument count");

                using unchecked_type = remove_const_pointer_t<std::tuple_element_t<
                    F.replacement().value.argid(),
                    typename FmtArgs::tuple_type>>;

                static_assert(
                    check_type<unchecked_type, char_type>(F.replacement()),
                    "cannot compile field due to type or argument check failure");

                return do_format<F.replacement()>(ctx, args);
            }
        }
    };

    struct str_type_t {
    } constexpr str_type;

    template<
        const_string RawFmtStr,
        format_string_t FS,
        string_buffer_of<typename std::decay_t<decltype(RawFmtStr)>::char_type> SB,
        typename... Args>
    constexpr auto vformat(SB& sb, const detail::FmtArgs<Args...>& args) noexcept
        -> ktl::expected<Result, Error>;

    template<const_string FmtStr, format_string_t FS, typename... Args>
    constexpr auto vformat(str_type_t, auto& str, const detail::FmtArgs<Args...>& args) noexcept
        -> ktl::expected<Result, Error>;

    template<const_string FmtStr, typename... Args>
    constexpr auto
    vformat(str_type_t str_type, auto& str, const detail::FmtArgs<Args...>& args) noexcept
        -> ktl::expected<Result, Error> {
        constexpr auto FormatStringRes = parse<FmtStr>();
        static_assert(FormatStringRes, "cannot parse format string");

        constexpr auto FormatString = *FormatStringRes;

        return vformat<FmtStr, FormatString, Args...>(str_type, str, args);
    }

    template<
        const_string FmtStr,
        string_buffer_of<typename std::decay_t<decltype(FmtStr)>::char_type> SB,
        typename... Args>
    constexpr auto vformat(SB& sb, const detail::FmtArgs<Args...>& args) noexcept
        -> ktl::expected<Result, Error> {
        constexpr auto FormatStringRes = parse<FmtStr>();
        static_assert(FormatStringRes, "cannot parse format string");

        constexpr auto FormatString = *FormatStringRes;
        return vformat<FmtStr, FormatString, SB, Args...>(sb, args);
    }

    template<typename CharT>
    class counting_buffer;
}  // namespace detail

template<typename CharT>
class fixed_buffer;

// Holds both the Raw Format string and Transformed one.
template<const_string RawFmtStr>
struct format_string_t {
    // NOLINTNEXTLINE(*-dynamic-static-initializers)
    static constexpr auto underlying_value = detail::parse_and_check<RawFmtStr>();
    using underlying_type = std::decay<decltype(underlying_value)>;
    using char_type = typename std::decay_t<decltype(RawFmtStr)>::char_type;

    // NOLINTNEXTLINE(*-dynamic-static-initializers)
    static constexpr auto raw_fmt_str = RawFmtStr;

    constexpr auto value() const noexcept {
        return underlying_value;
    }

    // Writes formatted string into StringBuffer (`sb`).
    // Only minimum of `formatted_size` and `sb available` chars are written into the buffer.
    template<string_buffer_of<char_type> SB, typename... Args>
    constexpr auto format(SB& sb, const Args&... args) const noexcept -> expected<Result, Error> {
        return detail::vformat<RawFmtStr, underlying_value>(sb, detail::FmtArgs {args...});
    }

    // Writes formatted chars into the provided fixed/static_string.
    // Only the maximum of capacity - 1 chars are written into the buffer.
    template<std::integral Size, typename... Args>
    constexpr auto format(
        fixed_string<char_type, Size, std::char_traits<char_type>>& str,
        const Args&... args) const noexcept -> expected<Result, Error> {
        return detail::vformat<RawFmtStr, underlying_value>(
            detail::str_type,
            str,
            detail::FmtArgs {args...});
    }

    template<auto Size, typename... Args>
    constexpr auto format(
        basic_static_string<char_type, Size, std::char_traits<char_type>>& str,
        const Args&... args) const noexcept -> expected<Result, Error> {
        return detail::vformat<RawFmtStr, underlying_value>(
            detail::str_type,
            str,
            detail::FmtArgs {args...});
    }

    // Returns the length of the formatted string.
    template<typename... Args>
    constexpr auto size(const Args&... args) const noexcept -> expected<usize, Error> {
        usize len = 0;
        detail::counting_buffer<char_type> cb {len};

        TryV((detail::vformat<RawFmtStr, underlying_value>(cb, detail::FmtArgs {args...})));

        return len;
    }

    // Format the string in compile time (consteval) and return static_string<char_type, size(...)>
    // containing the formatted chars.
    template<auto... Args>
    constexpr auto format() const noexcept {
        constexpr auto Len = *std::decay_t<decltype(*this)> {}.size(Args...);

        basic_static_string<char_type, Len + 1> buf;
        [[maybe_unused]] auto resize_res = buf.resize_uninitialized(Len);
        assert(resize_res);

        fixed_buffer ab {buf};

        [[maybe_unused]] auto res =
            detail::vformat<RawFmtStr, underlying_value>(ab, detail::FmtArgs {Args...});

        assert(res && "cannot format string");
        assert(res->len() == Len);

        return buf;
    }
};

template<const_string FmtStr>
constexpr auto make_format_string() noexcept -> format_string_t<FmtStr> {
    return {};
}

namespace literals {
    template<const_string FmtStr>
    constexpr auto operator""_cs() {
        return FmtStr;
    }

    template<const_string FmtStr>
    constexpr auto operator""_f() {
        return make_format_string<FmtStr>();
    }
}  // namespace literals

// Writes formatted string into StringBuffer (`sb`).
// Only minimum of `formatted_size` and `sb available` chars are written into the buffer.
template<
    const_string FmtStr,
    string_buffer_of<typename std::decay_t<decltype(FmtStr)>::char_type> SB,
    typename... Args>
constexpr auto format(SB& sb, const Args&... args) noexcept -> expected<Result, Error> {
    return detail::vformat<FmtStr>(sb, detail::FmtArgs {args...});
}

// Helper functions for formatting into string types
// XXX: Passed in string will be cleared.
template<const_string FmtStr, std::integral Size, typename... Args>
constexpr auto format(
    fixed_string<
        typename std::decay_t<decltype(FmtStr)>::char_type,
        Size,
        std::char_traits<typename std::decay_t<decltype(FmtStr)>::char_type>>& str,
    const Args&... args) noexcept -> expected<Result, Error> {
    return detail::vformat<FmtStr>(detail::str_type, str, detail::FmtArgs {args...});
}

template<const_string FmtStr, auto Size, typename... Args>
constexpr auto format(
    basic_static_string<typename std::decay_t<decltype(FmtStr)>::char_type, Size>& str,
    const Args&... args) noexcept -> expected<Result, Error> {
    return detail::vformat<FmtStr>(detail::str_type, str, detail::FmtArgs {args...});
}

// Returns the length of the formatted string.
template<const_string FmtStr, typename... Args>
constexpr auto formatted_size(const Args&... args) noexcept -> expected<usize, Error>;
}  // namespace ktl::fmt