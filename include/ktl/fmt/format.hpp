#pragma once

#include <optional>
#include <variant>

#include "core.hpp"
#include "ktl/contiguous_iterator.hpp"

namespace ktl::fmt {
namespace detail {
    template<fmt_spec_t FS>
    struct dyn_fmt_spec_t {
        // NOLINTNEXTLINE(*-dynamic-static-initializers)
        static constexpr auto FmtSpec = FS;

        u64 width;
        u64 precision;
    };

    template<direct_or_replacement SR, typename... Args>
    static constexpr auto replace(const FmtArgs<Args...>& args) -> ktl::expected<u64, Error> {
        using tuple_type = typename FmtArgs<Args...>::tuple_type;

        if constexpr (SR.has_replacement()) {
            static_assert(SR.replacement().has_argid());
            constexpr auto argid = SR.replacement().argid();

            static_assert(
                argid < std::tuple_size_v<tuple_type>,
                "argument_id exceeds argument count");

            using rep_type = remove_const_pointer_t<
                std::tuple_element_t<argid, typename FmtArgs<Args...>::tuple_type>>;

            static_assert(std::integral<rep_type>, "width is not integral type");

            auto val = get<argid>(args);
            if constexpr (std::is_signed_v<rep_type>) {
                if (val < 0) {
                    return make_unexpected(Error::ReplacementError);
                }
            }
            return static_cast<u64>(val);
        } else {
            return SR.direct();
        }
    }

    template<fmt_spec_t FmtSpec, typename CharT, typename ArgT, typename... Args>
    // NOLINTNEXTLINE(*-function-cognitive-complexity)
    static constexpr auto canonicalize() -> fmt_spec_t {
        auto converted = FmtSpec;

        if constexpr (FmtSpec.sign) {
            static_assert(
                std::is_arithmetic_v<ArgT>,
                "sign is only applicable to integer or floating point types");
        } else {
            if constexpr (std::is_arithmetic_v<ArgT>) {
                converted.sign = sign_t::minus;
            }
        }
        if constexpr (FmtSpec.use_alternative_form) {
            static_assert(
                std::is_arithmetic_v<ArgT>,
                "alternative form is only applicable to integer or floating point types");
        }
        if constexpr (FmtSpec.zero_pad) {
            static_assert(
                std::is_arithmetic_v<ArgT>,
                "zero pad is only applicable to integer or floating point types");
            // If the 0 character and an align option both appear, the 0 character is ignored.
            converted.zero_pad = !converted.fill_and_align;
        }
        if constexpr (FmtSpec.width) {
            if (!converted.fill_and_align && !converted.zero_pad) {
                converted.fill_and_align = fill_and_align_t {
                    .fill = fill_and_align_t::default_fill,
                    .align = fill_and_align_t::default_align,
                };
            }
        } else {
            converted.fill_and_align = {};
            converted.zero_pad = false;
        }
        if constexpr (FmtSpec.precision) {
            static_assert(
                std::floating_point<ArgT>,
                "precision is only applicable to floating point types");
        }
        if constexpr (FmtSpec.locale_specific) {
            static_assert(!FmtSpec.locale_specific, "locale support is not available");
        }
        if (!converted.type) {
            if constexpr (string_type<ArgT, CharT> || std::same_as<ArgT, bool>) {
                converted.type = type_t::string;
            } else if constexpr (std::is_arithmetic_v<ArgT>) {
                converted.type = type_t::decimal;
            } else {
                static_assert(
                    std::is_pointer_v<ArgT> || std::same_as<ArgT, std::nullptr_t>,
                    "must be a pointer type");
                converted.type = type_t::pointer;
            }
        }
        assert(converted.type && "must contain a type");

        return converted;
    }

    template<fmt_spec_t FmtSpec, typename CharT, typename ArgT, typename... Args>
    static constexpr auto flatten(const FmtArgs<Args...>& args)
        -> ktl::expected<dyn_fmt_spec_t<FmtSpec>, Error> {
        dyn_fmt_spec_t<FmtSpec> fp = {};

        if constexpr (FmtSpec.width) {
            if (auto w = replace<FmtSpec.width.value()>(args)) {
                fp.width = *w;
            } else {
                return make_unexpected(w.error());
            }
        }
        if constexpr (FmtSpec.precision) {
            if (auto p = replace<FmtSpec.precision.value()>(args)) {
                fp.precision = *p;
            } else {
                return make_unexpected(p.error());
            }
        }

        return fp;
    }

    template<replacement_t R, string_buffer SB, typename... Args>
    constexpr auto do_format(FormatContext<SB>& ctx, const FmtArgs<Args...>& args) noexcept
        -> ktl::expected<bool, Error> {
        using fmt_arg_t = remove_const_pointer_t<
            std::tuple_element_t<R.value.argid(), typename FmtArgs<Args...>::tuple_type>>;
        using char_type = typename FormatContext<SB>::char_type;

        constexpr auto FmtSpec = []() {
            if constexpr (R.fmt_spec) {
                return canonicalize<R.fmt_spec.value(), char_type, fmt_arg_t, Args...>();
            } else {
                return canonicalize<fmt_spec_t {}, char_type, fmt_arg_t, Args...>();
            }
        }();

        formatter<char_type, fmt_arg_t> formatter;
        auto fmt_spec = flatten<FmtSpec, char_type, fmt_arg_t>(args);
        if (!fmt_spec) {
            return make_unexpected(std::move(fmt_spec).error());
        }

        return formatter.format(ctx, *fmt_spec, get<R.value.argid()>(args));
    }

    template<
        fixed_string RawFmtStr,
        usize I,
        format_string_t FmtStr,
        string_buffer_of<typename std::decay_t<decltype(RawFmtStr)>::char_type> SB,
        typename... Args>
    constexpr auto
    vformat_apply(FormatContext<SB>& ctx, const detail::FmtArgs<Args...>& args) noexcept
        -> ktl::expected<bool, Error> {
        using char_type = typename SB::char_type;
        using fmt_base_t = formatter_base<RawFmtStr, FmtStr[I], Args...>;

        if (auto res = fmt_base_t::format(ctx, args); !res) {
            return make_unexpected(std::move(res).error());
        } else {  // NOLINT
            if (!*res) {
                return false;
            }
        }

        if constexpr (I != std::decay_t<decltype(FmtStr)>::FieldCount - 1) {
            if (auto res = vformat_apply<RawFmtStr, I + 1, FmtStr>(ctx, args); !res) {
                return make_unexpected(std::move(res).error());
            } else {  // NOLINT
                if (!*res) {
                    return false;
                }
            }
        }

        return true;
    }

    template<
        fixed_string RawFmtStr,
        format_string_t FS,
        string_buffer_of<typename std::decay_t<decltype(RawFmtStr)>::char_type> SB,
        typename... Args>
    constexpr auto vformat(SB& sb, const detail::FmtArgs<Args...>& args) noexcept
        -> ktl::expected<Result, Error> {
        FormatContext ctx {sb};
        auto res = vformat_apply<RawFmtStr, 0, FS>(ctx, args);
        if (res) {
            return Result {ctx.FormattedLen(), *res};
        }
        return make_unexpected(std::move(res).error());
    }

    template<typename CharT>
    class counting_iterator {
      public:
        class deref_proxy {
          public:
            constexpr explicit deref_proxy(usize& len) : m_len {&len} {}

            // NOLINTNEXTLINE(*-unconventional-assign-operator, *-c-copy-assignment-signature)
            constexpr void operator=(CharT /* c */) const noexcept {
                *m_len += 1;
            }

          private:
            usize* m_len;
        };

        using difference_type = isize;
        using value_type = CharT;

        constexpr explicit counting_iterator(usize& len) : m_len {&len} {}

        constexpr auto operator*() noexcept -> deref_proxy {
            return deref_proxy {*m_len};
        }

        constexpr auto operator++() noexcept -> counting_iterator& {
            return *this;
        }

        constexpr auto operator++(int) noexcept -> counting_iterator {
            auto temp = *this;
            return ++temp;
        }

      private:
        usize* m_len;
    };

    template<typename CharT>
    class counting_buffer {
      public:
        using char_type = CharT;

        constexpr explicit counting_buffer(usize& len) : m_iter {len} {}

        constexpr auto reserve(usize len) noexcept -> buffer_view<CharT, counting_iterator<CharT>> {
            return {m_iter, len};
        }

        constexpr void putback(usize len) noexcept {}

      private:
        counting_iterator<CharT> m_iter;
    };
}  // namespace detail

template<fixed_string FmtStr, typename... Args>
constexpr auto formatted_size(Args&&... args) noexcept -> expected<usize, Error> {
    return make_format_string<FmtStr>().size(std::forward<Args>(args)...);
}

template<string_buffer SB>
class FormatContext {
  public:
    using char_type = typename SB::char_type;

    constexpr explicit FormatContext(SB& sb) noexcept : m_sb(&sb) {}

    constexpr auto FormattedLen() const noexcept {
        return m_len;
    }

    constexpr auto Write(const char_type* begin, const char_type* end) noexcept
        -> expected<bool, Error> {
        return write<false>(begin, end);
    }

    template<detail::fmt_spec_t FS, std::integral Int>
    constexpr auto Format(const detail::dyn_fmt_spec_t<FS>& fmt_spec, Int val) noexcept
        -> expected<bool, Error> {
        to_chars_res res;

        if (auto error = to_chars<FS>(res, val)) {
            return make_unexpected(std::move(*error));
        }

        return format(fmt_spec, res);
    }

    template<detail::fmt_spec_t FS, typename T>
    constexpr auto Format(const detail::dyn_fmt_spec_t<FS>& fmt_spec, const T* ptr) noexcept
        -> expected<bool, Error> {
        static_assert(FS.type.value() == detail::type_t::pointer);
        to_chars_res res;

        constexpr detail::optional<detail::sign_t> Sign = detail::sign_t::minus;
        res.len = to_chars_helper_hex<false, Sign, true>(
            res.buf.data(),
            std::bit_cast<std::uintptr_t>(ptr));

        return format(fmt_spec, res);
    }

    template<detail::fmt_spec_t FS>
    constexpr auto
    Format(const detail::dyn_fmt_spec_t<FS>& fmt_spec, std::nullptr_t /* ptr */) noexcept
        -> expected<bool, Error> {
        static_assert(FS.type.value() == detail::type_t::pointer);
        return Format(fmt_spec, static_cast<void*>(nullptr));
    }

    template<detail::fmt_spec_t FS>
    constexpr auto Format(
        const detail::dyn_fmt_spec_t<FS>& fmt_spec,
        const char_type* begin,
        const char_type* end) noexcept -> expected<bool, Error> {
        constexpr auto Escape = FS.type.value() == detail::type_t::escape;
        auto len = end - begin;

        if constexpr (FS.width) {
            if (len >= fmt_spec.width) {
                return write<Escape>(begin, end);
            }
            return format_str(fmt_spec, begin, end);
        } else {
            return write<Escape>(begin, end);
        }
    }

  private:
    static constexpr auto IntMaxDigits = std::numeric_limits<u64>::digits;
    static constexpr auto IntMaxLen = IntMaxDigits + 3;  // 1(sign)+2(Alt form 0b/0x)

    struct to_chars_len {
        u8 sign_and_prefix;
        u8 num;
    };

    struct to_chars_res {
        std::array<char_type, IntMaxLen> buf;
        to_chars_len len;
    };

    template<bool Escape>
    constexpr auto write(const char_type* begin, const char_type* end) noexcept -> bool {
        auto req_len = end - begin;
        auto buf = m_sb->reserve(req_len);

        static_assert(!Escape, "C++23 escape sequence is not implemented");

        std::copy_n(begin, buf.len, buf.out);

        m_len += buf.len;
        return buf.len == req_len;
    }

    template<char_type C, bool Escape>
    constexpr auto fill(usize count) noexcept -> bool {
        auto buf = m_sb->reserve(count);

        static_assert(!Escape, "C++23 escape sequence is not implemented");

        std::fill_n(buf.out, buf.len, C);

        m_len += buf.len;
        return buf.len == count;
    }

    template<detail::fmt_spec_t FS>
    constexpr auto format(const detail::dyn_fmt_spec_t<FS>& fmt_spec, const to_chars_res& res)
        -> expected<bool, Error> {
        const auto& [buf, reslen] = res;
        auto [prefix_len, num_len] = reslen;
        auto len = prefix_len + num_len;

        constexpr auto Escape = FS.type.value() == detail::type_t::escape;

        if constexpr (FS.width) {
            if (len >= fmt_spec.width) {
                return write<Escape>(buf.data(), buf.data() + len);
            }

            if constexpr (FS.zero_pad) {
                static_assert(!FS.fill_and_align, "both fill align and zeropad cannot be present");
                return format_zero_pad(fmt_spec, res);
            } else {
                return format_str(fmt_spec, buf.data(), buf.data() + len);
            }
        } else {
            return write<Escape>(buf.data(), buf.data() + len);
        }
    }

    template<detail::fmt_spec_t FS, std::integral Int>
    static constexpr auto to_chars(to_chars_res& res, Int val) noexcept -> std::optional<Error> {
        res.len = {};

        using enum detail::type_t;
        if constexpr (FS.type.value() == string) {
            static_assert(std::same_as<Int, bool>);
            std::basic_string_view str = val ? "true" : "false";
            res.len.num = str.length();
            std::copy_n(str.begin(), res.len.num, res.buf.begin());
        } else if constexpr (FS.type.value() == escape || FS.type.value() == char_) {
            if constexpr (FS.type.value() == escape) {
                static_assert(std::same_as<Int, char_type>);
            } else {
                static_assert(std::same_as<Int, bool>);
            }

            auto c = static_cast<u64>(val);
            if (c < std::numeric_limits<char_type>::min()
                || c > std::numeric_limits<char_type>::max()) {
                return Error::ValueOverflow;
            }
            res.buf[0] = static_cast<char_type>(c);
            res.len.num = 1;
        } else {
            res.len = to_chars_helper<FS>(res.buf.data(), val);
        }

        return {};
    }

    template<detail::fmt_spec_t FS>
    constexpr auto format_str(
        const detail::dyn_fmt_spec_t<FS>& fmt_spec,
        const char_type* begin,
        const char_type* end) noexcept -> expected<bool, Error> {
        static_assert(FS.width);
        static_assert(FS.fill_and_align);
        constexpr auto Escape = FS.type.value() == detail::type_t::escape;

        auto len = end - begin;

        assert(fmt_spec.width > len);

        constexpr auto FillChar = FS.fill_and_align.value().fill;
        constexpr auto Align = FS.fill_and_align.value().align;

        auto fill_len = fmt_spec.width - len;

        if constexpr (Align == detail::fill_and_align_t::start) {
            return write<Escape>(begin, end) && fill<FillChar, Escape>(fill_len);
        } else if constexpr (Align == detail::fill_and_align_t::end) {
            return fill<FillChar, Escape>(fill_len) && write<Escape>(begin, end);
        } else if constexpr (Align == detail::fill_and_align_t::center) {
            auto left_fill_len = fill_len / 2;
            auto right_fill_len = fill_len - left_fill_len;

            return fill<FillChar, Escape>(left_fill_len) && write<Escape>(begin, end)
                && fill<FillChar, Escape>(right_fill_len);
        } else {
            abort_("unknown align value");
        }
    }

    template<detail::fmt_spec_t FS>
    constexpr auto
    format_zero_pad(const detail::dyn_fmt_spec_t<FS>& fmt_spec, const to_chars_res& chars) noexcept
        -> expected<bool, Error> {
        static_assert(FS.width);
        static_assert(FS.zero_pad);
        constexpr auto Escape = FS.type.value() == detail::type_t::escape;

        const auto& [buf, reslen] = chars;
        auto [prefix_len, num_len] = reslen;
        const auto* prefix = buf.data();
        const auto* num = prefix + prefix_len;
        auto len = prefix_len + num_len;

        assert(fmt_spec.width > len);

        auto padlen = fmt_spec.width - len;

        return write<Escape>(prefix, prefix + prefix_len) && fill<'0', Escape>(padlen)
            && write<Escape>(num, num + num_len);
    }

    template<detail::fmt_spec_t FS, std::integral Int>
    static constexpr auto to_chars_helper(char_type* buf, Int v) noexcept -> to_chars_len {
        auto val = [&] {
            if constexpr (std::same_as<Int, bool>) {
                return static_cast<u8>(v);
            } else {
                return v;
            }
        }();

        constexpr auto Sign = FS.sign;
        constexpr auto AppendPrefix = FS.use_alternative_form;

        using enum detail::type_t;
        if constexpr (FS.type.value() == binary) {
            return to_chars_helper_binary<false, Sign, AppendPrefix>(buf, val);
        } else if constexpr (FS.type.value() == upper_binary) {
            return to_chars_helper_binary<true, Sign, AppendPrefix>(buf, val);
        } else if constexpr (FS.type.value() == decimal) {
            return to_chars_helper_decimal<Sign>(buf, val);
        } else if constexpr (FS.type.value() == octal) {
            return to_chars_helper_octal<Sign, AppendPrefix>(buf, val);
        } else if constexpr (FS.type.value() == hex) {
            return to_chars_helper_hex<false, Sign, AppendPrefix>(buf, val);
        } else if constexpr (FS.type.value() == upper_hex) {
            return to_chars_helper_hex<true, Sign, AppendPrefix>(buf, val);
        } else {
            abort_("unsupported type for integer");
        }
    }

    template<
        bool Upper,
        detail::optional<detail::sign_t> Sign,
        bool AppendPrefix,
        std::integral Int>
    static constexpr auto to_chars_helper_binary(char_type* buf, Int v) noexcept -> to_chars_len {
        auto* prefix = buf;
        u8 sign_and_prefix_len = append_sign<Sign>(v, prefix[0]) ? 1 : 0;

        if constexpr (AppendPrefix) {
            prefix[sign_and_prefix_len] = '0';
            prefix[sign_and_prefix_len + 1] = Upper ? 'B' : 'b';
            sign_and_prefix_len += 2;
        }

        auto num_len = to_chars_impl<Int, 2, Upper>(v, buf + sign_and_prefix_len);

        return {sign_and_prefix_len, num_len};
    }

    template<detail::optional<detail::sign_t> Sign, std::integral Int>
    static constexpr auto to_chars_helper_decimal(char_type* buf, Int v) noexcept -> to_chars_len {
        u8 sign_and_prefix_len = append_sign<Sign>(v, buf[0]) ? 1 : 0;
        auto num_len = to_chars_impl<Int, 10, false>(  // NOLINT(*-magic-numbers)
            v,
            buf + sign_and_prefix_len);

        return {sign_and_prefix_len, num_len};
    }

    template<detail::optional<detail::sign_t> Sign, bool AppendPrefix, std::integral Int>
    static constexpr auto to_chars_helper_octal(char_type* buf, Int v) noexcept -> to_chars_len {
        auto* prefix = buf;
        u8 sign_and_prefix_len = append_sign<Sign>(v, prefix[0]) ? 1 : 0;

        if constexpr (AppendPrefix) {
            prefix[sign_and_prefix_len++] = '0';
        }

        auto num_len = to_chars_impl<Int, 8, false>(  // NOLINT(*-magic-numbers)
            v,
            buf + sign_and_prefix_len);

        return {sign_and_prefix_len, num_len};
    }

    template<
        bool Upper,
        detail::optional<detail::sign_t> Sign,
        bool AppendPrefix,
        std::integral Int>
    static constexpr auto to_chars_helper_hex(char_type* buf, Int v) noexcept -> to_chars_len {
        auto* prefix = buf;
        u8 sign_and_prefix_len = append_sign<Sign>(v, prefix[0]) ? 1 : 0;

        if constexpr (AppendPrefix) {
            prefix[sign_and_prefix_len] = '0';
            prefix[sign_and_prefix_len + 1] = Upper ? 'X' : 'x';
            sign_and_prefix_len += 2;
        }

        auto num_len = to_chars_impl<Int, 16, Upper>(  // NOLINT(*-magic-numbers)
            v,
            buf + sign_and_prefix_len);

        return {sign_and_prefix_len, num_len};
    }

    template<detail::optional<detail::sign_t> Sign, std::integral Int>
    static constexpr auto append_sign(Int v, char_type& s) -> bool {
        static_assert(Sign, "sign must not be empty");
        using enum detail::sign_t;

        if constexpr (Sign.value() == plus || Sign.value() == space) {
            s = v < 0 ? '-' : static_cast<char_type>(Sign.value());
            return true;
        } else if constexpr (Sign.value() == minus) {
            if constexpr (std::is_signed_v<Int>) {
                if (v < 0) {
                    s = '-';
                    return true;
                }
            }
            return false;
        } else {
            abort_("unknown sign value");
        }
    }

    template<std::integral Int, Int Base, bool Upper>
    // NOLINTNEXTLINE(*-magic-numbers)
        requires requires { Base == 2 || Base == 8 || Base == 10 || Base == 16; }
    static constexpr auto to_chars_impl(Int v, char_type* buf) -> u8 {
        u8 len = 0;
        const auto* digits = [] {
            if constexpr (Upper) {
                return "0123456789ABCDEF";
            }
            return "0123456789abcdef";
        }();
        auto abs = [](auto v) {
            if constexpr (std::is_signed_v<Int>) {
                return v < 0 ? -v : v;
            } else {
                return v;
            }
        };

        buf[0] = digits[0];  // Handle, `v == 0` case.

        while (v) {
            buf[len] = digits[abs(v % Base)];
            len++;
            v /= Base;
        }

        std::reverse(buf, buf + len);
        return std::max<u8>(len, 1);  // Handle, `v == 0` case.
    }

    SB* m_sb;
    usize m_len = 0;
};

// fixed_buffer provides `string_buffer` interface for range of chars.
// It allows `format` API to write formatted output safely into range of chars b/w `{begin, end}`.
// Atmost `end - begin` chars are written.
template<typename CharT>
class fixed_buffer {
  public:
    using char_type = CharT;
    using iterator_type = contiguous_iterator<char_type, KTL_CHECKS_ENABLED>;

    constexpr explicit fixed_buffer(char_type* begin, char_type* end) :
        m_buf {begin},
        m_len {static_cast<usize>(end - begin)} {
        assert(end >= begin);
    }

    constexpr auto reserve(usize len) noexcept -> buffer_view<char_type, iterator_type> {
        auto it = make_contiguous_iterator<KTL_CHECKS_ENABLED>(m_buf + m_pos, m_buf, m_buf + m_len);
        if (m_pos + len <= m_len) [[likely]] {
            m_pos += len;
        } else {
            len = m_len - m_pos;
            m_pos = m_len;
        }
        return {it, len};
    }

    constexpr void putback(usize len) noexcept {
        assert(len <= m_pos);
        m_pos -= len;
    }

  private:
    char_type* m_buf;
    usize m_len;
    usize m_pos = 0;
};

// Partial specializations of `formatter` for builtin types
//---------------------------------------------------------

// Integeral types
template<typename CharT, std::integral I>
struct formatter<CharT, I> {
    template<typename FormatContext, typename FmtSpec>
        requires std::same_as<CharT, typename FormatContext::char_type>
    constexpr auto format(FormatContext& ctx, const FmtSpec& fmt_spec, const I& val) noexcept
        -> expected<bool, Error> {
        return ctx.Format(fmt_spec, val);
    }
};

// Pointer types
template<typename CharT, typename T>
struct formatter<CharT, const T*> {
    template<typename FormatContext, typename FmtSpec>
        requires std::same_as<CharT, typename FormatContext::char_type>
    constexpr auto format(FormatContext& ctx, const FmtSpec& fmt_spec, const T* ptr) noexcept
        -> expected<bool, Error> {
        return ctx.Format(fmt_spec, ptr);
    }
};
template<typename CharT, typename T>
struct formatter<CharT, T*> {
    template<typename FormatContext, typename FmtSpec>
        requires std::same_as<CharT, typename FormatContext::char_type>
    constexpr auto format(FormatContext& ctx, const FmtSpec& fmt_spec, T* ptr) noexcept
        -> expected<bool, Error> {
        return ctx.Format(fmt_spec, ptr);
    }
};
template<typename CharT>
struct formatter<CharT, std::nullptr_t> {
    template<typename FormatContext, typename FmtSpec>
        requires std::same_as<CharT, typename FormatContext::char_type>
    constexpr auto format(FormatContext& ctx, const FmtSpec& fmt_spec, std::nullptr_t ptr) noexcept
        -> expected<bool, Error> {
        return ctx.Format(fmt_spec, ptr);
    }
};

// String types (with `begin()` and `end()`)
template<typename CharT, detail::string_type<CharT> Str>
struct formatter<CharT, Str> {
    template<typename FormatContext, typename FmtSpec>
        requires std::same_as<CharT, typename FormatContext::char_type>
    constexpr auto format(FormatContext& ctx, const FmtSpec& fmt_spec, const Str& str) noexcept
        -> expected<bool, Error> {
        return ctx.Format(fmt_spec, std::begin(str), std::end(str));
    }
};

template<typename CharT>
struct formatter<CharT, const CharT*> {
    template<typename FormatContext, typename FmtSpec>
        requires std::same_as<CharT, typename FormatContext::char_type>
    constexpr auto format(FormatContext& ctx, const FmtSpec& fmt_spec, const CharT* str) noexcept
        -> expected<bool, Error> {
        return ctx.Format(fmt_spec, str, str + std::char_traits<CharT>::length(str));
    }
};
template<typename CharT>
struct formatter<CharT, CharT*> {
    template<typename FormatContext, typename FmtSpec>
        requires std::same_as<CharT, typename FormatContext::char_type>
    constexpr auto format(FormatContext& ctx, const FmtSpec& fmt_spec, CharT* str) noexcept
        -> expected<bool, Error> {
        return ctx.Format(fmt_spec, str, str + std::char_traits<CharT>::length(str));
    }
};
}  // namespace ktl::fmt