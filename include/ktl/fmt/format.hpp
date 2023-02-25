#pragma once

#include <optional>
#include <variant>

#include "core.hpp"

namespace ktl::fmt {
namespace fmt_spec {
    struct fmt_spec_t {
        std::optional<fill_and_align_t> fill_and_align;
        std::optional<sign_t> sign;
        bool use_alternative_form = false;
        bool zero_pad = false;
        std::optional<u64> width;
        std::optional<u64> precision;
        bool locale_specific = false;
        type_t type = type_t::none;
    };
}  // namespace fmt_spec

namespace detail {
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
    static constexpr auto convert(const FmtArgs<Args...>& args)
        -> ktl::expected<fmt_spec::fmt_spec_t, Error> {
        fmt_spec::fmt_spec_t fp;

        if constexpr (FmtSpec.fill_and_align) {
            fp.fill_and_align = FmtSpec.fill_and_align.value();
        }
        if constexpr (FmtSpec.sign) {
            static_assert(
                std::is_arithmetic_v<ArgT>,
                "sign is only applicable to integer or floating types");

            fp.sign = FmtSpec.sign.value();
        } else {
            if constexpr (std::is_arithmetic_v<ArgT>) {
                fp.sign = sign_t::minus;
            }
        }
        if constexpr (FmtSpec.use_alternative_form) {
            static_assert(
                std::is_arithmetic_v<ArgT>,
                "alternative form is only applicable to integer or floating types");
            fp.use_alternative_form = true;
        }
        if constexpr (FmtSpec.zero_pad) {
            static_assert(
                std::is_arithmetic_v<ArgT>,
                "zero pad is only applicable to integer or floating types");
            // If the 0 character and an align option both appear, the 0 character is ignored.
            fp.zero_pad = !FmtSpec.fill_and_align;
        }
        if constexpr (FmtSpec.width) {
            if (auto w = replace<FmtSpec.width.value()>(args)) {
                fp.width = *w;
            } else {
                return make_unexpected(w.error());
            }

            if constexpr (!FmtSpec.fill_and_align && !FmtSpec.zero_pad) {
                fp.fill_and_align = {
                    .fill = fill_and_align_t::default_fill,
                    .align = fill_and_align_t::default_align,
                };
            }
        } else {
            fp.fill_and_align = {};
            fp.zero_pad = false;
        }
        if constexpr (FmtSpec.precision) {
            static_assert(
                std::floating_point<ArgT>,
                "precision is only applicable to floating point types");
            if (auto p = replace<FmtSpec.precision.value()>(args)) {
                fp.precision = *p;
            } else {
                return make_unexpected(p.error());
            }
        }
        if constexpr (FmtSpec.locale_specific) {
            static_assert(!FmtSpec.locale_specific, "locale support is not available");
            fp.locale_specific = true;
        }
        if constexpr (FmtSpec.type) {
            fp.type = FmtSpec.type.value();
        } else {
            if constexpr (string_type<ArgT, CharT> || std::same_as<ArgT, bool>) {
                fp.type = type_t::string;
            } else if constexpr (std::is_arithmetic_v<ArgT>) {
                fp.type = type_t::decimal;
            } else {
                static_assert(std::is_pointer_v<ArgT>, "must be a pointer type");
                fp.type = type_t::pointer;
            }
        }

        return fp;
    }

    template<replacement_t R, typename CharT, string_buffer_of<CharT> SB, typename... Args>
    constexpr auto do_format(FormatContext<SB>& ctx, const FmtArgs<Args...>& args) noexcept
        -> ktl::expected<bool, Error> {
        using fmt_arg_t = remove_const_pointer_t<
            std::tuple_element_t<R.value.argid(), typename FmtArgs<Args...>::tuple_type>>;

        formatter<CharT, fmt_arg_t> formatter;
        auto fmt_spec =
            convert<(R.fmt_spec ? R.fmt_spec.value() : fmt_spec_t {}), CharT, fmt_arg_t>(args);
        if (!fmt_spec) {
            return make_unexpected(fmt_spec.error());
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

    template<typename CharT, usize N>
    class array_out_iterator {
      public:
        using difference_type = isize;
        using value_type = CharT;

        constexpr explicit array_out_iterator(std::array<CharT, N>& buf, usize pos) :
            m_buf {&buf},
            m_pos(pos) {}

        constexpr auto operator*() noexcept -> CharT& {
            return (*m_buf)[m_pos++];
        }

        constexpr auto operator++() noexcept -> array_out_iterator& {
            return *this;
        }

        constexpr auto operator++(int) noexcept -> array_out_iterator {
            auto temp = *this;
            return ++temp;
        }

      private:
        std::array<CharT, N>* m_buf;
        usize m_pos;
    };

    template<typename CharT, usize N>
    class array_buffer {
      public:
        using char_type = CharT;

        constexpr explicit array_buffer(std::array<char_type, N>& buf) : m_buf {&buf} {}

        constexpr auto reserve(usize len) noexcept
            -> buffer_view<CharT, array_out_iterator<CharT, N>> {
            return {array_out_iterator {*m_buf, std::exchange(m_len, m_len + len)}, len};
        }

        constexpr void putback(usize len) noexcept {
            m_len -= len;
        }

      private:
        std::array<char_type, N>* m_buf;
        usize m_len = 0;
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
        return write(begin, end, false);
    }

    template<std::integral Int>
    constexpr auto Format(const fmt_spec::fmt_spec_t& fmt_spec, Int val) noexcept
        -> expected<bool, Error> {
        to_chars_res res;

        if (auto error = to_chars(res, fmt_spec, val)) {
            return make_unexpected(std::move(*error));
        }

        const auto& [buf, prefix_len, num_len] = res;
        auto len = prefix_len + num_len;

        if (!fmt_spec.width || len >= *fmt_spec.width) {
            return write(buf.data(), buf.data() + len, fmt_spec.type == fmt_spec::type_t::escape);
        }

        if (fmt_spec.zero_pad) {
            return format_zero_pad(fmt_spec, res);
        }
        return format_str(fmt_spec, buf.data(), buf.data() + len);
    }

    template<typename T>
    constexpr auto Format(const fmt_spec::fmt_spec_t& fmt_spec, const T* ptr) noexcept
        -> expected<bool, Error> {
        assert(fmt_spec.type == fmt_spec::type_t::pointer);
        auto res = to_chars_helper_hex<false>(
            std::bit_cast<std::uintptr_t>(ptr),
            fmt_spec::sign_t::minus,
            false);
        if (!res) {
            return make_unexpected(res.error());
        }

        auto&& [buf, prefix_len, num_len] = *res;
        return format_str(fmt_spec, res->buf.data(), res->buf.data() + prefix_len + num_len);
    }

    constexpr auto Format(const fmt_spec::fmt_spec_t& fmt_spec, std::nullptr_t /* ptr */) noexcept
        -> expected<bool, Error> {
        assert(fmt_spec.type == fmt_spec::type_t::pointer);
        return Format(fmt_spec, static_cast<void*>(nullptr));
    }

    constexpr auto Format(
        const fmt_spec::fmt_spec_t& fmt_spec,
        const char_type* begin,
        const char_type* end) noexcept -> expected<bool, Error> {
        auto len = end - begin;
        if (!fmt_spec.width || len >= *fmt_spec.width) {
            return write(begin, end, fmt_spec.type == fmt_spec::type_t::escape);
        }
        return format_str(fmt_spec, begin, end);
    }

  private:
    static constexpr auto IntMaxDigits = std::numeric_limits<u64>::digits;
    static constexpr auto IntMaxLen = IntMaxDigits + 3;  // 1(sign)+2(Alt form 0b/0x)

    struct to_chars_res {
        std::array<char_type, IntMaxLen> buf;
        u8 sign_and_prefix_len;
        u8 num_len;
    };

    constexpr auto
    write(const char_type* begin, const char_type* end, [[maybe_unused]] bool escape) noexcept
        -> bool {
        auto req_len = end - begin;
        auto buf = m_sb->reserve(req_len);

        assert(escape == false && "C++23 escape sequence is not implemented");

        std::copy_n(begin, buf.len, buf.out);
        if (req_len != buf.len)
            m_sb->putback(req_len - buf.len);

        m_len += buf.len;
        return buf.len == req_len;
    }

    constexpr auto fill(char_type fill_char, usize count, [[maybe_unused]] bool escape) noexcept
        -> bool {
        auto buf = m_sb->reserve(count);

        assert(escape == false && "C++23 escape sequence is not implemented");

        std::fill_n(buf.out, buf.len, fill_char);
        if (count != buf.len)
            m_sb->putback(count - buf.len);

        m_len += buf.len;
        return buf.len == count;
    }

    template<std::integral Int>
    static constexpr auto
    to_chars(to_chars_res& res, const fmt_spec::fmt_spec_t& fmt_spec, Int val) noexcept
        -> std::optional<Error> {
        res.sign_and_prefix_len = res.num_len = 0;

        switch (fmt_spec.type) {
            using enum fmt_spec::type_t;
            case string: {
                assert((std::same_as<Int, bool>));
                std::basic_string_view str = val ? "true" : "false";
                res.num_len = str.length();
                std::copy_n(str.begin(), res.num_len, res.buf.begin());
            } break;

            case escape:
                assert((std::same_as<Int, char_type>));
            case char_: {
                assert((!std::same_as<Int, bool>));
                auto c = static_cast<u64>(val);
                if (c < std::numeric_limits<char_type>::min()
                    || c > std::numeric_limits<char_type>::max()) {
                    return Error::ValueOverflow;
                }
                res.buf[0] = static_cast<char_type>(c);
                res.num_len = 1;
            } break;

            default:
                std::tie(res.sign_and_prefix_len, res.num_len) =
                    to_chars_helper(res.buf.data(), fmt_spec, val);
                break;
        }

        return {};
    }

    constexpr auto format_str(
        const fmt_spec::fmt_spec_t& fmt_spec,
        const char_type* begin,
        const char_type* end) noexcept -> expected<bool, Error> {
        assert(fmt_spec.width);
        assert(fmt_spec.fill_and_align);

        auto len = end - begin;
        auto width = *fmt_spec.width;

        assert(width > len);

        auto fill_char = fmt_spec.fill_and_align->fill;
        auto fill_len = width - len;
        bool res = true;
        bool escape = fmt_spec.type == fmt_spec::type_t::escape;

        switch (fmt_spec.fill_and_align->align) {
            case fmt_spec::fill_and_align_t::start:
                res = write(begin, end, escape) && fill(fill_char, fill_len, escape);
                break;

            case fmt_spec::fill_and_align_t::end:
                res = fill(fill_char, fill_len, escape) && write(begin, end, escape);
                break;

            case fmt_spec::fill_and_align_t::center: {
                auto left_fill_len = fill_len / 2;
                auto right_fill_len = fill_len - left_fill_len;

                res = fill(fill_char, left_fill_len, escape) && write(begin, end, escape)
                    && fill(fill_char, right_fill_len, escape);
            } break;
        }

        return res;
    }

    constexpr auto
    format_zero_pad(const fmt_spec::fmt_spec_t& fmt_spec, const to_chars_res& chars) noexcept
        -> expected<bool, Error> {
        assert(fmt_spec.width);
        assert(fmt_spec.zero_pad);

        auto&& [buf, prefix_len, num_len] = chars;
        const auto* prefix = buf.data();
        const auto* num = prefix + chars.sign_and_prefix_len;
        auto len = prefix_len + num_len;
        auto width = *fmt_spec.width;
        bool escape = fmt_spec.type == fmt_spec::type_t::escape;

        assert(width > len);

        auto padlen = width - len;

        return write(prefix, prefix + prefix_len, escape) && fill('0', padlen, escape)
            && write(num, num + num_len, escape);
    }

    template<std::integral Int>
    static constexpr auto
    to_chars_helper(char_type* buf, const fmt_spec::fmt_spec_t& fmt_spec, Int v) noexcept
        -> std::pair<u8, u8> {
        auto val = [&] {
            if constexpr (std::same_as<Int, bool>) {
                return static_cast<u8>(v);
            } else {
                return v;
            }
        }();

        auto sign = fmt_spec.sign;
        auto append_prefix = fmt_spec.use_alternative_form;

        switch (fmt_spec.type) {
            using enum fmt_spec::type_t;
            case binary:
                return to_chars_helper_binary<false>(buf, val, sign, append_prefix);
            case upper_binary:
                return to_chars_helper_binary<true>(buf, val, sign, append_prefix);
            case decimal:
                return to_chars_helper_decimal(buf, val, sign);
            case octal:
                return to_chars_helper_octal(buf, val, sign, append_prefix);
            case hex:
                return to_chars_helper_hex<false>(buf, val, sign, append_prefix);
            case upper_hex:
                return to_chars_helper_hex<true>(buf, val, sign, append_prefix);

            default:
                assert(false && "unsupported type for integer");
                __builtin_unreachable();
        }
    }

    template<bool Upper, std::integral Int>
    static constexpr auto to_chars_helper_binary(
        char_type* buf,
        Int v,
        std::optional<fmt_spec::sign_t> sign,
        bool append_prefix) noexcept -> std::pair<u8, u8> {
        auto* prefix = buf;
        u8 sign_and_prefix_len = append_sign(v, prefix[0], sign) ? 1 : 0;

        if (append_prefix) {
            prefix[sign_and_prefix_len] = '0';
            prefix[sign_and_prefix_len + 1] = Upper ? 'B' : 'b';
            sign_and_prefix_len += 2;
        }

        u8 num_len = to_chars_impl<Int, 2, Upper>(v, buf + sign_and_prefix_len);

        return {sign_and_prefix_len, num_len};
    }

    template<std::integral Int>
    static constexpr auto
    to_chars_helper_decimal(char_type* buf, Int v, std::optional<fmt_spec::sign_t> sign) noexcept
        -> std::pair<u8, u8> {
        u8 sign_and_prefix_len = append_sign(v, buf[0], sign) ? 1 : 0;
        u8 num_len = to_chars_impl<Int, 10, false>(  // NOLINT(*-magic-numbers)
            v,
            buf + sign_and_prefix_len);

        return {sign_and_prefix_len, num_len};
    }

    template<std::integral Int>
    static constexpr auto to_chars_helper_octal(
        char_type* buf,
        Int v,
        std::optional<fmt_spec::sign_t> sign,
        bool append_prefix) noexcept -> std::pair<u8, u8> {
        auto* prefix = buf;
        u8 sign_and_prefix_len = append_sign(v, prefix[0], sign) ? 1 : 0;

        if (append_prefix) {
            prefix[sign_and_prefix_len++] = '0';
        }

        u8 num_len = to_chars_impl<Int, 8, false>(  // NOLINT(*-magic-numbers)
            v,
            buf + sign_and_prefix_len);

        return {sign_and_prefix_len, num_len};
    }

    template<bool Upper, std::integral Int>
    static constexpr auto to_chars_helper_hex(
        char_type* buf,
        Int v,
        std::optional<fmt_spec::sign_t> sign,
        bool append_prefix) noexcept -> std::pair<u8, u8> {
        auto* prefix = buf;
        u8 sign_and_prefix_len = append_sign(v, prefix[0], sign) ? 1 : 0;

        if (append_prefix) {
            prefix[sign_and_prefix_len] = '0';
            prefix[sign_and_prefix_len + 1] = Upper ? 'X' : 'x';
            sign_and_prefix_len += 2;
        }

        u8 num_len = to_chars_impl<Int, 16, Upper>(  // NOLINT(*-magic-numbers)
            v,
            buf + sign_and_prefix_len);

        return {sign_and_prefix_len, num_len};
    }

    template<std::integral Int>
    static constexpr auto append_sign(Int v, char_type& i, std::optional<fmt_spec::sign_t> sign)
        -> bool {
        assert(sign);
        switch (*sign) {
            using enum fmt_spec::sign_t;
            case plus:
                i = v < 0 ? '-' : '+';
                return true;
            case minus:
                if (v < 0) {
                    i = '-';
                    return true;
                }
                return false;
            case space:
                i = v < 0 ? '-' : ' ';
                return true;
        }
        assert(false);
        __builtin_unreachable();
    }

    template<std::integral Int, Int Base, bool Upper>
    // NOLINTNEXTLINE(*-magic-numbers)
        requires requires { Base == 2 || Base == 8 || Base == 10 || Base == 16; }
    static constexpr auto to_chars_impl(Int v, char_type* buf) -> usize {
        usize len = 0;
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
        return std::max(len, usize {1});
    }

    SB* m_sb;
    usize m_len = 0;
};

template<typename CharT, std::integral I>
struct formatter<CharT, I> {
    template<typename FormatContext>
        requires std::same_as<CharT, typename FormatContext::char_type>
    constexpr auto
    format(FormatContext& ctx, const fmt_spec::fmt_spec_t& fmt_spec, const I& val) noexcept
        -> expected<bool, Error> {
        return ctx.Format(fmt_spec, val);
    }
};

template<typename CharT, typename T>
struct formatter<CharT, const T*> {
    template<typename FormatContext>
        requires std::same_as<CharT, typename FormatContext::char_type>
    constexpr auto
    format(FormatContext& ctx, const fmt_spec::fmt_spec_t& fmt_spec, const T* ptr) noexcept
        -> expected<bool, Error> {
        return ctx.Format(fmt_spec, ptr);
    }
};

template<typename CharT, detail::string_type<CharT> Str>
struct formatter<CharT, Str> {
    template<typename FormatContext>
        requires std::same_as<CharT, typename FormatContext::char_type>
    constexpr auto
    format(FormatContext& ctx, const fmt_spec::fmt_spec_t& fmt_spec, const Str& str) noexcept
        -> expected<bool, Error> {
        return ctx.Format(fmt_spec, std::begin(str), std::end(str));
    }
};
}  // namespace ktl::fmt