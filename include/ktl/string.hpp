#pragma once

#include <ktl/access.hpp>
#include <ktl/bitops.hpp>

#include "detail/string_ops.hpp"

namespace ktl {
template<typename CharT, std::integral Size, typename Traits>
    requires(!std::is_const_v<CharT>)
class fixed_string;

template<
    typename CharT,
    typename Traits,
    allocator_for<CharT> Allocator,
    typename GP = default_growth_policy>
    requires std::is_trivial_v<CharT> && growth_policy<GP> || growth_policy_for<GP, Allocator>
         class basic_string:
    public detail::string_ops<CharT, Traits, usize, basic_string<CharT, Traits, Allocator, GP>> {
  public:
    using traits_type = Traits;
    using allocator_type = Allocator;
    using value_type = CharT;
    using size_type = usize;
    using difference_type = isize;
    using reference = CharT&;
    using const_reference = const CharT&;
    using pointer = CharT*;
    using const_pointer = const CharT*;

  private:
    using alloc_traits = allocator_traits<Allocator>;
    using base =
        detail::string_ops<CharT, Traits, size_type, basic_string<CharT, Traits, Allocator, GP>>;

  public:
    // ------------------------ Special member functions --------------------------
    constexpr explicit basic_string(const Allocator& a = {}) noexcept : m_alloc {a} {
        if (std::is_constant_evaluated()) {
            const_init();
        }
    }

    constexpr ~basic_string() {
        auto [is_short, chars, _, capacity] = m_storage.extract();
        if (!is_short) {
            alloc_traits::deallocate(m_alloc, chars, capacity);
        }
    }

    constexpr basic_string(const basic_string&) = delete;
    constexpr auto operator=(const basic_string&) -> basic_string& = delete;

    constexpr basic_string(basic_string&& o) noexcept :
        m_storage(std::move(o.m_storage).take()),
        m_alloc {std::move(o.m_alloc)} {}

    // Based on Howard Hinnat's answer: https://stackoverflow.com/a/27472502
    constexpr auto operator=(basic_string&& o) noexcept -> basic_string& {
        using std::swap;

        if constexpr (alloc_traits::propagate_on_container_move_assignment::value) {
            m_storage.swap(o.m_storage);
            swap(m_alloc, o.m_alloc);
        } else {
            if (alloc_traits::equals(m_alloc, o.m_alloc)) {
                m_storage.swap(o.m_storage);
            } else {
                check_(this->assign(o.begin(), o.end()), "move assignment must not fail");
            }
        }

        return *this;
    }

    // Follows move assignment.
    constexpr void swap(basic_string& o) noexcept {
        using std::swap;

        if constexpr (alloc_traits::propagate_on_container_swap::value) {
            m_storage.swap(o.m_storage);
            swap(m_alloc, o.m_alloc);
        } else {
            if (alloc_traits::equals(m_alloc, o.m_alloc)) {
                m_storage.swap(o.m_storage);
            } else {
                basic_string tmp {m_alloc};

                check_(tmp.assign(o.begin(), o.end()), "move assignment must not fail");
                check_(o.assign(this->begin(), this->end()), "move assignment must not fail");

                m_storage.swap(tmp.m_storage);
            }
        }
    }

    [[nodiscard]] constexpr auto max_size() const noexcept -> size_type {
        // Short String optimization takes away `1 byte` away from capacity to store tag.
        return std::numeric_limits<size_type>::max() / std::numeric_limits<u8>::max() - 1;
    }

    constexpr auto reserve(size_type new_cap) noexcept -> expected<void, Error> {
        return grow(new_cap);
    }

    constexpr auto shrink_to_fit() noexcept -> expected<void, Error> {
        auto [is_short, chars, len, capacity] = m_storage.extract();
        assert(len > 0);

        if (len != capacity && !is_short) {
            if (storage_t::is_short(len)) {
                auto lstr = std::move(m_storage).take();

                std::construct_at(&m_storage);
                auto new_chars = std::get<pointer>(m_storage.extract());
                uninitialized_move_n(chars, len, new_chars);
                alloc_traits::deallocate(m_alloc, chars, capacity);
                m_storage.set_len(len);
            } else {
                Try(new_chars, alloc_traits::allocate(m_alloc, len));

                uninitialized_move_n(chars, len, static_cast<value_type*>(new_chars));
                alloc_traits::deallocate(m_alloc, chars, capacity);
                m_storage.set_long_str(new_chars, len, len);
            }
        }

        return {};
    }

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr operator fixed_string<value_type, size_type, Traits>() const noexcept {
        return fixed_string {this->data(), this->capacity(), this->size()};
    }

    constexpr auto get_allocator() const noexcept -> Allocator {
        return m_alloc;
    }
    constexpr auto get_allocator_for_clone() const noexcept -> Allocator {
        return alloc_traits::select_on_container_copy_construction(m_alloc);
    }

    // --------------------- Substr ---------------------

    constexpr auto substr(size_type pos = 0, size_type count = base::npos) const& noexcept
        -> expected<basic_string, Error> {
        Try(ss, basic_string_view {*this}.substr(pos, count));

        basic_string str {m_alloc};
        TryV(str.assign(ss.begin(), ss.end()));

        return str;
    }

    constexpr auto substr(size_type pos = 0, size_type count = base::npos) && noexcept
        -> expected<typename base::non_null_ptr, Error> {
        return base::substr_destructive(pos, count);
    }

  private:
    // Allow access to internal members. Classic CRTP.
    friend class detail::
        string_ops<CharT, Traits, size_type, basic_string<CharT, Traits, Allocator, GP>>;

    // NOLINTBEGIN(*-pro-type-union-access)
    class storage_t {
      public:
        constexpr storage_t() {
            if (std::is_constant_evaluated()) {
                lstr = {};
            } else {
                sstr.chars[0] = base::NUL;
                sstr.len = 1;
            }
        }

        constexpr auto take() && noexcept -> storage_t {
            auto copy = *this;
            lstr.chars = nullptr;
            return copy;
        }
        constexpr void swap(storage_t& o) & noexcept {
            using std::swap;
            swap(lstr, o.lstr);
        }

        constexpr auto extract() const noexcept -> std::tuple<bool, const_pointer, usize, usize> {
            return extract(*this);
        }
        constexpr auto extract() noexcept -> std::tuple<bool, pointer, usize, usize> {
            return extract(*this);
        }

        // NOLINTNEXTLINE(*-easily-swappable-parameters)
        constexpr void set_long_str(pointer chars, size_type len, size_type capacity) noexcept {
            assert(chars != nullptr);
            assert(len <= capacity);
            lstr.chars = chars;
            lstr.len = len;
            lstr.cap = capacity;

            if (!std::is_constant_evaluated()) {
                if constexpr (std::endian::native == std::endian::big) {
                    lstr.cap <<= LongStrTagBits;
                }
                lstr.cap = SetMaskedBits(lstr.cap, LongStrTagMask, LongStrTagMask);
            }
        }

        constexpr void set_len(size_type len) noexcept {
            if (is_short()) {
                assert(is_short(len));
                assert(len <= short_string::Capacity);
                sstr.len = len;
            } else {
                assert(lstr.len <= lstr.cap);
                lstr.len = len;
            }
        }

        [[nodiscard]] static constexpr auto is_short(size_type cap) noexcept -> bool {
            // Always use `long_string` during const evaluation.
            return !std::is_constant_evaluated() && cap <= storage_t::short_string::Capacity;
        }

      private:
        struct long_string {
            pointer chars;
            size_type len;  // Including NUL char
            size_type cap;
        };

        struct short_string {
            // NOLINTNEXTLINE(*-dynamic-static-initializers)
            static constexpr auto Capacity = sizeof(long_string) / sizeof(CharT) - 1;

            alignas(long_string) std::array<char, Capacity> chars;
            u8 len;  // Len == Cap. Including NUL char
        };

        template<typename Self>
        static constexpr auto extract(Self& self) noexcept -> std::tuple<
            bool,
            std::conditional_t<std::is_const_v<Self>, const_pointer, pointer>,
            usize,
            usize> {
            if (self.is_short()) {
                return {
                    true,
                    self.sstr.chars.data(),
                    static_cast<usize>(self.sstr.len),
                    static_cast<usize>(short_string::Capacity)};
            }

            auto cap = [&] {
                if (std::is_constant_evaluated()) {
                    return self.lstr.cap;
                } else {  // NOLINT
                    if constexpr (std::endian::native == std::endian::little) {
                        return self.lstr.cap & ~LongStrTagMask;
                    } else {  // NOLINT
                        return self.lstr.cap >> LongStrTagBits;
                    }
                }
            }();
            return {
                false,
                self.lstr.chars,
                static_cast<usize>(self.lstr.len),
                static_cast<usize>(cap)};
        }

        [[nodiscard]] constexpr auto is_short() const noexcept -> bool {
            if (std::is_constant_evaluated()) {
                return false;
            } else {  // NOLINT
                return sstr.len != LongStrTag;
            }
        }

        static constexpr size_type LongStrTag = std::numeric_limits<u8>::max();
        static constexpr size_type LongStrTagBits = std::numeric_limits<u8>::digits;
        static constexpr size_type LongStrTagMask = std::endian::native == std::endian::little
            ? static_cast<size_type>(std::numeric_limits<u8>::max())
                << (std::numeric_limits<size_type>::digits - LongStrTagBits)
            : LongStrTagBits;

        union {
            long_string lstr;
            short_string sstr;
        };
    };
    // NOLINTEND(*-pro-type-union-access)

    [[nodiscard]] constexpr auto get_storage() const noexcept
        -> detail::string_storage<const CharT> {
        auto [_, chars, len, capacity] = m_storage.extract();
        return {.begin = chars, .end = chars + len, .end_cap = chars + capacity};
    }
    constexpr auto get_storage() noexcept -> detail::string_storage<CharT> {
        auto [_, chars, len, capacity] = m_storage.extract();
        return {.begin = chars, .end = chars + len, .end_cap = chars + capacity};
    }

    constexpr auto grow(usize req_cap) noexcept -> expected<void, Error> {
        return grow_impl(req_cap, [](CharT* new_chars, auto* chars, auto len) {
            uninitialized_move_n(chars, len, new_chars);
        });
    }
    constexpr auto grow_uninit(usize req_cap) noexcept -> expected<void, Error> {
        return grow_impl(req_cap, [](CharT* new_chars, auto* /* chars */, auto len) {
            std::construct_at(new_chars + len - 1, base::NUL);
        });
    }

    template<typename Initializer>
    constexpr auto grow_impl(usize req_cap, Initializer&& initializer) noexcept
        -> expected<void, Error> {
        auto [is_short, chars, len, capacity] = m_storage.extract();
        assert(len != 0);

        if (req_cap > capacity) [[unlikely]] {
            if (is_short && storage_t::is_short(req_cap)) {
                return {};
            }

            auto new_cap = ktl::grow<GP>(m_alloc, capacity, req_cap);
            Try(new_chars, alloc_traits::allocate(m_alloc, new_cap));

            std::invoke(std::forward<Initializer>(initializer), new_chars, chars, len);

            if (!is_short) {
                alloc_traits::deallocate(m_alloc, chars, capacity);
            }
            m_storage.set_long_str(new_chars, len, new_cap);
        }
        return {};
    }

    constexpr auto set_len(usize new_len) noexcept {
        auto [_is_short, _chars, _len, capacity] = m_storage.extract();
        assert(new_len <= capacity && "length cannot exceed capacity");
        m_storage.set_len(new_len);
    }

    constexpr void const_init() noexcept {
        auto res = alloc_traits::allocate(m_alloc, 1);
        check_(res, "must not fail during const evaluation");
        CharT* chars = *res;
        m_storage.set_long_str(chars, 1, 1);
        std::construct_at(chars, base::NUL);
    }

    storage_t m_storage = {};
    [[no_unique_address]] Allocator m_alloc = {};
};

template<allocator_for<char> Allocator>
using string = basic_string<char, std::char_traits<char>, Allocator>;
template<allocator_for<char> Allocator>
using u8string = basic_string<char8_t, std::char_traits<char8_t>, Allocator>;
template<allocator_for<char> Allocator>
using u16string = basic_string<char16_t, std::char_traits<char16_t>, Allocator>;
template<allocator_for<char> Allocator>
using u32string = basic_string<char32_t, std::char_traits<char32_t>, Allocator>;
template<allocator_for<char> Allocator>
using wstring = basic_string<wchar_t, std::char_traits<wchar_t>, Allocator>;

template<allocator_like Allocator, typename CharT>
constexpr auto make_string(const CharT* s, const Allocator& alloc = Allocator {}) noexcept
    -> expected<basic_string<CharT, std::char_traits<CharT>, Allocator>, Error> {
    basic_string<CharT, std::char_traits<CharT>, Allocator> str {alloc};

    TryV(str.assign(s));
    return str;
}

template<allocator_like Allocator, typename CharT, typename SizeT>
constexpr auto
make_string(const CharT* s, SizeT len, const Allocator& alloc = Allocator {}) noexcept
    -> expected<basic_string<CharT, std::char_traits<CharT>, Allocator>, Error> {
    basic_string<CharT, std::char_traits<CharT>, Allocator> str {alloc};

    TryV(str.assign(s, len));
    return str;
}

template<allocator_like Allocator, typename CharT, typename SizeT>
constexpr auto
make_string(basic_string_view<CharT> s, const Allocator& alloc = Allocator {}) noexcept
    -> expected<basic_string<CharT, std::char_traits<CharT>, Allocator>, Error> {
    basic_string<CharT, std::char_traits<CharT>, Allocator> str {alloc};

    TryV(str.assign(s));
    return str;
}

template<allocator_like Allocator, typename CharT, typename SizeT>
constexpr auto make_string(SizeT count, CharT ch, const Allocator& alloc = Allocator {}) noexcept
    -> expected<basic_string<CharT, std::char_traits<CharT>, Allocator>, Error> {
    basic_string<CharT, std::char_traits<CharT>, Allocator> str {alloc};

    TryV(str.assign(count, ch));
    return str;
}

template<allocator_like Alloc, std::input_iterator InputIt>
constexpr auto
make_string(InputIt first, InputIt last, const Alloc& a = Alloc {}) noexcept -> expected<
    basic_string<std::iter_value_t<InputIt>, std::char_traits<std::iter_value_t<InputIt>>, Alloc>,
    Error> {
    basic_string<std::iter_value_t<InputIt>, std::char_traits<std::iter_value_t<InputIt>>, Alloc>
        str {a};

    TryV(str.assign(first, last));

    return str;
}

template<allocator_like Allocator, typename CharT>
constexpr auto
make_string(std::initializer_list<CharT> ilist, const Allocator& alloc = Allocator {}) noexcept
    -> expected<basic_string<CharT, std::char_traits<CharT>, Allocator>, Error> {
    basic_string<CharT, std::char_traits<CharT>, Allocator> str {alloc};

    TryV(str.assign(ilist));
    return str;
}
}  // namespace ktl