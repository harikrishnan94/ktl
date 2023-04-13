#pragma once

#include <ktl/access.hpp>

#include "detail/string_ops.hpp"

namespace ktl {
template<typename CharT, auto Capacity, typename Traits>
    requires std::is_trivial_v<CharT> && std::integral<std::decay_t<decltype(Capacity)>>
class basic_static_string;

template<typename CharT, std::integral Size, typename Traits = std::char_traits<CharT>>
    requires(!std::is_const_v<CharT>)
class fixed_string:
    public detail::
        string_ops<CharT, Traits, std::make_unsigned_t<Size>, fixed_string<CharT, Size, Traits>> {
  public:
    using traits_type = Traits;
    using value_type = CharT;
    using size_type = std::make_unsigned_t<Size>;
    using difference_type = isize;
    using reference = CharT&;
    using const_reference = const CharT&;
    using pointer = CharT*;
    using const_pointer = const CharT*;

  private:
    using base =
        detail::string_ops<CharT, Traits, size_type, fixed_string<CharT, size_type, Traits>>;

  public:
    constexpr fixed_string() = delete;

    // NOLINTNEXTLINE(*-easily-swappable-parameters)
    constexpr fixed_string(not_null<pointer> chars, size_type& len, size_type capacity) noexcept :
        m_chars {chars},
        m_len {&len},
        m_capacity {capacity} {
        check_(*m_len > 0 && len <= m_capacity, "");
        m_chars[*m_len - 1] = base::NUL;
    }

    template<auto Capacity>
        requires std::integral<std::decay_t<decltype(Capacity)>>
        && (std::numeric_limits<size_type>::max() >= Capacity)
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr fixed_string(std::array<CharT, Capacity>& arr, size_type& len) :
        fixed_string {arr.data(), len, arr.size()} {}

    template<auto Capacity>
        requires std::integral<std::decay_t<decltype(Capacity)>>
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr fixed_string(basic_static_string<CharT, Capacity, Traits>& str) :
        fixed_string {str.as_fixed_string()} {}

    constexpr auto max_size() const noexcept -> size_type {
        return m_capacity - 1;
    }

  private:
    // Allow access to internal members. Classic CRTP.
    friend class detail::string_ops<CharT, Traits, size_type, fixed_string<CharT, Size, Traits>>;

    [[nodiscard]] constexpr auto get_storage() const noexcept
        -> detail::string_storage<const CharT> {
        return {.begin = m_chars, .end = m_chars + *m_len, .end_cap = m_chars + m_capacity};
    }
    constexpr auto get_storage() noexcept -> detail::string_storage<CharT> {
        return {.begin = m_chars, .end = m_chars + *m_len, .end_cap = m_chars + m_capacity};
    }

    constexpr auto grow(usize req_len) noexcept -> expected<void, Error> {
        if (req_len > m_capacity) [[unlikely]] {
            Throw(Error::BufferFull);
        }
        return {};
    }
    constexpr auto grow_uninit(usize req_len) noexcept -> expected<void, Error> {
        return grow(req_len);
    }

    constexpr auto set_len(size_type new_len) noexcept {
        assert(new_len <= m_capacity && "length cannot exceed capacity");
        *m_len = new_len;
    }

    pointer m_chars = nullptr;
    size_type* __restrict__ m_len = 0;
    size_type m_capacity = 0;
};

template<typename CharT, typename SizeT>
fixed_string(not_null<CharT*> ptr, SizeT& len, SizeT capacity) -> fixed_string<CharT, SizeT>;

template<typename CharT, typename SizeT>
fixed_string(CharT* ptr, SizeT& len, SizeT capacity) -> fixed_string<CharT, SizeT>;

template<typename CharT, auto Capacity>
fixed_string(std::array<CharT, Capacity>&, typename std::array<CharT, Capacity>::size_type&)
    -> fixed_string<CharT, typename std::array<CharT, Capacity>::size_type>;

template<typename CharT, auto Capacity, typename Traits>
fixed_string(basic_static_string<CharT, Capacity, Traits>&) -> fixed_string<
    CharT,
    typename basic_static_string<CharT, Capacity, Traits>::size_type,
    Traits>;
}  // namespace ktl

namespace std {
template<typename T, std::integral Capacity, typename Traits>
inline constexpr bool ranges::enable_borrowed_range<ktl::fixed_string<T, Capacity, Traits>> = true;

template<typename ElementType, std::integral Capacity, typename Traits>
inline constexpr bool ranges::enable_view<ktl::fixed_string<ElementType, Capacity, Traits>> = true;
}  // namespace std
