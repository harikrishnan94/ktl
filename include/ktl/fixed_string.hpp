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
    constexpr fixed_string() = default;

    template<auto Capacity>
        requires std::integral<std::decay_t<decltype(Capacity)>>
                     && (std::numeric_limits<size_type>::max() >= Capacity)
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr fixed_string(std::array<CharT, Capacity>& arr, usize len = 1) :
        m_chars {arr.data()},
        m_capacity {Capacity},
        m_len {static_cast<size_type>(len)} {
        check_(m_len > 0 && len <= m_capacity, "");
        m_chars[m_len - 1] = base::NUL;
    }

    template<auto Capacity>
        requires std::integral<std::decay_t<decltype(Capacity)>>
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr fixed_string(basic_static_string<CharT, Capacity, Traits>& str) :
        m_chars {str.data()},
        m_capacity {str.capacity()},
        m_len {str.length() + 1} {}

    constexpr auto max_size() const noexcept -> size_type {
        return m_capacity - 1;
    }

    constexpr auto deep_swap(fixed_string& o) noexcept -> expected<void, Error> {
        if (m_len > o.m_capacity || o.m_len > m_capacity)
            Throw(Error::BufferFull);

        if (m_len > o.m_len) {
            std::swap_ranges(m_chars, m_chars + o.m_len, o.m_chars);
            std::copy_n(m_chars + o.m_len, m_len - o.m_len, o.m_chars + o.m_len);
        } else {
            std::swap_ranges(m_chars, m_chars + m_len, o.m_chars);
            std::copy_n(o.m_chars + m_len, o.m_len - m_len, m_chars + m_len);
        }

        using std::swap;
        swap(m_len, o.m_len);
        return {};
    }

    template<std::input_iterator InputIter>
    constexpr auto clear_and_assign(InputIter first, InputIter last) noexcept
        -> expected<void, std::pair<InputIter, Error>> {
        this->clear();
        return this->assign_iter(first, last);
    }

  private:
    // Allow access to internal members. Classic CRTP.
    friend class detail::string_ops<CharT, Traits, size_type, fixed_string<CharT, Size, Traits>>;

    [[nodiscard]] constexpr auto get_storage() const noexcept
        -> detail::string_storage<const CharT> {
        return {.begin = m_chars, .end = m_chars + m_len, .end_cap = m_chars + m_capacity};
    }
    constexpr auto get_storage() noexcept -> detail::string_storage<CharT> {
        return {.begin = m_chars, .end = m_chars + m_len, .end_cap = m_chars + m_capacity};
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
        m_len = new_len;
    }

    pointer m_chars = nullptr;
    size_type m_capacity = 0;
    size_type m_len = 0;
};

template<typename CharT, auto Capacity>
fixed_string(std::array<CharT, Capacity>&) -> fixed_string<CharT, detail::size_t<Capacity>>;

template<typename CharT, auto Capacity>
fixed_string(std::array<CharT, Capacity>&, usize) -> fixed_string<CharT, detail::size_t<Capacity>>;

template<typename CharT, auto Capacity, typename Traits>
fixed_string(basic_static_string<CharT, Capacity, Traits>&)
    -> fixed_string<CharT, detail::size_t<Capacity>, Traits>;

template<typename CharT, typename SizeT>
fixed_string(CharT* ptr, SizeT) -> fixed_string<CharT, SizeT>;

template<typename CharT, typename SizeT>
fixed_string(CharT* ptr, SizeT, SizeT) -> fixed_string<CharT, SizeT>;
}  // namespace ktl