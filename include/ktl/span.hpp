#pragma once

#include <array>
#include <bit>
#include <limits>
#include <ranges>
#include <type_traits>

#include "assert.hpp"
#include "contiguous_iterator.hpp"
#include "int.hpp"

namespace ktl {
inline constexpr usize dynamic_extent = std::numeric_limits<usize>::max();

template<typename T, usize Extent = dynamic_extent>
class span;

template<typename T>
struct is_std_array: std::false_type {};

template<typename T, usize Size>
struct is_std_array<std::array<T, Size>>: std::true_type {};

template<typename T>
struct is_span: std::false_type {};

template<typename T, usize Size>
struct is_span<span<T, Size>>: std::true_type {};

template<typename Range, typename ElementType>
concept span_compatible_range = std::ranges::contiguous_range<Range>
    && std::ranges::sized_range<Range>
    && (std::ranges::borrowed_range<Range> || std::is_const_v<ElementType>) && !
is_span<std::remove_cvref_t<Range>>::value && !is_std_array<std::remove_cvref_t<Range>>::value
    && !std::is_array_v<std::remove_cvref_t<Range>>
    && std::is_convertible_v<
        // NOLINTBEGIN(*-avoid-c-arrays)
        std::remove_reference_t<std::ranges::range_reference_t<Range>> (*)[],
        ElementType (*)[]>;
// NOLINTEND(*-avoid-c-arrays)

template<typename From, typename To>
// NOLINTNEXTLINE(*-avoid-c-arrays)
concept span_array_convertible = std::is_convertible_v<From (*)[], To (*)[]>;

template<typename It, typename T>
concept span_compatible_iterator = std::contiguous_iterator<It>
    && span_array_convertible<std::remove_reference_t<std::iter_reference_t<It>>, T>;

template<typename Sentinel, typename It>
concept span_compatible_sentinel_for = std::sized_sentinel_for<Sentinel, It> && !
std::is_convertible_v<Sentinel, usize>;

template<typename T, usize Extent>
class span {
  public:
    //  constants and types
    using element_type = T;
    using value_type = std::remove_cv_t<T>;
    using size_type = usize;
    using difference_type = isize;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using iterator = contiguous_iterator<value_type, KTL_CHECKS_ENABLED>;
    using reverse_iterator = std::reverse_iterator<iterator>;

    // NOLINTNEXTLINE(*-dynamic-static-initializers)
    static constexpr size_type extent = Extent;

    // [span.cons], span constructors, copy, assignment, and destructor
    template<usize Size = Extent>
        requires(Size == 0)
    constexpr span() noexcept : m_data {nullptr} {}

    constexpr span(const span&) noexcept = default;
    constexpr auto operator=(const span&) noexcept -> span& = default;
    constexpr span(span&&) noexcept = default;
    constexpr auto operator=(span&&) noexcept -> span& = default;
    constexpr ~span() noexcept = default;

    template<span_compatible_iterator<element_type> It>
    constexpr explicit span(It first, size_type count) : m_data {std::to_address(first)} {
        (void)count;
        check_(Extent == count, "size mismatch in span's constructor (iterator, len)");
    }

    template<span_compatible_iterator<element_type> It, span_compatible_sentinel_for<It> End>
    constexpr explicit span(It first, End last) : m_data {std::to_address(first)} {
        (void)last;
        check_((last - first >= 0), "invalid range in span's constructor (iterator, sentinel)");
        check_(
            last - first == Extent,
            "invalid range in span's constructor (iterator, sentinel): last - first != extent");
    }

    // NOLINTNEXTLINE(*-explicit-conversions, *-avoid-c-arrays)
    constexpr span(std::type_identity_t<element_type> (&arr)[Extent]) noexcept : m_data {arr} {}

    template<span_array_convertible<element_type> OtherElementType>
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr span(std::array<OtherElementType, Extent>& arr) noexcept : m_data {arr.data()} {}

    template<typename OtherElementType>
        requires span_array_convertible<const OtherElementType, element_type>
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr span(const std::array<OtherElementType, Extent>& arr) noexcept :
        m_data {arr.data()} {}

    template<span_compatible_range<element_type> Range>
    // NOLINTNEXTLINE(*-forwarding-reference-overload)
    constexpr explicit span(Range&& r) : m_data {std::ranges::data(r)} {
        check_(std::ranges::size(r) == Extent, "size mismatch in span's constructor (range)");
    }

    template<span_array_convertible<element_type> OtherElementType>
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr span(const span<OtherElementType, Extent>& other) : m_data {other.data()} {}

    template<span_array_convertible<element_type> OtherElementType>
    constexpr explicit span(const span<OtherElementType, dynamic_extent>& other) noexcept :
        m_data {other.data()} {
        check_(Extent == other.size(), "size mismatch in span's constructor (other span)");
    }

    //  ~span() noexcept = default;

    template<usize Count>
    constexpr auto first() const noexcept -> span<element_type, Count> {
        static_assert(Count <= Extent, "span<T, N>::first<Count>(): Count out of range");
        return span<element_type, Count> {data(), Count};
    }

    template<usize Count>
    constexpr auto last() const noexcept -> span<element_type, Count> {
        static_assert(Count <= Extent, "span<T, N>::last<Count>(): Count out of range");
        return span<element_type, Count> {data() + size() - Count, Count};
    }

    constexpr auto first(size_type count) const noexcept -> span<element_type, dynamic_extent> {
        check_(count <= size(), "span<T, N>::first(count): count out of range");
        return {data(), count};
    }

    constexpr auto last(size_type count) const noexcept -> span<element_type, dynamic_extent> {
        check_(count <= size(), "span<T, N>::last(count): count out of range");
        return {data() + size() - count, count};
    }

    template<usize Offset, usize Count = dynamic_extent>
    constexpr auto subspan() const noexcept
        -> span<element_type, Count != dynamic_extent ? Count : Extent - Offset> {
        static_assert(
            Offset <= Extent,
            "span<T, N>::subspan<Offset, Count>(): Offset out of range");
        static_assert(
            Count == dynamic_extent || Count <= Extent - Offset,
            "span<T, N>::subspan<Offset, Count>(): Offset + Count out of range");

        using ReturnType = span<element_type, Count != dynamic_extent ? Count : Extent - Offset>;
        return ReturnType {data() + Offset, Count == dynamic_extent ? size() - Offset : Count};
    }

    constexpr auto subspan(size_type offset, size_type count = dynamic_extent) const noexcept
        -> span<element_type, dynamic_extent> {
        check_(offset <= size(), "span<T, N>::subspan(offset, count): offset out of range");
        check_(
            count <= size() || count == dynamic_extent,
            "span<T, N>::subspan(offset, count): count out of range");
        if (count == dynamic_extent)
            return {data() + offset, size() - offset};
        check_(
            count <= size() - offset,
            "span<T, N>::subspan(offset, count): offset + count out of range");
        return {data() + offset, count};
    }

    [[nodiscard]] constexpr auto size() const noexcept -> size_type {
        return Extent;
    }
    [[nodiscard]] constexpr auto size_bytes() const noexcept -> size_type {
        return Extent * sizeof(element_type);
    }
    [[nodiscard]] constexpr auto empty() const noexcept -> bool {
        return Extent == 0;
    }

    constexpr auto operator[](size_type idx) const noexcept -> reference {
        check_(idx < size(), "span<T, N>::operator[](index): index out of range");
        return m_data[idx];
    }

    constexpr auto front() const noexcept -> reference {
        check_(!empty(), "span<T, N>::front() on empty span");
        return m_data[0];
    }

    constexpr auto back() const noexcept -> reference {
        check_(!empty(), "span<T, N>::back() on empty span");
        return m_data[size() - 1];
    }

    constexpr auto data() const noexcept -> pointer {
        return m_data;
    }

    // [span.iter], span iterator support
    constexpr auto begin() const noexcept -> iterator {
        return make_contiguous_iterator(data(), data(), data() + size());
    }
    constexpr auto end() const noexcept -> iterator {
        return make_contiguous_iterator(data() + size(), data(), data() + size());
    }
    constexpr auto rbegin() const noexcept -> reverse_iterator {
        return reverse_iterator(end());
    }
    constexpr auto rend() const noexcept -> reverse_iterator {
        return reverse_iterator(begin());
    }

    auto as_bytes() const noexcept -> span<const std::byte, Extent * sizeof(element_type)> {
        return span<const std::byte, Extent * sizeof(element_type)> {
            std::bit_cast<const std::byte*>(data()),
            size_bytes()};
    }

    auto as_writable_bytes() const noexcept -> span<std::byte, Extent * sizeof(element_type)> {
        return span<std::byte, Extent * sizeof(element_type)> {
            std::bit_cast<std::byte*>(data()),
            size_bytes()};
    }

  private:
    pointer m_data;  // NOLINT(*-use-default-member-init)
};

template<typename T>
class span<T, dynamic_extent> {
  public:
    //  constants and types
    using element_type = T;
    using value_type = std::remove_cv_t<T>;
    using size_type = usize;
    using difference_type = isize;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using iterator = contiguous_iterator<value_type, KTL_CHECKS_ENABLED>;
    using reverse_iterator = std::reverse_iterator<iterator>;

    // NOLINTNEXTLINE(*-dynamic-static-initializers)
    static constexpr size_type extent = dynamic_extent;

    // [span.cons], span constructors, copy, assignment, and destructor
    constexpr span() = default;

    constexpr span(const span&) noexcept = default;
    constexpr auto operator=(const span&) noexcept -> span& = default;
    constexpr span(span&&) noexcept = default;
    constexpr auto operator=(span&&) noexcept -> span& = default;
    constexpr ~span() noexcept = default;

    template<span_compatible_iterator<element_type> It>
    constexpr span(It first, size_type count) : m_data {std::to_address(first)}, m_size {count} {}

    template<span_compatible_iterator<element_type> It, span_compatible_sentinel_for<It> End>
    constexpr span(It first, End last) : m_data(std::to_address(first)), m_size(last - first) {
        check_(last - first >= 0, "invalid range in span's constructor (iterator, sentinel)");
    }

    template<usize Size>
    // NOLINTNEXTLINE(*-explicit-conversions, *-avoid-c-arrays)
    constexpr span(std::type_identity_t<element_type> (&arr)[Size]) noexcept :
        m_data {arr},
        m_size {Size} {}

    template<span_array_convertible<element_type> OtherElementType, usize Size>
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr span(std::array<OtherElementType, Size>& arr) noexcept :
        m_data {arr.data()},
        m_size {Size} {}

    template<typename OtherElementType, usize Size>
        requires span_array_convertible<const OtherElementType, element_type>
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr span(const std::array<OtherElementType, Size>& arr) noexcept :
        m_data {arr.data()},
        m_size {Size} {}

    template<span_compatible_range<element_type> Range>
    // NOLINTNEXTLINE(*-explicit-conversions, *-forwarding-reference-overload)
    constexpr span(Range&& r) : m_data(std::ranges::data(r)), m_size {std::ranges::size(r)} {}

    template<span_array_convertible<element_type> OtherElementType, usize OtherExtent>
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr span(const span<OtherElementType, OtherExtent>& other) noexcept :
        m_data {other.data()},
        m_size {other.size()} {}

    //    ~span() noexcept = default;

    template<usize Count>
    constexpr auto first() const noexcept -> span<element_type, Count> {
        check_(Count <= size(), "span<T>::first<Count>(): Count out of range");
        return span<element_type, Count> {data(), Count};
    }

    template<usize Count>
    constexpr auto last() const noexcept -> span<element_type, Count> {
        check_(Count <= size(), "span<T>::last<Count>(): Count out of range");
        return span<element_type, Count> {data() + size() - Count, Count};
    }

    [[nodiscard]] constexpr auto first(size_type count) const noexcept
        -> span<element_type, dynamic_extent> {
        check_(count <= size(), "span<T>::first(count): count out of range");
        return {data(), count};
    }

    [[nodiscard]] constexpr auto last(size_type count) const noexcept
        -> span<element_type, dynamic_extent> {
        check_(count <= size(), "span<T>::last(count): count out of range");
        return {data() + size() - count, count};
    }

    template<usize Offset, usize Count = dynamic_extent>
    constexpr auto subspan() const noexcept -> span<element_type, Count> {
        check_(Offset <= size(), "span<T>::subspan<Offset, Count>(): Offset out of range ");
        check_(
            Count == dynamic_extent || Count <= size() - Offset,
            "span<T>::subspan<Offset, Count>(): Offset + Count out of range");
        return span<element_type, Count> {
            data() + Offset,
            Count == dynamic_extent ? size() - Offset : Count};
    }

    [[nodiscard]] constexpr auto
    subspan(size_type offset, size_type count = dynamic_extent) const noexcept
        -> span<element_type, dynamic_extent> {
        check_(offset <= size(), "span<T>::subspan(offset, count): offset out of range ");
        check_(
            count <= size() || count == dynamic_extent,
            "span<T>::subspan(offset, count): count out of range");
        if (count == dynamic_extent)
            return {data() + offset, size() - offset};
        check_(
            count <= size() - offset,
            "span<T>::subspan(offset, count): offset + count out of range");
        return {data() + offset, count};
    }

    [[nodiscard]] constexpr auto size() const noexcept -> size_type {
        return m_size;
    }
    [[nodiscard]] constexpr auto size_bytes() const noexcept -> size_type {
        return m_size * sizeof(element_type);
    }
    [[nodiscard]] constexpr auto empty() const noexcept -> bool {
        return m_size == 0;
    }

    constexpr auto operator[](size_type idx) const noexcept -> reference {
        check_(idx < size(), "span<T>::operator[](index): index out of range");
        return m_data[idx];
    }

    [[nodiscard]] constexpr auto front() const noexcept -> reference {
        check_(!empty(), "span<T>::front() on empty span");
        return m_data[0];
    }

    [[nodiscard]] constexpr auto back() const noexcept -> reference {
        check_(!empty(), "span<T>::back() on empty span");
        return m_data[size() - 1];
    }

    [[nodiscard]] constexpr auto data() const noexcept -> pointer {
        return m_data;
    }

    // [span.iter], span iterator support
    [[nodiscard]] constexpr auto begin() const noexcept -> iterator {
        return make_contiguous_iterator(data(), data(), data() + size());
    }
    [[nodiscard]] constexpr auto end() const noexcept -> iterator {
        return make_contiguous_iterator(data() + size(), data(), data() + size());
    }
    [[nodiscard]] constexpr auto rbegin() const noexcept -> reverse_iterator {
        return reverse_iterator(end());
    }
    [[nodiscard]] constexpr auto rend() const noexcept -> reverse_iterator {
        return reverse_iterator(begin());
    }

    [[nodiscard]] auto as_bytes() const noexcept -> span<const std::byte, dynamic_extent> {
        return {std::bit_cast<const std::byte*>(data()), size_bytes()};
    }

    [[nodiscard]] auto as_writable_bytes() const noexcept -> span<std::byte, dynamic_extent> {
        return {std::bit_cast<std::byte*>(data()), size_bytes()};
    }

  private:
    pointer m_data {};
    size_type m_size {};
};
}  // namespace ktl

namespace std {
template<typename T, ktl::usize Extent>
inline constexpr bool ranges::enable_borrowed_range<ktl::span<T, Extent>> = true;

template<typename ElementType, ktl::usize Extent>
inline constexpr bool ranges::enable_view<ktl::span<ElementType, Extent>> = true;
}  // namespace std

namespace ktl {
//  as_bytes & as_writable_bytes
template<typename T, usize Extent>
auto as_bytes(span<T, Extent> s) noexcept {
    return s.as_bytes();
}

template<typename T, usize Extent>
    requires(!std::is_const_v<T>)
auto as_writable_bytes(span<T, Extent> s) noexcept {
    return s.as_writable_bytes();
}

// Deduction guides
template<typename It, typename EndOrSize>
    requires std::contiguous_iterator<It>
span(It, EndOrSize) -> span<std::remove_reference_t<std::iter_reference_t<It>>>;

template<typename T, usize Size>
span(T (&)[Size]) -> span<T, Size>;  // NOLINT(*-avoid-c-arrays)

template<typename T, usize Size>
span(std::array<T, Size>&) -> span<T, Size>;

template<typename T, usize Size>
span(const std::array<T, Size>&) -> span<const T, Size>;

template<std::ranges::contiguous_range Range>
span(Range&&) -> span<std::remove_reference_t<std::ranges::range_reference_t<Range>>>;
}  // namespace ktl