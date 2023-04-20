#pragma once

#include <limits>
#include <optional>

#include <ktl/contiguous_iterator.hpp>
#include <ktl/error.hpp>
#include <ktl/expected.hpp>
#include <ktl/int.hpp>
#include <ktl/memory.hpp>

#include "contiguous_container_common_defs.hpp"

namespace ktl::detail::vec {
// Determine the size_type for the given capacity
template<auto Capacity>
using size_t = std::conditional_t<
    (Capacity > std::numeric_limits<u32>::max()),
    u64,
    std::conditional_t<
        (Capacity > std::numeric_limits<u16>::max()),
        u32,
        std::conditional_t<(Capacity > std::numeric_limits<u8>::max()), u16, u8>>>;

// Describes the boundary of contiguous range of elements stored in the vector
template<typename T>
struct vector_storage {
    // Pointer to first element of the vector. Base of the vector
    T* begin;
    // Pointer to element following the last element of the vector.
    T* end;
    // Pointer to element (assuming it's valid) following the end of storage.
    T* end_cap;
};

template<typename VectorT, typename T, typename SizeT>
concept vector_like = requires(VectorT vec, const VectorT cvec, SizeT req_len, SizeT new_len) {
    { vec.get_storage() } -> std::same_as<vector_storage<T>>;
    { cvec.get_storage() } -> std::same_as<vector_storage<const T>>;

    // Grow must ensure capacity for atleast 'req_len' (1st parameter)
    // elements.
    { vec.grow(req_len) } -> std::same_as<expected<void, Error>>;
    { vec.grow_uninit(req_len) } -> std::same_as<expected<void, Error>>;

    { vec.set_len(new_len) };
};

template<typename T, typename SizeT, typename VectorT>
class vector_ops {
  public:
    using iterator = contiguous_iterator<T>;
    using const_iterator = contiguous_iterator<const T>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    static constexpr bool is_vector = true;

    // Element access and size
    constexpr auto data() noexcept -> T* {
        return get_storage().begin;
    }
    constexpr auto data() const noexcept -> const T* {
        return get_storage().begin;
    }

    constexpr auto size() const noexcept -> SizeT {
        auto [begin, end, _] = get_storage();
        return end - begin;
    }
    constexpr auto capacity() const noexcept -> SizeT {
        [[maybe_unused]] auto [begin, end, end_cap] = get_storage();
        return end_cap - begin;
    }
    [[nodiscard]] constexpr auto empty() const noexcept -> bool {
        return size() == 0;
    }

    constexpr auto operator[](SizeT i) const noexcept -> const T& {
        check_(i < size(), "vector[] index out of bounds");
        return data()[i];
    }
    constexpr auto operator[](SizeT i) noexcept -> T& {
        check_(i < size(), "vector[] index out of bounds");
        return data()[i];
    }

    constexpr auto at(SizeT i) const noexcept -> expected<std::reference_wrapper<const T>, Error> {
        if (i >= size()) {
            Throw(Error::IndexOutOfBounds);
        }
        return data()[i];
    }
    constexpr auto at(SizeT i) noexcept -> expected<std::reference_wrapper<T>, Error> {
        if (i >= size()) {
            Throw(Error::IndexOutOfBounds);
        }
        return data()[i];
    }

    constexpr auto front() const noexcept -> const T& {
        check_(!empty(), "front() called on empty vector");
        return data()[0];
    }
    constexpr auto front() noexcept -> T& {
        check_(!empty(), "front() called on empty vector");
        return data()[0];
    }

    constexpr auto back() const noexcept -> const T& {
        check_(!empty(), "back() called on empty vector");
        return data()[size() - 1];
    }
    constexpr auto back() noexcept -> T& {
        check_(!empty(), "back() called on empty vector");
        return data()[size() - 1];
    }

    // Iterators
    constexpr auto begin() const noexcept -> const_iterator {
        auto [begin, end, _] = get_storage();
        return make_contiguous_iterator(begin, begin, end);
    }
    constexpr auto begin() noexcept -> iterator {
        auto [begin, end, _] = get_storage();
        return make_contiguous_iterator(begin, begin, end);
    }
    constexpr auto cbegin() noexcept -> const_iterator {
        return begin();
    }

    constexpr auto end() const noexcept -> const_iterator {
        auto [begin, end, _] = get_storage();
        return make_contiguous_iterator(end, begin, end);
    }
    constexpr auto end() noexcept -> iterator {
        auto [begin, end, _] = get_storage();
        return make_contiguous_iterator(end, begin, end);
    }
    constexpr auto cend() noexcept -> const_iterator {
        return end();
    }

    constexpr auto rbegin() const noexcept -> const_reverse_iterator {
        return reverse_iterator {end()};
    }
    constexpr auto rbegin() noexcept -> reverse_iterator {
        return reverse_iterator {end()};
    }
    constexpr auto crbegin() noexcept -> const_reverse_iterator {
        return rbegin();
    }

    constexpr auto rend() const noexcept -> const_reverse_iterator {
        return reverse_iterator {begin()};
    }
    constexpr auto rend() noexcept -> reverse_iterator {
        return reverse_iterator {begin()};
    }
    constexpr auto crend() noexcept -> const_reverse_iterator {
        return rend();
    }

    constexpr auto push_back(T&& val) noexcept -> expected<void, Error> {
        TryV(emplace_back(std::move(val)));
        return {};
    }
    constexpr auto push_back(const T& val) noexcept -> expected<void, Error> {
        TryV(emplace_back(val));
        return {};
    }

    template<typename... Args>
    constexpr auto emplace_back(Args&&... args) noexcept
        -> expected<std::reference_wrapper<T>, Error> {
        auto [begin, end, end_cap] = get_storage();
        auto len = end - begin;
        auto new_len = len + 1;
        if (end == end_cap) [[unlikely]] {
            TryV(grow(new_len));
            auto [nbegin, nend, nend_cap] = get_storage();
            std::tie(begin, end, end_cap) = std::tie(nbegin, nend, nend_cap);
        } else {
            adjust_lifetime(new_len);
        }
        set_len<!UpdateLifetime>(new_len);
        return *std::construct_at(end, std::forward<Args>(args)...);
    }

    constexpr void pop_back() noexcept {
        auto [begin, end, _] = get_storage();
        auto len = end - begin;

        check_(len != 0, "cannot pop_back empty vector");

        if constexpr (!std::is_trivially_destructible_v<T>) {
            std::destroy_at(end - 1);
        }
        set_len<UpdateLifetime>(len - 1);
    }

    constexpr void clear() noexcept {
        auto [begin, end, _] = get_storage();

        if (end != begin) {
            if constexpr (!std::is_trivially_destructible_v<T>) {
                std::destroy(begin, end);
            }
            set_len<UpdateLifetime>(0);
        }
    }

    constexpr auto resize(SizeT new_len, const T& new_value) noexcept -> expected<void, Error> {
        return resize_with(new_len, [&]() -> const T& { return new_value; });
    }

    constexpr auto resize(SizeT new_len) noexcept -> expected<void, Error> {
        return resize_with(new_len, [&] { return T {}; });
    }

    constexpr auto resize_uninitialized(SizeT new_len) noexcept -> expected<void, Error> {
        return resize_impl(new_len);
    }

    constexpr auto assign(SizeT count, const T& value) noexcept -> expected<void, Error> {
        return assign_fill(count, value);
    }

    template<std::input_iterator InputIt>
    constexpr auto assign(InputIt first, InputIt last) noexcept
        -> expected<void, std::pair<InputIt, Error>> {
        clear();
        return assign_iter(first, last);
    }

    template<std::random_access_iterator RandAccIt>
        requires std::convertible_to<std::iter_value_t<RandAccIt>, T>
    constexpr auto assign(RandAccIt first, RandAccIt last) noexcept -> expected<void, Error> {
        return assign_range(first, std::distance(first, last));
    }

    template<std::input_iterator InputIter>
        requires(!std::random_access_iterator<InputIter>)
        && std::convertible_to<std::iter_value_t<InputIter>, T>
        && std::is_default_constructible_v<VectorT>
    constexpr auto assign(InputIter first, InputIter last) noexcept
        -> expected<void, std::pair<InputIter, Error>> {
        if (this->empty()) {
            TryV(this->assign_iter(first, last));
        } else {
            VectorT tmp;

            TryV(tmp.assign_iter(first, last));
            check_(assign_range(std::make_move_iterator(tmp.begin()), tmp.size()), "");
        }

        return {};
    }

    constexpr auto assign(std::initializer_list<T> ilist) -> expected<void, Error> {
        return assign_range(ilist.begin(), ilist.size());
    }

    constexpr auto insert(const_iterator pos, const T& value) noexcept
        -> expected<iterator, Error> {
        Try(it, make_space_at(pos, 1));
        std::construct_at(&*it, value);
        return it;
    }

    constexpr auto insert(const_iterator pos, T&& value) noexcept -> expected<iterator, Error> {
        Try(it, make_space_at(pos, 1));
        std::construct_at(&*it, std::move(value));
        return it;
    }

    constexpr auto insert(const_iterator pos, SizeT count, const T& value) noexcept
        -> expected<iterator, Error> {
        Try(it, make_space_at(pos, count));
        ktl::uninitialized_fill_n(it, count, value);
        return it;
    }

    template<std::random_access_iterator RandAccIt>
        requires std::convertible_to<std::iter_value_t<RandAccIt>, T>
    constexpr auto insert(const_iterator pos, RandAccIt first, RandAccIt last) noexcept
        -> expected<iterator, Error> {
        return insert_range(pos, first, std::distance(first, last));
    }

    template<std::input_iterator InputIt>
        requires(!std::random_access_iterator<InputIt>)
        && std::convertible_to<std::iter_value_t<InputIt>, T>
        && std::is_default_constructible_v<VectorT>
    constexpr auto insert(const_iterator pos, InputIt first, InputIt last)
        -> expected<iterator, Error> {
        if (pos == end()) {
            return insert_at_end(first, last);
        }

        VectorT tmp;
        auto tmp_res = tmp.assign(first, last);
        auto res = insert_range(pos, std::make_move_iterator(tmp.begin()), tmp.size());

        // All rows inseted into `tmp` vector? If so, return the `res`.
        if (tmp_res || !res) {
            return res;
        }
        // If, all rows were not inserted into the `tmp` vector, error must be returned.
        Throw(std::move(tmp_res).error().second);
    }

    constexpr auto insert(const_iterator pos, std::initializer_list<T> ilist) noexcept
        -> expected<iterator, Error> {
        auto count = ilist.size();
        Try(it, make_space_at(pos, count));
        ktl::uninitialized_copy_n(ilist.begin(), count, it);
        return it;
    }

    template<typename... Args>
    constexpr auto emplace(const_iterator pos, Args&&... args) noexcept
        -> expected<iterator, Error> {
        Try(it, make_space_at(pos, 1));
        std::construct_at(&*it, std::forward<Args>(args)...);
        return it;
    }

    constexpr auto erase(const_iterator pos) noexcept -> iterator {
        if (pos == end())
            return end();
        return erase(pos, std::next(pos));
    }

    constexpr auto erase(const_iterator first, const_iterator last) -> iterator {
        check_(first >= cbegin() && first <= cend(), "iterator does not belong to the container");
        check_(last >= first && last <= cend(), "iterator range does not belong to the container");

        if (first == last) {
            return begin() + std::distance(cbegin(), last);
        }

        auto it = begin() + std::distance(cbegin(), first);
        std::move(last, cend(), it);
        set_len<UpdateLifetime>(size() - std::distance(first, last));
        return it;
    }

  protected:
    template<std::input_iterator InputIt>
    constexpr auto insert_at_end(InputIt first, InputIt last) -> expected<iterator, Error> {
        // TODO: Optimize by doing bulk insert
        for (; first != last; first++) {
            TryV(emplace_back(std::move(*first)));
        }
        return end();
    }

    template<std::input_iterator InputIter>
    constexpr auto assign_iter(InputIter first, InputIter last) noexcept
        -> expected<void, std::pair<InputIter, Error>> {
        assert(empty() && "assign_iter must be called on empty vector");
        for (; first != last; ++first) {
            if (auto res = emplace_back(*first); !res) {
                Throw(std::make_pair(first, std::move(res).error()));
            }
        }
        return {};
    }

    ASAN_ANNOTATION_HELPERS;

  private:
    using container = VectorT;

    static constexpr bool UpdateLifetime = true;

    template<std::input_iterator InputIt>
    constexpr auto insert_range(const_iterator pos, InputIt first, usize count) noexcept
        -> expected<iterator, Error> {
        Try(it, make_space_at(pos, count));
        ktl::uninitialized_copy_n(first, count, it);
        return it;
    }

    constexpr auto make_space_at(const_iterator pos, usize count) noexcept
        -> expected<iterator, Error> {
        check_(pos >= begin() && pos <= end(), "iterator does not belong to the container");

        auto [beg, end, end_cap] = get_storage();
        usize size = end - beg;
        usize capacity = end_cap - beg;
        auto new_size = size + count;
        auto pos_i = std::distance(cbegin(), pos);

        if (new_size > capacity) {
            TryV(grow(new_size));
            auto [nbeg, nend, nend_cap] = get_storage();
            std::tie(beg, end, end_cap) = std::tie(nbeg, nend, nend_cap);
        } else {
            adjust_lifetime(new_size);
        }
        // NOTE: Cannot use `pos` here as it might have been invalidated by previous call to
        // grow.
        ktl::uninitialized_move_backward(beg + pos_i, end, end + count);
        set_len<!UpdateLifetime>(new_size);
        return begin() + pos_i;
    }

    constexpr auto assign_range(std::input_iterator auto first, usize count) noexcept
        -> expected<void, Error> {
        return assign_impl(count, [&](auto* begin) {
            ktl::uninitialized_copy_n(first, count, begin);
        });
    }

    constexpr auto assign_fill(SizeT count, const T& value) -> expected<void, Error> {
        return assign_impl(count, [&](auto* begin) {
            ktl::uninitialized_fill_n(begin, count, value);
        });
    }

    constexpr auto assign_impl(usize count, auto&& initializer) noexcept -> expected<void, Error> {
        auto [begin, _, end_cap] = get_storage();
        usize capacity = end_cap - begin;

        if (count > capacity) {
            TryV(grow_uninit(count));
            // Vector is cleared and contains enough space to construct count elements

            auto [nbeg, nend, nend_cap] = get_storage();
            std::tie(begin, _, end_cap) = std::tie(nbeg, nend, nend_cap);
            capacity = end_cap - begin;
        } else {
            adjust_lifetime(count);
        }

        assert(capacity >= count);

        initializer(begin);
        set_len<!UpdateLifetime>(count);

        return {};
    }

    template<typename FillValueGetter>
    constexpr auto resize_with(SizeT new_len, FillValueGetter&& get_fill_value) noexcept
        -> expected<void, Error> {
        auto [begin, end, _end_cap] = get_storage();
        usize old_len = end - begin;

        TryV(resize_impl(new_len));

        if (new_len > old_len) {
            auto&& new_value = std::invoke(std::forward<FillValueGetter>(get_fill_value));
            begin = get_storage().begin;
            ktl::uninitialized_fill_n(begin + old_len, new_len - old_len, new_value);
        }

        return {};
    }

    constexpr auto resize_impl(SizeT new_len) noexcept -> expected<void, Error> {
        auto [begin, end, end_cap] = get_storage();
        auto len = end - begin;
        usize capacity = end_cap - begin;

        if (new_len > capacity) {
            TryV(grow(new_len));
            auto [nbegin, nend, nend_cap] = get_storage();
            std::tie(begin, end, end_cap) = std::tie(nbegin, nend, nend_cap);
        } else {
            adjust_lifetime(new_len);
        }

        if constexpr (!std::is_trivially_destructible_v<T>) {
            if (new_len < len) {
                std::destroy(begin + new_len, end);
            }
        }

        set_len<!UpdateLifetime>(new_len);
        return {};
    }

    constexpr auto get_storage() const noexcept -> vector_storage<const T> {
        static_assert(vector_like<VectorT, T, SizeT>, "VectorT is not a vector");
        auto res = static_cast<const VectorT*>(this)->get_storage();
        check_(res.end_cap >= res.end, "vector pointers are invalid");
        check_(res.end >= res.begin, "vector pointers are invalid");
        return res;
    }
    constexpr auto get_storage() noexcept -> vector_storage<T> {
        static_assert(vector_like<VectorT, T, SizeT>, "VectorT is not a vector");
        auto res = static_cast<VectorT*>(this)->get_storage();
        check_(res.end_cap >= res.end, "vector pointers are invalid");
        check_(res.end >= res.begin, "vector pointers are invalid");
        return res;
    }

    constexpr auto grow(usize req_len) noexcept -> expected<void, Error> {
        static_assert(vector_like<VectorT, T, SizeT>, "VectorT is not a vector");
        return static_cast<VectorT*>(this)->grow(req_len);
    }

    constexpr auto grow_uninit(usize req_len) noexcept -> expected<void, Error> {
        static_assert(vector_like<VectorT, T, SizeT>, "VectorT is not a vector");
        return static_cast<VectorT*>(this)->grow_uninit(req_len);
    }

    template<bool UpdateLifetime>
    constexpr void set_len(SizeT new_len) noexcept {
        static_assert(vector_like<VectorT, T, SizeT>, "VectorT is not a vector");
        static_cast<VectorT*>(this)->set_len(new_len);
        if constexpr (UpdateLifetime) {
            adjust_lifetime(new_len);
        }
    }
};

template<typename V>
concept comparable_vector = requires(const V& v) {
    typename V::value_type;
    { std::begin(v) } -> std::contiguous_iterator;
    { std::end(v) } -> std::contiguous_iterator;
    { V::is_vector == true };
    requires std::
        same_as<std::iter_value_t<std::decay_t<decltype(std::begin(v))>>, typename V::value_type>;
    requires std::
        same_as<std::iter_value_t<std::decay_t<decltype(std::end(v))>>, typename V::value_type>;
};

}  // namespace ktl::detail::vec

namespace ktl {
template<detail::vec::comparable_vector Vec1, detail::vec::comparable_vector Vec2>
    requires std::equality_comparable_with<typename Vec1::value_type, typename Vec2::value_type>
constexpr auto operator==(const Vec1& lhs, const Vec2& rhs) noexcept -> bool {
    auto beg1 = std::begin(lhs);
    auto beg2 = std::begin(rhs);
    auto end1 = std::end(lhs);
    auto end2 = std::end(rhs);
    auto s1 = std::distance(beg1, end1);
    auto s2 = std::distance(beg2, end2);

    if (s1 != s2) {
        return false;
    }
    return std::equal(beg1, end1, beg2);
}

template<detail::vec::comparable_vector Vec1, detail::vec::comparable_vector Vec2>
    requires std::three_way_comparable_with<typename Vec1::value_type, typename Vec2::value_type>
constexpr auto operator<=>(const Vec1& lhs, const Vec2& rhs) noexcept {
    return std::lexicographical_compare_three_way(
        std::begin(lhs),
        std::end(lhs),
        std::begin(rhs),
        std::end(rhs));
}
}  // namespace ktl