#pragma once

#include <optional>

#include <ktl/contiguous_iterator.hpp>
#include <ktl/error.hpp>
#include <ktl/expected.hpp>
#include <ktl/int.hpp>

namespace ktl::detail {
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

template<typename ForwardIterator, typename Size, typename T>
constexpr auto uninitialized_fill_n(ForwardIterator first, Size n, const T& x) -> ForwardIterator {
    if constexpr (std::is_trivial_v<T>) {
        return std::fill_n(first, n, x);
    } else {
        return std::uninitialized_fill_n(first, n, x);
    }
}

template<typename InputIterator, typename Size, typename ForwardIterator>
constexpr auto uninitialized_copy_n(InputIterator first, Size n, ForwardIterator result)
    -> ForwardIterator {
    if constexpr (std::is_trivial_v<std::iter_value_t<ForwardIterator>>) {
        return std::copy_n(first, n, result);
    } else {
        return std::uninitialized_copy_n(first, n, result);
    }
}

template<typename InputIterator, typename Size, typename ForwardIterator>
inline auto uninitialized_move_n(InputIterator first, Size n, ForwardIterator result)
    -> std::pair<InputIterator, ForwardIterator> {
    if constexpr (std::is_trivial_v<std::iter_value_t<ForwardIterator>>) {
        return std::copy_n(first, n, result);
    } else {
        return std::uninitialized_move_n(first, n, result);
    }
}

template<typename VectorT, typename T, typename SizeT>
concept vector_like = requires(VectorT vec, const VectorT cvec, SizeT req_len, SizeT new_len) {
                          { vec.get_storage() } -> std::same_as<vector_storage<T>>;
                          { cvec.get_storage() } -> std::same_as<vector_storage<const T>>;

                          // Grow must ensure capacity for atleast 'req_len' (1st parameter)
                          // elements.
                          { vec.grow(req_len) } -> std::same_as<std::optional<Error>>;

                          { vec.set_len(new_len) };
                      };

template<typename T, typename SizeT, typename VectorT>
class vector_ops {
  public:
    using iterator = contiguous_iterator<T>;
    using const_iterator = contiguous_iterator<const T>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    struct InsertError {
        Error err;
        SizeT num_inserted;
    };

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
            return make_unexpected(Error::IndexOutOfBounds);
        }
        return data()[i];
    }
    constexpr auto at(SizeT i) noexcept -> expected<std::reference_wrapper<T>, Error> {
        if (i >= size()) {
            return make_unexpected(Error::IndexOutOfBounds);
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

    [[nodiscard("must check if push_back succeeded")]] constexpr auto push_back(T&& val) noexcept
        -> expected<void, Error> {
        TryV(emplace_back(std::move(val)));
        return {};
    }
    [[nodiscard("must check if push_back succeeded")]] constexpr auto
    push_back(const T& val) noexcept -> expected<void, Error> {
        TryV(emplace_back(val));
        return {};
    }

    template<typename... Args>
    [[nodiscard("must check if emplace_back succeeded")]] constexpr auto
    emplace_back(Args&&... args) noexcept -> expected<std::reference_wrapper<T>, Error> {
        auto [begin, end, end_cap] = get_storage();
        auto len = end - begin;
        if (end == end_cap) [[unlikely]] {
            if (auto err = grow(len + 1)) {
                return make_unexpected(*std::move(err));
            }
            auto [nbegin, nend, nend_cap] = get_storage();
            std::tie(begin, end, end_cap) = std::tie(nbegin, nend, nend_cap);
        }
        set_len(len + 1);
        return *std::construct_at(end, std::forward<Args>(args)...);
    }

    constexpr void pop_back() noexcept {
        auto [begin, end, _] = get_storage();
        auto len = end - begin;

        check_(len != 0, "cannot pop_back empty vector");

        set_len(len - 1);
        if constexpr (!std::is_trivially_destructible_v<T>) {
            std::destroy_at(end - 1);
        }
    }

    constexpr void clear() noexcept {
        auto [begin, end, _] = get_storage();

        if (end != begin) {
            if constexpr (!std::is_trivially_destructible_v<T>) {
                std::destroy(begin, end);
            }
        }

        set_len(0);
    }

    [[nodiscard("must check if resize succeeded")]] constexpr auto
    resize(SizeT new_len, const T& new_value) noexcept -> expected<void, Error> {
        return resize_with(new_len, [&]() -> const T& { return new_value; });
    }

    [[nodiscard("must check if resize succeeded")]] constexpr auto resize(SizeT new_len) noexcept
        -> expected<void, Error> {
        return resize_with(new_len, [&] { return T {}; });
    }

    constexpr auto assign(SizeT count, const T& value) noexcept -> expected<SizeT, InsertError> {
        clear();

        auto [begin, _, end_cap] = get_storage();
        usize capacity = end_cap - begin;
        auto len = std::min<usize>(capacity, count);

        uninitialized_fill_n(begin, len, value);
        set_len(len);

        if (count > capacity) [[unlikely]] {
            return make_unexpected(InsertError {
                .err = Error::BufferFull,
                .num_inserted = static_cast<SizeT>(capacity)});
        }
        return count;
    }

    template<std::input_iterator InputIt>
    constexpr auto assign(InputIt first, InputIt last) noexcept -> expected<SizeT, InsertError> {
        clear();

        auto [begin, _, end_cap] = get_storage();
        usize capacity = end_cap - begin;
        usize len = 0;

        while (len <= capacity && first != last) {
            std::construct_at(begin, *first);
            begin++;
            first++;
            len++;
        }
        set_len(len);

        if (len > capacity) {
            return make_unexpected(InsertError {
                .err = Error::BufferFull,
                .num_inserted = static_cast<SizeT>(capacity)});
        }
        return len;
    }

    template<std::random_access_iterator RandomAccIt>
    constexpr auto assign(RandomAccIt first, RandomAccIt last) noexcept
        -> expected<SizeT, InsertError> {
        clear();

        auto [begin, _, end_cap] = get_storage();
        usize capacity = end_cap - begin;
        usize count = std::distance(first, last);
        auto len = std::min(count, capacity);

        uninitialized_copy_n(first, std::min(count, capacity), begin);
        set_len(len);

        if (count > capacity) {
            return make_unexpected(InsertError {
                .err = Error::BufferFull,
                .num_inserted = static_cast<SizeT>(capacity)});
        }
        return count;
    }

    constexpr auto assign(std::initializer_list<T> ilist) -> expected<SizeT, InsertError> {
        clear();

        auto [begin, _, end_cap] = get_storage();
        usize capacity = end_cap - begin;
        usize count = ilist.size();
        auto len = std::min(count, capacity);

        uninitialized_copy_n(ilist.begin(), len, begin);
        set_len(len);

        if (count > capacity) {
            return make_unexpected(InsertError {
                .err = Error::BufferFull,
                .num_inserted = static_cast<SizeT>(capacity)});
        }
        return count;
    }

  private:
    template<typename FillValueGetter>
    constexpr auto resize_with(SizeT new_len, FillValueGetter&& get_fill_value) noexcept
        -> expected<void, Error> {
        auto [begin, end, end_cap] = get_storage();
        auto len = end - begin;
        auto capacity = end_cap - begin;

        if (new_len > capacity) {
            if (auto res = grow(new_len)) {
                return make_unexpected(*std::move(res));
            }
            auto [nbegin, nend, nend_cap] = get_storage();
            std::tie(begin, end, end_cap) = std::tie(nbegin, nend, nend_cap);
        }

        if (new_len > len) {
            auto&& new_value = std::invoke(std::forward<FillValueGetter>(get_fill_value));
            uninitialized_fill_n(end, new_len - len, new_value);
        } else {
            if constexpr (!std::is_trivially_destructible_v<T>) {
                std::destroy(begin + new_len, end);
            }
        }

        set_len(new_len);
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

    constexpr auto grow(SizeT req_len) noexcept -> std::optional<Error> {
        static_assert(vector_like<VectorT, T, SizeT>, "VectorT is not a vector");
        return static_cast<VectorT*>(this)->grow(req_len);
    }

    constexpr void set_len(SizeT new_len) noexcept {
        static_assert(vector_like<VectorT, T, SizeT>, "VectorT is not a vector");
        static_cast<VectorT*>(this)->set_len(new_len);
    }
};

}  // namespace ktl::detail