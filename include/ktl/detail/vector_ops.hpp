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
        check_(begin <= end, "Invalid vector");
        return end - begin;
    }
    constexpr auto capacity() const noexcept -> SizeT {
        [[maybe_unused]] auto [begin, end, end_cap] = get_storage();
        check_(end_cap >= begin, "Invalid vector");
        check_(end_cap >= end, "Invalid vector");
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
        check_(end_cap >= end, "vector pointers are invalid");
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

    // // constexpr auto assign(SizeT count, const T& value) noexcept -> expected<void,
    // InsertError> {
    // //     return {};
    // }

    // template<std::input_iterator InputIt>
    //     requires std::same_as<std::iter_value_t<InputIt>, T>
    // constexpr auto assign(InputIt first, InputIt last) noexcept -> expected<void, InsertError> {
    //     TryReturnV(insert(end(), first, last));
    // }

    // constexpr auto assign(std::initializer_list<T> ilist) noexcept -> expected<void, InsertError>
    // {
    //     TryReturnV(insert(end(), ilist.begin(), ilist.end()));
    // }

    // constexpr auto insert(const_iterator pos, const T& value) noexcept
    //     -> expected<iterator, InsertError>;

    // constexpr auto insert(const_iterator pos, T&& value) noexcept
    //     -> expected<iterator, InsertError>;

    // constexpr auto insert(const_iterator pos, SizeT count, const T& value) noexcept
    //     -> expected<iterator, InsertError>;

    // template<std::input_iterator InputIt>
    //     requires std::same_as<std::iter_value_t<InputIt>, T>
    // constexpr auto insert(const_iterator pos, InputIt first, InputIt last) noexcept
    //     -> expected<iterator, InsertError>;

    // template<std::forward_iterator ForwardIt>
    //     requires std::same_as<std::iter_value_t<ForwardIt>, T>
    // constexpr auto insert(const_iterator pos, ForwardIt first, ForwardIt last) noexcept
    //     -> expected<iterator, InsertError>;

    // template<std::random_access_iterator RandAccIt>
    //     requires std::same_as<std::iter_value_t<RandAccIt>, T>
    // constexpr auto insert(const_iterator pos, RandAccIt first, RandAccIt last) noexcept
    //     -> expected<iterator, InsertError>;

    // constexpr auto insert(const_iterator pos, std::initializer_list<T> ilist) noexcept
    //     -> expected<iterator, InsertError>;

  private:
    constexpr auto get_storage() const noexcept -> vector_storage<const T> {
        static_assert(vector_like<VectorT, T, SizeT>, "VectorT is not a vector");
        return static_cast<const VectorT*>(this)->get_storage();
    }
    constexpr auto get_storage() noexcept -> vector_storage<T> {
        static_assert(vector_like<VectorT, T, SizeT>, "VectorT is not a vector");
        return static_cast<VectorT*>(this)->get_storage();
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