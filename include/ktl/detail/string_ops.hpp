#pragma once

#include <limits>
#include <optional>

#include <ktl/contiguous_iterator.hpp>
#include <ktl/error.hpp>
#include <ktl/expected.hpp>
#include <ktl/int.hpp>
#include <ktl/memory.hpp>
#include <ktl/string_view.hpp>

#include "contiguous_container_common_defs.hpp"

namespace ktl::detail::str {
// Determine the size_type for the given capacity
template<auto Capacity>
using size_t = std::conditional_t<
    (Capacity >= std::numeric_limits<u32>::max()),
    u64,
    std::conditional_t<
        (Capacity >= std::numeric_limits<u16>::max()),
        u32,
        std::conditional_t<(Capacity >= std::numeric_limits<u8>::max()), u16, u8>>>;

// Describes the boundary of contiguous range of elements stored in the string
template<typename CharT>
struct string_storage {
    // Pointer to first element of the string. Base of the string
    CharT* begin;
    // Pointer to element following the last element of the string.
    CharT* end;
    // Pointer to element (assuming it's valid) following the end of storage.
    CharT* end_cap;
};

template<typename StringT, typename CharT, typename SizeT>
concept string_like = requires(StringT str, const StringT cstr, SizeT req_len, SizeT new_len) {
    { str.get_storage() } -> std::same_as<string_storage<CharT>>;
    { cstr.get_storage() } -> std::same_as<string_storage<const CharT>>;

    // Grow must ensure capacity for atleast 'req_len' (1st parameter)
    // elements.
    { str.grow(req_len) } -> std::same_as<expected<void, Error>>;
    { str.grow_uninit(req_len) } -> std::same_as<expected<void, Error>>;

    { str.set_len(new_len) };
};

template<typename CharT, typename Traits, typename SizeT, typename StringT>
    requires std::is_trivial_v<CharT>
class string_ops {
  public:
    using iterator = contiguous_iterator<CharT>;
    using const_iterator = contiguous_iterator<const CharT>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    static constexpr bool is_string = true;

    static constexpr auto npos = std::numeric_limits<SizeT>::max();

  protected:
    class non_null_ptr {
      public:
        non_null_ptr(std::nullptr_t) = delete;

        constexpr explicit non_null_ptr(StringT& str) : m_str {&str} {
            assert(m_str != nullptr);
        }

        constexpr auto operator*() const noexcept -> StringT& {
            return *m_str;
        }

        constexpr auto operator->() const noexcept -> StringT& {
            return m_str;
        }

      private:
        StringT* m_str;
    };

  public:
    // Element access and size
    constexpr auto data() noexcept -> CharT* {
        return get_storage().begin;
    }
    constexpr auto data() const noexcept -> const CharT* {
        return get_storage().begin;
    }
    constexpr auto c_str() const noexcept -> const CharT* {
        return data();
    }

    constexpr auto size() const noexcept -> SizeT {
        auto [begin, end, _] = get_storage();
        return end - begin - 1;  // Don't count trailing 'NUL' character
    }
    constexpr auto length() const noexcept -> SizeT {
        auto [begin, end, _] = get_storage();
        return end - begin - 1;  // Don't count trailing 'NUL' character
    }
    constexpr auto capacity() const noexcept -> SizeT {
        [[maybe_unused]] auto [begin, end, end_cap] = get_storage();
        return end_cap - begin;
    }
    [[nodiscard]] constexpr auto empty() const noexcept -> bool {
        return size() == 0;
    }

    constexpr auto operator[](SizeT i) const noexcept -> const CharT& {
        check_(i < size(), "string[] index out of bounds");
        return data()[i];
    }
    constexpr auto operator[](SizeT i) noexcept -> CharT& {
        check_(i < size(), "string[] index out of bounds");
        return data()[i];
    }

    constexpr auto at(SizeT i) const noexcept
        -> expected<std::reference_wrapper<const CharT>, Error> {
        if (i >= size()) {
            Throw(Error::IndexOutOfBounds);
        }
        return data()[i];
    }
    constexpr auto at(SizeT i) noexcept -> expected<std::reference_wrapper<CharT>, Error> {
        if (i >= size()) {
            Throw(Error::IndexOutOfBounds);
        }
        return data()[i];
    }

    constexpr auto front() const noexcept -> const CharT& {
        check_(!empty(), "front() called on empty string");
        return data()[0];
    }
    constexpr auto front() noexcept -> CharT& {
        check_(!empty(), "front() called on empty string");
        return data()[0];
    }

    constexpr auto back() const noexcept -> const CharT& {
        check_(!empty(), "back() called on empty string");
        return data()[size() - 1];
    }
    constexpr auto back() noexcept -> CharT& {
        check_(!empty(), "back() called on empty string");
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
        return begin() + size();
    }
    constexpr auto end() noexcept -> iterator {
        return begin() + size();
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

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr operator basic_string_view<CharT, Traits>() const noexcept {
        return basic_string_view {data(), size()};
    }

    // --------------------- operator+= ---------------------

    constexpr auto operator+=(const StringT& str) noexcept -> expected<non_null_ptr, Error> {
        return append(str);
    }

    constexpr auto operator+=(CharT ch) noexcept -> expected<non_null_ptr, Error> {
        TryV(push_back(ch));
        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    constexpr auto operator+=(const CharT* s) noexcept -> expected<non_null_ptr, Error> {
        return append(s);
    }

    constexpr auto operator+=(std::initializer_list<CharT> ilist) noexcept
        -> expected<non_null_ptr, Error> {
        return append(ilist);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto operator+=(const StringViewLike& t) noexcept -> expected<non_null_ptr, Error> {
        return append(t);
    }

    // --------------------- Member functions ---------------------

    constexpr auto push_back(CharT c) noexcept -> expected<void, Error> {
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
        std::construct_at(end - 1, c);
        std::construct_at(end, NUL);
        set_len<!UpdateLifetime>(new_len);
        return {};
    }

    constexpr void pop_back() noexcept {
        auto [begin, end, _] = get_storage();
        auto len = end - begin;

        check_(len != 1, "cannot pop_back empty string");  // Contains trailing NUL char

        std::construct_at(end - 2, NUL);
        set_len<UpdateLifetime>(len - 1);
    }

    constexpr void clear() noexcept {
        auto [begin, _end, _end_cap] = get_storage();
        std::construct_at(begin, NUL);  // Empty NUL terminate,string
        set_len<UpdateLifetime>(1);
    }

    constexpr auto resize(SizeT count, CharT ch) noexcept -> expected<void, Error> {
        return resize_impl<true>(count, ch);
    }

    constexpr auto resize(SizeT new_len) noexcept -> expected<void, Error> {
        return resize(new_len, NUL);
    }

    constexpr auto resize_uninitialized(SizeT new_len) noexcept -> expected<void, Error> {
        return resize_impl<false>(new_len, NUL);
    }

// NOLINTNEXTLINE(*-macro-usage)
#define as_view(str) static_cast<basic_string_view<CharT, Traits>>(str)

    // --------------------- Assign ---------------------

    constexpr auto assign(SizeT count, CharT ch) noexcept -> expected<non_null_ptr, Error> {
        return assign_fill(count, ch);
    }

    constexpr auto assign(const StringT& str) noexcept -> expected<non_null_ptr, Error> {
        return assign_range(str.begin(), str.size());
    }

    constexpr auto assign(const StringT& str, SizeT pos, SizeT count) noexcept
        -> expected<non_null_ptr, Error> {
        Try(ss, as_view(str).substr(pos, count));
        return assign_string_view(ss);
    }

    constexpr auto assign(const CharT* s, SizeT count) noexcept -> expected<non_null_ptr, Error> {
        return assign_range(s, count);
    }

    constexpr auto assign(const CharT* s) noexcept -> expected<non_null_ptr, Error> {
        return assign_range(s, Traits::length(s));
    }

    template<std::random_access_iterator RandAccIt>
        requires std::convertible_to<std::iter_value_t<RandAccIt>, CharT>
    constexpr auto assign(RandAccIt first, RandAccIt last) -> expected<non_null_ptr, Error> {
        return assign_range(first, std::distance(first, last));
    }

    template<std::input_iterator InputIter>
        requires(!std::random_access_iterator<InputIter>)
        && std::convertible_to<std::iter_value_t<InputIter>, CharT>
        && std::is_default_constructible_v<StringT>
    constexpr auto assign(InputIter first, InputIter last) noexcept
        -> expected<non_null_ptr, std::pair<InputIter, Error>> {
        if (this->empty()) {
            TryV(this->assign_iter(first, last));
        } else {
            StringT tmp;

            TryV(tmp.assign_iter(first, last));
            check_(assign(tmp.begin(), tmp.end()), "");
        }

        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    constexpr auto assign(std::initializer_list<CharT> ilist) noexcept
        -> expected<non_null_ptr, Error> {
        return assign_range(ilist.begin(), ilist.size());
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto assign(const StringViewLike& t) noexcept -> expected<non_null_ptr, Error> {
        return assign_string_view(t);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto assign(const StringViewLike& t, SizeT pos, SizeT count = npos) noexcept
        -> expected<non_null_ptr, Error> {
        Try(ss, as_view(t).substr(pos, count));
        return assign(ss);
    }

    // --------------------- Append ---------------------

    constexpr auto append(SizeT count, CharT ch) noexcept -> expected<non_null_ptr, Error> {
        return append_fill(count, ch);
    }

    constexpr auto append(const StringT& str) noexcept -> expected<non_null_ptr, Error> {
        return append_string_view(str);
    }

    constexpr auto append(const StringT& str, SizeT pos, SizeT count = npos) noexcept
        -> expected<non_null_ptr, Error> {
        Try(ss, as_view(str).substr(pos, count));
        return append(ss);
    }

    constexpr auto append(const CharT* s, SizeT count) noexcept -> expected<non_null_ptr, Error> {
        return append_range(s, count);
    }

    constexpr auto append(const CharT* s) noexcept -> expected<non_null_ptr, Error> {
        return append(s, Traits::length(s));
    }

    template<std::random_access_iterator RandAccIt>
        requires std::convertible_to<std::iter_value_t<RandAccIt>, CharT>
    constexpr auto append(RandAccIt first, RandAccIt last) noexcept
        -> expected<non_null_ptr, Error> {
        return append_range(first, std::distance(first, last));
    }

    template<std::input_iterator InputIt>
        requires std::convertible_to<std::iter_value_t<InputIt>, CharT>
    constexpr auto append(InputIt first, InputIt last) noexcept
        -> expected<non_null_ptr, std::pair<InputIt, Error>> {
        return append_iter(first, last);
    }

    constexpr auto append(std::initializer_list<CharT> ilist) noexcept
        -> expected<non_null_ptr, Error> {
        return append_range(ilist.begin(), ilist.size());
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto append(const StringViewLike& t) noexcept -> expected<non_null_ptr, Error> {
        return append_string_view(t);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto append(const StringViewLike& t, SizeT pos, SizeT count = npos) noexcept
        -> expected<non_null_ptr, Error> {
        Try(ss, as_view(t).substr(pos, count));
        return append(ss);
    }

    // --------------------- Insert ---------------------

    template<typename IndexT>
        requires std::convertible_to<IndexT, SizeT> || std::convertible_to<IndexT, const_iterator>
    constexpr auto insert(IndexT index, SizeT count, CharT ch) noexcept
        -> expected<non_null_ptr, Error> {
        if constexpr (std::convertible_to<IndexT, SizeT>) {
            Try(it, make_space_at(index, count));
            ktl::uninitialized_fill_n(it, count, ch);
        } else {
            Try(it, make_space_at(index, count));
            ktl::uninitialized_fill_n(it, count, ch);
        }
        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    constexpr auto insert(SizeT index, const CharT* s, SizeT count) noexcept
        -> expected<non_null_ptr, Error> {
        Try(it, make_space_at(index, count));

        ktl::uninitialized_copy_n(s, count, it);
        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    constexpr auto insert(SizeT index, const CharT* s) noexcept -> expected<non_null_ptr, Error> {
        return insert(index, s, Traits::length(s));
    }

    constexpr auto insert(SizeT index, const StringT& str) noexcept
        -> expected<non_null_ptr, Error> {
        return insert(index, as_view(str));
    }

    constexpr auto
    insert(SizeT index, const StringT& str, SizeT index_str, SizeT count = npos) noexcept
        -> expected<non_null_ptr, Error> {
        return insert(index, as_view(str), index_str, count);
    }

    constexpr auto insert(const_iterator pos, CharT ch) noexcept -> expected<non_null_ptr, Error> {
        Try(it, make_space_at(pos, 1));

        std::construct_at(&*it, ch);
        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto insert(SizeT index, const StringViewLike& t) noexcept
        -> expected<non_null_ptr, Error> {
        auto str = as_view(t);
        Try(it, make_space_at(index, str.size()));

        ktl::uninitialized_copy_n(str.begin(), str.size(), it);
        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto
    insert(SizeT index, const StringViewLike& t, SizeT index_str, SizeT count = npos) noexcept
        -> expected<non_null_ptr, Error> {
        Try(str, as_view(t).substr(index_str, count));
        return insert(index, str);
    }

    template<std::random_access_iterator RandAccIt>
        requires std::convertible_to<std::iter_value_t<RandAccIt>, CharT>
    constexpr auto insert(const_iterator pos, RandAccIt first, RandAccIt last)
        -> expected<non_null_ptr, Error> {
        auto n = std::distance(first, last);
        Try(it, make_space_at(pos, n));

        ktl::uninitialized_copy_n(first, n, it);
        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    template<std::input_iterator InputIter>
        requires(!std::random_access_iterator<InputIter>)
        && std::convertible_to<std::iter_value_t<InputIter>, CharT>
        && std::is_default_constructible_v<StringT>
    constexpr auto insert(const_iterator pos, InputIter first, InputIter last)
        -> expected<non_null_ptr, std::pair<InputIter, Error>> {
        if (pos == end()) {
            return insert_at_end(first, last);
        }

        StringT tmp;
        auto tmp_res = tmp.assign(first, last);
        auto res = insert(pos, tmp.begin(), tmp.end());

        // All rows inseted into `tmp` vector? If so, return the `res`.
        if (tmp_res || !res) {
            Throw(std::make_pair(last, std::move(res).error()));
        }
        // If, all rows were not inserted into the `tmp` vector, error must be returned.
        return tmp_res;
    }

    constexpr auto insert(const_iterator pos, std::initializer_list<CharT> ilist)
        -> expected<non_null_ptr, Error> {
        Try(it, make_space_at(pos, ilist.size()));

        ktl::uninitialized_copy_n(ilist.begin(), ilist.size(), it);
        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    // --------------------- Replace ---------------------

    constexpr auto replace(SizeT pos, SizeT count, const StringT& str) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(pos, count, as_view(str));
    }

    constexpr auto replace(const_iterator first, const_iterator last, const StringT& str) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(first, last, as_view(str));
    }

    constexpr auto
    replace(SizeT pos, SizeT count, const StringT& str, SizeT pos2, SizeT count2 = npos) noexcept
        -> expected<non_null_ptr, Error> {
        Try(ss, as_view(str).substr(pos2, count2));
        return replace_impl(pos, count, ss);
    }

    template<std::random_access_iterator RandAccIt>
        requires std::convertible_to<std::iter_value_t<RandAccIt>, CharT>
    constexpr auto
    replace(const_iterator first, const_iterator last, RandAccIt first2, RandAccIt last2) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(first, last, first2, std::distance(first2, last2));
    }

    template<std::input_iterator InputIt>
        requires(!std::random_access_iterator<InputIt>)
        && std::convertible_to<std::iter_value_t<InputIt>, CharT>
        && std::is_default_constructible_v<StringT>
    constexpr auto
    replace(const_iterator first, const_iterator last, InputIt first2, InputIt last2) noexcept
        -> expected<non_null_ptr, std::pair<InputIt, Error>> {
        StringT tmp;

        if (auto res = tmp.append(this->cbegin(), first); !res) {
            Throw(std::make_pair(first2, std::move(res).error()));
        }
        TryV(tmp.append(first2, last2));
        if (auto res = tmp.append(last, this->cend()); !res) {
            Throw(std::make_pair(last2, std::move(res).error()));
        }

        check_(assign(tmp.begin(), tmp.end()), "");

        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    constexpr auto replace(SizeT pos, SizeT count, const CharT* cstr, SizeT count2) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(pos, count, basic_string_view {cstr, count2});
    }

    constexpr auto
    replace(const_iterator first, const_iterator last, const CharT* cstr, SizeT count2) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(first, last, basic_string_view {cstr, count2});
    }

    constexpr auto replace(SizeT pos, SizeT count, const CharT* cstr) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(pos, count, basic_string_view {cstr, Traits::length(cstr)});
    }

    constexpr auto replace(const_iterator first, const_iterator last, const CharT* cstr) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(first, last, basic_string_view {cstr, Traits::length(cstr)});
    }

    constexpr auto replace(SizeT pos, SizeT count, SizeT count2, CharT ch) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(pos, count, ch, count2);
    }

    constexpr auto
    replace(const_iterator first, const_iterator last, SizeT count2, CharT ch) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(first, last, ch, count2);
    }

    constexpr auto
    replace(const_iterator first, const_iterator last, std::initializer_list<CharT> ilist) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(first, last, ilist.begin(), ilist.size());
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto replace(SizeT pos, SizeT count, const StringViewLike& t) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(pos, count, as_view(t));
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto
    replace(const_iterator first, const_iterator last, const StringViewLike& t) noexcept
        -> expected<non_null_ptr, Error> {
        return replace_impl(first, last, as_view(t));
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto replace(
        SizeT pos,
        SizeT count,
        const StringViewLike& t,
        SizeT pos2,
        SizeT count2 = npos) noexcept -> expected<non_null_ptr, Error> {
        Try(ss, as_view(t).substr(pos2, count2));
        return replace(pos, count, ss);
    }

    // --------------------- Erase ---------------------

    constexpr auto erase(SizeT index = 0, SizeT count = npos) noexcept -> StringT& {
        erase_impl(index, count);
        return static_cast<StringT&>(*this);
    }

    constexpr auto erase(const_iterator position) noexcept -> iterator {
        erase_impl(std::distance(cbegin(), position), 1);
        return begin() + std::distance(cbegin(), position);
    }

    constexpr auto erase(const_iterator first, const_iterator last) noexcept -> iterator {
        erase_impl(std::distance(cbegin(), first), std::distance(first, last));
        return begin() + std::distance(cbegin(), first);
    }

    // --------------------- Copy ---------------------

    constexpr auto copy(CharT* dest, SizeT count, SizeT pos = 0) const -> expected<SizeT, Error> {
        auto [beg, end, _] = get_storage();
        usize size = end - beg - 1;

        if (pos > size) {
            Throw(Error::IndexOutOfBounds);
        }

        count = std::min<SizeT>(count, size - pos);
        auto dest_last = ktl::uninitialized_copy_n(beg + pos, count, dest);

        return std::distance(dest, dest_last);
    }

    // --------------------- Compare ---------------------

    constexpr auto compare(const StringT& str) const noexcept -> expected<int, Error> {
        return as_view(*this).compare(basic_string_view {str});
    }

    constexpr auto compare(SizeT pos1, SizeT count1, const StringT& str) const noexcept
        -> expected<int, Error> {
        Try(ss, as_view(*this).substr(pos1, count1));
        return ss.compare(basic_string_view {str});
    }

    constexpr auto
    compare(SizeT pos1, SizeT count1, const StringT& str, SizeT pos2, SizeT count2 = npos)
        const noexcept -> expected<int, Error> {
        Try(ss1, as_view(*this).substr(pos1, count1));
        Try(ss2, basic_string_view {str}.substr(pos2, count2));
        return ss1.compare(ss2);
    }

    constexpr auto compare(const CharT* s) const noexcept -> expected<int, Error> {
        return as_view(*this).compare(s);
    }

    constexpr auto compare(SizeT pos1, SizeT count1, const CharT* s) const noexcept
        -> expected<int, Error> {
        Try(ss1, as_view(*this).substr(pos1, count1));
        return ss1.compare(s);
    }

    constexpr auto compare(SizeT pos1, SizeT count1, const CharT* s, SizeT count2) const noexcept
        -> expected<int, Error> {
        Try(ss1, as_view(*this).substr(pos1, count1));
        return ss1.compare({s, count2});
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto compare(const StringViewLike& t) const noexcept -> expected<int, Error> {
        return as_view(*this).compare(t);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto compare(SizeT pos1, SizeT count1, const StringViewLike& t) const noexcept
        -> expected<int, Error> {
        Try(ss1, as_view(*this).substr(pos1, count1));
        return ss1.compare(t);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto
    compare(SizeT pos1, SizeT count1, const StringViewLike& t, SizeT pos2, SizeT count2 = npos)
        const noexcept -> expected<int, Error> {
        Try(ss1, as_view(*this).substr(pos1, count1));
        Try(ss2, as_view(t).substr(pos2, count2));
        return ss1.compare(ss2);
    }

    // --------------------- starts_with ---------------------

    constexpr auto starts_with(std::basic_string_view<CharT, Traits> sv) const noexcept -> bool {
        return as_view(*this).starts_with(sv);
    }

    constexpr auto starts_with(CharT ch) const noexcept -> bool {
        return as_view(*this).starts_with(ch);
    }

    constexpr auto starts_with(const CharT* s) const noexcept -> bool {
        return as_view(*this).starts_with(s);
    }

    // --------------------- ends_with ---------------------

    constexpr auto ends_with(std::basic_string_view<CharT, Traits> sv) const noexcept -> bool {
        return as_view(*this).ends_with(sv);
    }

    constexpr auto ends_with(CharT ch) const noexcept -> bool {
        return as_view(*this).ends_with(ch);
    }

    constexpr auto ends_with(const CharT* s) const noexcept -> bool {
        return as_view(*this).ends_with(s);
    }

    // --------------------- contains ---------------------

    constexpr auto contains(std::basic_string_view<CharT, Traits> sv) const noexcept -> bool {
        return as_view(*this).contains(sv);
    }

    constexpr auto contains(CharT ch) const noexcept -> bool {
        return as_view(*this).contains(ch);
    }

    constexpr auto contains(const CharT* s) const noexcept -> bool {
        return as_view(*this).contains(s);
    }

    // --------------------- find ---------------------

    constexpr auto find(const StringT& str, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find(as_view(str), pos);
    }

    constexpr auto find(const CharT* s, SizeT pos, SizeT count) const noexcept -> SizeT {
        return as_view(*this).find(s, pos, count);
    }

    constexpr auto find(const CharT* s, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find(s, pos);
    }

    constexpr auto find(CharT ch, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find(ch, pos);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto find(const StringViewLike& t, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find(as_view(t), pos);
    }

    // --------------------- rfind ---------------------

    constexpr auto rfind(const StringT& str, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).rfind(as_view(str), pos);
    }

    constexpr auto rfind(const CharT* s, SizeT pos, SizeT count) const noexcept -> SizeT {
        return as_view(*this).rfind(s, pos, count);
    }

    constexpr auto rfind(const CharT* s, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).rfind(s, pos);
    }

    constexpr auto rfind(CharT ch, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).rfind(ch, pos);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto rfind(const StringViewLike& t, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).rfind(as_view(t), pos);
    }

    // --------------------- find_first_of ---------------------

    constexpr auto find_first_of(const StringT& str, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find_first_of(as_view(str), pos);
    }

    constexpr auto find_first_of(const CharT* s, SizeT pos, SizeT count) const noexcept -> SizeT {
        return as_view(*this).find_first_of(s, pos, count);
    }

    constexpr auto find_first_of(const CharT* s, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find_first_of(s, pos);
    }

    constexpr auto find_first_of(CharT ch, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find_first_of(ch, pos);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto find_first_of(const StringViewLike& t, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find_first_of(as_view(t), pos);
    }

    // --------------------- find_first_not_of ---------------------

    constexpr auto find_first_not_of(const StringT& str, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find_first_not_of(as_view(str), pos);
    }

    constexpr auto find_first_not_of(const CharT* s, SizeT pos, SizeT count) const noexcept
        -> SizeT {
        return as_view(*this).find_first_not_of(s, pos, count);
    }

    constexpr auto find_first_not_of(const CharT* s, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find_first_not_of(s, pos);
    }

    constexpr auto find_first_not_of(CharT ch, SizeT pos = 0) const noexcept -> SizeT {
        return as_view(*this).find_first_not_of(ch, pos);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto find_first_not_of(const StringViewLike& t, SizeT pos = 0) const noexcept
        -> SizeT {
        return as_view(*this).find_first_not_of(as_view(t), pos);
    }

    // --------------------- find_last_of ---------------------

    constexpr auto find_last_of(const StringT& str, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).find_last_of(as_view(str), pos);
    }

    constexpr auto find_last_of(const CharT* s, SizeT pos, SizeT count) const noexcept -> SizeT {
        return as_view(*this).find_last_of(s, pos, count);
    }

    constexpr auto find_last_of(const CharT* s, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).find_last_of(s, pos);
    }

    constexpr auto find_last_of(CharT ch, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).find_last_of(ch, pos);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto find_last_of(const StringViewLike& t, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).find_last_of(as_view(t), pos);
    }

    // --------------------- find_last_not_of ---------------------

    constexpr auto find_last_not_of(const StringT& str, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).find_last_not_of(as_view(str), pos);
    }

    constexpr auto find_last_not_of(const CharT* s, SizeT pos, SizeT count) const noexcept
        -> SizeT {
        return as_view(*this).find_last_not_of(s, pos, count);
    }

    constexpr auto find_last_not_of(const CharT* s, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).find_last_not_of(s, pos);
    }

    constexpr auto find_last_not_of(CharT ch, SizeT pos = npos) const noexcept -> SizeT {
        return as_view(*this).find_last_not_of(ch, pos);
    }

    template<typename StringViewLike>
        requires(
            std::is_convertible_v<const StringViewLike&, basic_string_view<CharT, Traits>>
            && !std::is_convertible_v<const StringViewLike&, const CharT*>)
    constexpr auto find_last_not_of(const StringViewLike& t, SizeT pos = npos) const noexcept
        -> SizeT {
        return as_view(*this).find_last_not_of(as_view(t), pos);
    }

  protected:
    static constexpr CharT NUL = {};

    // --------------------- Substr ---------------------

    constexpr auto substr_destructive(SizeT pos, SizeT count) noexcept
        -> expected<non_null_ptr, Error> {
        auto [beg, end, _] = get_storage();
        usize size = end - beg - 1;

        if (pos > size) [[unlikely]] {
            Throw(Error::IndexOutOfBounds);
        }

        count = std::min<usize>(count, size - pos);

        if (pos > 0) {
            end = std::move(beg + pos, beg + pos + count, beg);
        }
        std::construct_at(beg + count, NUL);
        set_len<UpdateLifetime>(count + 1);

        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    template<std::input_iterator InputIter>
    constexpr auto assign_iter(InputIter first, InputIter last)
        -> expected<non_null_ptr, std::pair<InputIter, Error>> {
        assert(empty() && "assign_iter must be called on empty vector");
        return append_iter(first, last);
    }

    template<std::input_iterator InputIt>
    constexpr auto insert_at_end(InputIt first, InputIt last)
        -> expected<non_null_ptr, std::pair<InputIt, Error>> {
        // TODO: Optimize by doing bulk copy.
        for (; first != last; ++first) {
            if (auto res = push_back(*first); !res) {
                Throw(std::make_pair(first, std::move(res).error()));
            }
        }
        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    ASAN_ANNOTATION_HELPERS;

  private:
    using container = StringT;

    static constexpr bool UpdateLifetime = true;

    constexpr void erase_impl(SizeT index, SizeT count) noexcept {
        check_(index < size(), "erase index cannot exceed string length");

        count = std::min<SizeT>(count, size() - index);

        auto [beg, end, end_cap] = get_storage();
        auto size = end - beg;

        std::move(beg + index + count, end, beg + index);
        set_len<UpdateLifetime>(size - count);
    }

    constexpr auto
    replace_impl(SizeT pos, SizeT count, const basic_string_view<CharT, Traits>& str) noexcept
        -> expected<non_null_ptr, Error> {
        check_(pos + count <= size(), "replace range must be valid");
        auto str_len = str.size();
        TryV(shift_chars_at(
            pos + std::min<usize>(count, str_len),
            static_cast<isize>(count) - static_cast<isize>(str_len)));

        ktl::uninitialized_copy_n(str.begin(), str_len, begin() + pos);

        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    constexpr auto replace_impl(
        const_iterator first,
        const_iterator last,
        const basic_string_view<CharT, Traits>& str) noexcept -> expected<non_null_ptr, Error> {
        return replace_impl(std::distance(cbegin(), first), std::distance(first, last), str);
    }

    constexpr auto replace_impl(
        const_iterator first,
        const_iterator last,
        std::input_iterator auto first2,
        isize str_len) noexcept -> expected<non_null_ptr, Error> {
        isize pos = std::distance(first, first);
        isize count = std::distance(first, last);

        check_(pos + count <= size(), "replace range must be valid");
        TryV(shift_chars_at(pos + std::min(count, str_len), count - str_len));
        ktl::uninitialized_copy_n(first2, str_len, begin() + pos);

        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    constexpr auto
    replace_impl(const_iterator first, const_iterator last, CharT ch, isize times) noexcept
        -> expected<non_null_ptr, Error> {
        isize pos = std::distance(first, first);
        isize count = std::distance(first, last);

        check_(pos + count <= size(), "replace range must be valid");
        TryV(shift_chars_at(pos + std::min(count, times), count - times));
        ktl::uninitialized_fill_n(begin() + pos, times, ch);

        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    constexpr auto replace_impl(SizeT pos, SizeT count, CharT ch, isize times) noexcept
        -> expected<non_null_ptr, Error> {
        check_(pos + count <= size(), "replace range must be valid");
        TryV(shift_chars_at(
            pos + std::min<isize>(count, times),
            static_cast<isize>(count) - static_cast<isize>(times)));
        ktl::uninitialized_fill_n(begin() + pos, times, ch);

        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    constexpr auto shift_chars_at(isize pos, isize count) noexcept -> expected<void, Error> {
        if (count > 0) {
            erase_impl(pos, count);
        } else if (count < 0) {
            TryV(make_space_at(pos, -count));
        }

        return {};
    }

    constexpr auto make_space_at(usize pos, usize count) noexcept -> expected<iterator, Error> {
        check_(pos <= size(), "insert position cannot exceed string length");

        auto [beg, end, end_cap] = get_storage();
        usize size = end - beg;
        usize capacity = end_cap - beg;
        auto new_len = size + count;

        if (new_len > capacity) {
            TryV(grow(new_len));
            auto [nbeg, nend, nend_cap] = get_storage();
            std::tie(beg, end, end_cap) = std::tie(nbeg, nend, nend_cap);
            capacity = end_cap - beg;
        } else {
            adjust_lifetime(new_len);
        }
        assert(capacity >= new_len);

        ktl::uninitialized_move_backward(beg + pos, end, end + count);
        set_len<!UpdateLifetime>(new_len);

        return begin() + pos;
    }

    constexpr auto make_space_at(const_iterator pos, usize count) noexcept
        -> expected<iterator, Error> {
        return make_space_at(std::distance(cbegin(), pos), count);
    }

    constexpr auto assign_range(std::input_iterator auto first, usize count) noexcept
        -> expected<non_null_ptr, Error> {
        return assign_impl(count, [&](auto* begin) {
            ktl::uninitialized_copy_n(first, count, begin);
        });
    }

    constexpr auto assign_string_view(basic_string_view<CharT, Traits> str) noexcept
        -> expected<non_null_ptr, Error> {
        return assign_range(str.begin(), str.size());
    }

    constexpr auto assign_fill(SizeT count, CharT ch) noexcept -> expected<non_null_ptr, Error> {
        return assign_impl(count, [&](auto* begin) {
            ktl::uninitialized_fill_n(begin, count, ch);
        });
    }

    constexpr auto assign_impl(usize count, auto&& initializer) noexcept
        -> expected<non_null_ptr, Error> {
        auto [begin, _, end_cap] = get_storage();
        usize capacity = end_cap - begin;
        auto new_len = count + 1;

        if (new_len > capacity) {  // account for trailing NUL char
            TryV(grow_uninit(new_len));
            // String is cleared and contains enough space to construct count elements

            auto [nbeg, nend, nend_cap] = get_storage();
            std::tie(begin, _, end_cap) = std::tie(nbeg, nend, nend_cap);
            capacity = end_cap - begin;
        } else {
            adjust_lifetime(new_len);
        }
        assert(capacity > count);

        initializer(begin);
        std::construct_at(begin + count, NUL);
        set_len<!UpdateLifetime>(new_len);

        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    template<std::input_iterator InputIter>
    constexpr auto append_iter(InputIter first, InputIter last)
        -> expected<non_null_ptr, std::pair<InputIter, Error>> {
        // TODO: Optimize by doing bulk copy.
        for (; first != last; ++first) {
            if (auto res = push_back(*first); !res) {
                Throw(std::make_pair(first, std::move(res).error()));
            }
        }
        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    constexpr auto append_range(std::input_iterator auto first, usize count) noexcept
        -> expected<non_null_ptr, Error> {
        return append_impl(count, [&](auto* end) {
            return ktl::uninitialized_copy_n(first, count, end);
        });
    }

    constexpr auto append_string_view(basic_string_view<CharT, Traits> str) noexcept
        -> expected<non_null_ptr, Error> {
        return assign_range(str.begin(), str.size());
    }

    constexpr auto append_fill(SizeT count, CharT ch) noexcept -> expected<non_null_ptr, Error> {
        return append_impl(count, [&](auto* end) {
            return ktl::uninitialized_fill_n(end, count, ch);
        });
    }

    constexpr auto append_impl(usize count, auto&& initializer) noexcept
        -> expected<non_null_ptr, Error> {
        auto [begin, end, end_cap] = get_storage();
        usize capacity = end_cap - begin;
        usize old_len = end - begin - 1;  // account for trailing NUL char
        auto new_len = old_len + count + 1;

        if (new_len > capacity) {
            TryV(grow(new_len));

            auto [nbeg, nend, nend_cap] = get_storage();
            std::tie(begin, end, end_cap) = std::tie(nbeg, nend, nend_cap);
            capacity = end_cap - begin;
        } else {
            adjust_lifetime(new_len);
        }

        auto new_end = initializer(begin + old_len);
        std::construct_at(new_end, NUL);
        set_len<!UpdateLifetime>(new_len);

        return non_null_ptr {static_cast<StringT&>(*this)};
    }

    template<bool Initialize>
    constexpr auto resize_impl(SizeT count, CharT ch) noexcept -> expected<void, Error> {
        auto [begin, end, end_cap] = get_storage();
        usize len = end - begin;
        usize capacity = end_cap - begin;
        usize new_len = count + 1;

        if (new_len > capacity) {
            TryV(grow(new_len));
            auto [nbegin, nend, nend_cap] = get_storage();
            std::tie(begin, end, end_cap) = std::tie(nbegin, nend, nend_cap);
        } else {
            adjust_lifetime(new_len);
        }

        if constexpr (Initialize) {
            if (count > len - 1) {
                ktl::uninitialized_fill_n(
                    end - 1,
                    count - (len - 1),
                    ch);  // 'end[-1]' contains NUL char
            }
        }

        std::construct_at(begin + count, NUL);
        set_len<!UpdateLifetime>(new_len);
        return {};
    }

#undef as_view

    constexpr auto get_storage_impl() const noexcept -> string_storage<const CharT> {
        static_assert(string_like<StringT, CharT, SizeT>, "StringT is not a string");
        return static_cast<const StringT*>(this)->get_storage();
    }

    constexpr auto get_storage() const noexcept -> string_storage<const CharT> {
        static_assert(string_like<StringT, CharT, SizeT>, "StringT is not a string");
        auto res = static_cast<const StringT*>(this)->get_storage();
        check_(res.end_cap >= res.end, "string pointers are invalid");
        check_(res.end >= res.begin, "string pointers are invalid");
        check_(res.end[-1] == NUL, "string must contain trailing NULL character");
        return res;
    }
    constexpr auto get_storage() noexcept -> string_storage<CharT> {
        static_assert(string_like<StringT, CharT, SizeT>, "StringT is not a string");
        auto res = static_cast<StringT*>(this)->get_storage();
        check_(res.end_cap >= res.end, "string pointers are invalid");
        check_(res.end >= res.begin, "string pointers are invalid");
        check_(res.end[-1] == NUL, "string must contain trailing NULL character");
        return res;
    }

    constexpr auto grow(usize req_len) noexcept -> expected<void, Error> {
        static_assert(string_like<StringT, CharT, SizeT>, "StringT is not a string");
        return static_cast<StringT*>(this)->grow(req_len);
    }
    constexpr auto grow_uninit(usize req_len) noexcept -> expected<void, Error> {
        static_assert(string_like<StringT, CharT, SizeT>, "StringT is not a string");
        return static_cast<StringT*>(this)->grow_uninit(req_len);
    }

    template<bool UpdateLifetime>
    constexpr void set_len(SizeT new_len) noexcept {
        static_assert(string_like<StringT, CharT, SizeT>, "StringT is not a string");
        static_cast<StringT*>(this)->set_len(new_len);
        assert(*end() == NUL);  // Must contain trailing NUL char
        if constexpr (UpdateLifetime) {
            adjust_lifetime(new_len);
        }
    }
};

template<typename V>
concept comparable_string = requires(const V& v) {
    requires V::is_string == true;
    { static_cast<basic_string_view<typename V::value_type, typename V::traits_type>>(v) };
};

}  // namespace ktl::detail::str

// ---------------------- Comparison Operators ---------------------
namespace ktl {
template<detail::str::comparable_string Str1, detail::str::comparable_string Str2>
constexpr auto operator==(const Str1& lhs, const Str2& rhs) noexcept -> bool {
    return basic_string_view {lhs} == rhs;
}

template<detail::str::comparable_string Str>
constexpr auto operator==(const Str& lhs, const typename Str::value_type* rhs) noexcept -> bool {
    return basic_string_view {lhs} == rhs;
}

template<detail::str::comparable_string Str1, detail::str::comparable_string Str2>
constexpr auto operator<=>(const Str1& lhs, const Str2& rhs) noexcept {
    return basic_string_view {lhs} <=> rhs;
}

template<detail::str::comparable_string Str>
constexpr auto operator<=>(const Str& lhs, const typename Str::value_type* rhs) noexcept {
    return basic_string_view {lhs} <=> rhs;
}
}  // namespace ktl