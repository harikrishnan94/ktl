#include <ktl/access.hpp>
#include <ktl/fixed_vector.hpp>
#include <ktl/memory.hpp>
#include <ktl/span.hpp>
#include <ktl/static_vector.hpp>
#include <ktl/vector.hpp>

#include "allocator.hpp"
#include "input_iterator.hpp"

using namespace ktl;

struct int_t {
    int_t() = default;
    constexpr explicit int_t(int v) : v(v) {}

    auto operator==(const int_t&) const -> bool = default;
    auto operator<=>(const int_t&) const = default;

    int v = 0;
};

static void svector_insert_test();
static void svector_assign_test();
static void svector_erase_test();
static void svector_operators_test();
static void vector_test();

static void svector_test() {
    static_assert(ASAN_ENABLED ? true : sizeof(static_vector<u8, 3>) == sizeof(u8) * 4);
    static_assert(ASAN_ENABLED ? true : sizeof(static_vector<u16, 3>) == sizeof(u16) * 4);
    static_assert(ASAN_ENABLED ? true : sizeof(static_vector<u32, 3>) == sizeof(u32) * 4);
    static_assert(ASAN_ENABLED ? true : sizeof(static_vector<u64, 3>) == sizeof(u64) * 4);
    static_assert(ASAN_ENABLED ? true : std::is_trivially_destructible_v<static_vector<u64, 3>>);
    static_assert(make_static_vector<4>(1, 2, 3).size() == 3);
    static_assert(make_static_vector(1.0, 2, 3, .0f)[0] == 1.0);

    // Size and element access
    {
        constexpr auto vec = [] {
            static_vector<int, 3> vec;
            check_(vec.push_back(1), "push_back failed");
            check_(vec.push_back(2), "push_back failed");
            return vec;
        }();
        static_assert(vec.size() == 2);
        static_assert(vec[0] == 1);
        static_assert(vec[1] == 2);
    }

    // Push back
    [[maybe_unused]] static constinit auto push_back = [] {
        static_vector<int, 1> vec;
        check_(vec.push_back(1).has_value(), "push_back must succeed");
        check_(!vec.push_back(2).has_value(), "push_back must fail");
        return vec.empty();
    }();

    // Move/Copy count
    {
        static int cons_count = 0;
        static int copy_cons_count = 0;
        static int move_cons_count = 0;
        [[maybe_unused]] static int copy_assign_count = 0;
        [[maybe_unused]] static int move_assign_count = 0;
        [[maybe_unused]] static int destroy_count = 0;
        struct move_test {
            move_test() {
                ++cons_count;
            }

            ~move_test() {
                ++destroy_count;
            }

            move_test(const move_test& /*unused*/) {
                ++copy_cons_count;
            }
            move_test(move_test&& /*unused*/) noexcept {
                ++move_cons_count;
            }

            auto operator=(const move_test& o) -> move_test& {
                if (this != &o) {
                    ++copy_assign_count;
                }
                return *this;
            }
            auto operator=(move_test&& /*unused*/) noexcept -> move_test& {
                ++move_assign_count;
                return *this;
            }
        };

        static_vector<move_test, 4> vec;

        check_(vec.empty(), "vector must be empty");
        check_(vec.push_back({}), "push_back must succeed");
        check_(vec.push_back({}), "push_back must succeed");

        check_(cons_count == 2, "construction count mismatch");
        check_(copy_cons_count == 0, "copy count failure");
        check_(move_cons_count == 2, "move count failure");

        auto vec1 = vec;

        check_(cons_count == 2, "construction count mismatch");
        check_(copy_cons_count == 2, "copy count failure");
        check_(move_cons_count == 2, "move count failure");

        auto vec2 = std::move(vec);

        check_(cons_count == 2, "construction count mismatch");
        check_(copy_cons_count == 2, "copy count failure");
        check_(move_cons_count == 4, "move count failure");

        vec = std::move(vec2);

        check_(cons_count == 2, "construction count mismatch");
        check_(copy_cons_count == 2, "copy count failure");
        check_(move_cons_count == 6, "move count failure");
    }

    // Non-Trivial type
    {
        auto vec = make_static_vector(int_t {}, int_t {}, int_t {});
        auto vec2 = vec;
        check_(vec2[0].v == vec[0].v, "");
    }

    // Pop back
    [[maybe_unused]] static constinit auto pop_back = [] {
        auto vec = make_static_vector(1);
        vec.pop_back();
        check_(vec.empty(), "must be empty after pop_back");

        return vec.empty();
    }();

    // Clear
    [[maybe_unused]] static constinit auto clear = [] {
        auto vec = make_static_vector(1, 2, 3);
        vec.clear();
        check_(vec.empty(), "must be empty after clear");

        return vec.empty();
    }();

    // Resize
    [[maybe_unused]] static constinit auto resize = [] {
        auto vec = make_static_vector<4>(1, 2, 3);
        check_(vec.resize(1), "resize must succeed");
        check_(vec.resize(2, 2), "resize must succeed");
        check_(vec[1] == 2, "must contain `filled_value` after resize");
        check_(vec.resize(4, 4), "");
        check_(vec[3] == 4, "must contain `filled_value` after resize");
        check_(!vec.resize(5), "resize over capacity must fail");

        return vec.empty();
    }();

    svector_assign_test();
    svector_insert_test();
    svector_erase_test();
    svector_operators_test();
}

static void svector_assign_test() {
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector(1, 2, 3);
            check_(vec.assign({3, 2}), "");

            return vec;
        }();

        static_assert(vec.size() == 2, "count must match after assign");
        static_assert(vec[0] == 3 && vec[1] == 2, "values must match after assign");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector(1, 2, 3);
            const auto ovec = make_static_vector(3, 2);
            check_(vec.assign(ovec.begin(), ovec.end()), "");

            return vec;
        }();

        static_assert(vec.size() == 2, "count must match after assign");
        static_assert(vec[0] == 3 && vec[1] == 2, "values must match after assign");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector(1, 2, 3);
            check_(vec.assign(2, 1), "");

            return vec;
        }();

        static_assert(vec.size() == 2, "count must match after assign");
        static_assert(vec[0] == 1 && vec[1] == 1, "values must match after assign");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector(1, 2, 3);
            const auto ovec = make_static_vector(3, 2);
            InputIterator begin {ovec.begin()};
            InputIterator end {ovec.end()};

            check_(vec.assign(begin, end), "");

            return vec;
        }();

        check_(vec.size() == 2, "count must match after assign");
        check_(vec[0] == 3 && vec[1] == 2, "values must match after assign");
    }

    // Tests for post conditions of a failed assign operation
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector(1, 2, 3);
            const auto ovec = make_static_vector(3, 2, 1, 0);
            InputIterator begin {ovec.begin()};
            InputIterator end {ovec.end()};

            check_(
                vec.assign(begin, end).error().second == error::BufferFull,
                "large assign must fail");

            return vec;
        }();

        static_assert(vec.size() == 3, "count must not change after assign failure");
        static_assert(vec[0] == 1 && vec[1] == 2 && vec[2] == 3, "values must match after assign");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector(1, 2, 3);
            check_(vec.assign(4, 1).error() == error::BufferFull, "large assign must fail");

            return vec;
        }();

        static_assert(vec.size() == 3, "count must not change after assign failure");
        static_assert(vec[0] == 1 && vec[1] == 2 && vec[2] == 3, "values must match after assign");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector(1, 2, 3);
            check_(vec.assign({1, 2, 3, 4}).error() == error::BufferFull, "large assign must fail");

            return vec;
        }();

        static_assert(vec.size() == 3, "count must not change after assign failure");
        static_assert(vec[0] == 1 && vec[1] == 2 && vec[2] == 3, "values must match after assign");
    }
}

// NOLINTBEGIN(*-magic-numbers)
static void svector_insert_test() {
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector<6>(1, 2, 8);
            const auto ovec = make_static_vector(3, 2);

            check_(vec.insert(std::next(vec.begin()), ovec.begin(), ovec.end()), "");
            return vec;
        }();

        static_assert(vec.size() == 5, "count must match after insert");
        static_assert(
            vec[0] == 1 && vec[1] == 3 && vec[2] == 2 && vec[3] == 2 && vec[4] == 8,
            "values must match after insert");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector<6>(1, 2, 8);
            const auto ovec = make_static_vector(3, 2);
            InputIterator begin {ovec.begin()};
            InputIterator end {ovec.end()};

            auto res = vec.insert(vec.begin() + 2, begin, end);
            check_(res, "insert must succeed");
            check_(**res == 3, "iterator must point to first inserted value");
            return vec;
        }();

        static_assert(vec.size() == 5, "count must match after insert");
        static_assert(
            vec[0] == 1 && vec[1] == 2 && vec[2] == 3 && vec[3] == 2 && vec[4] == 8,
            "values must match after insert");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector<6>(1, 2, 3);

            auto res = vec.insert(vec.begin(), {3, 2});
            check_(res, "insert must succeed");
            check_(**res == 3, "iterator must point to first inserted value");
            return vec;
        }();

        static_assert(vec.size() == 5, "count must match after insert");
        static_assert(
            vec[0] == 3 && vec[1] == 2 && vec[2] == 1 && vec[3] == 2 && vec[4] == 3,
            "values must match after insert");
    }
    {
        [[maybe_unused]] static constinit auto vec = [] {
            auto vec = make_static_vector<4>(1, 2, 3);
            auto res = vec.insert(vec.end(), 4);
            check_(res, "insert must succeed");
            check_(**res == 4, "iterator must point to first inserted value");
            return vec;
        }();
    }
    // Non-Trivial type
    {
        auto vec = make_static_vector<5>(int_t {1}, int_t {2}, int_t {3});

        check_(vec.insert(vec.begin(), {int_t {4}, int_t {5}}), "insert must succeed");
        check_(
            vec[0] == int_t {4} && vec[1] == int_t {5} && vec[2] == int_t {1} && vec[3] == int_t {2}
                && vec[4] == int_t {3},
            "values must match after insert");
    }

    // Tests for post conditions of a failed insert operation
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector(1, 2, 3);
            check_(!vec.insert(vec.begin(), {3, 2}), "insert must fail");
            return vec;
        }();

        static_assert(vec.size() == 3, "count must not change after failed insert");
        static_assert(vec[0] == 1 && vec[1] == 2 && vec[2] == 3, "values must match after insert");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector(1, 2, 3);
            check_(!vec.insert(vec.end(), 4), "insert must not succeed");
            return vec;
        }();

        static_assert(vec.size() == 3, "count must not change after failed insert");
        static_assert(vec[0] == 1 && vec[1] == 2 && vec[2] == 3, "values must match after insert");
    }
}
// NOLINTEND(*-magic-numbers)

void svector_erase_test() {
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector<4>(1, 2, 3);
            auto it = vec.erase(vec.begin());
            check_(*it == 2, "iterator must point to last removed element");
            return vec;
        }();

        static_assert(vec.size() == 2, "count must match after erase");
        static_assert(vec[0] == 2 && vec[1] == 3, "values must match after erase");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector<4>(1, 2, 3);
            auto it = vec.erase(std::next(vec.begin()), vec.end());
            check_(it == vec.end(), "iterator must point to last removed element");
            return vec;
        }();

        static_assert(vec.size() == 1, "count must match after erase");
        static_assert(vec[0] == 1, "values must match after erase");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector<4>(1, 2, 3);
            auto it = vec.erase(vec.begin(), vec.end());
            check_(it == vec.end(), "iterator must point to last removed element");
            return vec;
        }();

        static_assert(vec.empty(), "vector must be empty after reset");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector<4>(1, 2, 3);
            erase(vec, 3);
            return vec;
        }();

        static_assert(vec.size() == 2, "count must match after erase");
        static_assert(vec[0] == 1 && vec[1] == 2, "values must match after erase");
    }
    {
        constexpr auto vec = [] {
            auto vec = make_static_vector<4>(1, 2, 3);
            erase_if(vec, [](auto e) { return e < 3; });
            return vec;
        }();

        static_assert(vec.size() == 1, "count must match after erase");
        static_assert(vec[0] == 3, "values must match after erase");
    }
}

static void fvector_test() {
    // sanity test

    [[maybe_unused]] static constinit auto _ = [] {
        {
            std::array<int, 4> storage {};
            usize len = 0;
            fixed_vector vec {storage.data(), len, storage.size()};
            check_(vec.max_size() == storage.size(), "max_size must equal to array's size");
        }

        std::array<int, 4> storage;  // NOLINT(*-member-init)
        usize len = 0;
        fixed_vector vec {storage, len};

        check_(vec.capacity() == storage.size(), "capacity must match array's size");
        check_(vec.push_back(1), "push_back failed");
        check_(vec.push_back(2), "push_back failed");
        check_(vec.size() == 2, "");
        check_(vec[0] == 1, "");
        check_(vec[1] == 2, "");

        check_(vec.insert(vec.begin(), {0}), "");
        check_(vec[0] == 0, "");

        check_(vec.assign({1, 2, 3}), "");
        check_(vec[0] == 1, "");
        check_(vec[1] == 2, "");
        check_(vec[2] == 3, "");

        erase_if(vec, [](auto e) { return e < 3; });
        check_(vec.size() == 1, "");
        check_(vec[0] == 3, "");

        {
            const std::array arr = {1};
            check_(vec.assign(InputIterator {arr.begin()}, InputIterator {arr.end()}), "");
            check_(vec[0] == 1, "");
        }

        // Cast operators
        {
            auto svec = make_static_vector(0);
            fixed_vector vec {svec};
            span s1 = vec;
            span s2 = svec;

            check_(!s1.empty(), "");
            check_(!s2.empty(), "");
        }

        // Swap
        {
            std::array arr1 = {1, 2};
            std::array arr2 = {3, 4, 0};
            usize len1 = 2;
            usize len2 = 3;
            fixed_vector vec1 {arr1, len1};
            fixed_vector vec2 {arr2, len2};

            std::swap(vec1, vec2);

            check_(vec1[0] == 3 && vec1[1] == 4 && vec1[2] == 0, "swap failure");
            check_(vec2[0] == 1 && vec2[1] == 2, "swap failure");
        }
        return vec.size();
    }();
}

void svector_operators_test() {
    [[maybe_unused]] static constinit auto _ = [] {
        auto v1 = make_static_vector(1, 2, 3);
        std::array a1 = {1, 2, 3};
        usize len1 = a1.size();
        usize len2 = 2;
        fixed_vector v2 {a1, len1};
        fixed_vector v3 {a1, len2};

        check_(v1 == v2, "");
        check_(v1 != v3, "");

        check_(v1 > v3, "");
        check_(v3 < v1, "");

        check_(v1 >= v2, "");
        check_(v2 <= v1, "");

        return v1.size();
    }();
}

void vector_test() {
    [[maybe_unused]] static constinit auto sanity = [] {
        auto v1 = *make_vector<int, Allocator<int>>({1, 1, 1});
        auto v2 = *make_vector<int, Allocator<int>>(3, 1);
        auto v3 = clone(v1);

        static_assert(sizeof(v1) == 24);  // Empty allocator must not consume space.

        check_(v3, "clone must succeed");
        check_(v1 == v2 && v2 == v3, "vectors must match");
        check_(v3->assign(v2.begin(), v2.end()), "assign must succeed");
        check_(v1 == v3, "vectors must match");

        check_(v1.insert(v1.begin(), {1, 2, 3}), "");
        check_(
            v1[0] == 1 && v1[1] == 2 && v1[2] == 3 && v1[3] == 1 && v1[4] == 1 && v1[5] == 1,
            "values must match after insert");

        check_(erase(v1, 1) == 4, "");
        check_(v1[0] == 2 && v1[1] == 3, "values must match after insert");

        check_(v1.capacity() == 6, "");
        check_(v1.shrink_to_fit(), "");
        check_(v1[0] == 2 && v1[1] == 3, "values must match after insert");
        check_(v1.capacity() == v1.size(), "");

        v1.swap(v2);
        check_(v1 == v3, "vectors must match");
        check_(v2 != v3, "vectors must not match");

        v1.clear();
        check_(v1.shrink_to_fit(), "");
        check_(v1.empty() && v1.capacity() == 0, "");
        check_(v1.resize(10, 0), "");

        v2.clear();
        check_(v2.insert(v2.begin(), InputIterator {v1.begin()}, InputIterator {v1.end()}), "");
        check_(v1 == v2, "");

        return v1.empty();
    }();

    [[maybe_unused]] static constinit auto non_trivial = [] {
        vector<int_t, Allocator<int_t>> v1;

        check_(v1.assign({int_t {}, int_t {1}, int_t {2}}), "");

        auto v2 = clone(v1);

        check_(v1 == v2, "");
        check_(v1[2] == int_t {2}, "");

        return v1.empty();
    }();

    // Move/Copy count
    {
        static int cons_count = 0;
        static int copy_cons_count = 0;
        static int move_cons_count = 0;
        [[maybe_unused]] static int copy_assign_count = 0;
        [[maybe_unused]] static int move_assign_count = 0;
        static int destroy_count = 0;
        struct move_test {
            move_test() {
                ++cons_count;
            }

            ~move_test() {
                ++destroy_count;
            }

            move_test(const move_test& /*unused*/) {
                ++copy_cons_count;
            }
            move_test(move_test&& /*unused*/) noexcept {
                ++move_cons_count;
            }

            auto operator=(const move_test& o) -> move_test& {
                if (this != &o) {
                    ++copy_assign_count;
                }
                return *this;
            }
            auto operator=(move_test&& /*unused*/) noexcept -> move_test& {
                ++move_assign_count;
                return *this;
            }
        };

        vector<move_test, Allocator<move_test>> vec;

        check_(vec.empty(), "vector must be empty");
        check_(vec.push_back({}), "push_back must succeed");
        check_(vec.push_back({}), "push_back must succeed");

        check_(cons_count == 2, "construction count mismatch");
        check_(copy_cons_count == 0, "copy count failure");
        check_(move_cons_count == 3, "move count failure");  // One extra `move` due to resize.

        auto vec1 = clone(vec);

        check_(cons_count == 2, "construction count mismatch");
        check_(copy_cons_count == 2, "copy count failure");
        check_(move_cons_count == 3, "move count failure");

        auto vec2 = std::move(vec);

        check_(cons_count == 2, "construction count mismatch");
        check_(copy_cons_count == 2, "copy count failure");
        check_(move_cons_count == 3, "move count failure");

        vec = std::move(vec2);

        check_(cons_count == 2, "construction count mismatch");
        check_(copy_cons_count == 2, "copy count failure");
        check_(move_cons_count == 3, "move count failure");

        check_(vec.reserve(10), "");
        check_(cons_count == 2, "construction count mismatch");
        check_(copy_cons_count == 2, "copy count failure");
        check_(move_cons_count == 5, "move count failure");

        (void)std::move(vec);
        check_(cons_count == 2, "construction count mismatch");
        check_(copy_cons_count == 2, "copy count failure");
        check_(move_cons_count == 5, "move count failure");
        check_(destroy_count == 2, "destroy count failure");
    }

    // non_copyable type
    [[maybe_unused]] static constinit auto non_copyable = [] {
        struct non_copyable {
            int v;

            constexpr non_copyable(int v) : v {v} {}
            non_copyable(non_copyable&&) = default;
            auto operator=(non_copyable&&) -> non_copyable& = default;
            non_copyable(const non_copyable&) = delete;
            auto operator=(const non_copyable&) -> non_copyable& = delete;

            constexpr auto operator==(const non_copyable& b) const -> bool {
                return v == b.v;
            }
        };

        vector<non_copyable, Allocator<non_copyable>> vec;

        check_(vec.push_back({1}), "");
        check_(vec.insert(vec.end(), non_copyable {2}), "");
        check_(vec.insert(vec.begin(), non_copyable {3}), "");
        check_(
            vec[0] == non_copyable {3} && vec[1] == non_copyable {1} && vec[2] == non_copyable {2},
            "");

        return vec.empty();
    }();
}

auto main() -> int {
    svector_test();
    fvector_test();
    vector_test();
    return 0;
}