#include <ktl/stack_vector.hpp>

using namespace ktl;

auto main() -> int {
    static_assert(sizeof(svector<u8, 3>) == sizeof(u8) * 4);
    static_assert(sizeof(svector<u16, 3>) == sizeof(u16) * 4);
    static_assert(sizeof(svector<u32, 3>) == sizeof(u32) * 4);
    static_assert(sizeof(svector<u64, 3>) == sizeof(u64) * 4);
    static_assert(std::conjunction_v<
                  std::is_trivially_destructible<svector<u64, 3>>,
                  std::is_trivially_copy_constructible<svector<u64, 3>>,
                  std::is_trivially_copy_assignable<svector<u64, 3>>,
                  std::is_trivially_move_constructible<svector<u64, 3>>,
                  std::is_trivially_move_assignable<svector<u64, 3>>>);
    static_assert(make_svector<4>(1, 2, 3).size() == 3);
    static_assert(make_svector(1.0, 2, 3, .0f)[0] == 1.0);

    // Size and element access
    {
        constexpr auto vec = [] {
            svector<int, 3> vec;
            check_(vec.push_back(1), "push_back failed");
            check_(vec.push_back(2), "push_back failed");
            return vec;
        }();
        static_assert(vec.size() == 2);
        static_assert(vec[0] == 1);
        static_assert(vec[1] == 2);
    }

    // Push back
    {
        svector<int, 1> vec;
        check_(vec.push_back(1).has_value(), "push_back must succeed");
        check_(!vec.push_back(2).has_value(), "push_back must fail");
    }

    // Move/Copy count
    {
        static int cons_count = 0;
        static int copy_cons_count = 0;
        static int move_cons_count = 0;
        static int copy_assign_count = 0;
        static int move_assign_count = 0;
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

        svector<move_test, 4> vec;

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
        struct int_t {
            int_t() = default;
            int v = 0;
        };
        auto vec = make_svector(int_t {}, int_t {}, int_t {});
        auto vec2 = vec;
        check_(vec2[0].v == vec[0].v, "");
    }

    // Pop back
    {
        auto vec = make_svector(1);
        vec.pop_back();
        check_(vec.empty(), "must be empty after pop_back");
    }

    // Clear
    {
        auto vec = make_svector(1, 2, 3);
        vec.clear();
        check_(vec.empty(), "must be empty after clear");
    }

    // Resize
    {
        auto vec = make_svector(1, 2, 3);
        check_(vec.resize(1), "resize must succeed");
        check_(vec.resize(2, 2), "resize must succeed");
        check_(vec[1] == 2, "must contain `filled_value` after resize");
        check_(!vec.resize(4), "resize over capacity must fail");
    }

    return 0;
}