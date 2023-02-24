#include <ktl/fmt/format.hpp>

#include "platform.h"

using namespace ktl;
using namespace ktl::fmt::literals;

template<fmt::fixed_string FmtStr>
static void print();

template<fmt::fixed_string Str, std::integral T, fmt::detail::int_base Base>
struct from_chars {
    static constexpr auto value = []() -> std::optional<T> {
        T v;
        auto res = fmt::detail::from_chars<Base>(std::begin(Str.value), std::end(Str.value), v);
        if (res.ec != std::errc {}) {
            return {};
        }
        return v;
    }();
};

template<
    fmt::fixed_string Str,
    std::integral T,
    fmt::detail::int_base Base = fmt::detail::int_base::dec>
static constexpr auto from_chars_v = from_chars<Str, T, Base>::value;

auto main() -> int {
    static_assert(from_chars_v<"100", int> == 100);
    static_assert(from_chars_v<"-340404", int> == -340404);
    static_assert(from_chars_v<"-12a", int> == -12);
    static_assert(!from_chars_v<"+12a", int>.has_value());
    static_assert(!from_chars_v<"a100", int>.has_value());

    static_assert(*fmt::detail::field_count("format {} is {{ }} {:} valid \n{}") == 8);
    static_assert(*fmt::detail::field_count("{{}}") == 2);
    static_assert(*fmt::detail::field_count("a{{{:} }}") == 3);
    static_assert(*fmt::detail::field_count("{{ {:} }}") == 4);
    static_assert(*fmt::detail::field_count("a{{ {:.<5.5s} }}") == 4);
    static_assert(!fmt::detail::field_count("{"));
    static_assert(!fmt::detail::field_count("}"));
    static_assert(!fmt::detail::field_count("{}}"));
    static_assert(!fmt::detail::field_count("Hi}{"));
    static_assert(*fmt::detail::field_count("Hi}}{{") == 2);
    static_assert(*fmt::detail::field_count("{1002:*^-#0{20}.{30}Ld}") == 1);
    static_assert(*fmt::detail::field_count("{1002:*^-#0{20}.22Ld}") == 1);
    static_assert(*fmt::detail::field_count("{1002:*^-#0{}.22Ld}") == 1);

    static_assert(
        std::string_view {"{:*^{}}"_f.format_to_array<"this"_s, 10>().data()} == "***this***");

    // write("{:*^{}}\n"_f.format_to_array<"this"_s, 10>().data());

    print<"format {} is {{ }} {:} valid \n{}">();
    print<"{{}}">();
    print<"a{{{:} }}">();
    print<"{{ {:} }}">();
    print<"a{{ {:.<5.5s} }}">();
    print<"Hi}}{{">();
    print<"{1002:*^-#0{20}.{30}Ld}">();
    print<"{1002:*^-#0{20}.22Ld}">();
    print<"{1002:*^-#0{}.22Ld}">();

    return 0;
}

static void print(etl::string_view str, fmt::detail::range_t rng);

template<fmt::fixed_string FmtStr>
void print() {
    etl::string_view fmt_str = FmtStr.value;

    write(fmt_str);
    write("\n");
    write("#################################################\n");
    for (const fmt::detail::field_t& field : fmt::make_format_string<FmtStr>().value()) {
        if (field.has_literal()) {
            const fmt::detail::literal_t& lit = field.literal();
            print(fmt_str, lit.range);
        } else if (field.has_replacement()) {
            const auto& rep = field.replacement();
            write('{');
            if (rep.value.has_argid()) {
                auto argid = rep.value.argid();
                write("argument_id = ");
                write(argid);
                write(",");
            }

            if (auto fmt_spec = rep.fmt_spec) {
                if (auto fill_and_align = fmt_spec.value().fill_and_align) {
                    write("fill_and_align = ");
                    write((char)fill_and_align.value().fill);
                    write((char)fill_and_align.value().align);
                    write(",");
                }

                if (auto sign = fmt_spec.value().sign) {
                    write("sign = ");
                    write((char)sign.value());
                    write(",");
                }

                if (fmt_spec.value().use_alternative_form) {
                    write("use_alternative_form = t,");
                }

                if (fmt_spec.value().zero_pad) {
                    write("zero_pad = t,");
                }

                if (auto width = fmt_spec.value().width) {
                    write("width = ");
                    auto w = width.value();
                    if (w.has_replacement()) {
                        if (w.replacement().has_argid()) {
                            write("argid = ");
                            write(w.replacement().argid());
                            write(",");
                        } else {
                            write("{},");
                        }
                    } else {
                        write(w.direct());
                        write(",");
                    }
                }

                if (auto precision = fmt_spec.value().precision) {
                    write("precision = ");
                    auto p = precision.value();
                    if (p.has_replacement()) {
                        if (p.replacement().has_argid()) {
                            write("argid = ");
                            write(p.replacement().argid());
                            write(",");
                        } else {
                            write("{},");
                        }
                    } else {
                        write(p.direct());
                        write(",");
                    }
                }

                if (fmt_spec.value().locale_specific) {
                    write("locale_specific = t,");
                }

                if (auto type = fmt_spec.value().type) {
                    write("type = ");
                    write((char)type.value());
                    write(",");
                }
            }
            write("}");
        }
    }
    write("\n#################################################\n");
}

void print(etl::string_view str, fmt::detail::range_t rng) {
    auto start = static_cast<usize>(rng.start);
    auto end = static_cast<usize>(rng.end) + 1;

    str = str.substr(start, end - start);
    write(str);
}
