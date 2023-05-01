set(ENTRYPOINT start)
add_library(platform OBJECT platform.cpp)
add_library(ktl::test::platform ALIAS platform)
target_compile_definitions(platform PRIVATE -DENTRYPOINT=${ENTRYPOINT})
target_link_libraries(platform PUBLIC ktl stdc++)
target_link_options(platform PUBLIC -e ${ENTRYPOINT})

include(CTest)

function(define_test name)
    cmake_parse_arguments(
        PARSE_ARGV 1 define_test "" ""
        "SOURCES")

    add_executable(${name} ${define_test_SOURCES})

    # Tests - All tests must link against `platform`
    target_link_libraries(${name} PRIVATE ktl::test::platform)
    target_compile_options(${name} PRIVATE -ftemplate-backtrace-limit=0)
    add_test(NAME ${name} COMMAND ${name})
endfunction()

function(define_simple_test name)
    define_test(${name} SOURCES ${name}.cpp)
endfunction()

define_simple_test(format_test)
define_simple_test(string_view_test)
define_simple_test(vector_test)
define_simple_test(bitops_test)
define_simple_test(string_test)
define_simple_test(container_test)
define_simple_test(generator_test)
