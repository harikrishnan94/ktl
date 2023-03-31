set(ENTRYPOINT start)
add_library(platform OBJECT platform.cpp)
target_compile_definitions(platform PRIVATE -DENTRYPOINT=${ENTRYPOINT})
target_link_libraries(platform PUBLIC ktl)
target_link_options(platform PUBLIC -e ${ENTRYPOINT})

include(CTest)

function(define_test name)
    add_executable(${name} ${name}.cpp)

    # Tests - All tests must link against `platform`
    target_link_libraries(${name} PRIVATE platform)
    add_test(NAME ${name} COMMAND ${name})
endfunction()

define_test(format_test)
define_test(string_view_test)
define_test(vector_test)
define_test(bitops_test)
define_test(string_test)
