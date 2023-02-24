set(ENTRYPOINT start)
add_library(platform OBJECT platform.cpp)
target_compile_definitions(platform PRIVATE -DENTRYPOINT=${ENTRYPOINT})
target_link_libraries(platform PUBLIC ktl)
target_link_options(platform PUBLIC -e ${ENTRYPOINT})

# Tests - All tests must link against `platform`
# Ex:
add_executable(format_test format_test.cpp)
target_link_libraries(format_test PRIVATE platform)
