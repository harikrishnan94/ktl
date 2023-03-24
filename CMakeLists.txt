cmake_minimum_required(VERSION 3.23)

project(
    ktl
    VERSION 0.0.1
    HOMEPAGE_URL "https://github.com/harikrishnan94/ktl"
    DESCRIPTION "Minimal and Safe C++ standard library usable for Kernel and other bare metal developments"
    LANGUAGES CXX ASM)

find_program(CCACHE_PROGRAM ccache)

if(CCACHE_PROGRAM)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
endif()

add_subdirectory(src)

option(KTL_ENABLE_TESTS "Enable KTL tests" OFF)

if(KTL_ENABLE_TESTS)
    enable_testing()
    add_subdirectory(test)
endif()
