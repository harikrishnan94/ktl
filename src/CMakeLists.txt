add_library(ktl STATIC builtins.cpp monotonic_buffer_resource.cpp)
add_library(ktl::ktl ALIAS ktl)

option(KTL_ENABLE_UBSAN_MINIMAL "Enable UBSAN minimal runtime" OFF)

if(KTL_ENABLE_UBSAN_MINIMAL)
    target_sources(ktl ubsan.cpp)
endif()

find_package(Boost REQUIRED)

target_include_directories(ktl PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../include)
target_compile_features(ktl PUBLIC cxx_std_20)
target_compile_options(ktl PUBLIC -ffreestanding -mgeneral-regs-only -fPIC
    -fno-exceptions -fno-unwind-tables -fno-rtti
    -fno-threadsafe-statics -fno-builtin)
target_link_options(ktl PUBLIC -nostdlib)
target_link_libraries(ktl PUBLIC Boost::boost)

if((NOT KTL_ENABLE_CHECKS) AND(CMAKE_BUILD_TYPE STREQUAL "Release"))
    target_compile_definitions(ktl PUBLIC DISABLE_CHECKS=1)
endif()

if(CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
    target_compile_definitions(ktl PUBLIC ARCH_IS_X86_64=1)
elseif(CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "arm64")
    target_compile_definitions(ktl PUBLIC ARCH_IS_AARCH64=1)
    target_compile_options(ktl PUBLIC -mno-outline-atomics)
else()
    message(FATAL_ERROR "Unsupported architecture ${CMAKE_SYSTEM_PROCESSOR}")
endif()
