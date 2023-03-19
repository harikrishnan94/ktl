include(FetchContent)
FetchContent_Declare(
    etl
    GIT_REPOSITORY https://github.com/ETLCPP/etl.git
    GIT_TAG 38c807401ea886f8d0e35dba7ab785c6ddf31e13 # 20.35.11
)
FetchContent_MakeAvailable(etl)

add_library(ktl STATIC builtins.cpp)
target_include_directories(ktl PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_compile_features(ktl PUBLIC cxx_std_20)
target_compile_definitions(ktl PUBLIC
    ETL_TARGET_DEVICE_GENERIC=1
    ETL_TARGET_OS_NONE=1
    ETL_USING_TYPE_TRAITS_BUILTINS=1)
target_compile_options(ktl PUBLIC -ffreestanding -mgeneral-regs-only -fPIC
    -fno-exceptions -fno-unwind-tables -fno-rtti
    -fno-threadsafe-statics -fno-builtin)
target_link_options(ktl PUBLIC -nostdlib)
target_link_libraries(ktl PUBLIC etl)

if ((NOT KTL_ENABLE_CHECKS) AND (CMAKE_BUILD_TYPE STREQUAL "Release"))
    target_compile_definitions(ktl PUBLIC DISABLE_CHECKS=1)
endif()


if(CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
    target_compile_definitions(ktl PUBLIC ARCH_IS_X86_64=1)
elseif(CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64")
    target_compile_definitions(ktl PUBLIC ARCH_IS_AARCH64=1)
else()
    message(FATAL_ERROR "Unsupported architecture ${CMAKE_SYSTEM_PROCESSOR}")
endif()
