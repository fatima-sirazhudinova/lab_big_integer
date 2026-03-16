cmake_minimum_required(VERSION 3.14)
project(BigIntegerLab LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# ──────────────────────────────────────────────
#  Compiler flags: warnings are errors
# ──────────────────────────────────────────────
add_compile_options(-Wall -Wextra -Wpedantic -Werror)

# ──────────────────────────────────────────────
#  Sanitizers (Address + Undefined Behavior)
# ──────────────────────────────────────────────
add_compile_options(-fsanitize=address,undefined -fno-sanitize-recover=all)
add_link_options(-fsanitize=address,undefined)

# ──────────────────────────────────────────────
#  Google Test (fetched automatically)
# ──────────────────────────────────────────────
include(FetchContent)
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG        v1.14.0
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

# ──────────────────────────────────────────────
#  BigInteger library (student implements this)
# ──────────────────────────────────────────────
add_library(big_integer STATIC big_integer.cpp)
target_include_directories(big_integer PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# ──────────────────────────────────────────────
#  Tests
# ──────────────────────────────────────────────
enable_testing()

add_executable(big_integer_tests tests.cpp)
target_link_libraries(big_integer_tests PRIVATE big_integer GTest::gtest_main)

include(GoogleTest)
gtest_discover_tests(big_integer_tests)
