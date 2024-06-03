cmake_minimum_required(VERSION 3.22)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/build)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(Puzzles CXX)

add_compile_options(-Wall -Wextra -Wpedantic -Werror)

find_package(PkgConfig REQUIRED)
pkg_check_modules(Z3 REQUIRED z3)

add_executable(towers towers.cpp)
target_link_libraries(towers PRIVATE ${Z3_LIBRARIES})
