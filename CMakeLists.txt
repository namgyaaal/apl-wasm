cmake_minimum_required(VERSION 3.10)
project(apl_wasm)

find_package(PkgConfig REQUIRED)

pkg_check_modules(PCRE2 REQUIRED libpcre2-32)

message(${PCRE2_LIBRARIES} ${PCRE2_LIBRARY_DIRS} ${PCRE2_LDFLAGS})

file(GLOB_RECURSE SRC_FILES src/*.hh src/*.cc src/*.icc src/*.h)
add_executable(apl_wasm ${SRC_FILES})
target_include_directories(apl_wasm PRIVATE ${PCRE2_INCLUDE_DIRS})
target_link_libraries(apl_wasm PRIVATE ${PCRE2_LDFLAGS})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src)