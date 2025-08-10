cmake_minimum_required(VERSION 3.10)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(apl_wasm)


set(PCRE2_BUILD_PCRE2_32 ON CACHE BOOL "Build 32 bit PCRE2 library" FORCE)
add_subdirectory(pcre2)

add_compile_options(-w)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmax-errors=5")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fmax-errors=5")

file(GLOB_RECURSE SRC_FILES src/*.hh src/*.cc src/*.icc src/*.h)
add_executable(apl_wasm ${SRC_FILES})

target_include_directories(apl_wasm PRIVATE ${PROJECT_BINARY_DIR}/pcre2/interface)
target_link_libraries(apl_wasm PRIVATE ${PCRE2_LDFLAGS} pcre2-32)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src)
