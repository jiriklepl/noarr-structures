cmake_minimum_required(VERSION 3.10)

# set the project name
project(
  NoarrTests VERSION 0.20.0
  DESCRIPTION "Tests for the Noarr library"
  LANGUAGES CXX
)

include(CTest)
include(CheckCXXCompilerFlag)
enable_testing()

# specify the C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# setup the test runner executable
file(GLOB_RECURSE tests_source_files CONFIGURE_DEPENDS
    "*_test.cpp"
)

add_library(noarr_test STATIC noarr-test/src/noarr_test.cpp)
target_include_directories(noarr_test PUBLIC noarr-test/include)

add_executable(test-runner ${tests_source_files})
target_include_directories(test-runner PRIVATE ../include)

target_link_libraries(test-runner PRIVATE noarr_test)

# ask the compiler to print maximum warnings
if(MSVC)
  target_compile_options(test-runner PRIVATE /W4)
else()
  check_cxx_compiler_flag("-Wall" COMPILER_SUPPORTS_WALL)
  check_cxx_compiler_flag("-Wextra" COMPILER_SUPPORTS_WEXTRA)
  check_cxx_compiler_flag("-pedantic" COMPILER_SUPPORTS_PEDANTIC)
  check_cxx_compiler_flag("-Wno-parentheses" COMPILER_SUPPORTS_WNO_PARENTHESES)
  if(COMPILER_SUPPORTS_WALL)
    target_compile_options(test-runner PRIVATE -Wall)
  endif()
  if(COMPILER_SUPPORTS_WEXTRA)
    target_compile_options(test-runner PRIVATE -Wextra)
  endif()
  if(COMPILER_SUPPORTS_PEDANTIC)
    target_compile_options(test-runner PRIVATE -pedantic)
  endif()
  if(COMPILER_SUPPORTS_WNO_PARENTHESES)
    target_compile_options(test-runner PRIVATE -Wno-parentheses)
  endif()
endif()

add_test(NAME test-runner COMMAND test-runner)
