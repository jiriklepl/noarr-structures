cmake_minimum_required(VERSION 3.18)

project(Noarr
    VERSION 0.20.0
    LANGUAGES CXX
    DESCRIPTION "Noarr is a header-only library that facilitates the creation of many data structures and provides a layout-agnostic way of accessing the values stored in them."
)

add_library(Noarr INTERFACE)

target_compile_features(Noarr INTERFACE cxx_std_20 cuda_std_20)
target_compile_options(Noarr INTERFACE
    $<$<COMPILE_LANG_AND_ID:CUDA,NVIDIA>:--expt-relaxed-constexpr>
)

set(NOARR_DIR ${CMAKE_CURRENT_SOURCE_DIR})

target_include_directories(Noarr INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

add_library(Noarr::Noarr ALIAS Noarr)

install(DIRECTORY include/ DESTINATION include)
install(TARGETS Noarr EXPORT NoarrTargets)
install(EXPORT NoarrTargets
    FILE NoarrConfig.cmake
    NAMESPACE Noarr::
    DESTINATION share/cmake/Noarr
)

option(NOARR_BUILD_TESTS "Build Noarr tests" OFF)
if(NOARR_BUILD_TESTS)
    include(CTest)
    enable_testing()
    add_subdirectory(tests)
endif()

option(NOARR_BUILD_EXAMPLES "Build Noarr examples" OFF)
if(NOARR_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()
