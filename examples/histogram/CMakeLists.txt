cmake_minimum_required(VERSION 3.18)
project(NoarrHistogramExample LANGUAGES CXX)

# Find Noarr as an external dependency
find_package(Noarr CONFIG QUIET)

if(NOT TARGET Noarr::Noarr)
	# Fallback when built inside the source tree or standalone
	if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeLists.txt")
		add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/../.." "${CMAKE_CURRENT_BINARY_DIR}/noarr-build" EXCLUDE_FROM_ALL)
	else()
		include(FetchContent)
		FetchContent_Declare(
			Noarr
			GIT_REPOSITORY https://github.com/ParaCoToUl/noarr-structures.git
			GIT_TAG main
		)
		FetchContent_MakeAvailable(Noarr)
	endif()
endif()

add_executable(histogram histogram.cpp)
target_link_libraries(histogram PRIVATE Noarr::Noarr)

if(MSVC)
	target_compile_options(histogram PRIVATE /W4)
else()
	target_compile_options(histogram PRIVATE -Wall -Wextra -pedantic)
endif()
