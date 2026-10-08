cmake_minimum_required(VERSION 3.18)
project(NoarrExamples LANGUAGES CXX)

include(CTest)

# Find Noarr as an external dependency
find_package(Noarr CONFIG QUIET)

if(NOT TARGET Noarr::Noarr)
	# Fallback when built within the source tree or standalone
	if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../CMakeLists.txt")
		add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/.." "${CMAKE_CURRENT_BINARY_DIR}/noarr-build" EXCLUDE_FROM_ALL)
	else()
		include(FetchContent)
		FetchContent_Declare(
			Noarr
			GIT_REPOSITORY https://github.com/ParaCoToUl/noarr-structures.git
			GIT_TAG devel
		)
		FetchContent_MakeAvailable(Noarr)
	endif()
endif()

add_subdirectory(matrix)
add_subdirectory(histogram)
add_subdirectory(stencil)

if(BUILD_TESTING)
	add_test(NAME matrix COMMAND matrix rows 10)
	add_test(NAME histogram COMMAND histogram 16)
	add_test(NAME stencil COMMAND stencil 5 2)
endif()
