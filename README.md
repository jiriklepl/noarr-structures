[![Ubuntu-22.04](../../actions/workflows/noarr_test_ubuntu_22.yml/badge.svg)](../../actions/workflows/noarr_test_ubuntu_22.yml)
[![Ubuntu-24.04](../../actions/workflows/noarr_test_ubuntu_24.yml/badge.svg)](../../actions/workflows/noarr_test_ubuntu_24.yml)
[![Windows-latest](../../actions/workflows/noarr_test_winl.yml/badge.svg)](../../actions/workflows/noarr_test_winl.yml)
[![macOS-14](../../actions/workflows/noarr_test_macos_14.yml/badge.svg)](../../actions/workflows/noarr_test_macos_14.yml)

> GCC 10-14, Clang 13-18, and MSVC 17 on Ubuntu, macOS, and Windows GitHub runners.

[![Docs check - Ubuntu](../../actions/workflows/noarr_docs_check_ubuntu.yml/badge.svg)](../../actions/workflows/noarr_docs_check_ubuntu.yml)
[![Docs check - Windows](../../actions/workflows/noarr_docs_check_windows.yml/badge.svg)](../../actions/workflows/noarr_docs_check_windows.yml)


# Noarr

Noarr is a header-only library that facilitates the creation of many data structures and provides a layout-agnostic way of accessing stored values.

It is free software and distributed using the MIT [license](LICENSE).

The library consists of two main parts

1. Data layout modeling (Noarr Structures)
2. Flexible traversal abstraction and parallelization (Noarr Traversers)

For more details, see the [documentation](docs/README.md).

## Data layout modeling (Noarr Structures)

The library provides a flexible way of defining data structure layouts from simple building blocks. The resulting structures then offer a layout-agnostic way of accessing the data.


### Building blocks

The following code snippet demonstrates defining a two-dimensional matrix layout using the library:

```cpp
auto row_major_matrix = noarr::scalar<int>() ^
                        noarr::array<'r', ROWS>() ^
                        noarr::array<'c', COLS>();

auto col_major_matrix = noarr::scalar<int>() ^
                        noarr::array<'c', COLS>() ^
                        noarr::array<'r', ROWS>();
```

The `^` operator combines the building blocks into a single structure. The `noarr::scalar<int>()` represents a layout of a single integer. The layout is then extended in two dimensions using `noarr::array`.

We can then pair the layout with data to create a complete data structure:

```cpp
auto matrix = noarr::bag(row_major_matrix);

// `matrix.data()` and `matrix.structure()` return the data and the layout, respectively.
```


### Layout-agnostic data access

We can access the raw data in the matrix using a `noarr::idx` object in the familiar `[]` syntax:

```cpp
matrix[noarr::idx<'r', 'c'>(row, column)] = value;
```

If we define a matrix with a different layout, the same access code will work and will automatically adjust to the new layout:

```cpp
auto matrix = noarr::bag(col_major_matrix);
matrix[noarr::idx<'r', 'c'>(row, column)] = value;
```


## Flexible traversal abstraction and parallelization (Noarr Traversers)

The library provides a way to traverse data structures in a flexible way. The traversers can be used to perform operations on the data in a layout-agnostic way.

The following code snippet demonstrates how to create a traverser that iterates over the values in a matrix in a default traversal order for the given layout and sets them to zero:

```cpp
// Prepare the traverser
auto traverser = noarr::traverser(matrix);

// Use the traverser to iterate over the values
traverser | [&](auto idx) {
    matrix[idx] = 0;
};
```

If we want to iterate over the values in a different order, we can simply modify the traverser:

```cpp
// Prepare the traverser to iterate in a specific order
// - iterate over the rows in the outer loop and columns in the inner
auto traverser = noarr::traverser(matrix) ^ noarr::hoist<'r', 'c'>();

// Use the traverser to iterate over the values
// - the code does not need to change
traverser | [&](auto idx) {
    matrix[idx] = 0;
};
```


## Using the library

Noarr is a header-only C++20 library. To use it in your CMake project, link against the `Noarr::Noarr` interface target.

If integrated using CMake or present on the compiler's include path, the library can be included in your source files as follows:

```cpp
// Include the entire library
#include <noarr/noarr.hpp>

// or specific components:
#include <noarr/structures_extended.hpp>
#include <noarr/structures/interop/bag.hpp>
#include <noarr/structures/extra/traverser.hpp>
```

The library requires C++20 or later and supports `-fno-exceptions` and `-fno-rtti` flags.
For CUDA, the library requires `--expt-relaxed-constexpr` flag.

Noarr along with CUDA-specific components can be included as follows:

```cpp
#include <noarr/noarr_cuda.hpp>
```

To use the CMake integration, you can either install the library on your system, use the `FetchContent` module to download and integrate it automatically, or include it as a submodule or subdirectory in your project.

### Option 1: System-installed package

If Noarr is installed on your system:

```cmake
find_package(Noarr CONFIG REQUIRED)
target_link_libraries(<my-app> PRIVATE Noarr::Noarr)
```

### Option 2: FetchContent

To download and integrate Noarr automatically at configure time:

```cmake
include(FetchContent)
FetchContent_Declare(
    noarr
    GIT_REPOSITORY https://github.com/ParaCoToUl/noarr-structures.git
    GIT_TAG master
)
FetchContent_MakeAvailable(noarr)

target_link_libraries(<my-app> PRIVATE Noarr::Noarr)
```

### Option 3: Subdirectory / Embedded

If Noarr is included as a submodule or subdirectory:

```cmake
add_subdirectory(path/to/noarr-structures)
target_link_libraries(<my-app> PRIVATE Noarr::Noarr)
```


## Publications

The latest publication related to the library:

```bibtex
@article{klepl2024abstractions,
  title={Abstractions for C++ code optimizations in parallel high-performance applications},
  author={Klepl, Ji{\v{r}}{\'\i} and {\v{S}}melko, Adam and Rozsypal, Luk{\'a}{\v{s}} and Kruli{\v{s}}, Martin},
  journal={Parallel Computing},
  pages={103096},
  year={2024},
  publisher={Elsevier}
}
```

Previous publications related to the library:

```bibtex
@inproceedings{klepl2024pure,
  title={Pure C++ Approach to Optimized Parallel Traversal of Regular Data Structures},
  author={Klepl, Ji{\v{r}}{\'\i} and {\v{S}}melko, Adam and Rozsypal, Luk{\'a}{\v{s}} and Kruli{\v{s}}, Martin},
  booktitle={Proceedings of the 15th International Workshop on Programming Models and Applications for Multicores and Manycores},
  pages={42--51},
  year={2024},
  organization={Association for Computing Machinery}
}

@inproceedings{vsmelko2022astute,
  title={Astute Approach to Handling Memory Layouts of Regular Data Structures},
  author={{\v{S}}melko, Adam and Kruli{\v{s}}, Martin and Kratochv{\'\i}l, Miroslav and Klepl, Ji{\v{r}}{\'\i} and Mayer, Ji{\v{r}}{\'\i} and {\v{S}}im{\uu}nek, Petr},
  booktitle={International Conference on Algorithms and Architectures for Parallel Processing},
  pages={507--528},
  year={2022},
  organization={Springer}
}
```


## Examples

Runnable, standalone examples demonstrating modern Noarr features can be found in the [examples](examples) directory:

- [Matrix](examples/matrix): Layout-agnostic matrix multiplication (GEMM) and zero-copy transposition views.
- [Histogram](examples/histogram): 2D image pixel intensity histogram calculation and reduction.
- [Stencil](examples/stencil): 2D 5-point heat diffusion stencil smoothing using subgrid slicing.

Each example is self-contained and demonstrates consuming Noarr as an external dependency.

### Examples tests

[![Noarr examples test ubuntu-22](../../actions/workflows/noarr_examples_test_ubuntu_22.yml/badge.svg)](../../actions/workflows/noarr_examples_test_ubuntu_22.yml) [![Noarr examples test ubuntu-24](../../actions/workflows/noarr_examples_test_ubuntu_24.yml/badge.svg)](../../actions/workflows/noarr_examples_test_ubuntu_24.yml) [![Noarr examples test macOS-14](../../actions/workflows/noarr_examples_test_macos_14.yml/badge.svg)](../../actions/workflows/noarr_examples_test_macos_14.yml)

[![Noarr examples test windows-latest](../../actions/workflows/noarr_examples_test_winl.yml/badge.svg)](../../actions/workflows/noarr_examples_test_winl.yml)


## Running tests

To ensure the library works properly on your system, you can run the tests provided in the `tests` directory (using CMake and CTest):

```sh
# From the root of the repository:

# Enter the `tests` directory
cd tests

# Configure the build environment
cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug

# Build the `test-runner` executable according to the configuration
cmake --build build --config Debug

# Run the tests
ctest --test-dir build -C Debug -V
```

Note that adding `-j<N>` to the build command might speed up the build process by allowing multiple jobs to run in parallel.
