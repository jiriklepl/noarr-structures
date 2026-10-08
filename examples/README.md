# Noarr Examples

This directory contains standalone, runnable examples demonstrating the Noarr Structures library using modern C++20 syntax, traversers, and bags.

Each example is structured to consume Noarr as an **external dependency** (using `find_package(Noarr CONFIG QUIET)` / `FetchContent` / in-tree fallback), so they can be built independently or all at once via the master CMakeLists.

---

## Available Examples

| Example | Directory | Description | Key Noarr Features |
| :--- | :--- | :--- | :--- |
| **Matrix** | [matrix/](matrix/) | Layout-agnostic matrix multiplication (GEMM) and transposition | Traverser multi-structure joining (`for_dims`), `rename`, zero-copy views, row-major vs column-major layouts |
| **Histogram** | [histogram/](histogram/) | 2D image pixel intensity histogram calculation | Traversers, 2D to 1D mapping, reductions |
| **Stencil** | [stencil/](stencil/) | 2D 5-point heat diffusion stencil smoothing | Grid structures, `slice` interior views, neighbor stencil access |

---

## Building the Examples

### Option 1: Build All Examples Together

From the repository root:

```bash
# Configure all examples
cmake -B build_examples -S examples

# Build all examples
cmake --build build_examples

# Run individual examples:
./build_examples/matrix/matrix
./build_examples/histogram/histogram
./build_examples/stencil/stencil
```

Or from the root project by enabling the examples option:

```bash
cmake -B build -S . -DNOARR_BUILD_EXAMPLES=ON
cmake --build build
```

---

### Option 2: Build a Single Example Standalone

Every example directory has its own self-contained `CMakeLists.txt`:

```bash
cd examples/matrix
cmake -B build -S .
cmake --build build
./build/matrix rows 6
```
