# Histogram Example

Demonstrates computing a pixel intensity histogram from a 2D grayscale image using Noarr Structures.

---

## Key Concepts Demonstrated

1. **Multi-Dimensional Grid to 1D Array Mapping**:
   - 2D image layout: dimensions `'x'` (width) and `'y'` (height).
   - 1D histogram layout: dimension `'b'` (bins).

2. **Traverser-Based Reduction**:
   Iterates through all pixels without manual nested coordinate loops:
   ```cpp
   noarr::traverser(image).for_each([&](auto s) {
       auto pixel_val = image[s];
       std::size_t bin = static_cast<std::size_t>(pixel_val) / bin_range;
       hist[noarr::idx<'b'>(bin)]++;
   });
   ```

---

## Building and Running

```bash
cmake -B build -S .
cmake --build build

# Run with default 32x32 image
./build/histogram

# Run with custom image size (e.g. 64x64)
./build/histogram 64
```
