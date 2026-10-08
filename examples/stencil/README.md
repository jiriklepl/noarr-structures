# 2D Stencil Example

Demonstrates a 2D 5-point heat diffusion stencil filter over a 2D grid using Noarr Structures.

---

## Key Concepts Demonstrated

1. **2D Grid Definition**:
   Two grids (current and next generation) modeled with dimensions `'x'` and `'y'`.

2. **Interior Sub-grid Slicing**:
   Slices out the 1-pixel boundary around the grid with zero copies:
   ```cpp
   // defines the 1-pixel boundary slicing
   auto make_interior = noarr::slice<'x'>(1, width - 2)
       ^ noarr::slice<'y'>(1, height - 2);

   // creates views for the interior of both grids
   auto in_view = in.get_ref() ^ make_interior;
   auto out_view = out.get_ref() ^ make_interior;
   ```

3. **Multi-Bag Traverser Iteration**:
   Copying/swapping between grids with joined traversers:
   ```cpp
   noarr::traverser(current, next).for_each([&](auto s) {
       current[s] = next[s];
   });
   ```

---

## Building and Running

```bash
cmake -B build -S .
cmake --build build

# Run with default 7x7 grid for 3 steps
./build/stencil

# Run with custom size and steps (e.g. 10x10, 5 steps)
./build/stencil 10 5
```

