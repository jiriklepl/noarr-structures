# Matrix Example

Demonstrates 2D matrix representations, layout-agnostic matrix multiplication, and zero-copy transposition views using Noarr Structures.

---

## Key Concepts Demonstrated

1. **Separation of Layout from Algorithm**:
   Matrices are defined with named dimensions:
   - Rows: dimension `'i'`
   - Columns: dimension `'j'`

   Row-major layout:
   ```cpp
   auto row_major = noarr::scalar<float>()
       ^ noarr::vector<'j'>(cols)
       ^ noarr::vector<'i'>(rows);
   ```

   Column-major layout:
   ```cpp
   auto col_major = noarr::scalar<float>()
       ^ noarr::vector<'i'>(rows)
       ^ noarr::vector<'j'>(cols);
   ```

2. **Dimension Renaming & Multi-Structure Traversers**:
   Matrix multiplication $C = A \cdot B$ contracts the inner dimension ($A_{ik} \cdot B_{kj} \to C_{ij}$). Noarr's `rename` and `traverser` allow expressing this cleanly across any layout combinations:
   ```cpp
   template<class BagA, class BagB, class BagC>
   void matrix_multiply(const BagA& A, const BagB& B, BagC& C) {
       auto A_k = A.get_ref() ^ noarr::rename<'j', 'k'>();
       auto B_k = B.get_ref() ^ noarr::rename<'i', 'k'>();

       noarr::traverser(C).for_each([&](auto state) { C[state] = 0; });

       noarr::traverser(A_k, B_k, C).template for_dims<'i', 'j'>([&](auto inner) {
           inner.for_each([&](auto state) {
               C[state] += A_k[state] * B_k[state];
           });
       });
   }
   ```

3. **Zero-Copy Transposed View**:
   Transposing a matrix without moving or copying elements:
   ```cpp
   auto A_T = A.get_ref() ^ noarr::rename<'i', 'j', 'j', 'i'>();
   ```

---

## Building and Running

```bash
cmake -B build -S .
cmake --build build

# Run with default 4x4 matrices
./build/matrix

# Run with specific layout and size
./build/matrix rows 6
./build/matrix columns 6
```
