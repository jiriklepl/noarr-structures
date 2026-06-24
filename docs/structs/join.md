# join

Replace two [dimensions](../Glossary.md#dimension) of the same length with one dimension whose index is used for both original dimensions.

```hpp
#include <noarr/structures_extended.hpp>

template<typename T, auto DimA, auto DimB, auto Dim>
struct noarr::join_t;

template<auto DimA, auto DimB, auto Dim = DimA>
constexpr proto noarr::join();
```

(`proto` is an unspecified [proto-structure](../Glossary.md#proto-structure))


## Description

`join_t` removes dimensions `DimA` and `DimB` from the public interface of the wrapped structure and introduces dimension `Dim`.
The index in `Dim` is passed to the wrapped structure as the index in both `DimA` and `DimB`.
The joined dimensions must have the same [length](../Glossary.md#length) kind and cannot be dependent dimensions.

This is different from [`merge_blocks`](merge_blocks.md).
`merge_blocks` combines every pair of indices into a longer product dimension.
`join` keeps only index pairs where both original indices are equal, so it is useful for diagonal-like views and traversals.

Note that the memory layout is not modified -- only the view is changed.


## Usage examples

```cpp
auto matrix = noarr::scalar<float>() ^ noarr::array<'j', 8>() ^ noarr::array<'i', 8>();

// Expose the main diagonal through dimension 'd'.
auto diagonal = matrix ^ noarr::join<'i', 'j', 'd'>();

assert((diagonal | noarr::offset<'d'>(3)) == (matrix | noarr::offset<'i', 'j'>(3, 3)));
```

For example, this can be used to visit the diagonal offsets:

```cpp
for (std::size_t d = 0; d < 8; d++) {
	std::size_t off = diagonal | noarr::offset<'d'>(d);
}
```
