# span

Restrict the specified [dimension](../Glossary.md#dimension) to a half-open range of indices.

```hpp
#include <noarr/structures_extended.hpp>

template<auto Dim, typename T, typename StartT, typename EndT>
struct noarr::span_t;

template<auto Dim>
constexpr proto noarr::span(auto start, auto end);

template<auto Dim>
constexpr proto noarr::span(auto end);
```

(`proto` is an unspecified [proto-structure](../Glossary.md#proto-structure))


## Description

A `span_t` structure is similar to its wrapped `T` structure, except that it exposes only indices in the range from `start` inclusive to `end` exclusive in dimension `Dim`.
The length of the resulting dimension is `end - start`.
The new [indices](../Glossary.md#index) run from 0 inclusive to that length exclusive, and each new index is shifted by `start` before it is passed to the wrapped structure.

Note that the memory layout is not modified -- only the view is changed.

`span` is close to [`slice`](slice.md), but its second argument is the end index, not the resulting length.
The one-argument overload is equivalent to `span<Dim>(0, end)`.

Both arguments are unsigned integers.
For static lengths and static arguments, `start` must be less than or equal to `end`, and `end` must be less than or equal to the original length.

See the first section of [Dimension Kinds](../DimensionKinds.md) for the allowed types of `start` and `end` (and `StartT` and `EndT` respectively).


## Usage examples

```cpp
auto matrix = noarr::scalar<float>() ^ noarr::vector<'j'>(12) ^ noarr::vector<'i'>(8);

// Get the submatrix consisting of columns with j in {2, 3, 4, 5, 6}
auto submatrix = matrix ^ noarr::span<'j'>(2, 7);

assert((submatrix | noarr::get_length<'j'>()) == 5);
assert((submatrix | noarr::offset<'i', 'j'>(0, 0)) == (matrix | noarr::offset<'i', 'j'>(0, 2)));
```

It can also be used in traversal orders:

```cpp
noarr::traverser(matrix).order(noarr::span<'i'>(2, 6)).for_each([&](auto state) {
	auto i = noarr::get_index<'i'>(state);
	assert(i >= 2 && i < 6);
});
```
