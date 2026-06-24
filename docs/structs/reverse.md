# reverse

Reverse indexing in the specified [dimension](../Glossary.md#dimension).

```hpp
#include <noarr/structures_extended.hpp>

template<auto Dim, typename T>
struct noarr::reverse_t;

template<auto... Dims>
constexpr proto noarr::reverse();
```

(`proto` is an unspecified [proto-structure](../Glossary.md#proto-structure))


## Description

A `reverse_t` structure is similar to its wrapped `T` structure, except that an [index](../Glossary.md#index) in dimension `Dim` is mapped to the opposite end of that dimension.
If the length in `Dim` is `n`, then index `i` is passed to the wrapped structure as `n - 1 - i`.

The length and memory layout are not modified -- only the view is changed.

The `reverse` function can accept a list of dimensions: it will compose multiple `reverse_t`s if necessary.
Applying `reverse` twice in the same dimension restores the original index order.


## Usage examples

```cpp
auto values = noarr::scalar<float>() ^ noarr::vector<'i'>(8);
auto reversed = values ^ noarr::reverse<'i'>();

assert((reversed | noarr::get_length<'i'>()) == 8);
assert((reversed | noarr::offset<'i'>(0)) == (values | noarr::offset<'i'>(7)));
assert((reversed | noarr::offset<'i'>(7)) == (values | noarr::offset<'i'>(0)));
```

It can also be used to traverse a dimension backwards:

```cpp
noarr::traverser(values).order(noarr::reverse<'i'>()).for_each([&](auto state) {
	std::size_t off = values | noarr::offset(state);
});
```
