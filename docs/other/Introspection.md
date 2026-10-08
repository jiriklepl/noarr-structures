# Introspection

Introspection tools inspect structural and layout properties of [structures](../Glossary.md#structure),
such as compile-time staticness, memory contiguity, sub-structure uniformity across dimensions, and addressing properties (stride, offset, lower bound).
They are declared in `<noarr/introspection.hpp>`.


## Static Layout

`IsStatic` and `is_static` check whether all dimensions, lengths, and parameters of a structure are known at compile time for a given [state](../Glossary.md#state).

```cpp
auto s1 = noarr::scalar<int>() ^ noarr::array<'x', 10>() ^ noarr::array<'y', 20>();
auto s2 = noarr::scalar<int>() ^ noarr::vector<'x'>() ^ noarr::vector<'y'>();

static_assert(noarr::IsStatic<decltype(s1)>);
static_assert(!noarr::IsStatic<decltype(s2)>);
assert((s1 | noarr::is_static()));
assert(!noarr::is_static(s2));
```


## Contiguity

`IsContiguous` and `is_contiguous` check whether the elements of a structure form a single contiguous block in memory for a given [state](../Glossary.md#state).

```cpp
auto s = noarr::scalar<int>() ^ noarr::vector<'x'>(10) ^ noarr::vector<'y'>(20);

static_assert(noarr::IsContiguous<decltype(s)>);
assert((s | noarr::is_contiguous()));
assert(noarr::is_contiguous(s));
```


## Uniformity along Dimension

`IsUniformAlong` and `is_uniform_along` inspect whether the [sub-structures](../Glossary.md#sub-structure) produced along a [dimension](../Glossary.md#dimension) are uniform.
A dimension is uniform if indexing or slicing along it yields identical sub-structure layouts across all indices (e.g. in a [vector](../structs/vector.md), or a [tuple](../structs/tuple.md) whose elements share the same static layout, as opposed to a heterogeneous or dynamic tuple). Nonexistent or already consumed/fixed dimensions are trivially uniform since slicing along them leaves the structure unchanged.
Note that this inspects sub-structure layout uniformity rather than memory addressing strides.

```cpp
auto s = noarr::scalar<int>() ^ noarr::vector<'x'>(10) ^ noarr::vector<'y'>(20);

static_assert(noarr::IsUniformAlong<decltype(s), 'x'>);
assert(noarr::is_uniform_along<'x'>(s));
```


## Stride along Dimension

`HasStrideAlong` and `stride_along` inspect the reference property: whether addressing elements along a [dimension](../Glossary.md#dimension) advances with a constant byte stride in memory.
When the property holds, `stride_along` returns the stride in bytes.

```cpp
auto s = noarr::scalar<int>() ^ noarr::vector<'y'>(20) ^ noarr::vector<'x'>(10);

static_assert(noarr::HasStrideAlong<decltype(s), 'y'>);
static_assert(noarr::HasStrideAlong<decltype(s), 'x'>);
assert(noarr::stride_along<'y'>(s) == sizeof(int));
assert(noarr::stride_along<'x'>(s) == 20 * sizeof(int));
```


## Offset along Dimension

`HasOffsetAlong` and `offset_along` query the byte [offset](../Glossary.md#offset) of an element along a [dimension](../Glossary.md#dimension) within the structure for a given [state](../Glossary.md#state).

```cpp
auto s = noarr::scalar<int>() ^ noarr::vector<'y'>(20) ^ noarr::vector<'x'>(10);
auto state = noarr::empty_state.template with<noarr::index_in<'x'>, noarr::index_in<'y'>>(1, 2);

static_assert(noarr::HasOffsetAlong<decltype(s), 'x', decltype(state)>);
assert(noarr::offset_along<'x'>(s, state) == 1 * 20 * sizeof(int));
assert(noarr::offset_along<'y'>(s, state) == 2 * sizeof(int));
```


## Lower Bound along Dimension

`HasLowerBoundAlong`, `lower_bound_along`, and `lower_bound_at` query the minimum byte offset (`lower_bound_along`) or canonical index (`lower_bound_at`) along a [dimension](../Glossary.md#dimension).
Overloads accepting index ranges `(structure, state, min, end)` are also available.

```cpp
auto s = noarr::scalar<int>() ^ noarr::vector<'x'>(10);

static_assert(noarr::HasLowerBoundAlong<decltype(s), 'x'>);
assert(noarr::lower_bound_along<'x'>(s) == 0);
assert(noarr::lower_bound_at<'x'>(s) == 0);
```
