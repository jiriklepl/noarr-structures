#ifndef NOARR_STRUCTURES_STRUCT_CONCEPTS_HPP
#define NOARR_STRUCTURES_STRUCT_CONCEPTS_HPP

#include "../base/structs_common.hpp"
#include "../base/utility.hpp"
#include "../extra/to_struct.hpp"
#include "../extra/sig_utils.hpp"

namespace noarr {

namespace helpers {

template<class T, auto ...Dims> requires IsDimPack<decltype(Dims)...>
struct has_dims {
	using struct_type = typename to_struct<T>::type;
	using dim_tree = sig_dim_tree<typename struct_type::signature>;
	using restricted = dim_tree_restrict<dim_tree, dim_sequence<Dims...>>;

	static constexpr bool value = (... && dim_tree_contains<Dims, dim_tree>);
};

} // namespace helpers

template<class T, auto ...Dims>
concept HasDims = IsDimPack<decltype(Dims)...> && requires(T) {
	typename to_struct<T>::type;
} && helpers::has_dims<T, Dims...>::value;

} // namespace noarr

#endif // NOARR_STRUCTURES_STRUCT_CONCEPTS_HPP
