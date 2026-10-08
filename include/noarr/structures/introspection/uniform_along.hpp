#ifndef NOARR_STRUCTURES_UNIFORM_ALONG_HPP
#define NOARR_STRUCTURES_UNIFORM_ALONG_HPP

#include <cstddef>
#include <tuple>
#include <type_traits>

#include "../base/state.hpp"
#include "../base/structs_common.hpp"
#include "../base/utility.hpp"

#include "../structs/bcast.hpp"
#include "../structs/blocks.hpp"
#include "../structs/layouts.hpp"
#include "../structs/scalar.hpp"
#include "../structs/setters.hpp"
#include "../structs/slice.hpp"
#include "../structs/views.hpp"
#include "../structs/zcurve.hpp"

#include "is_static.hpp"

namespace noarr {

namespace helpers {

// implicitly implements specialization for all incorrect structures and unknown layouts
template<IsDim auto QDim, class T, IsState State>
struct is_uniform_along : std::false_type {};

template<IsDim auto QDim, class ValueType, IsState State>
struct is_uniform_along<QDim, scalar<ValueType>, State> : std::true_type {};

template<IsDim auto QDim, class T, IsState State>
struct generic_is_uniform_along {
private:
	using Structure = T;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept { return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value; }

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, IsDim auto Dim, class T, IsState State>
struct is_uniform_along<QDim, bcast_t<Dim, T>, State> {
private:
	using Structure = bcast_t<Dim, T>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (QDim == Dim) {
			return true;
		} else {
			return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, IsDim auto Dim, class T, IsState State>
struct is_uniform_along<QDim, vector_t<Dim, T>, State> {
private:
	using Structure = vector_t<Dim, T>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (QDim == Dim) {
			return true;
		} else {
			return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, IsDim auto Dim, class... Ts, IsState State>
struct is_uniform_along<QDim, tuple_t<Dim, Ts...>, State> {
private:
	using Structure = tuple_t<Dim, Ts...>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (sizeof...(Ts) == 0) {
			return true;
		} else if constexpr (state_contains<State, index_in<Dim>>) {
			using index_t = state_get_t<State, index_in<Dim>>;
			if constexpr (requires {
							  index_t::value;
							  requires (index_t::value < sizeof...(Ts));
						  }) {
				if constexpr (QDim == Dim) {
					return true;
				} else {
					using sub_structure_t = struct_sub_structure_t<Structure, State>;
					return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value;
				}
			} else {
				return false;
			}
		} else {
			using first_t = std::tuple_element_t<0, std::tuple<Ts...>>;
			if constexpr ((std::is_same_v<first_t, Ts> && ...) && helpers::is_static<first_t, sub_state_t>::value) {
				if constexpr (QDim == Dim) {
					return true;
				} else {
					return is_uniform_along<QDim, first_t, sub_state_t>::value;
				}
			} else {
				return false;
			}
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, IsDim auto Dim, class T, class IdxT, IsState State>
struct is_uniform_along<QDim, fix_t<Dim, T, IdxT>, State> {
private:
	using Structure = fix_t<Dim, T, IdxT>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (QDim == Dim) {
			return true;
		} else {
			return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, IsDim auto Dim, class T, class LenT, IsState State>
struct is_uniform_along<QDim, set_length_t<Dim, T, LenT>, State>
    : generic_is_uniform_along<QDim, set_length_t<Dim, T, LenT>, State> {};

template<IsDim auto QDim, IsDim auto Dim, class T, IsState State>
struct is_uniform_along<QDim, hoist_t<Dim, T>, State> : generic_is_uniform_along<QDim, hoist_t<Dim, T>, State> {};

template<IsDim auto QDim, class T, auto... Dims, IsState State>
requires IsDimPack<decltype(Dims)...>
struct is_uniform_along<QDim, reorder_t<T, Dims...>, State>
    : generic_is_uniform_along<QDim, reorder_t<T, Dims...>, State> {};

template<IsDim auto QDim, class T, auto... DimPairs, IsState State>
requires IsDimPack<decltype(DimPairs)...> && (sizeof...(DimPairs) % 2 == 0)
struct is_uniform_along<QDim, rename_t<T, DimPairs...>, State> {
private:
	using Structure = rename_t<T, DimPairs...>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr auto QDimNew =
		helpers::rename_dim<QDim, typename Structure::external, typename Structure::internal>::dim;

	static constexpr bool get_value() noexcept {
		if constexpr (Structure::internal::template contains<QDim> && !Structure::external::template contains<QDim>) {
			return true;
		} else {
			return is_uniform_along<QDimNew, sub_structure_t, sub_state_t>::value;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, IsDim auto DimA, IsDim auto DimB, IsDim auto Dim, class T, IsState State>
requires (DimA != DimB)
struct is_uniform_along<QDim, join_t<T, DimA, DimB, Dim>, State> {
private:
	using Structure = join_t<T, DimA, DimB, Dim>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (QDim == Dim) {
			return is_uniform_along<DimA, sub_structure_t, sub_state_t>::value &&
			       is_uniform_along<DimB, sub_structure_t, sub_state_t>::value;
		} else if constexpr (QDim == DimA || QDim == DimB) {
			return true;
		} else {
			return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, IsDim auto Dim, class T, class StartT, IsState State>
struct is_uniform_along<QDim, shift_t<Dim, T, StartT>, State>
    : generic_is_uniform_along<QDim, shift_t<Dim, T, StartT>, State> {};

template<IsDim auto QDim, IsDim auto Dim, class T, class StartT, class LenT, IsState State>
struct is_uniform_along<QDim, slice_t<Dim, T, StartT, LenT>, State>
    : generic_is_uniform_along<QDim, slice_t<Dim, T, StartT, LenT>, State> {};

template<IsDim auto QDim, IsDim auto Dim, class T, class StartT, class EndT, IsState State>
struct is_uniform_along<QDim, span_t<Dim, T, StartT, EndT>, State>
    : generic_is_uniform_along<QDim, span_t<Dim, T, StartT, EndT>, State> {};

template<IsDim auto QDim, IsDim auto Dim, class T, class StartT, class StrideT, IsState State>
struct is_uniform_along<QDim, step_t<Dim, T, StartT, StrideT>, State>
    : generic_is_uniform_along<QDim, step_t<Dim, T, StartT, StrideT>, State> {};

template<IsDim auto QDim, IsDim auto Dim, class T, IsState State>
struct is_uniform_along<QDim, reverse_t<Dim, T>, State> : generic_is_uniform_along<QDim, reverse_t<Dim, T>, State> {};

template<IsDim auto QDim, IsDim auto Dim, IsDim auto DimMajor, IsDim auto DimMinor, class T, IsState State>
requires (DimMajor != DimMinor)
struct is_uniform_along<QDim, into_blocks_t<Dim, DimMajor, DimMinor, T>, State> {
private:
	using Structure = into_blocks_t<Dim, DimMajor, DimMinor, T>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (QDim == DimMinor || QDim == DimMajor) {
			return is_uniform_along<Dim, sub_structure_t, sub_state_t>::value;
		} else if constexpr (QDim == Dim) {
			return true;
		} else {
			return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, IsDim auto Dim, IsDim auto DimIsBorder, IsDim auto DimMajor, IsDim auto DimMinor, class T,
         class MinorLenT, IsState State>
requires (DimIsBorder != DimMajor) && (DimIsBorder != DimMinor) && (DimMajor != DimMinor)
struct is_uniform_along<QDim, into_blocks_static_t<Dim, DimIsBorder, DimMajor, DimMinor, T, MinorLenT>, State> {
private:
	using Structure = into_blocks_static_t<Dim, DimIsBorder, DimMajor, DimMinor, T, MinorLenT>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (QDim == DimMajor || QDim == DimMinor || QDim == DimIsBorder) {
			return is_uniform_along<Dim, sub_structure_t, sub_state_t>::value;
		} else if constexpr (QDim == Dim) {
			return true;
		} else {
			return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, IsDim auto Dim, IsDim auto DimMajor, IsDim auto DimMinor, IsDim auto DimIsPresent, class T,
         IsState State>
requires (DimMajor != DimMinor) && (DimMinor != DimIsPresent) && (DimIsPresent != DimMajor)
struct is_uniform_along<QDim, into_blocks_dynamic_t<Dim, DimMajor, DimMinor, DimIsPresent, T>, State> {
private:
	using Structure = into_blocks_dynamic_t<Dim, DimMajor, DimMinor, DimIsPresent, T>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (QDim == DimMajor || QDim == DimMinor || QDim == DimIsPresent) {
			return is_uniform_along<Dim, sub_structure_t, sub_state_t>::value;
		} else if constexpr (QDim == Dim) {
			return true;
		} else {
			return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, IsDim auto DimMajor, IsDim auto DimMinor, IsDim auto Dim, class T, IsState State>
struct is_uniform_along<QDim, merge_blocks_t<DimMajor, DimMinor, Dim, T>, State> {
private:
	using Structure = merge_blocks_t<DimMajor, DimMinor, Dim, T>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (QDim == Dim) {
			return is_uniform_along<DimMajor, sub_structure_t, sub_state_t>::value &&
			       is_uniform_along<DimMinor, sub_structure_t, sub_state_t>::value;
		} else if constexpr (QDim == DimMajor || QDim == DimMinor) {
			return true;
		} else {
			return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

template<IsDim auto QDim, std::size_t SpecialLevel, std::size_t GeneralLevel, IsDim auto Dim, class T, auto... Dims,
         IsState State>
requires IsDimPack<decltype(Dims)...>
struct is_uniform_along<QDim, merge_zcurve_t<SpecialLevel, GeneralLevel, Dim, T, Dims...>, State> {
private:
	using Structure = merge_zcurve_t<SpecialLevel, GeneralLevel, Dim, T, Dims...>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (QDim == Dim) {
			return (... && is_uniform_along<Dims, sub_structure_t, sub_state_t>::value);
		} else if constexpr ((... || (QDim == Dims))) {
			return true;
		} else {
			return is_uniform_along<QDim, sub_structure_t, sub_state_t>::value;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

} // namespace helpers

/**
 * @brief Checks whether the sub-structures produced along a dimension are uniform.
 *
 * A dimension is uniform if indexing along it yields identical sub-structure layouts
 * for all indices. Inspects sub-structure uniformity, not memory stride (see HasStrideAlong).
 */
template<class T, auto Dim, class State = state<>>
concept IsUniformAlong = requires {
	requires IsStruct<T>;
	requires IsState<State>;
	requires IsDim<decltype(Dim)>;

	requires helpers::is_uniform_along<Dim, T, State>::value;
};

/**
 * @brief Checks whether the sub-structures produced along a dimension are uniform.
 */
template<auto Dim, class Structure, class State = state<>>
constexpr bool is_uniform_along(Structure /*structure*/, State /*state*/ = State{}) noexcept {
	return helpers::is_uniform_along<Dim, Structure, State>::value;
}

} // namespace noarr

#endif // NOARR_STRUCTURES_UNIFORM_ALONG_HPP
