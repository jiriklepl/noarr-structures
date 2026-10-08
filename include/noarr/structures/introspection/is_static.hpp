#ifndef NOARR_STRUCTURES_IS_STATIC_HPP
#define NOARR_STRUCTURES_IS_STATIC_HPP

#include <cstddef>
#include <type_traits>
#include <utility>

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

namespace noarr {

namespace helpers {

// Base template: conservative toward unknown structures
template<class T, IsState State>
struct is_static : std::false_type {};

template<class T, IsState State>
struct generic_is_static {
private:
	using Structure = T;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept { return is_static<sub_structure_t, sub_state_t>::value; }

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// scalar is always static
template<class ValueType, IsState State>
struct is_static<scalar<ValueType>, State> : std::true_type {};

// vector_t is static only if its length is statically fixed in State
template<IsDim auto Dim, class T, IsState State>
struct is_static<vector_t<Dim, T>, State> {
private:
	using Structure = vector_t<Dim, T>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (state_contains<State, length_in<Dim>>) {
			using length_t = state_get_t<State, length_in<Dim>>;
			if constexpr (requires { length_t::value; }) {
				return is_static<sub_structure_t, sub_state_t>::value;
			} else {
				return false;
			}
		} else {
			return false;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// bcast_t is static only if its length is statically fixed in State
template<IsDim auto Dim, class T, IsState State>
struct is_static<bcast_t<Dim, T>, State> {
private:
	using Structure = bcast_t<Dim, T>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (state_contains<State, length_in<Dim>>) {
			using length_t = state_get_t<State, length_in<Dim>>;
			if constexpr (requires { length_t::value; }) {
				return is_static<sub_structure_t, sub_state_t>::value;
			} else {
				return false;
			}
		} else {
			return false;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// tuple_t
template<IsDim auto Dim, class... Ts, IsState State>
struct is_static<tuple_t<Dim, Ts...>, State> {
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
				using sub_structure_t = struct_sub_structure_t<Structure, State>;
				return is_static<sub_structure_t, sub_state_t>::value;
			} else {
				return false;
			}
		} else {
			return (... && is_static<Ts, sub_state_t>::value);
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// fix_t
template<IsDim auto Dim, class T, class IdxT, IsState State>
struct is_static<fix_t<Dim, T, IdxT>, State> {
private:
	using Structure = fix_t<Dim, T, IdxT>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (requires { IdxT::value; }) {
			return is_static<sub_structure_t, sub_state_t>::value;
		} else {
			return false;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// set_length_t
template<IsDim auto Dim, class T, class LenT, IsState State>
struct is_static<set_length_t<Dim, T, LenT>, State> {
private:
	using Structure = set_length_t<Dim, T, LenT>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (requires { LenT::value; }) {
			return is_static<sub_structure_t, sub_state_t>::value;
		} else {
			return false;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// hoist_t
template<IsDim auto Dim, class T, IsState State>
struct is_static<hoist_t<Dim, T>, State> : generic_is_static<hoist_t<Dim, T>, State> {};

// reorder_t
template<class T, auto... Dims, IsState State>
requires IsDimPack<decltype(Dims)...>
struct is_static<reorder_t<T, Dims...>, State> : generic_is_static<reorder_t<T, Dims...>, State> {};

// rename_t
template<class T, auto... DimPairs, IsState State>
requires IsDimPack<decltype(DimPairs)...> && (sizeof...(DimPairs) % 2 == 0)
struct is_static<rename_t<T, DimPairs...>, State> : generic_is_static<rename_t<T, DimPairs...>, State> {};

// join_t
template<IsDim auto DimA, IsDim auto DimB, IsDim auto Dim, class T, IsState State>
requires (DimA != DimB)
struct is_static<join_t<T, DimA, DimB, Dim>, State> : generic_is_static<join_t<T, DimA, DimB, Dim>, State> {};

// shift_t
template<IsDim auto Dim, class T, class StartT, IsState State>
struct is_static<shift_t<Dim, T, StartT>, State> {
private:
	using Structure = shift_t<Dim, T, StartT>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (requires { StartT::value; }) {
			return is_static<sub_structure_t, sub_state_t>::value;
		} else {
			return false;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// slice_t
template<IsDim auto Dim, class T, class StartT, class LenT, IsState State>
struct is_static<slice_t<Dim, T, StartT, LenT>, State> {
private:
	using Structure = slice_t<Dim, T, StartT, LenT>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (requires {
						  StartT::value;
						  LenT::value;
					  }) {
			return is_static<sub_structure_t, sub_state_t>::value;
		} else {
			return false;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// span_t
template<IsDim auto Dim, class T, class StartT, class EndT, IsState State>
struct is_static<span_t<Dim, T, StartT, EndT>, State> {
private:
	using Structure = span_t<Dim, T, StartT, EndT>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (requires {
						  StartT::value;
						  EndT::value;
					  }) {
			return is_static<sub_structure_t, sub_state_t>::value;
		} else {
			return false;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// step_t
template<IsDim auto Dim, class T, class StartT, class StrideT, IsState State>
struct is_static<step_t<Dim, T, StartT, StrideT>, State> {
private:
	using Structure = step_t<Dim, T, StartT, StrideT>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (requires {
						  StartT::value;
						  StrideT::value;
					  }) {
			return is_static<sub_structure_t, sub_state_t>::value;
		} else {
			return false;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// reverse_t
template<IsDim auto Dim, class T, IsState State>
struct is_static<reverse_t<Dim, T>, State> : generic_is_static<reverse_t<Dim, T>, State> {};

// into_blocks_t
template<IsDim auto Dim, IsDim auto DimMajor, IsDim auto DimMinor, class T, IsState State>
requires (DimMajor != DimMinor)
struct is_static<into_blocks_t<Dim, DimMajor, DimMinor, T>, State>
    : generic_is_static<into_blocks_t<Dim, DimMajor, DimMinor, T>, State> {};

// into_blocks_static_t
template<IsDim auto Dim, IsDim auto DimIsBorder, IsDim auto DimMajor, IsDim auto DimMinor, class T, class MinorLenT,
         IsState State>
requires (DimIsBorder != DimMajor) && (DimIsBorder != DimMinor) && (DimMajor != DimMinor)
struct is_static<into_blocks_static_t<Dim, DimIsBorder, DimMajor, DimMinor, T, MinorLenT>, State> {
private:
	using Structure = into_blocks_static_t<Dim, DimIsBorder, DimMajor, DimMinor, T, MinorLenT>;
	using sub_structure_t = struct_sub_structure_t<Structure, State>;
	using sub_state_t = struct_sub_state_t<Structure, State>;

	static constexpr bool get_value() noexcept {
		if constexpr (requires { MinorLenT::value; }) {
			return is_static<sub_structure_t, sub_state_t>::value;
		} else {
			return false;
		}
	}

public:
	using value_type = bool;
	static constexpr bool value = get_value();
};

// into_blocks_dynamic_t is always dynamic
template<IsDim auto Dim, IsDim auto DimMajor, IsDim auto DimMinor, IsDim auto DimIsPresent, class T, IsState State>
requires (DimMajor != DimMinor) && (DimMinor != DimIsPresent) && (DimIsPresent != DimMajor)
struct is_static<into_blocks_dynamic_t<Dim, DimMajor, DimMinor, DimIsPresent, T>, State> : std::false_type {};

// merge_blocks_t
template<IsDim auto DimMajor, IsDim auto DimMinor, IsDim auto Dim, class T, IsState State>
struct is_static<merge_blocks_t<DimMajor, DimMinor, Dim, T>, State>
    : generic_is_static<merge_blocks_t<DimMajor, DimMinor, Dim, T>, State> {};

// merge_zcurve_t
template<std::size_t SpecialLevel, std::size_t GeneralLevel, IsDim auto Dim, class T, auto... Dims, IsState State>
requires IsDimPack<decltype(Dims)...>
struct is_static<merge_zcurve_t<SpecialLevel, GeneralLevel, Dim, T, Dims...>, State>
    : generic_is_static<merge_zcurve_t<SpecialLevel, GeneralLevel, Dim, T, Dims...>, State> {};

} // namespace helpers

/**
 * @brief Checks whether all dimensions and parameters of a structure are known at compile time.
 */
template<class T, class State = state<>>
concept IsStatic = requires {
	requires IsStruct<T>;
	requires IsState<State>;

	requires helpers::is_static<T, State>::value;
};

/**
 * @brief Checks whether all dimensions and parameters of a structure are known at compile time.
 */
template<class T, IsState State = state<>>
constexpr bool is_static() noexcept {
	return helpers::is_static<T, State>::value;
}

/**
 * @brief Creates a pipe-compatible function object to check whether a structure is static.
 */
template<IsState State = state<>>
constexpr auto is_static(State /*unused*/ = State{}) noexcept {
	return []<class Struct>(Struct /*unused*/) constexpr noexcept { return is_static<Struct, State>(); };
}

/**
 * @brief Checks whether all dimensions and parameters of a structure are known at compile time.
 */
template<class T, IsState State = state<>>
constexpr bool is_static(const T & /*unused*/, State /*unused*/ = State{}) noexcept {
	return is_static<T, State>();
}

} // namespace noarr

#endif // NOARR_STRUCTURES_IS_STATIC_HPP
