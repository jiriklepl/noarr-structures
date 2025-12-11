#ifndef NOARR_STRUCTURES_BCAST_HPP
#define NOARR_STRUCTURES_BCAST_HPP

#include "../base/contain.hpp"
#include "../base/signature.hpp"
#include "../base/state.hpp"
#include "../base/structs_common.hpp"
#include "../base/utility.hpp"

namespace noarr {

template<IsDim auto Dim, class T>
struct bcast_t : strict_contain<T> {
	using strict_contain<T>::strict_contain;

	static constexpr char name[] = "bcast_t";
	using params = struct_params<dim_param<Dim>, structure_param<T>>;

	template<IsState State>
	constexpr decltype(auto) sub_structure(State /*state*/) const noexcept {
		return strict_contain<T>::get();
	}

	[[nodiscard]]
	constexpr decltype(auto) sub_structure() const noexcept {
		return strict_contain<T>::get();
	}

	template<IsState State>
	[[nodiscard]]
	static constexpr auto sub_state(State state) noexcept {
		return state.template remove<index_in<Dim>, length_in<Dim>>();
	}

	template<IsState State>
	[[nodiscard]]
	static constexpr auto clean_state(State state) noexcept {
		return state.template remove<index_in<Dim>, length_in<Dim>>();
	}

	using sub_structure_t = T;
	template<IsState State>
	using sub_state_t = decltype(sub_state(std::declval<State>()));
	template<IsState State>
	using clean_state_t = decltype(clean_state(std::declval<State>()));

	using signature = function_sig<Dim, dynamic_arg_length, typename T::signature>;

	template<IsState State>
	requires (struct_has_size<bcast_t, State>())
	[[nodiscard]]
	constexpr auto align(State state) const noexcept {
		return sub_structure().align(sub_state(state));
	}

	template<auto QDim, IsState State>
	requires IsDim<decltype(QDim)>
	[[nodiscard]]
	static consteval bool has_length() noexcept {
		if constexpr (QDim == Dim) {
			return state_contains<State, length_in<Dim>> && !state_contains<State, index_in<Dim>>;
		} else {
			return struct_has_length<QDim, sub_structure_t, sub_state_t<State>>();
		}
	}

	template<auto QDim, IsState State>
	requires (struct_has_length<QDim, bcast_t, State>())
	[[nodiscard]]
	constexpr auto length(State state) const noexcept {
		if constexpr (QDim == Dim) {
			return state.template get<length_in<Dim>>();
		} else {
			return struct_length<QDim>(sub_structure(), sub_state(state));
		}
	}
};

template<IsDim auto Dim>
struct bcast_proto {
	static constexpr bool proto_preserves_layout = true;

	template<class Struct>
	[[nodiscard]]
	constexpr auto instantiate_and_construct(Struct s) const noexcept {
		return bcast_t<Dim, Struct>(s);
	}
};

template<auto... Dims>
requires IsDimPack<decltype(Dims)...>
constexpr auto bcast() noexcept {
	return (... ^ bcast_proto<Dims>());
}

template<>
constexpr auto bcast<>() noexcept {
	return neutral_proto();
}

} // namespace noarr

#endif // NOARR_STRUCTURES_BCAST_HPP
