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
	using params = struct_params<
		dim_param<Dim>,
		structure_param<T>>;

	[[nodiscard]]
	constexpr T sub_structure() const noexcept { return strict_contain<T>::get(); }

	template<IsState State>
	[[nodiscard]]
	static constexpr auto sub_state(State state) noexcept { return state.template remove<index_in<Dim>, length_in<Dim>>(); }

	using sub_structure_t = T;
	template<IsState State>
	using sub_state_t = decltype(sub_state(std::declval<State>()));

	static_assert(!T::signature::template any_accept<Dim>, "Dimension name already used");
	using signature = function_sig<Dim, dynamic_arg_length, typename T::signature>;

	template<IsState State>
	[[nodiscard]]
	static constexpr bool has_size() noexcept {
		return State::template contains<length_in<Dim>> &&
			sub_structure_t::template has_size<sub_state_t<State>>();
	}

	template<IsState State>
	[[nodiscard]]
	constexpr auto size(State state) const noexcept
	requires (has_size<State>()) {
		return sub_structure().size(sub_state(state));
	}

	template<IsState State>
	[[nodiscard]]
	constexpr auto align(State state) const noexcept
	requires (has_size<State>()) {
		return sub_structure().align(sub_state(state));
	}

	template<class Sub, IsState State>
	[[nodiscard]]
	static constexpr bool has_strict_offset_of() noexcept {
		if constexpr(State::template contains<index_in<Dim>, length_in<Dim>>) {
			return has_offset_of<Sub, sub_structure_t, sub_state_t<State>>();
		} else {
			return false;
		}
	}

	template<class Sub, IsState State> requires (HasSetIndex<State, Dim>)
	[[nodiscard]]
	constexpr auto strict_offset_of(State state) const noexcept
	requires (has_offset_of<Sub, bcast_t, State>()) {
		return offset_of<Sub>(sub_structure(), sub_state(state));
	}

	template<auto QDim, IsState State> requires IsDim<decltype(QDim)>
	[[nodiscard]]
	static constexpr bool has_length() noexcept {
		if constexpr(QDim == Dim) {
			return State::template contains<length_in<Dim>>;
		} else {
			return sub_structure_t::template has_length<QDim, sub_state_t<State>>();
		}
	}

	template<auto QDim, IsState State> requires (QDim != Dim || HasNotSetIndex<State, QDim>) && IsDim<decltype(QDim)>
	[[nodiscard]]
	constexpr auto length(State state) const noexcept
	requires (has_length<QDim, State>()) {
		if constexpr(QDim == Dim) {
			static_assert(State::template contains<length_in<Dim>>, "This length has not been set yet");
			return state.template get<length_in<Dim>>();
		} else {
			return sub_structure().template length<QDim>(sub_state(state));
		}
	}

	template<class Sub, IsState State>
	[[nodiscard]]
	static constexpr bool has_strict_state_at() noexcept {
		return has_state_at<Sub, sub_structure_t, sub_state_t<State>>();
	}

	template<class Sub, IsState State>
	[[nodiscard]]
	constexpr auto strict_state_at(State state) const noexcept
	requires (has_state_at<Sub, bcast_t, State>()) {
		return state_at<Sub>(sub_structure(), sub_state(state));
	}
};

template<IsDim auto Dim>
struct bcast_proto {
	static constexpr bool proto_preserves_layout = true;

	template<class Struct>
	[[nodiscard]]
	constexpr auto instantiate_and_construct(Struct s) const noexcept { return bcast_t<Dim, Struct>(s); }
};

template<auto ...Dims> requires IsDimPack<decltype(Dims)...>
constexpr auto bcast() noexcept { return (... ^ bcast_proto<Dims>()); }

template<>
constexpr auto bcast<>() noexcept { return neutral_proto(); }

} // namespace noarr

#endif // NOARR_STRUCTURES_BCAST_HPP
