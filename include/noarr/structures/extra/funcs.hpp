#ifndef NOARR_STRUCTURES_FUNCS_HPP
#define NOARR_STRUCTURES_FUNCS_HPP

#include <cstddef>
#include <type_traits>

#include "../base/state.hpp"
#include "../base/structs_common.hpp"
#include "../base/utility.hpp"
#include "../extra/struct_traits.hpp"
#include "../extra/to_struct.hpp"
#include "../structs/scalar.hpp"

namespace noarr {

template<auto Dim, class State>
requires IsDim<decltype(Dim)> && IsState<State>
constexpr auto has_length() noexcept {
	return []<class Struct>(const Struct &/*unused*/) constexpr noexcept { return std::remove_cvref_t<Struct>::template has_length<Dim, State>(); };
}

template<auto Dim, IsState State>
requires IsDim<decltype(Dim)>
constexpr auto get_length(const State &state) noexcept {
	return [state]<class Struct>(Struct &&structure) constexpr noexcept { return std::forward<Struct>(structure).template length<Dim>(state); };
}

/**
 * @brief checks if a structure can be indexed in the dimension specified by the dimension name
 *
 * @tparam Dim: the dimension name of the desired structure
 */
template<auto Dim>
requires IsDim<decltype(Dim)>
constexpr auto has_length() noexcept {
	return has_length<Dim, state<>>();
}

/**
 * @brief returns the number of indices in the structure specified by the dimension name
 *
 * @tparam Dim: the dimension name of the desired structure
 */
template<auto Dim>
requires IsDim<decltype(Dim)>
constexpr auto get_length() noexcept {
	return get_length<Dim, state<>>(empty_state);
}

template<class SubStruct, IsState State>
constexpr auto has_offset() noexcept {
	return []<class Struct>(const Struct &/*unused*/) constexpr noexcept { return has_offset_of<SubStruct, std::remove_cvref_t<Struct>, State>(); };
}

template<class SubStruct, IsState State>
constexpr auto offset(const State &state) noexcept {
	return [state]<class Struct>(Struct &&structure) constexpr noexcept { return offset_of<SubStruct>(std::forward<Struct>(structure), state); };
}

template<IsState State>
constexpr auto has_offset() noexcept {
	return []<class Struct>(const Struct &/*unused*/) constexpr noexcept {
		using struct_t = std::remove_cvref_t<Struct>;
		if constexpr (requires { scalar<scalar_t<struct_t, State>>(); }) {
			using type = scalar_t<struct_t, State>;
			return has_offset_of<scalar<type>, struct_t, State>();
		} else {
			return false;
		}
	};
}

constexpr auto has_offset() noexcept { return has_offset<state<>>(); }

template<class SubStruct, auto... Dims, class... Idxs>
requires IsDimPack<decltype(Dims)...>
constexpr auto offset(Idxs... idxs) noexcept {
	return offset<SubStruct>(empty_state.with<index_in<Dims>...>(idxs...));
}

template<IsState State>
constexpr auto offset(const State &state) noexcept {
	return [state]<class Struct>(Struct &&structure) constexpr noexcept {
		using type = scalar_t<std::remove_cvref_t<Struct>, std::remove_cvref_t<State>>;
		return offset_of<scalar<type>>(std::forward<Struct>(structure), state);
	};
}

template<auto... Dims, class... Idxs>
requires IsDimPack<decltype(Dims)...>
constexpr auto offset(Idxs... idxs) noexcept {
	return offset(empty_state.with<index_in<Dims>...>(idxs...));
}

template<IsState State>
constexpr auto has_size() noexcept {
	return []<class Struct>(const Struct &/*unused*/) constexpr noexcept { return std::remove_cvref_t<Struct>::template has_size<State>(); };
}

template<IsState State>
constexpr auto get_size(const State &state) noexcept {
	return [state]<class Struct>(Struct &&structure) constexpr noexcept { return std::forward<Struct>(structure).size(state); };
}

/**
 * @brief checks if a structure has a size
 */
constexpr auto has_size() noexcept { return has_size<state<>>(); }

/**
 * @brief returns the size (in bytes) of the structure
 */
constexpr auto get_size() noexcept { return get_size(empty_state); }

namespace helpers {

template<class T>
constexpr auto sub_ptr(void *ptr, std::size_t off) noexcept {
	return reinterpret_cast<T *>(reinterpret_cast<char *>(ptr) + off);
}

template<class T>
constexpr auto sub_ptr(const void *ptr, std::size_t off) noexcept {
	return reinterpret_cast<const T *>(reinterpret_cast<const char *>(ptr) + off);
}

template<class T>
constexpr auto sub_ptr(volatile void *ptr, std::size_t off) noexcept {
	return reinterpret_cast<volatile T *>(reinterpret_cast<volatile char *>(ptr) + off);
}

template<class T>
constexpr auto sub_ptr(const volatile void *ptr, std::size_t off) noexcept {
	return reinterpret_cast<const volatile T *>(reinterpret_cast<const volatile char *>(ptr) + off);
}

} // namespace helpers

/**
 * @brief returns the item in the blob specified by `ptr` offset of which is specified by a structure
 *
 * @param ptr: the pointer to blob structure
 */
template<class CvVoid, IsState State>
constexpr auto get_at(CvVoid *ptr, const State &state) noexcept {
	return [ptr, state]<class Struct>(Struct &&structure) constexpr noexcept -> decltype(auto) {
		using type = scalar_t<std::remove_cvref_t<Struct>, std::remove_cvref_t<State>>;
		return *helpers::sub_ptr<type>(ptr, offset_of<scalar<type>>(std::forward<Struct>(structure), state));
	};
}

/**
 * @brief returns the item in the blob specified by `ptr` offset of which is specified by a structure with some fixed
 * indices (see `fix`)
 * @tparam Dims: the dimension names of the fixed dimensions
 * @param ptr: the pointer to blob structure
 */
template<auto... Dims, class... Idxs, class CvVoid>
requires IsDimPack<decltype(Dims)...>
constexpr auto get_at(CvVoid *ptr, Idxs... idxs) noexcept {
	return get_at(ptr, empty_state.with<index_in<Dims>...>(idxs...));
}

/**
 * @brief performs a simple application of `F` to `S`
 *
 * @tparam S: the structure type
 * @tparam F: the function type
 * @param s: the structure
 * @param f: the function
 * @return the result of the piping
 */
template<ToStruct S, class F>
constexpr decltype(auto) operator|(S &&s, F &&f) {
	return std::forward<F>(f)(to_struct<std::remove_cvref_t<S>>::convert(std::forward<S>(s)));
}

} // namespace noarr

#endif // NOARR_STRUCTURES_FUNCS_HPP
