#ifndef NOARR_STRUCTURES_UTILITY_HPP
#define NOARR_STRUCTURES_UTILITY_HPP

#include <cstddef>
#include <type_traits>
#include <utility>

namespace noarr {

namespace helpers {

template<class... Packs>
struct integer_sequence_concat_impl;

template<class T, T... vs1, T... vs2, class...Packs>
struct integer_sequence_concat_impl<std::integer_sequence<T, vs1...>, std::integer_sequence<T, vs2...>, Packs...> {
	using type = typename integer_sequence_concat_impl<std::integer_sequence<T, vs1..., vs2...>, Packs...>::type;
};

template<class T, T... vs1>
struct integer_sequence_concat_impl<std::integer_sequence<T, vs1...>> {
	using type = std::integer_sequence<T, vs1...>;
};

template<class Sep, class... Packs>
struct integer_sequence_concat_sep_impl;

template<class T, T... vs1, T... vs2, T... sep, class...Packs>
struct integer_sequence_concat_sep_impl<std::integer_sequence<T, sep...>, std::integer_sequence<T, vs1...>, std::integer_sequence<T, vs2...>, Packs...> {
	using type = typename integer_sequence_concat_sep_impl<std::integer_sequence<T, sep...>, std::integer_sequence<T, vs1..., vs2...>, Packs...>::type;
};

template<class T, T v1, T v2, T... vs1, T... vs2, T... sep, class...Packs>
struct integer_sequence_concat_sep_impl<std::integer_sequence<T, sep...>, std::integer_sequence<T, v1, vs1...>, std::integer_sequence<T, v2, vs2...>, Packs...> {
	using type = typename integer_sequence_concat_sep_impl<std::integer_sequence<T, sep...>, std::integer_sequence<T, v1, vs1..., sep..., v2, vs2...>, Packs...>::type;
};

template<class T, T... vs1, T... sep>
struct integer_sequence_concat_sep_impl<std::integer_sequence<T, sep...>, std::integer_sequence<T, vs1...>> {
	using type = std::integer_sequence<T, vs1...>;
};

template<class T, T V, class Pack>
struct integer_sequence_contains_impl;

template<class T, T V, class Pack>
static constexpr bool integer_sequence_contains = integer_sequence_contains_impl<T, V, Pack>::value;

template<class T, T V, T... VS>
struct integer_sequence_contains_impl<T, V, std::integer_sequence<T, V, VS...>> : std::true_type {};

template<class T, T V, T v, T... VS> requires (v != V)
struct integer_sequence_contains_impl<T, V, std::integer_sequence<T, v, VS...>> : integer_sequence_contains_impl<T, V, std::integer_sequence<T, VS...>> {};

template<class T, T V>
struct integer_sequence_contains_impl<T, V, std::integer_sequence<T>> : std::false_type {};

template<class In, class Set>
struct integer_sequence_restrict_impl;

template<class T, T v, T ...vs, class Set> requires integer_sequence_contains<T, v, Set>
struct integer_sequence_restrict_impl<std::integer_sequence<T, v, vs...>, Set> {
	using type = typename integer_sequence_concat_impl<std::integer_sequence<T, v>, typename integer_sequence_restrict_impl<std::integer_sequence<T, vs...>, Set>::type>::type;
};

template<class T, T v, T ...vs, class Set> requires (!integer_sequence_contains<T, v, Set>)
struct integer_sequence_restrict_impl<std::integer_sequence<T, v, vs...>, Set> {
	using type = typename integer_sequence_restrict_impl<std::integer_sequence<T, vs...>, Set>::type;
};

template<class T, class Set>
struct integer_sequence_restrict_impl<std::integer_sequence<T>, Set> {
	using type = std::integer_sequence<T>;
};

template<class T, T v, class ...Branches>
struct integer_tree {
	using value_type = T;
	static constexpr value_type root_value = v;
};

template<class T, T v, class Tree>
struct integer_tree_contains_impl : std::false_type {};


template<class T, T v, class Tree>
static constexpr bool integer_tree_contains = helpers::integer_tree_contains_impl<T, v, Tree>::value;

template<class T, T v, T V, class ...Branches>
struct integer_tree_contains_impl<T, v, integer_tree<T, V, Branches...>> {
	using value_type = bool;
	static constexpr bool value = ( ... || integer_tree_contains<T, v, Branches>);
};

template<class T, T v, class ...Branches>
struct integer_tree_contains_impl<T, v, integer_tree<T, v, Branches...>> : std::true_type {};

template<class Tree, class Set>
struct integer_tree_restrict_impl;

template<class T, T v, class Branch, class Set> requires (!integer_sequence_contains<T, v, Set>)
struct integer_tree_restrict_impl<integer_tree<T, v, Branch>, Set> {
	using type = typename integer_tree_restrict_impl<Branch, Set>::type;
};

template<class T, T v, class ...Branches, class Set> requires (integer_sequence_contains<T, v, Set>)
struct integer_tree_restrict_impl<integer_tree<T, v, Branches...>, Set> {
	using type = integer_tree<T, v, typename integer_tree_restrict_impl<Branches, Set>::type...>;
};

template<class T, class Set>
struct integer_tree_restrict_impl<std::integer_sequence<T>, Set>{
	using type = std::integer_sequence<T>;
};

template<class Sequence>
struct integer_tree_from_sequence_impl;

template<class T, T v, T ...vs>
struct integer_tree_from_sequence_impl<std::integer_sequence<T, v, vs...>> {
	using type = integer_tree<T, v, typename integer_tree_from_sequence_impl<std::integer_sequence<T, vs...>>::type>;
};

template<class T>
struct integer_tree_from_sequence_impl<std::integer_sequence<T>> {
	using type = std::integer_sequence<T>;
};


} // namespace helpers

/**
 * @brief concatenates multiple integral `Packs`
 * 
 * @tparam Packs: the input integral packs
 */
template<class... Packs>
using integer_sequence_concat = typename helpers::integer_sequence_concat_impl<Packs...>::type;

/**
 * @brief concatenates multiple integral packs (the 2nd, 3rd etc. member of `Packs`) pasting the 1st member of `Packs` between each consecutive packs
 * 
 * @tparam Packs: the input integral packs, the first one is the separator used when concatenating
 */
template<class... Packs>
using integer_sequence_concat_sep = typename helpers::integer_sequence_concat_sep_impl<Packs...>::type;

template<class In, class Set>
using integer_sequence_restrict = typename helpers::integer_sequence_restrict_impl<In, Set>::type;

using helpers::integer_sequence_contains;

using helpers::integer_tree;

template<class In, class Set>
using integer_tree_restrict = typename helpers::integer_tree_restrict_impl<In, Set>::type;

using helpers::integer_tree_contains;

template<class Seq>
using integer_tree_from_sequence = typename helpers::integer_tree_from_sequence_impl<Seq>::type;

/**
 * @brief an alias for std::integer_sequence<char, ...>
 * 
 * @tparam VS: the contained values
 */
template<char... VS>
using char_sequence = std::integer_sequence<char, VS...>;

template<std::size_t I>
struct lit_t : std::integral_constant<std::size_t, I> {
	auto operator()() = delete; // using `lit<42>()` by mistake should be rejected, not evaluate to dynamic size_t of 42
};

template<std::size_t I>
constexpr lit_t<I> lit;

template<class>
static constexpr bool always_false = false;
template<auto>
static constexpr bool value_always_false = false;

template<class T>
struct some {
	static constexpr bool present = true;
	T value;

	template<class F>
	constexpr some<decltype(std::declval<F>()(std::declval<T>()))> and_then(const F &f) const noexcept {
		return {f(value)};
	}
};

struct none {
	static constexpr bool present = false;

	template<class F>
	constexpr none and_then(const F &) const noexcept {
		return {};
	}
};

template<class T>
concept is_simple = true
	&& std::is_standard_layout_v<T>
	&& (!std::is_empty_v<T> || std::is_trivially_default_constructible_v<T>)
	&& (!std::is_default_constructible_v<T> || std::is_trivially_default_constructible_v<T>)
	&& std::is_trivially_copy_constructible_v<T>
	&& std::is_trivially_move_constructible_v<T>
	&& (std::is_trivially_copy_assignable_v<T> || !std::is_copy_assignable_v<T>)
	&& (std::is_trivially_move_assignable_v<T> || !std::is_move_assignable_v<T>)
	&& std::is_trivially_destructible_v<T>
	;


namespace constexpr_arithmetic {

template<std::size_t N>
using make_const = std::integral_constant<std::size_t, N>;

template<std::size_t A, std::size_t B>
constexpr make_const<A + B> operator+(make_const<A>, make_const<B>) noexcept { return {}; }

template<std::size_t A, std::size_t B>
constexpr make_const<A - B> operator-(make_const<A>, make_const<B>) noexcept { return {}; }

template<std::size_t A, std::size_t B>
constexpr make_const<A * B> operator*(make_const<A>, make_const<B>) noexcept { return {}; }

template<std::size_t A, std::size_t B>
constexpr make_const<A / B> operator/(make_const<A>, make_const<B>) noexcept { return {}; }

template<std::size_t A, std::size_t B>
constexpr make_const<A % B> operator%(make_const<A>, make_const<B>) noexcept { return {}; }

} // namespace constexpr_arithmetic

} // namespace noarr

#endif // NOARR_STRUCTURES_UTILITY_HPP
