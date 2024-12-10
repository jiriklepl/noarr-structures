#ifndef NOARR_STRUCTURES_MU_HPP
#define NOARR_STRUCTURES_MU_HPP

#include <concepts>
#include <functional>
#include <type_traits>

#include "contain.hpp"
#include "utility.hpp"
#include "state.hpp"

// for comparison with traditional noarr structures:
#include "../structs/layouts.hpp"
#include "../structs/scalar.hpp"
#include "../structs/blocks.hpp"
#include "../extra/shortcuts.hpp"
#include "../extra/funcs.hpp"

namespace noarr::mu {

// --------------------------------------------------------------------------------
// DEFINITIONS
// --------------------------------------------------------------------------------

template<class Left, class Right> // TODO: IsDefinable<Left> && IsExpression<Right>
struct definition_t;

// TODO: definition_t for signatures

template<class T>
struct is_definition : std::false_type {};

template<class Left, class T>
struct definition_for : std::false_type {};

template<class Left, class Right>
struct is_definition<definition_t<Left, Right>> : std::true_type {};

template<class Left, class Right>
struct definition_for<Left, definition_t<Left, Right>> : std::true_type {};

template<class T>
constexpr bool is_definition_v = is_definition<T>::value;

template<class Left, class T>
constexpr bool definition_for_v = definition_for<Left, T>::value;

template<class T>
concept IsDefinition = is_definition_v<T>;

template<class T, class Left>
concept DefinitionFor = definition_for_v<Left, T>;

template<class ...Ts>
constexpr auto contain_merge(Ts ...ts) {
	return contain_cat(ts...); // TODO: implement
}

template<std::size_t I, std::size_t ...Is, class Left, class ...Rights>
constexpr auto contain_set_minus_impl(std::index_sequence<I, Is...>, Left left, helpers::contain<Rights...> right) {
	if constexpr ((... || std::is_same_v<std::remove_cvref_t<decltype(left.template get<I>())>, Rights>)) {
		return contain_set_minus_impl(std::index_sequence<Is...>{}, left, right);
	} else {
		return contain_cat(helpers::contain(left.template get<I>()), contain_set_minus_impl(std::index_sequence<Is...>{}, left, right));
	}
}

template<class Left, class ...Rights>
constexpr auto contain_set_minus_impl(std::index_sequence<>, Left, helpers::contain<Rights...>) {
	return helpers::contain<>{};
}

template<class ...Lefts, class ...Rights>
constexpr auto contain_set_minus(helpers::contain<Lefts...> left, helpers::contain<Rights...> right) {
	constexpr auto is = std::index_sequence_for<Lefts...>{};
	return contain_set_minus_impl(is, left, right);
}

template<class Left, class Right> requires IsDefinition<definition_t<Left, Right>>
constexpr auto bound_left(definition_t<Left, Right>) {
	return bound_left(Left{});
};

template<class Left, class Right> requires IsDefinition<definition_t<Left, Right>>
constexpr auto bound_right(definition_t<Left, Right>) {
	return bound_right(Left{});
}

template<class Left, class Right> requires IsDefinition<definition_t<Left, Right>>
constexpr auto free_left(definition_t<Left, Right> t) {
	return free_left(t.template get<0>());
}

template<class Left, class Right> requires IsDefinition<definition_t<Left, Right>>
constexpr auto free_right(definition_t<Left, Right> t) {
	return free_right(t.template get<0>());
}

struct enable_expression_t {};

template<class T>
concept IsExpression = std::is_base_of_v<enable_expression_t, T> && requires(T t) {
	{T::sub_expressions} -> std::convertible_to<std::size_t>;
	std::integral_constant<std::size_t, T::sub_expressions>{};
	[]<std::size_t ...Is>(std::index_sequence<Is...>) {
		(std::declval<T>().template get<Is>(), ..., void());
	}(std::make_index_sequence<T::sub_expressions>{});
};

struct deleted_t : public enable_expression_t, public flexible_contain<> {
	using flexible_contain<>::flexible_contain;
	using flexible_contain<>::get;

	static constexpr std::size_t sub_expressions = 0;

	[[nodiscard("evaluates to a deleted value")]]
	constexpr auto operator()([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state, [[maybe_unused]] IsState auto sub_state) const {
		// static_assert(always_false<deleted_t>, "Requested deleted value");
		return *this;
	}
};

constexpr deleted_t deleted;

enum class propagation_t {
	left,
	right,
};

template<class T>
struct get_propagation {};

template<class T>
constexpr propagation_t get_propagation_v = get_propagation<T>::value;

template<class Left, class Right>
struct get_propagation<definition_t<Left, Right>> {
	static constexpr propagation_t value = Left::propagates;
};

template<class T>
struct definable_t {
	template<class U>
	[[nodiscard("creates a definition")]]
	constexpr definition_t<T, U> operator=(U value) const {
		return definition_t<T, U>{value};
	}
};


template<class T>
concept IsDefinable = std::is_base_of_v<definable_t<T>, T> && std::is_trivially_default_constructible_v<T> && requires(T t) {
	{ T::propagates } -> std::convertible_to<propagation_t>;
	{ std::integral_constant<propagation_t, T::propagates>{} } -> std::convertible_to<propagation_t>;
};

template<class T> requires IsDefinable<T>
struct get_propagation<T> {
	static constexpr propagation_t value = T::propagates;
};

template<class T> requires IsDefinable<T> && IsExpression<T>
constexpr auto bound_left(T) {
	if constexpr (T::propagates == propagation_t::left) {
		return helpers::contain<T>{};
	} else {
		return helpers::contain<>{};
	}
}

template<class T> requires IsDefinable<T> && IsExpression<T>
constexpr auto bound_right(T) {
	if constexpr (T::propagates == propagation_t::right) {
		return helpers::contain<T>{};
	} else {
		return helpers::contain<>{};
	}
}

template<class T> requires IsExpression<T>
constexpr auto free_left(T t) {
	return [t]<std::size_t ...Is>(std::index_sequence<Is...>) {
		return contain_merge(free_left(t.template get<Is>())...);
	}(std::make_index_sequence<T::sub_expressions>{});
}

template<class T> requires IsExpression<T>
constexpr auto free_right(T t) {
	return [t]<std::size_t ...Is>(std::index_sequence<Is...>) {
		return contain_merge(free_right(t.template get<Is>())...);
	}(std::make_index_sequence<T::sub_expressions>{});
}

template<class T> requires IsDefinable<T> && IsExpression<T>
constexpr auto free_left(T t) {
	return bound_left(t);
}

template<class T> requires IsDefinable<T> && IsExpression<T>
constexpr auto free_right(T t) {
	return bound_right(t);
}

struct size_t : public definable_t<size_t>, public enable_expression_t, public flexible_contain<> {
	using flexible_contain<>::flexible_contain;
	using flexible_contain<>::get;
	using definable_t<size_t>::operator=;

	static constexpr propagation_t propagates = propagation_t::right;
	static constexpr std::size_t sub_expressions = 0;

	[[nodiscard("evaluates to a size")]]
	constexpr auto operator()(auto sub_structure, [[maybe_unused]] IsState auto state, IsState auto sub_state) const {
		return sub_structure.size(sub_state);
	}
};

static_assert(IsDefinable<size_t>);

template<class Right>
struct definition_t<size_t, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto operator()([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state, IsState auto sub_state) const {
		if constexpr (std::is_same_v<Right, deleted_t>) {
			static_assert(always_false<definition_t>, "Requested deleted size");
			return constexpr_arithmetic::make_const<0>();
		} else {
			return base::template get<>()(sub_structure, state, sub_state);
		}
	}
};

struct offset_t : public definable_t<offset_t>, public enable_expression_t, public flexible_contain<> {
	using flexible_contain<>::flexible_contain;
	using flexible_contain<>::get;
	using definable_t<offset_t>::operator=;

	static constexpr propagation_t propagates = propagation_t::right;
	static constexpr std::size_t sub_expressions = 0;

	[[nodiscard("evaluates to an offset")]]
	constexpr auto operator()(auto sub_structure, [[maybe_unused]] IsState auto state, IsState auto sub_state) const {
		return sub_structure.offset(sub_state);
	}
};

static_assert(IsDefinable<offset_t>);

template<class Right>
struct definition_t<offset_t, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto operator()([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state, IsState auto sub_state) const {
		if constexpr (std::is_same_v<Right, deleted_t>) {
			static_assert(always_false<definition_t>, "Requested deleted offset");
			return constexpr_arithmetic::make_const<0>();
		} else {
			return base::template get<>()(sub_structure, state, sub_state);
		}
	}
};

template<IsDim auto Dim>
struct length_t : public definable_t<length_t<Dim>>, public enable_expression_t, public flexible_contain<> {
	using flexible_contain<>::flexible_contain;
	using flexible_contain<>::get;
	using definable_t<length_t<Dim>>::operator=;

	static constexpr propagation_t propagates = propagation_t::right;
	static constexpr std::size_t sub_expressions = 0;

	[[nodiscard("evaluates to a length")]]
	constexpr auto operator()(auto sub_structure, [[maybe_unused]] IsState auto state, IsState auto sub_state) const {
		return sub_structure.template length<Dim>(sub_state);
	}
};

static_assert(IsDefinable<length_t<'x'>>);

template<IsDim auto Dim, class Right>
struct definition_t<length_t<Dim>, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto operator()(auto sub_structure, IsState auto state, IsState auto sub_state) const {
		if constexpr (std::is_same_v<Right, deleted_t>) {
			static_assert(always_false<definition_t>, "Requested deleted length");
			return constexpr_arithmetic::make_const<0>();
		} else {
			return base::template get<>()(sub_structure, state, sub_state);
		}
	}
};

constexpr size_t size;
constexpr offset_t offset;
template<IsDim auto Dim> constexpr length_t<Dim> length;

template<IsDim auto Dim>
struct length_in_t : public definable_t<length_in_t<Dim>>, public enable_expression_t, public flexible_contain<> {
	using flexible_contain<>::flexible_contain;
	using flexible_contain<>::get;
	using definable_t<length_in_t<Dim>>::operator=;

	static constexpr propagation_t propagates = propagation_t::left;
	static constexpr std::size_t sub_expressions = 0;

	[[nodiscard("evaluates to a length")]]
	constexpr auto operator()([[maybe_unused]] auto sub_structure, IsState auto state, [[maybe_unused]] IsState auto sub_state) const {
		if constexpr (state.template contains<noarr::length_in<Dim>>) {
			return state.template get<noarr::length_in<Dim>>();
		} else {
			return deleted;
		}
	}
};

static_assert(IsDefinable<length_in_t<'x'>>);

template<IsDim auto Dim>
struct index_in_t : public definable_t<index_in_t<Dim>>, public enable_expression_t, public flexible_contain<> {
	using flexible_contain<>::flexible_contain;
	using flexible_contain<>::get;
	using definable_t<index_in_t<Dim>>::operator=;

	static constexpr propagation_t propagates = propagation_t::left;
	static constexpr std::size_t sub_expressions = 0;

	[[nodiscard("evaluates to an index")]]
	constexpr auto operator()([[maybe_unused]] auto sub_structure, IsState auto state, [[maybe_unused]] IsState auto sub_state) const {
		if constexpr (state.template contains<noarr::index_in<Dim>>) {
			return state.template get<noarr::index_in<Dim>>();
		} else {
			return deleted;
		}
	}
};

static_assert(IsDefinable<index_in_t<'x'>>);

template<IsDim auto Dim, class Right>
struct definition_t<index_in_t<Dim>, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto sub_state(auto sub_structure, IsState auto state) const {
		return state.template with<noarr::index_in<Dim>>(base::template get<>()(sub_structure, state, state));
	}
};

template<IsDim auto Dim, class Right>
struct definition_t<length_in_t<Dim>, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto sub_state(auto sub_structure, IsState auto state) const {
		return state.template with<noarr::length_in<Dim>>(base::template get<>()(sub_structure, state, state));
	}
};

template<IsDim auto Dim> constexpr length_in_t<Dim> length_in;
template<IsDim auto Dim> constexpr index_in_t<Dim> index_in;

template<class T>
struct param_t : public flexible_contain<T>, public enable_expression_t {
	using base = flexible_contain<T>;
	using base::base;
	using base::get;

	static constexpr std::size_t sub_expressions = 0;

	[[nodiscard("evaluates to a parameter")]]
	constexpr auto operator()([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state, [[maybe_unused]] IsState auto sub_state) const {
		return base::template get<>();
	}
};

template<class T>
[[nodiscard("creates a dynamic value")]]
constexpr auto param(T param) {
	return param_t<decltype(param)>(param);
}

template<class T, class U>
struct unary_op_t : public flexible_contain<T>, public enable_expression_t {
	using base = flexible_contain<T>;
	using base::base;
	using base::get;

	static constexpr std::size_t sub_expressions = 1;

	constexpr auto operator()(auto sub_structure, IsState auto state, IsState auto sub_state) const {
		using param_t = decltype(base::template get<>()(sub_structure, state, sub_state));

		if constexpr (std::is_same_v<param_t, deleted_t>)
			return deleted;
		else
			return U{}(base::template get<>()(sub_structure, state, sub_state));
	}
};

template<class Left, class Right, class Op>
struct binary_op_t : public flexible_contain<Left, Right>, public enable_expression_t {
	using base = flexible_contain<Left, Right>;
	using base::base;
	using base::get;

	static constexpr std::size_t sub_expressions = 2;

	constexpr auto operator()(auto sub_structure, IsState auto state, IsState auto sub_state) const {
		using left_t = decltype(base::template get<0>()(sub_structure, state, sub_state));
		using right_t = decltype(base::template get<1>()(sub_structure, state, sub_state));

		if constexpr (std::is_same_v<left_t, deleted_t> || std::is_same_v<right_t, deleted_t>)
			return deleted;
		else
			return Op{}(base::template get<0>()(sub_structure, state, sub_state),
			            base::template get<1>()(sub_structure, state, sub_state));
	}
};

// arithmetic operations

template<class T>
using negate_t = unary_op_t<T, std::negate<>>;

template<class Left, class Right>
using add_t = binary_op_t<Left, Right, std::plus<>>;

template<class Left, class Right>
using subtract_t = binary_op_t<Left, Right, std::minus<>>;

template<class Left, class Right>
using multiply_t = binary_op_t<Left, Right, std::multiplies<>>;

template<class Left, class Right>
using divide_t = binary_op_t<Left, Right, std::divides<>>;

template<class Left, class Right>
using modulo_t = binary_op_t<Left, Right, std::modulus<>>;

// logical operations

template<class Left, class Right>
using and_t = binary_op_t<Left, Right, std::logical_and<>>;

template<class Left, class Right>
using or_t = binary_op_t<Left, Right, std::logical_or<>>;

template<class T>
using not_t = unary_op_t<T, std::logical_not<>>;

// comparison operations

template<class Left, class Right>
using equal_t = binary_op_t<Left, Right, std::equal_to<>>;

template<class Left, class Right>
using not_equal_t = binary_op_t<Left, Right, std::not_equal_to<>>;

template<class Left, class Right>
using less_t = binary_op_t<Left, Right, std::less<>>;

template<class Left, class Right>
using less_equal_t = binary_op_t<Left, Right, std::less_equal<>>;

template<class Left, class Right>
using greater_t = binary_op_t<Left, Right, std::greater<>>;

template<class Left, class Right>
using greater_equal_t = binary_op_t<Left, Right, std::greater_equal<>>;

// arithmetic operators

template<class T> requires IsExpression<T>
[[nodiscard("creates a negation")]]
constexpr negate_t<T> operator-(T value) {
	return negate_t<T>{value};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates an addition")]]
constexpr add_t<Left, Right> operator+(Left left, Right right) {
	return add_t<Left, Right>{left, right};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a subtraction")]]
constexpr subtract_t<Left, Right> operator-(Left left, Right right) {
	return subtract_t<Left, Right>{left, right};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a multiplication")]]
constexpr multiply_t<Left, Right> operator*(Left left, Right right) {
	return multiply_t<Left, Right>{left, right};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a division")]]
constexpr divide_t<Left, Right> operator/(Left left, Right right) {
	return divide_t<Left, Right>{left, right};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a modulo")]]
constexpr modulo_t<Left, Right> operator%(Left left, Right right) {
	return modulo_t<Left, Right>{left, right};
}

// logical operators

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a logical and")]]
constexpr and_t<Left, Right> operator&&(Left left, Right right) {
	return and_t<Left, Right>{left, right};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a logical or")]]
constexpr or_t<Left, Right> operator||(Left left, Right right) {
	return or_t<Left, Right>{left, right};
}

template<class T> requires IsExpression<T>
[[nodiscard("creates a logical not")]]
constexpr not_t<T> operator!(T value) {
	return not_t<T>{value};
}

// comparison operators

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates an equal comparison")]]
constexpr equal_t<Left, Right> operator==(Left left, Right right) {
	return equal_t<Left, Right>{left, right};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a not equal comparison")]]
constexpr not_equal_t<Left, Right> operator!=(Left left, Right right) {
	return not_equal_t<Left, Right>{left, right};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a less comparison")]]
constexpr less_t<Left, Right> operator<(Left left, Right right) {
	return less_t<Left, Right>{left, right};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a less equal comparison")]]
constexpr less_equal_t<Left, Right> operator<=(Left left, Right right) {
	return less_equal_t<Left, Right>{left, right};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a greater comparison")]]
constexpr greater_t<Left, Right> operator>(Left left, Right right) {
	return greater_t<Left, Right>{left, right};
}

template<class Left, class Right> requires IsExpression<Left> && IsExpression<Right>
[[nodiscard("creates a greater equal comparison")]]
constexpr greater_equal_t<Left, Right> operator>=(Left left, Right right) {
	return greater_equal_t<Left, Right>{left, right};
}

// --------------------------------------------------------------------------------

// TODO: each definition_t throughout the definition_pack_t should be unique
template<class ...Ts> requires (... && IsDefinition<Ts>)
struct definition_pack_t : public flexible_contain<Ts...> {
	using base = flexible_contain<Ts...>;
	using base::base;
	using base::get;

	// TODO: this can be improved; <name>_in_t parameters should not depend on the ones that propagate right
	constexpr auto sub_state(auto sub_structure, IsState auto state) const {
		return sub_state(std::index_sequence_for<Ts...>{}, sub_structure, state);
	}

	constexpr auto size(auto sub_structure, IsState auto state) const requires (... || DefinitionFor<Ts, size_t>) {
		return size(std::index_sequence_for<Ts...>{}, sub_structure, state, sub_state(sub_structure, state));
	}

	constexpr auto size(auto sub_structure, IsState auto state) const {
		return sub_structure.size(sub_state(sub_structure, state));
	}

	template<auto Dim> requires (... || DefinitionFor<Ts, length_t<Dim>>)
	constexpr auto length(auto sub_structure, IsState auto state) const {
		return length<Dim>(std::index_sequence_for<Ts...>{}, sub_structure, state, sub_state(sub_structure, state));
	}

	template<auto Dim>
	constexpr auto length(auto sub_structure, IsState auto state) const {
		return sub_structure.template length<Dim>(sub_state(sub_structure, state));
	}

	constexpr auto offset(auto sub_structure, IsState auto state) const requires (... || DefinitionFor<Ts, offset_t>) {
		return offset(std::index_sequence_for<Ts...>{}, sub_structure, state, sub_state(sub_structure, state));
	}

	constexpr auto offset(auto sub_structure, IsState auto state) const {
		return sub_structure.offset(sub_state(sub_structure, state));
	}

private:
	template<std::size_t I, std::size_t ...Is> requires (get_propagation_v<std::remove_cvref_t<decltype(base::template get<I>())>> == propagation_t::left)
	constexpr auto sub_state([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state) const {
		return sub_state(std::index_sequence<Is...>{}, sub_structure, base::template get<I>().sub_state(sub_structure, state));
	}

	template<std::size_t I, std::size_t ...Is>
	constexpr auto sub_state([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state) const {
		return sub_state(std::index_sequence<Is...>{}, sub_structure, state);
	}

	constexpr auto sub_state([[maybe_unused]] std::index_sequence<> is, [[maybe_unused]] auto sub_structure, IsState auto state) const {
		return state;
	}

	template<std::size_t I, std::size_t ...Is> requires DefinitionFor<std::remove_cvref_t<decltype(std::declval<base>().template get<I>())>, size_t>
	constexpr auto size([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return base::template get<I>()(sub_structure, state, sub_state);
	}

	template<std::size_t I, std::size_t ...Is>
	constexpr auto size([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return size(std::index_sequence<Is...>{}, sub_structure, state, sub_state);
	}

	template<std::size_t I, std::size_t ...Is> requires DefinitionFor<std::remove_cvref_t<decltype(std::declval<base>().template get<I>())>, offset_t>
	constexpr auto offset([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return base::template get<I>()(sub_structure, state, sub_state);
	}

	template<std::size_t I, std::size_t ...Is>
	constexpr auto offset([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return offset(std::index_sequence<Is...>{}, sub_structure, state, sub_state);
	}

	template<auto Dim, std::size_t I, std::size_t ...Is> requires DefinitionFor<std::remove_cvref_t<decltype(std::declval<base>().template get<I>())>, length_t<Dim>>
	constexpr auto length([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return base::template get<I>()(sub_structure, state, sub_state);
	}

	template<auto Dim, std::size_t I, std::size_t ...Is>
	constexpr auto length([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return length<Dim>(std::index_sequence<Is...>{}, sub_structure, state, sub_state);
	}
};

// bound values of a definition pack are the merged bound values of its definitions
template<class ...Ts>
constexpr auto bound_left(definition_pack_t<Ts...> pack) {
	return [pack]<std::size_t ...Is>(std::index_sequence<Is...>) {
		return contain_merge(bound_left(pack.template get<Is>())...);
	}(std::make_index_sequence<sizeof...(Ts)>{});
}

// bound values of a definition pack are the merged bound values of its definitions
template<class ...Ts>
constexpr auto bound_right(definition_pack_t<Ts...> pack) {
	return [pack]<std::size_t ...Is>(std::index_sequence<Is...>) {
		return contain_merge(bound_right(pack.template get<Is>())...);
	}(std::make_index_sequence<sizeof...(Ts)>{});
}

// parameters of a definition pack are the merged parameters of its definitions
template<class ...Ts>
constexpr auto free_left(definition_pack_t<Ts...> pack) {
	return [pack]<std::size_t ...Is>(std::index_sequence<Is...>) {
		return contain_merge(free_left(pack.template get<Is>())...);
	}(std::make_index_sequence<sizeof...(Ts)>{});
}

// parameters of a definition pack are the merged parameters of its definitions
template<class ...Ts>
constexpr auto free_right(definition_pack_t<Ts...> pack) {
	return [pack]<std::size_t ...Is>(std::index_sequence<Is...>) {
		return contain_merge(free_right(pack.template get<Is>())...);
	}(std::make_index_sequence<sizeof...(Ts)>{});
}

// a definition pack consumes all parameters unless they are rebound
template<class ...Ts>
constexpr auto consume_left(definition_pack_t<Ts...> pack) {
	return contain_set_minus(free_left(pack), bound_left(pack));
}

// a definition pack consumes all parameters unless they are rebound
template<class ...Ts>
constexpr auto consume_right(definition_pack_t<Ts...> pack) {
	return contain_set_minus(free_right(pack), bound_right(pack));
}

// --------------------------------------------------------------------------------
// MU
// --------------------------------------------------------------------------------

template<class ...Ts>
struct mu_t;

template<>
struct mu_t<> : public flexible_contain<> {
	using flexible_contain<>::flexible_contain;

	template<class _Anything = void>
	[[nodiscard("creates a mu")]]
	constexpr auto sub_structure() const {
		static_assert(always_false<mu_t>, "Requested sub_structure on an empty mu");
		return mu_t<>{};
	}

	template<class _Anything = void>
	[[nodiscard]]
	constexpr auto definition() const {
		static_assert(always_false<mu_t>, "Requested definition on an empty mu");
		return;
	}

	template<std::size_t I>
	constexpr auto get() const {
		static_assert(always_false<mu_t>, "Requested get on an empty mu");
		return;
	}

	[[nodiscard]]
	constexpr auto sub_state(IsState auto state) const {
		return state;
	}

	[[nodiscard]]
	constexpr auto size([[maybe_unused]] IsState auto state) const {
		static_assert(always_false<mu_t>, "Requested size on an empty mu");
		return constexpr_arithmetic::make_const<0>();
	}

	template<IsDim auto Dim>
	[[nodiscard]]
	constexpr auto length([[maybe_unused]] IsState auto state) const {
		static_assert(always_false<mu_t>, "Requested length on an empty mu");
		return constexpr_arithmetic::make_const<0>();
	}

	[[nodiscard]]
	constexpr auto offset([[maybe_unused]] IsState auto state) const {
		static_assert(always_false<mu_t>, "Requested offset on an empty mu");
		return constexpr_arithmetic::make_const<0>();
	}
};

template<class T>
struct mu_t<T> : public flexible_contain<T> {
	using base = flexible_contain<T>;
	using base::base;

	explicit constexpr mu_t(T param) : base(param) {}

	[[nodiscard("creates a mu")]]
	constexpr auto sub_structure() const { return mu_t<>{}; }

	[[nodiscard("creates a definition")]]
	constexpr auto definition() const { return base::template get<0>(); }

	template<std::size_t I>
	constexpr auto get() const {
		if constexpr (I == 0) {
			return definition();
		} else {
			return sub_structure().template get<I - 1>();
		}
	}

	[[nodiscard]]
	constexpr auto sub_state(IsState auto state) const {
		return definition().sub_state(sub_structure(), state);
	}

	[[nodiscard]]
	constexpr auto size(IsState auto state) const {
		return definition().size(sub_structure(), state);
	}

	template<IsDim auto Dim>
	[[nodiscard]]
	constexpr auto length(IsState auto state) const {
		return definition().template length<Dim>(sub_structure(), state);
	}

	[[nodiscard]]
	constexpr auto offset(IsState auto state) const {
		return definition().offset(sub_structure(), state);
	}
};

template<class T>
constexpr auto free_left(mu_t<T> mu) {
	return free_left(mu.definition());
}

template<class T>
constexpr auto free_right(mu_t<T> mu) {
	return free_right(mu.definition());
}

template<class T>
constexpr auto bound_left(mu_t<T> mu) {
	return bound_left(mu.definition());
}

template<class T>
constexpr auto bound_right(mu_t<T> mu) {
	return bound_right(mu.definition());
}

template<class T>
constexpr auto consume_left(mu_t<T> mu) {
	return consume_left(mu.definition());
}

template<class T>
constexpr auto consume_right(mu_t<T> mu) {
	return consume_right(mu.definition());
}

template<class T, class ...Ts>
struct mu_t<T, Ts...> : public flexible_contain<T, mu_t<Ts...>> {
	using base = flexible_contain<T, mu_t<Ts...>>;
	using base::base;

	explicit constexpr mu_t(T param, Ts ...params) : base{param, mu_t<Ts...>{params...}} {}

	[[nodiscard("creates a mu")]]
	constexpr auto sub_structure() const { return flexible_contain<T, mu_t<Ts...>>::template get<1>(); }

	[[nodiscard("creates a definition")]]
	constexpr auto definition() const { return flexible_contain<T, mu_t<Ts...>>::template get<0>(); }

	template<std::size_t I>
	constexpr auto get() const {
		if constexpr (I == 0) {
			return definition();
		} else {
			return sub_structure().template get<I - 1>();
		}
	}

	constexpr auto sub_state(IsState auto state) const {
		return definition().sub_state(sub_structure(), state);
	}

	constexpr bool has_size(IsState auto state) const {
		return definition().has_size(sub_structure(), state);
	}

	constexpr auto size(IsState auto state) const {
		return definition().size(sub_structure(), state);
	}

	template<IsDim auto Dim>
	constexpr auto length(IsState auto state) const {
		return definition().template length<Dim>(sub_structure(), state);
	}

	constexpr auto offset(IsState auto state) const {
		return definition().offset(sub_structure(), state);
	}
};

// parameters of a composite mu are the parameters of the inner mu except those provided by the outer one extended by the parameters of the outer one
template<class ...Ts> requires (sizeof...(Ts) > 1)
constexpr auto free_left(mu_t<Ts...> mu) {
	// in this case, the sub_structure is the inner mu
	return contain_merge(contain_set_minus(free_left(mu.sub_structure()), bound_left(mu.definition())), free_left(mu.definition()));
}

// parameters of a composite mu are the parameters of the inner mu except those provided by the outer one extended by the parameters of the outer one
template<class ...Ts> requires (sizeof...(Ts) > 1)
constexpr auto free_right(mu_t<Ts...> mu) {
	// in this case, the sub_structure is the outer mu
	return contain_merge(contain_set_minus(free_right(mu.definition()), bound_right(mu.sub_structure())), free_right(mu.sub_structure()));
}

// bound values of a composite mu are the bound values of the outer mu except those consumed by the inner one extended by the bound values of the inner one
template<class ...Ts> requires (sizeof...(Ts) > 1)
constexpr auto bound_left(mu_t<Ts...> mu) {
	// in this case, the sub_structure is the inner mu
	return contain_merge(contain_set_minus(bound_left(mu.definition()), consume_left(mu.sub_structure())), bound_left(mu.sub_structure()));
}

// bound values of a composite mu are the bound values of the outer mu except those consumed by the inner one extended by the bound values of the inner one
template<class ...Ts> requires (sizeof...(Ts) > 1)
constexpr auto bound_right(mu_t<Ts...> mu) {
	// in this case, the sub_structure is the outer mu
	return contain_merge(contain_set_minus(bound_right(mu.sub_structure()), consume_right(mu.definition())), bound_right(mu.sub_structure()));
}

// a composite mu consumes all parameters except those rebound by it
template<class ...Ts> requires (sizeof...(Ts) > 1)
constexpr auto consume_left(mu_t<Ts...> mu) {
	// in this case, the sub_structure is the inner mu
	return contain_set_minus(free_left(mu), bound_left(mu));
}

// a composite mu consumes all parameters except those rebound by it
template<class ...Ts> requires (sizeof...(Ts) > 1)
constexpr auto consume_right(mu_t<Ts...> mu) {
	// in this case, the sub_structure is the outer mu
	return contain_set_minus(free_right(mu), bound_right(mu));
}


template<class ...Ts>
[[nodiscard("creates a mu")]]
constexpr mu_t<definition_pack_t<Ts...>> mu(Ts... params) {
	return mu_t<definition_pack_t<Ts...>>{definition_pack_t<Ts...>{params...}};
}

// uses the greek letter mu (μ) as a symbol for a structure
template<class ...Ts>
[[nodiscard("creates a mu")]]
constexpr mu_t<definition_pack_t<Ts...>> μ(Ts... params) {
	return mu(params...);
}

template<class ...Ts, class ...Us, std::size_t ...Is, std::size_t ...Js>
[[nodiscard("combine mus")]]
constexpr auto combine_mus(mu_t<Ts...> left, mu_t<Us...> right, std::index_sequence<Is...>, std::index_sequence<Js...>) {
	return mu_t<Us..., Ts...>(
		right.template get<Js>()...,
		left.template get<Is>()...
	);
}

template<class ...Ts, class ...Us>
[[nodiscard("combine mus")]]
constexpr auto operator^(mu_t<Ts...> left, mu_t<Us...> right) {
	constexpr auto idxs = std::make_index_sequence<sizeof...(Ts)>{};
	constexpr auto jdxs = std::make_index_sequence<sizeof...(Us)>{};

	return combine_mus(left, right, idxs, jdxs);
}

// --------------------------------------------------------------------------------
// PROTO-STRUCTURES
// --------------------------------------------------------------------------------

template<class T>
constexpr auto scalar() {
	return μ(
		size = param(lit<sizeof(T)>),
		offset = param(constexpr_arithmetic::make_const<0>())
	);
}

// TODO: add signatures

template<IsDim auto Dim>
constexpr auto vector() {
	return μ(
		size = length_in<Dim> * size,
		length<Dim> = length_in<Dim>,
		offset = index_in<Dim> * size + offset
	);
}

template<IsDim auto OldDim, IsDim auto MajorDim, IsDim auto MinorDim = OldDim>
constexpr auto block() {
	return μ(
		index_in<OldDim> = index_in<MajorDim> * length_in<MinorDim> + index_in<MinorDim>,
		length<MinorDim> = length_in<MinorDim>,
		length<MajorDim> = length<OldDim> / length_in<MinorDim>
	);
}

// block_dynamic sets the length of the guard dimension to 1 iff the index is in bounds
template<IsDim auto OldDim, IsDim auto MajorDim, IsDim auto MinorDim, IsDim auto GuardDim>
constexpr auto block_dynamic() {
	return μ(
		index_in<OldDim> = index_in<MajorDim> * length_in<MinorDim> + index_in<MinorDim>,
		length<MinorDim> = length_in<MinorDim>,
		length<MajorDim> = (length<OldDim> + length_in<MinorDim> - param(lit<1>)) / length_in<MinorDim>,
		length<GuardDim> = (index_in<MajorDim> * length_in<MinorDim> + index_in<MinorDim>) < length<OldDim>
	);
}

template<IsDim auto Dim>
constexpr auto bcast() {
	return μ(
		length<Dim> = length_in<Dim>
	);
}

// --------------------------------------------------------------------------------

template<IsDim auto Dim>
constexpr auto shift(auto amount) {
	return μ(
		index_in<Dim> = index_in<Dim> + param(amount),
		length<Dim> = length<Dim> - param(amount)
	);
}

constexpr auto pad(auto before, auto after) {
	return μ(
		size = size + param(before) + param(after),
		offset = offset + param(before));
}

// --------------------------------------------------------------------------------

template<IsDim auto LeftDim, IsDim auto RightDim>
constexpr auto rename() {
	return μ(
		index_in<LeftDim> = index_in<RightDim>,
		length_in<LeftDim> = length_in<RightDim>,
		length<LeftDim> = length<RightDim>,

		index_in<RightDim> = index_in<LeftDim>,
		length_in<RightDim> = length_in<LeftDim>,
		length<RightDim> = length<LeftDim>
	);
}

// --------------------------------------------------------------------------------

template<IsDim auto Dim>
constexpr auto fix_length(auto value) {
	return μ(
		length_in<Dim> = param(value)
	);
}

template<IsDim auto Dim>
constexpr auto fix_index(auto value) {
	return μ(
		index_in<Dim> = param(value),
		length<Dim> = deleted
	);
}

template<IsDim auto Dim>
constexpr auto vector(auto len) {
	return vector<Dim>() ^ fix_length<Dim>(len);
}

template<IsDim auto Dim, std::size_t Len>
constexpr auto array() {
	return vector<Dim>() ^ fix_length<Dim>(lit<Len>);
}

template<IsDim auto OldDim, IsDim auto MajorDim, IsDim auto MinorDim = OldDim>
constexpr auto block(auto block_size) {
	return block<OldDim, MajorDim, MinorDim>() ^ fix_length<MinorDim>(block_size);
}

template<IsDim auto OldDim, IsDim auto MajorDim, IsDim auto MinorDim, IsDim auto GuardDim>
constexpr auto block_dynamic(auto block_size) {
	return block_dynamic<OldDim, MajorDim, MinorDim, GuardDim>() ^ fix_length<MinorDim>(block_size);
}

template<IsDim auto Dim>
constexpr auto bcast(auto len) {
	return bcast<Dim>() ^ fix_length<Dim>(len);
}

} // namespace noarr::mu

inline void test() {
	using noarr::lit;
	using namespace noarr::mu;

	constexpr auto scalar_structure = scalar<int>();

	// expected: 4
	constexpr auto scalar_size = scalar_structure.size(noarr::empty_state);
	static_assert(scalar_size == 4);

	constexpr auto structure = scalar<int>() ^ vector<'x'>(lit<10>) ^ vector<'y'>(lit<20>) ^ vector<'z'>(lit<30>);
	constexpr auto test_associativity = scalar<int>() ^ (vector<'x'>(lit<10>) ^ vector<'y'>(lit<20>) ^ vector<'z'>(lit<30>));
	constexpr auto test_associativity2 = (scalar<int>() ^ vector<'x'>(lit<10>)) ^ (vector<'y'>(lit<20>) ^ vector<'z'>(lit<30>));

	static_assert(std::is_same_v<decltype(structure), decltype(test_associativity)>);
	static_assert(std::is_same_v<decltype(structure), decltype(test_associativity2)>);

	// expected: 4 * 10 * 20 * 30 = 4 * 10 * 600 = 4 * 6000 = 24000
	constexpr auto size = structure.size(noarr::empty_state);
	static_assert(size == 24000);

	constexpr auto state = noarr::empty_state.template with<noarr::index_in<'x'>>(lit<5>).template with<noarr::index_in<'y'>>(lit<10>).template with<noarr::index_in<'z'>>(lit<15>);

	// expected: 4 * (5 + 10 * (10 + 15 * 20)) = 4 * (5 + 10 * (10 + 300)) = 4 * (5 + 10 * 310) = 4 * 3105 = 12420
	constexpr auto offset = structure.offset(state);
	static_assert(offset == 12420);

	// expected: 10
	constexpr auto x_length = structure.template length<'x'>(state);
	static_assert(x_length == 10);

	// expected: 20
	constexpr auto y_length = structure.template length<'y'>(state);
	static_assert(y_length == 20);

	// expected: 30
	constexpr auto z_length = structure.template length<'z'>(state);
	static_assert(z_length == 30);

	constexpr auto structure_renamed = structure ^ rename<'x', 'X'>();

	// expected: 10
	constexpr auto x_length_renamed = structure_renamed.template length<'X'>(state);
	static_assert(x_length_renamed == 10);

	constexpr auto state_renamed = noarr::empty_state.template with<noarr::index_in<'X'>>(lit<5>).template with<noarr::index_in<'y'>>(lit<10>).template with<noarr::index_in<'z'>>(lit<15>);

	// expected: 4 * (5 + 10 * (10 + 15 * 20)) = 4 * (5 + 10 * (10 + 300)) = 4 * (5 + 10 * 310) = 4 * 3105 = 12420
	constexpr auto offset_renamed = structure_renamed.offset(state_renamed);
	static_assert(offset_renamed == 12420);

	// --------------------------------------------------------------------------------

	constexpr auto structure2 = scalar<int>() ^ vector<'x'>(lit<128>) ^ vector<'y'>(lit<128>) ^ block<'x', 'X'>(lit<16>) ^ block<'y', 'Y'>(lit<16>);

	// expected: 4 * 128 * 128 = 4 * 16384 = 65536
	constexpr auto size2 = structure2.size(noarr::empty_state);
	static_assert(size2 == 65536);

	constexpr auto state2 = noarr::empty_state.template with<noarr::index_in<'x'>>(lit<5>).template with<noarr::index_in<'y'>>(lit<10>).template with<noarr::index_in<'X'>>(lit<3>).template with<noarr::index_in<'Y'>>(lit<4>);

	// expected: 4 * ((5 + 16 * 3) + 128 * (10 + 16 * 4)) = 4 * (5 + 48 + 1280 + 128 * 64) = 4 * (1333 + 8192) = 4 * 9525 = 38100
	constexpr auto offset2 = structure2.offset(state2);
	static_assert(offset2 == 38100);

	// expected: 16
	constexpr auto x_length2 = structure2.template length<'x'>(state2);
	static_assert(x_length2 == 16);

	// expected: 16
	constexpr auto y_length2 = structure2.template length<'y'>(state2);
	static_assert(y_length2 == 16);

	// expected: 8
	constexpr auto X_length2 = structure2.template length<'X'>(state2);
	static_assert(X_length2 == 8);

	// expected: 8
	constexpr auto Y_length2 = structure2.template length<'Y'>(state2);
	static_assert(Y_length2 == 8);

	// --------------------------------------------------------------------------------

	constexpr auto structure3 = structure2 ^ fix_index<'x'>(lit<5>) ^ fix_index<'y'>(lit<10>) ^ fix_index<'X'>(lit<3>) ^ fix_index<'Y'>(lit<4>);

	// expected: 4 * ((5 + 16 * 3) + 128 * (10 + 16 * 4)) = 4 * (5 + 48 + 1280 + 128 * 64) = 4 * (1333 + 8192) = 4 * 9525 = 38100
	constexpr auto offset3 = structure3.offset(noarr::empty_state);
	static_assert(offset3 == 38100);

	// // expected: false
	// constexpr auto has_x_length3 = structure3.template has_length<'x'>(noarr::empty_state);
	// static_assert(!has_x_length3);

	// // expected: false
	// constexpr auto has_y_length3 = structure3.template has_length<'y'>(noarr::empty_state);
	// static_assert(!has_y_length3);

	// // expected: false
	// constexpr auto has_X_length3 = structure3.template has_length<'X'>(noarr::empty_state);
	// static_assert(!has_X_length3);

	// // expected: false
	// constexpr auto has_Y_length3 = structure3.template has_length<'Y'>(noarr::empty_state);
	// static_assert(!has_Y_length3);

	// --------------------------------------------------------------------------------

	constexpr auto structure4 = scalar<int>() ^ vector<'x'>(lit<150>) ^ vector<'y'>(lit<150>) ^ block_dynamic<'x', 'X', 'x', 's'>(lit<16>) ^ block_dynamic<'y', 'Y', 'y', 't'>(lit<16>);

	static_assert(structure4.size(noarr::empty_state) == 90000);

	constexpr auto state4 = noarr::empty_state.template with<noarr::index_in<'x'>>(lit<5>).template with<noarr::index_in<'y'>>(lit<10>).template with<noarr::index_in<'X'>>(lit<3>).template with<noarr::index_in<'Y'>>(lit<4>).template with<noarr::index_in<'s'>>(lit<0>).template with<noarr::index_in<'t'>>(lit<0>);

	constexpr auto guard_length_1 = structure4.template length<'s'>(state4);
	constexpr auto guard_length_2 = structure4.template length<'t'>(state4);

	static_assert(guard_length_1 == 1);
	static_assert(guard_length_2 == 1);

	// a state for an out-of-bounds block
	// constexpr auto state5 = noarr::idx<'x', 'X', 'y', 'Y'>(6, 9, 6, 9);
	constexpr auto state5 = noarr::empty_state.template with<noarr::index_in<'x'>>(lit<6>).template with<noarr::index_in<'y'>>(lit<5>).template with<noarr::index_in<'X'>>(lit<9>).template with<noarr::index_in<'Y'>>(lit<9>);

	constexpr auto guard_length_3 = structure4.template length<'s'>(state5);
	constexpr auto guard_length_4 = structure4.template length<'t'>(state5);
	constexpr auto offset5 = structure4.offset(state5);
	constexpr auto size5 = structure4.size(state5);

	static_assert(guard_length_3 == 0);
	static_assert(guard_length_4 == 1);
	static_assert(offset5 == size5);

	constexpr auto mega_structure = scalar<int>() ^
		vector<'a'>(lit<120>) ^ block<'a', 'A'>(lit<10>) ^
		vector<'b'>(lit<120>) ^ block<'b', 'B'>(lit<10>) ^
		vector<'c'>(lit<120>) ^ block<'c', 'C'>(lit<10>) ^
		vector<'d'>(lit<120>) ^ block<'d', 'D'>(lit<10>) ^
		vector<'e'>(lit<120>) ^ block<'e', 'E'>(lit<10>) ^
		vector<'f'>(lit<120>) ^ block<'f', 'F'>(lit<10>) ^
		vector<'g'>(lit<120>) ^ block<'g', 'G'>(lit<10>) ^
		vector<'h'>(lit<120>) ^ block<'h', 'H'>(lit<10>) ^
		vector<'i'>(lit<120>) ^ block<'i', 'I'>(lit<10>) ^
		vector<'j'>(lit<120>) ^ block<'j', 'J'>(lit<10>) ^
		vector<'k'>(lit<120>) ^ block<'k', 'K'>(lit<10>) ^
		vector<'l'>(lit<120>) ^ block<'l', 'L'>(lit<10>) ^
		vector<'m'>(lit<120>) ^ block<'m', 'M'>(lit<10>) ^
		vector<'n'>(lit<120>) ^ block<'n', 'N'>(lit<10>) ^
		vector<'o'>(lit<120>) ^ block<'o', 'O'>(lit<10>) ^
		vector<'p'>(lit<120>) ^ block<'p', 'P'>(lit<10>) ^
		vector<'q'>(lit<120>) ^ block<'q', 'Q'>(lit<10>) ^
		vector<'r'>(lit<120>) ^ block<'r', 'R'>(lit<10>) ^
		vector<'s'>(lit<120>) ^ block<'s', 'S'>(lit<10>) ^
		vector<'t'>(lit<120>) ^ block<'t', 'T'>(lit<10>) ^
		vector<'u'>(lit<120>) ^ block<'u', 'U'>(lit<10>) ^
		vector<'v'>(lit<120>) ^ block<'v', 'V'>(lit<10>) ^
		vector<'w'>(lit<120>) ^ block<'w', 'W'>(lit<10>);

	// traditional noarr structures can fit much more data:
	constexpr auto mega_traditional_structure = noarr::scalar<int>() ^
		noarr::sized_vector<'a'>(lit<120>) ^ noarr::into_blocks<'a', 'A', 'a'>(lit<10>) ^
		noarr::sized_vector<'b'>(lit<120>) ^ noarr::into_blocks<'b', 'B', 'b'>(lit<10>) ^
		noarr::sized_vector<'c'>(lit<120>) ^ noarr::into_blocks<'c', 'C', 'c'>(lit<10>) ^
		noarr::sized_vector<'d'>(lit<120>) ^ noarr::into_blocks<'d', 'D', 'd'>(lit<10>) ^
		noarr::sized_vector<'e'>(lit<120>) ^ noarr::into_blocks<'e', 'E', 'e'>(lit<10>) ^
		noarr::sized_vector<'f'>(lit<120>) ^ noarr::into_blocks<'f', 'F', 'f'>(lit<10>) ^
		noarr::sized_vector<'g'>(lit<120>) ^ noarr::into_blocks<'g', 'G', 'g'>(lit<10>) ^
		noarr::sized_vector<'h'>(lit<120>) ^ noarr::into_blocks<'h', 'H', 'h'>(lit<10>) ^
		noarr::sized_vector<'i'>(lit<120>) ^ noarr::into_blocks<'i', 'I', 'i'>(lit<10>) ^
		noarr::sized_vector<'j'>(lit<120>) ^ noarr::into_blocks<'j', 'J', 'j'>(lit<10>) ^
		noarr::sized_vector<'k'>(lit<120>) ^ noarr::into_blocks<'k', 'K', 'k'>(lit<10>) ^
		noarr::sized_vector<'l'>(lit<120>) ^ noarr::into_blocks<'l', 'L', 'l'>(lit<10>) ^
		noarr::sized_vector<'m'>(lit<120>) ^ noarr::into_blocks<'m', 'M', 'm'>(lit<10>) ^
		noarr::sized_vector<'n'>(lit<120>) ^ noarr::into_blocks<'n', 'N', 'n'>(lit<10>) ^
		noarr::sized_vector<'o'>(lit<120>) ^ noarr::into_blocks<'o', 'O', 'o'>(lit<10>) ^
		noarr::sized_vector<'p'>(lit<120>) ^ noarr::into_blocks<'p', 'P', 'p'>(lit<10>) ^
		noarr::sized_vector<'q'>(lit<120>) ^ noarr::into_blocks<'q', 'Q', 'q'>(lit<10>) ^
		noarr::sized_vector<'r'>(lit<120>) ^ noarr::into_blocks<'r', 'R', 'r'>(lit<10>) ^
		noarr::sized_vector<'s'>(lit<120>) ^ noarr::into_blocks<'s', 'S', 's'>(lit<10>) ^
		noarr::sized_vector<'t'>(lit<120>) ^ noarr::into_blocks<'t', 'T', 't'>(lit<10>) ^
		noarr::sized_vector<'u'>(lit<120>) ^ noarr::into_blocks<'u', 'U', 'u'>(lit<10>) ^
		noarr::sized_vector<'v'>(lit<120>) ^ noarr::into_blocks<'v', 'V', 'v'>(lit<10>) ^
		noarr::sized_vector<'w'>(lit<120>) ^ noarr::into_blocks<'w', 'W', 'w'>(lit<10>) ^
		noarr::sized_vector<'x'>(lit<120>) ^ noarr::into_blocks<'x', 'X', 'x'>(lit<10>) ^
		noarr::sized_vector<'y'>(lit<120>) ^ noarr::into_blocks<'y', 'Y', 'y'>(lit<10>) ^
		noarr::sized_vector<'z'>(lit<120>) ^ noarr::into_blocks<'z', 'Z', 'z'>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'a'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'a'>{}, noarr::dim<'A'>{}, noarr::dim<'a'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'b'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'b'>{}, noarr::dim<'B'>{}, noarr::dim<'b'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'c'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'c'>{}, noarr::dim<'C'>{}, noarr::dim<'c'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'d'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'d'>{}, noarr::dim<'D'>{}, noarr::dim<'d'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'e'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'e'>{}, noarr::dim<'E'>{}, noarr::dim<'e'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'f'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'f'>{}, noarr::dim<'F'>{}, noarr::dim<'f'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'g'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'g'>{}, noarr::dim<'G'>{}, noarr::dim<'g'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'h'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'h'>{}, noarr::dim<'H'>{}, noarr::dim<'h'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'i'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'i'>{}, noarr::dim<'I'>{}, noarr::dim<'i'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'j'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'j'>{}, noarr::dim<'J'>{}, noarr::dim<'j'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'k'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'k'>{}, noarr::dim<'K'>{}, noarr::dim<'k'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'l'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'l'>{}, noarr::dim<'L'>{}, noarr::dim<'l'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'m'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'m'>{}, noarr::dim<'M'>{}, noarr::dim<'m'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'n'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'n'>{}, noarr::dim<'N'>{}, noarr::dim<'n'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'o'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'o'>{}, noarr::dim<'O'>{}, noarr::dim<'o'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'p'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'p'>{}, noarr::dim<'P'>{}, noarr::dim<'p'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'q'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'q'>{}, noarr::dim<'Q'>{}, noarr::dim<'q'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'r'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'r'>{}, noarr::dim<'R'>{}, noarr::dim<'r'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'s'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'s'>{}, noarr::dim<'S'>{}, noarr::dim<'s'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'t'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'t'>{}, noarr::dim<'T'>{}, noarr::dim<'t'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'u'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'u'>{}, noarr::dim<'U'>{}, noarr::dim<'u'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'v'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'v'>{}, noarr::dim<'V'>{}, noarr::dim<'v'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'w'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'w'>{}, noarr::dim<'W'>{}, noarr::dim<'w'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'x'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'x'>{}, noarr::dim<'X'>{}, noarr::dim<'x'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'y'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'y'>{}, noarr::dim<'Y'>{}, noarr::dim<'y'>{}>(lit<10>) ^
		noarr::sized_vector<noarr::dim<'z'>{}>(lit<120>) ^ noarr::into_blocks<noarr::dim<'z'>{}, noarr::dim<'Z'>{}, noarr::dim<'z'>{}>(lit<10>);
}

#endif // NOARR_STRUCTURES_MU_HPP
