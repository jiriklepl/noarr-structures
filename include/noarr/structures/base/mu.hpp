#ifndef NOARR_STRUCTURES_MU_HPP
#define NOARR_STRUCTURES_MU_HPP

#include <concepts>
#include <functional>
#include <type_traits>

#include "contain.hpp"
#include "utility.hpp"
#include "state.hpp"

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

template<class Left, class Right> requires IsDefinition<definition_t<Left, Right>>
constexpr auto bound_left(definition_t<Left, Right>) {
	return bound_left(Left{});
};

template<class Left, class Right> requires IsDefinition<definition_t<Left, Right>>
constexpr auto bound_right(definition_t<Left, Right>) {
	return bound_right(Left{});
}

template<class Left, class Right> requires IsDefinition<definition_t<Left, Right>>
constexpr auto free_left(definition_t<Left, Right>) {
	return free_left(std::declval<Right>());
}

template<class Left, class Right> requires IsDefinition<definition_t<Left, Right>>
constexpr auto free_right(definition_t<Left, Right>) {
	return free_right(std::declval<Right>());
}

struct enable_expression_t {};

struct deleted_t : public enable_expression_t {
	[[nodiscard("evaluates to a deleted value")]]
	constexpr auto evaluate([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state, [[maybe_unused]] IsState auto sub_state) const {
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

template<class T> requires IsDefinable<T>
constexpr auto bound_left(T) {
	if constexpr (T::propagates == propagation_t::left) {
		return helpers::contain<T>{};
	} else {
		return helpers::contain<>{};
	}
}

template<class T> requires IsDefinable<T>
constexpr auto bound_right(T) {
	if constexpr (T::propagates == propagation_t::right) {
		return helpers::contain<T>{};
	} else {
		return helpers::contain<>{};
	}
}

template<class T> requires IsDefinable<T>
constexpr auto free_left(T t) {
	return bound_left(t);
}

template<class T> requires IsDefinable<T>
constexpr auto free_right(T t) {
	return bound_right(t);
}

struct size_t : public definable_t<size_t>, public enable_expression_t {
	using definable_t<size_t>::operator=;

	static constexpr propagation_t propagates = propagation_t::right;

	[[nodiscard("evaluates to a size")]]
	constexpr auto evaluate(auto sub_structure, [[maybe_unused]] IsState auto state, IsState auto sub_state) const {
		return sub_structure.size(sub_state);
	}
};

static_assert(IsDefinable<size_t>);

template<class Right>
struct definition_t<size_t, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto evaluate([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state, IsState auto sub_state) const {
		if constexpr (std::is_same_v<Right, deleted_t>) {
			static_assert(always_false<definition_t>, "Requested deleted size");
			return constexpr_arithmetic::make_const<0>();
		} else {
			return base::template get<>().evaluate(sub_structure, state, sub_state);
		}
	}
};

struct offset_t : public definable_t<offset_t>, public enable_expression_t {
	using definable_t<offset_t>::operator=;

	static constexpr propagation_t propagates = propagation_t::right;

	[[nodiscard("evaluates to an offset")]]
	constexpr auto evaluate(auto sub_structure, [[maybe_unused]] IsState auto state, IsState auto sub_state) const {
		return sub_structure.offset(sub_state);
	}
};

static_assert(IsDefinable<offset_t>);

template<class Right>
struct definition_t<offset_t, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto evaluate([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state, IsState auto sub_state) const {
		if constexpr (std::is_same_v<Right, deleted_t>) {
			static_assert(always_false<definition_t>, "Requested deleted offset");
			return constexpr_arithmetic::make_const<0>();
		} else {
			return base::template get<>().evaluate(sub_structure, state, sub_state);
		}
	}
};

template<IsDim auto Dim>
struct length_t : public definable_t<length_t<Dim>>, public enable_expression_t {
	using definable_t<length_t<Dim>>::operator=;

	static constexpr propagation_t propagates = propagation_t::right;

	[[nodiscard("evaluates to a length")]]
	constexpr auto evaluate(auto sub_structure, [[maybe_unused]] IsState auto state, IsState auto sub_state) const {
		return sub_structure.template length<Dim>(sub_state);
	}
};

static_assert(IsDefinable<length_t<'x'>>);

template<IsDim auto Dim, class Right>
struct definition_t<length_t<Dim>, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto evaluate(auto sub_structure, IsState auto state, IsState auto sub_state) const {
		if constexpr (std::is_same_v<Right, deleted_t>) {
			static_assert(always_false<definition_t>, "Requested deleted length");
			return constexpr_arithmetic::make_const<0>();
		} else {
			return base::template get<>().evaluate(sub_structure, state, sub_state);
		}
	}
};

constexpr size_t size;
constexpr offset_t offset;
template<IsDim auto Dim> constexpr length_t<Dim> length;

template<IsDim auto Dim>
struct length_in_t : public definable_t<length_in_t<Dim>>, public enable_expression_t {
	using definable_t<length_in_t<Dim>>::operator=;

	static constexpr propagation_t propagates = propagation_t::left;

	[[nodiscard("evaluates to a length")]]
	constexpr auto evaluate([[maybe_unused]] auto sub_structure, IsState auto state, [[maybe_unused]] IsState auto sub_state) const {
		if constexpr (state.template contains<noarr::length_in<Dim>>) {
			return state.template get<noarr::length_in<Dim>>();
		} else {
			return deleted;
		}
	}
};

static_assert(IsDefinable<length_in_t<'x'>>);

template<IsDim auto Dim>
struct index_in_t : public definable_t<index_in_t<Dim>>, public enable_expression_t {
	using definable_t<index_in_t<Dim>>::operator=;

	static constexpr propagation_t propagates = propagation_t::left;

	[[nodiscard("evaluates to an index")]]
	constexpr auto evaluate([[maybe_unused]] auto sub_structure, IsState auto state, [[maybe_unused]] IsState auto sub_state) const {
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
		return state.template with<noarr::index_in<Dim>>(base::template get<>().evaluate(sub_structure, state, state));
	}
};

template<IsDim auto Dim, class Right>
struct definition_t<length_in_t<Dim>, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto sub_state(auto sub_structure, IsState auto state) const {
		return state.template with<noarr::length_in<Dim>>(base::template get<>().evaluate(sub_structure, state, state));
	}
};

template<IsDim auto Dim> constexpr length_in_t<Dim> length_in;
template<IsDim auto Dim> constexpr index_in_t<Dim> index_in;

template<class T>
struct param_t : public flexible_contain<T>, public enable_expression_t {
	using flexible_contain<T>::flexible_contain;

	constexpr auto param() const { return this->template get<0>(); }

	[[nodiscard("evaluates to a parameter")]]
	constexpr auto evaluate([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state, [[maybe_unused]] IsState auto sub_state) const {
		return param();
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

	constexpr auto param() const { return base::template get<0>(); }

	constexpr auto evaluate(auto sub_structure, IsState auto state, IsState auto sub_state) const {
		using param_t = decltype(param().evaluate(sub_structure, state, sub_state));

		if constexpr (std::is_same_v<param_t, deleted_t>)
			return deleted;
		else
			return U{}(param().evaluate(sub_structure, state, sub_state));
	}
};

template<class T, class U>
constexpr auto bound_left(unary_op_t<T, U> op) {
	return bound_left(op.param());
}

template<class T, class U>
constexpr auto bound_right(unary_op_t<T, U> op) {
	return bound_right(op.param());
}

template<class T, class U>
constexpr auto free_left(unary_op_t<T, U> op) {
	return free_left(op.param());
}

template<class T, class U>
constexpr auto free_right(unary_op_t<T, U> op) {
	return free_right(op.param());
}



template<class Left, class Right, class Op>
struct binary_op_t : public flexible_contain<Left, Right>, public enable_expression_t {
	using base = flexible_contain<Left, Right>;
	using base::base;

	constexpr auto left() const { return base::template get<0>(); }
	constexpr auto right() const { return base::template get<1>(); }

	constexpr auto evaluate(auto sub_structure, IsState auto state, IsState auto sub_state) const {
		using left_t = decltype(left().evaluate(sub_structure, state, sub_state));
		using right_t = decltype(right().evaluate(sub_structure, state, sub_state));

		if constexpr (std::is_same_v<left_t, deleted_t> || std::is_same_v<right_t, deleted_t>)
			return deleted;
		else
			return Op{}(left().evaluate(sub_structure, state, sub_state), right().evaluate(sub_structure, state, sub_state));
	}
};

template<class Left, class Right, class Op>
constexpr auto bound_left(binary_op_t<Left, Right, Op> op) {
	return helpers::contain_cat(bound_left(op.left()), bound_left(op.right()));
}

template<class Left, class Right, class Op>
constexpr auto bound_right(binary_op_t<Left, Right, Op> op) {
	return helpers::contain_cat(bound_right(op.left()), bound_right(op.right()));
}

template<class Left, class Right, class Op>
constexpr auto free_left(binary_op_t<Left, Right, Op> op) {
	return helpers::contain_cat(free_left(op.left()), free_left(op.right()));
}

template<class Left, class Right, class Op>
constexpr auto free_right(binary_op_t<Left, Right, Op> op) {
	return helpers::contain_cat(free_right(op.left()), free_right(op.right()));
}

template<class T>
concept IsExpression = std::is_base_of_v<enable_expression_t, T>;

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
		return base::template get<I>().evaluate(sub_structure, state, sub_state);
	}

	template<std::size_t I, std::size_t ...Is>
	constexpr auto size([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return size(std::index_sequence<Is...>{}, sub_structure, state, sub_state);
	}

	template<std::size_t I, std::size_t ...Is> requires DefinitionFor<std::remove_cvref_t<decltype(std::declval<base>().template get<I>())>, offset_t>
	constexpr auto offset([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return base::template get<I>().evaluate(sub_structure, state, sub_state);
	}

	template<std::size_t I, std::size_t ...Is>
	constexpr auto offset([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return offset(std::index_sequence<Is...>{}, sub_structure, state, sub_state);
	}

	template<auto Dim, std::size_t I, std::size_t ...Is> requires DefinitionFor<std::remove_cvref_t<decltype(std::declval<base>().template get<I>())>, length_t<Dim>>
	constexpr auto length([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return base::template get<I>().evaluate(sub_structure, state, sub_state);
	}

	template<auto Dim, std::size_t I, std::size_t ...Is>
	constexpr auto length([[maybe_unused]] std::index_sequence<I, Is...> is, [[maybe_unused]] auto sub_structure, IsState auto state, IsState auto sub_state) const {
		return length<Dim>(std::index_sequence<Is...>{}, sub_structure, state, sub_state);
	}
};

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

// TODO: make consuming implicit
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

	// TODO: define bound_left, bound_right, free_left, free_right for mu_t
	//         it should honor the implicit consumption
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
}

#endif // NOARR_STRUCTURES_MU_HPP
