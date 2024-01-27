#ifndef NOARR_STRUCTURES_MU_HPP
#define NOARR_STRUCTURES_MU_HPP

#include "contain.hpp"
#include "utility.hpp"
#include "state.hpp"

namespace noarr::mu {

// --------------------------------------------------------------------------------
// DEFINITIONS
// --------------------------------------------------------------------------------

template<class Left, class Right>
struct definition_t;

// empty definition
template<>
struct definition_t<void, void> {
	constexpr auto sub_state([[maybe_unused]] auto sub_structure, IsState auto state) const {
		return state;
	}

	constexpr auto size([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state) const {
		return sub_structure.size(state);
	}

	template<IsDim auto Dim>
	constexpr auto length([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state) const {
		return sub_structure.template length<Dim>(state);
	}

	constexpr auto offset([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state) const {
		return sub_structure.offset(state);
	}
};


template<class T>
struct definable_t {
	template<class U>
	[[nodiscard("creates a definition")]]
	constexpr definition_t<T, U> operator=(U value) const;
};


struct size_t : public definable_t<size_t> {
	using definable_t<size_t>::operator=;

	constexpr auto evaluate(auto sub_structure, IsState auto state) const {
		return sub_structure.size(state);
	}
};

template<class Right>
struct definition_t<size_t, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto sub_state([[maybe_unused]] auto sub_structure, IsState auto state) const {
		return state;
	}

	constexpr auto size(auto sub_structure, IsState auto state) const {
		return base::template get().evaluate(sub_structure, state);
	}

	template<IsDim auto Dim>
	constexpr auto length(auto sub_structure, IsState auto state) const {
		return sub_structure.template length<Dim>(state);
	}

	constexpr auto offset(auto sub_structure, IsState auto state) const {
		return sub_structure.offset(state);
	}
};

struct offset_t : public definable_t<offset_t> {
	using definable_t<offset_t>::operator=;

	constexpr auto evaluate(auto sub_structure, IsState auto state) const {
		return sub_structure.offset(state);
	}
};

template<class Right>
struct definition_t<offset_t, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto sub_state([[maybe_unused]] auto sub_structure, IsState auto state) const {
		return state;
	}

	constexpr auto size(auto sub_structure, IsState auto state) const {
		return sub_structure.size(state);
	}

	template<IsDim auto Dim>
	constexpr auto length(auto sub_structure, IsState auto state) const {
		return sub_structure.template length<Dim>(state);
	}

	constexpr auto offset(auto sub_structure, IsState auto state) const {
		return base::template get().evaluate(sub_structure, state);
	}
};

template<IsDim auto Dim>
struct length_t : public definable_t<length_t<Dim>> {
	using definable_t<length_t<Dim>>::operator=;

	constexpr auto evaluate(auto sub_structure, IsState auto state) const {
		return sub_structure.template length<Dim>(state);
	}
};

template<IsDim auto Dim, class Right>
struct definition_t<length_t<Dim>, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto sub_state([[maybe_unused]] auto sub_structure, IsState auto state) const {
		return state;
	}

	constexpr auto size(auto sub_structure, IsState auto state) const {
		return sub_structure.size(state);
	}

	constexpr auto offset(auto sub_structure, IsState auto state) const {
		return sub_structure.offset(state);
	}

	template<IsDim auto SubDim>
	constexpr auto length(auto sub_structure, IsState auto state) const {
		if constexpr (SubDim == Dim) {
			return base::template get().evaluate(sub_structure, state);
		} else {
			return sub_structure.template length<SubDim>(state);
		}
	}
};

constexpr size_t size;
constexpr offset_t offset;
template<IsDim auto Dim> constexpr length_t<Dim> length;

template<IsDim auto Dim>
struct length_in_t : public definable_t<length_in_t<Dim>> {
	using definable_t<length_in_t<Dim>>::operator=;

	constexpr auto evaluate([[maybe_unused]] auto sub_structure, IsState auto state) const {
		if constexpr (state.template contains<noarr::length_in<Dim>>) {
			return state.template get<noarr::length_in<Dim>>();
		} else {
			return sub_structure.template length<Dim>(state);
		}
	}
};
template<IsDim auto Dim>
struct index_in_t : public definable_t<index_in_t<Dim>> {
	using definable_t<index_in_t<Dim>>::operator=;

	constexpr auto evaluate([[maybe_unused]] auto sub_structure, IsState auto state) const {
		if constexpr (state.template contains<noarr::index_in<Dim>>) {
			return state.template get<noarr::index_in<Dim>>();
		} else {
			return sub_structure.template length<Dim>(state);
		}
	}
};

template<IsDim auto Dim, class Right>
struct definition_t<index_in_t<Dim>, Right> : public flexible_contain<Right> {
	using base = flexible_contain<Right>;
	using base::base;

	constexpr auto sub_state(auto sub_structure, IsState auto state) const {
		return state.template with<noarr::index_in<Dim>>(base::template get().evaluate(sub_structure, state));
	}

	constexpr auto size(auto sub_structure, IsState auto state) const {
		return sub_structure.size(state);
	}

	constexpr auto offset(auto sub_structure, IsState auto state) const {
		return sub_structure.offset(state);
	}

	template<IsDim auto SubDim>
	constexpr auto length(auto sub_structure, IsState auto state) const {
		return sub_structure.template length<SubDim>(state);
	}
};

template<IsDim auto Dim> constexpr length_in_t<Dim> length_in;
template<IsDim auto Dim> constexpr index_in_t<Dim> index_in;

template<class T>
struct param_t : public flexible_contain<T>, public definable_t<param_t<T>> {
	using flexible_contain<T>::flexible_contain;

	constexpr auto param() const { return this->template get<0>(); }

	constexpr auto evaluate([[maybe_unused]] auto sub_structure, [[maybe_unused]] IsState auto state) const {
		return param();
	}
};

template<class T>
[[nodiscard("creates a dynamic value")]]
constexpr auto param(T param) {
	return param_t<decltype(param)>(param);
}

template<class T>
struct negate_t : public flexible_contain<T>, public definable_t<negate_t<T>> {
	using base = flexible_contain<T>;
	using base::base;
	using definable_t<negate_t<T>>::operator=;

	constexpr auto param() const { return base::template get<0>(); }

	constexpr auto evaluate(auto sub_structure, IsState auto state) const {
		using namespace constexpr_arithmetic;
		return -param().evaluate(sub_structure, state);
	}
};

template<class Left, class Right>
struct add_t : public flexible_contain<Left, Right>, public definable_t<add_t<Left, Right>> {
	using base = flexible_contain<Left, Right>;
	using base::base;
	using definable_t<add_t<Left, Right>>::operator=;

	constexpr auto left() const { return base::template get<0>(); }
	constexpr auto right() const { return base::template get<1>(); }

	constexpr auto evaluate(auto sub_structure, IsState auto state) const {
		using namespace constexpr_arithmetic;

		return left().evaluate(sub_structure, state) + right().evaluate(sub_structure, state);
	}
};

template<class Left, class Right>
struct subtract_t : public flexible_contain<Left, Right>, public definable_t<subtract_t<Left, Right>> {
	using base = flexible_contain<Left, Right>;
	using base::base;
	using definable_t<subtract_t<Left, Right>>::operator=;

	constexpr auto left() const { return base::template get<0>(); }
	constexpr auto right() const { return base::template get<1>(); }

	constexpr auto evaluate(auto sub_structure, IsState auto state) const {
		using namespace constexpr_arithmetic;

		return left().evaluate(sub_structure, state) - right().evaluate(sub_structure, state);
	}
};

template<class Left, class Right>
struct multiply_t : public flexible_contain<Left, Right>, public definable_t<multiply_t<Left, Right>> {
	using base = flexible_contain<Left, Right>;
	using base::base;
	using definable_t<multiply_t<Left, Right>>::operator=;

	constexpr auto left() const { return base::template get<0>(); }
	constexpr auto right() const { return base::template get<1>(); }

	constexpr auto evaluate(auto sub_structure, IsState auto state) const {
		using namespace constexpr_arithmetic;

		return left().evaluate(sub_structure, state) * right().evaluate(sub_structure, state);
	}
};

template<class Left, class Right>
struct divide_t : public flexible_contain<Left, Right>, public definable_t<divide_t<Left, Right>> {
	using base = flexible_contain<Left, Right>;
	using base::base;
	using definable_t<divide_t<Left, Right>>::operator=;

	constexpr auto left() const { return base::template get<0>(); }
	constexpr auto right() const { return base::template get<1>(); }

	constexpr auto evaluate(auto sub_structure, IsState auto state) const {
		using namespace constexpr_arithmetic;

		return left().evaluate(sub_structure, state) / right().evaluate(sub_structure, state);
	}
};

template<class Left, class Right>
struct modulo_t : public flexible_contain<Left, Right>, public definable_t<modulo_t<Left, Right>> {
	using base = flexible_contain<Left, Right>;
	using base::base;
	using definable_t<modulo_t<Left, Right>>::operator=;

	constexpr auto left() const { return base::template get<0>(); }
	constexpr auto right() const { return base::template get<1>(); }

	constexpr auto evaluate(auto sub_structure, IsState auto state) const {
		using namespace constexpr_arithmetic;

		return left().evaluate(sub_structure, state) % right().evaluate(sub_structure, state);
	}
};

template<class T>
[[nodiscard("creates a negation")]]
constexpr negate_t<T> operator-(T value) {
	return negate_t<T>{value};
}

template<class Left, class Right>
[[nodiscard("creates an addition")]]
constexpr add_t<Left, Right> operator+(Left left, Right right) {
	return add_t<Left, Right>{left, right};
}

template<class Left, class Right>
[[nodiscard("creates a subtraction")]]
constexpr subtract_t<Left, Right> operator-(Left left, Right right) {
	return subtract_t<Left, Right>{left, right};
}

template<class Left, class Right>
[[nodiscard("creates a multiplication")]]
constexpr multiply_t<Left, Right> operator*(Left left, Right right) {
	return multiply_t<Left, Right>{left, right};
}

template<class Left, class Right>
[[nodiscard("creates a division")]]
constexpr divide_t<Left, Right> operator/(Left left, Right right) {
	return divide_t<Left, Right>{left, right};
}

template<class Left, class Right>
[[nodiscard("creates a modulo")]]
constexpr modulo_t<Left, Right> operator%(Left left, Right right) {
	return modulo_t<Left, Right>{left, right};
}


template<class T>
template<class U>
constexpr definition_t<T, U> definable_t<T>::operator=(U value) const {
	return definition_t<T, U>{value};
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
		return definition_t<void, void>();
	}

	template<std::size_t I>
	constexpr auto get() const {
		static_assert(always_false<mu_t>, "Requested get on an empty mu");
		return definition_t<void, void>();
	}

	[[nodiscard]]
	constexpr auto sub_state(IsState auto state) const {
		return state;
	}

	template<class _Anything = void>
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

	template<class _Anything = void>
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
		return definition().size(sub_structure(), sub_state(state));
	}

	template<IsDim auto Dim>
	[[nodiscard]]
	constexpr auto length(IsState auto state) const {
		return definition().template length<Dim>(sub_structure(), sub_state(state));
	}

	[[nodiscard]]
	constexpr auto offset(IsState auto state) const {
		return definition().offset(sub_structure(), sub_state(state));
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

	constexpr auto size(IsState auto state) const {
		return definition().size(sub_structure(), sub_state(state));
	}

	template<IsDim auto Dim>
	constexpr auto length(IsState auto state) const {
		return definition().template length<Dim>(sub_structure(), sub_state(state));
	}

	constexpr auto offset(IsState auto state) const {
		return definition().offset(sub_structure(), sub_state(state));
	}
};

template<class T, class ...Ts>
[[nodiscard("creates a mu")]]
constexpr mu_t<T, Ts...> mu(T param, Ts... params) {
	return mu_t<T, Ts...>{param, mu(params...)};
}

template<class T>
[[nodiscard("creates a mu")]]
constexpr mu_t<T> mu(T param) {
	return mu_t<T>{param};
}

// uses the greek letter mu (μ) as a symbol for a structure
template<class ...Ts>
[[nodiscard("creates a mu")]]
constexpr mu_t<Ts...> μ(Ts... params) {
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

template<IsDim auto Dim>
constexpr auto vector(auto len) {
	return μ(
		size = param(len) * size,
		offset = index_in<Dim> * size + offset,
		length<Dim> = param(len)
	);
}

template<IsDim auto OldDim, IsDim auto MajorDim, IsDim auto MinorDim = OldDim>
constexpr auto block(auto block_size) {
	return μ(
		index_in<OldDim> = index_in<MajorDim> * param(block_size) + index_in<MinorDim>,
		length<MinorDim> = param(block_size),
		length<MajorDim> = length<OldDim> / param(block_size)
	);
}

// template<IsDim auto Dim>
// constexpr auto fix(auto value) {
// 	return μ(
// 		index_in<Dim> = param(value),
// 		length<Dim> = is_set(legnth<Dim>)
// 	);
// }

} // namespace noarr::mu

inline void test() {
	using noarr::lit;
	using namespace noarr::mu;

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
}

#endif // NOARR_STRUCTURES_MU_HPP
