#ifndef NOARR_STRUCTURES_SERIALIZE_DATA_HPP
#define NOARR_STRUCTURES_SERIALIZE_DATA_HPP

#include <utility>

#include "../base/state.hpp"
#include "../extra/traverser.hpp"

namespace noarr {

template<class Istream, class Struct>
constexpr decltype(auto) deserialize_data(Istream &&in, Struct s, void *data) {
	traverser(s).for_each([&in, s, data]<IsState State>(State state) { in >> (s | get_at(data, state)); });
	return std::forward<Istream>(in);
}

template<class Istream, class Bag>
constexpr decltype(auto) deserialize_data(Istream &&in, Bag &bag) {
	return deserialize_data(std::forward<Istream>(in), bag.structure(), bag.data());
}

template<class Ostream, class Struct>
constexpr decltype(auto) serialize_data(Ostream &&out, Struct s, const void *data) {
	traverser(s).for_each([&out, s, data]<IsState State>(State state) { out << (s | get_at(data, state)) << '\n'; });
	return std::forward<Ostream>(out);
}

template<class Ostream, class Bag>
constexpr decltype(auto) serialize_data(Ostream &&out, const Bag &bag) {
	return serialize_data(std::forward<Ostream>(out), bag.structure(), bag.data());
}

} // namespace noarr

#endif // NOARR_STRUCTURES_SERIALIZE_DATA_HPP
