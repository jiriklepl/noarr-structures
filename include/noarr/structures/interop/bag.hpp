#ifndef NOARR_BAG_HPP
#define NOARR_BAG_HPP

#include <cstddef>
#include <memory>
#include <type_traits>
#include <vector>

#include "../base/contain.hpp"
#include "../base/state.hpp"
#include "../extra/funcs.hpp"
#include "../extra/to_struct.hpp"

namespace noarr {

namespace helpers {

// general case for std::vector etc.
template<template<class...> class container>
struct bag_policy {
	using type = container<char>;

	[[nodiscard]]
	static constexpr type construct(std::size_t size) {
		return container<char>(size);
	}

	[[nodiscard]]
	static constexpr char *get(container<char> &_container) noexcept {
		return _container.data();
	}

	[[nodiscard]]
	static constexpr const char *get(const container<char> &_container) noexcept {
		return _container.data();
	}
};

// a helper struct for 'bag_policy' as the '*' (pointer) specifier is not a class ()
template<class...>
struct bag_raw_pointer_tag;

// a helper struct for 'bag_policy' as the '*' (pointer) specifier is not a class ()
template<class...>
struct bag_const_raw_pointer_tag;

// a helper struct for 'bag_policy' as the 'nullptr' specifier is not a class ()
template<class...>
struct bag_nullptr_tag;

template<>
struct bag_policy<std::unique_ptr> {
	using type = std::unique_ptr<char[]>;

	[[nodiscard]]
	static type construct(std::size_t size) {
		return std::make_unique<char[]>(size);
	}

	[[nodiscard]]
	static void *get(const type &ptr) noexcept {
		return ptr.get();
	}
};

template<>
struct bag_policy<bag_raw_pointer_tag> {
	using type = void *;

	template<class Ptr>
	[[nodiscard]]
	static constexpr Ptr get(Ptr ptr) noexcept {
		return ptr;
	}
};

template<>
struct bag_policy<bag_const_raw_pointer_tag> {
	using type = const void *;

	[[nodiscard]]
	static constexpr const void *get(const void *ptr) noexcept {
		return ptr;
	}
};

template<>
struct bag_policy<bag_nullptr_tag> {
	using type = std::nullptr_t;

	[[nodiscard]]
	static constexpr std::nullptr_t construct(std::size_t /*size*/) noexcept {
		return nullptr;
	}

	[[nodiscard]]
	static constexpr std::nullptr_t get(std::nullptr_t /*ptr*/) noexcept {
		return nullptr;
	}
};

} // namespace helpers

/**
 * @brief A bag is an abstraction of a structure combined with data of a corresponding size.
 *
 * @tparam Structure: the structure that describes the data stored in the bag
 * @tparam BagPolicy: indicates what underlying structure contains the data blob (typically `std::unique_ptr`)
 */
template<class Structure, class BagPolicy>
class bag_t : flexible_contain<Structure, typename BagPolicy::type> {
	using base = flexible_contain<Structure, typename BagPolicy::type>;

public:
	explicit constexpr bag_t(Structure s) : base(s, BagPolicy::construct(s | noarr::get_size())) {}

	explicit constexpr bag_t(Structure s, typename BagPolicy::type &&data) : base(s, std::move(data)) {}

	explicit constexpr bag_t(Structure s, const typename BagPolicy::type &data) : base(s, data) {}

	explicit constexpr bag_t(Structure s, BagPolicy policy) : base(s, policy.construct(s | noarr::get_size())) {}

	/**
	 * @brief return the wrapped structure which describes the `data` blob
	 */
	[[nodiscard]]
	constexpr auto structure() const noexcept {
		return base::template get<0>();
	}

	using structure_t = Structure;

	/**
	 * @brief returns the underlying data blob
	 */
	[[nodiscard]]
	constexpr auto data() const noexcept {
		return BagPolicy::get(base::template get<1>());
	}

	using data_t = typename BagPolicy::type;

	/**
	 * @brief accesses a value in `data` by fixing dimensions in the `structure`
	 *
	 * @tparam Dims: the dimension names
	 * @param ts: the dimension values
	 */
	template<auto... Dims, class... Ts>
	requires IsDimPack<decltype(Dims)...>
	[[nodiscard]]
	constexpr decltype(auto) at(Ts... ts) const noexcept {
		return structure() | noarr::get_at<Dims...>(data(), ts...);
	}

	/**
	 * @brief accesses a value in `data` by fixing dimensions in the `structure`
	 *
	 * @param ts: the dimension values
	 */
	template<ToState HasState>
	[[nodiscard]]
	constexpr decltype(auto) operator[](HasState has_state) const noexcept {
		return structure() | noarr::get_at(data(), convert_to_state(has_state));
	}

	/**
	 * @brief returns an offset of a value in `data` by fixing dimensions in the `structure`
	 *
	 * @tparam Dims: the dimension names
	 * @param ts: the dimension values
	 */
	template<auto... Dims, class... Ts>
	requires IsDimPack<decltype(Dims)...>
	[[nodiscard]]
	constexpr auto offset(Ts... ts) const noexcept {
		return structure() | noarr::offset<Dims...>(ts...);
	}

	template<auto Dim, IsState State>
	requires IsDim<decltype(Dim)>
	[[nodiscard]]
	static constexpr bool has_length() noexcept {
		return structure_t::template has_length<Dim, State>();
	}

	/**
	 * @brief gets the length (number of indices) of a dimension in the `structure`
	 *
	 * @tparam Dim: the dimension name
	 */
	template<auto Dim, IsState State = state<>>
	requires (has_length<Dim, State>())
	[[nodiscard]]
	constexpr auto length(State state = empty_state) const noexcept {
		return structure() | noarr::get_length<Dim>(state);
	}

	template<IsState State>
	[[nodiscard]]
	static constexpr bool has_size() noexcept {
		return structure_t::template has_size<State>();
	}

	/**
	 * @brief gets the size of the data described by the `structure`
	 *
	 */
	template<IsState State = state<>>
	requires (has_size<State>())
	[[nodiscard]]
	constexpr auto size(State state = empty_state) const noexcept {
		return structure() | noarr::get_size(state);
	}

	/**
	 * @brief wraps the data pointer in a new bag with the raw pointer policy. If the current bag owns the data,
	 * it continues owning them, while the returned raw bag should be considered a non-owning reference.
	 */
	[[nodiscard]]
	constexpr auto get_ref() const noexcept;

	template<IsProtoStruct ProtoStruct>
	requires (ProtoStruct::proto_preserves_layout)
	[[nodiscard("Returns a new bag")]]
	friend constexpr auto operator^(bag_t &&s, ProtoStruct p) {
		return bag_t<decltype(s.structure() ^ p), BagPolicy>(s.structure() ^ p,
		                                                     std::move(std::move(s).template get<1>()));
	}

	template<IsProtoStruct ProtoStruct>
	requires (ProtoStruct::proto_preserves_layout && std::is_trivially_copy_constructible_v<typename BagPolicy::type>)
	[[nodiscard("Returns a new bag")]]
	friend constexpr auto operator^(const bag_t &s, ProtoStruct p) {
		return bag_t<decltype(s.structure() ^ p), BagPolicy>(s.structure() ^ p, s.template get<1>());
	}
};

template<class T>
concept IsBag = IsSpecialization<T, bag_t>;

template<IsBag T>
struct to_struct<T> : std::true_type {
	using type = std::remove_cvref_t<decltype(std::declval<T>().structure())>;

	static constexpr type convert(const T &b) noexcept { return b.structure(); }
};

template<class Structure>
constexpr bag_t<Structure, helpers::bag_policy<std::unique_ptr>> bag(Structure s,
                                                                     std::unique_ptr<char[]> &&ptr) noexcept {
	return bag_t<Structure, helpers::bag_policy<std::unique_ptr>>(s, std::move(ptr));
}

template<class Structure>
constexpr bag_t<Structure, helpers::bag_policy<std::unique_ptr>> bag(Structure s) {
	return bag_t<Structure, helpers::bag_policy<std::unique_ptr>>(s);
}

template<class Structure>
constexpr bag_t<Structure, helpers::bag_policy<helpers::bag_raw_pointer_tag>> bag(Structure s, void *ptr) noexcept {
	return bag_t<Structure, helpers::bag_policy<helpers::bag_raw_pointer_tag>>(s, ptr);
}

template<class Structure>
constexpr bag_t<Structure, helpers::bag_policy<helpers::bag_const_raw_pointer_tag>> bag(Structure s,
                                                                                        const void *ptr) noexcept {
	return bag_t<Structure, helpers::bag_policy<helpers::bag_const_raw_pointer_tag>>(s, ptr);
}

template<class Structure>
constexpr bag_t<Structure, helpers::bag_policy<helpers::bag_nullptr_tag>> bag(Structure s, std::nullptr_t /*unused*/) {
	return bag_t<Structure, helpers::bag_policy<helpers::bag_nullptr_tag>>(s);
}

template<class Structure>
using unique_bag = decltype(bag(std::declval<Structure>(), std::declval<std::unique_ptr<char[]>>()));
template<class Structure>
using raw_bag = decltype(bag(std::declval<Structure>(), std::declval<void *>()));
template<class Structure>
using const_raw_bag = decltype(bag(std::declval<Structure>(), std::declval<const void *>()));

/**
 * @brief creates a bag with the given structure and automatically creates the underlying data block implemented using
 * std::unique_ptr
 *
 * @param s: the structure
 */
template<class Structure>
constexpr auto make_unique_bag(Structure s) {
	return unique_bag<Structure>(s);
}

/**
 * @brief creates a bag with the given structure and automatically creates the underlying data block implemented using
 * std::vector
 *
 * @param s: the structure
 */
template<class Structure>
constexpr auto make_vector_bag(Structure s) {
	return bag_t<Structure, helpers::bag_policy<std::vector>>(s);
}

/**
 * @brief creates a bag with the given structure and automatically creates the underlying data block implemented using
 * std::unique_ptr
 *
 * @param s: the structure
 */
template<class Structure>
constexpr auto make_bag(Structure s) {
	return make_unique_bag(s);
}

/**
 * @brief creates a bag with the given structure and an underlying r/w observing data blob
 *
 * @param s: the structure
 * @param data: the data blob
 */
template<class Structure>
constexpr auto make_bag(Structure s, void *data) noexcept {
	return raw_bag<Structure>(s, data);
}

/**
 * @brief creates a bag with the given structure and an underlying r/o observing data blob
 *
 * @param s: the structure
 * @param data: the data blob
 */
template<class Structure>
constexpr auto make_bag(Structure s, const void *data) noexcept {
	return const_raw_bag<Structure>(s, data);
}

/**
 * @brief creates a bag with the given structure and an underlying r/o observing data blob
 *
 * @param s: the structure
 * @param data: the data blob
 */
template<class Structure>
constexpr auto make_bag(Structure s, std::nullptr_t /*unused*/) noexcept {
	return bag_t<Structure, helpers::bag_policy<helpers::bag_nullptr_tag>>(s);
}

template<class Structure, class BagPolicy>
constexpr auto bag_t<Structure, BagPolicy>::get_ref() const noexcept {
	return make_bag(structure(), data());
}

} // namespace noarr

#endif // NOARR_BAG_HPP
