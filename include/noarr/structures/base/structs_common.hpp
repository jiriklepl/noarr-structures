#ifndef NOARR_STRUCTURES_STRUCTS_COMMON_HPP
#define NOARR_STRUCTURES_STRUCTS_COMMON_HPP

#include <cstddef>
#include <type_traits>
#include <utility>

#include "contain.hpp"
#include "utility.hpp"
#include "signature.hpp"
#include "state.hpp"

namespace noarr {

/**
 * @brief a pack of the template parameters of the structure
 *
 * @tparam Params: template parameters of the structure
 */
template<class ...Params>
struct struct_params : flexible_contain<Params...> {
	using flexible_contain<Params...>::flexible_contain;
};

template<class Struct>
struct structure_param {
	using type = Struct;
};

template<class T>
struct type_param {
	using type = T;
};

template<auto V>
struct value_param {
	static constexpr auto value = V;
};

template<auto Dim> requires IsDim<decltype(Dim)>
struct dim_param {
	static constexpr auto value = Dim;
};

template<class StructInner, class StructOuter, IsState State>
constexpr bool has_offset_of() noexcept {
	if constexpr(std::is_same_v<StructInner, StructOuter>) {
		return true;
	} else {
		return StructOuter::template has_strict_offset_of<StructInner, State>();
	}
}

template<class StructInner, class StructOuter>
constexpr auto offset_of(StructOuter structure, IsState auto state) noexcept {
	if constexpr(std::is_same_v<StructInner, StructOuter>) {
		return constexpr_arithmetic::make_const<0>();
	} else {
		static_assert(has_offset_of<StructInner, StructOuter, decltype(state)>());
		if constexpr(has_offset_of<StructInner, StructOuter, decltype(state)>()) {
			return structure.template strict_offset_of<StructInner>(state);
		}
	}
}

template<class StructInner, class StructOuter>
constexpr auto state_at(StructOuter structure, IsState auto state) noexcept {
	if constexpr(std::is_same_v<StructInner, StructOuter>) {
		return state;
	} else {
		return structure.template strict_state_at<StructInner>(state);
	}
}

template<class StructInner, class StructOuter, IsState State>
constexpr bool has_state_at() noexcept {
	if constexpr(std::is_same_v<StructInner, StructOuter>) {
		return true;
	} else {
		return StructOuter::template has_strict_state_at<StructInner, State>();
	}
}

template<class ...Args>
struct pack : flexible_contain<Args...> {
	using flexible_contain<Args...>::flexible_contain;

	explicit constexpr pack(Args ...args) noexcept : flexible_contain<Args...>(args...) {}
};

// this is for the consistency of packs of packs
template<class ...Args>
pack(pack<Args...>) -> pack<pack<Args...>>;

template<class ProtoStruct>
struct to_each : ProtoStruct {
	using ProtoStruct::ProtoStruct;

	explicit constexpr to_each() noexcept = default;
	explicit constexpr to_each(ProtoStruct p) noexcept : ProtoStruct(p) {}
};

template<class ProtoStruct>
to_each(ProtoStruct) -> to_each<ProtoStruct>;

/**
 * @brief returns whether the type `T` meets the criteria for structures
 *
 * @tparam T: the input type
 */
template<class T>
concept IsStruct = IsSignature<typename T::signature>;

/**
 * @brief returns whether the type `T` meets the criteria for proto-structures
 *
 * @tparam T: the input type
 */
template<class T>
concept IsProtoStruct = std::same_as<decltype(T::proto_preserves_layout), const bool>;

namespace helpers {

template<class F, bool PreservesLayout>
struct make_proto_impl : F {
	static constexpr bool proto_preserves_layout = PreservesLayout;

	template<class Struct>
	constexpr auto instantiate_and_construct(Struct s) const noexcept { return (*this)(s); }

};

template<class ...Structs, class ProtoStruct, std::size_t ...Indices> requires IsProtoStruct<ProtoStruct>
constexpr auto pass_pack(pack<Structs...> s, ProtoStruct p, [[maybe_unused]] std::index_sequence<Indices...> is) noexcept {
	return p.instantiate_and_construct(s.template get<Indices>()...);
}

template<class Arg, class ...Args, std::size_t ...Indices>
constexpr auto pass_pack(Arg s, pack<Args...> p, [[maybe_unused]] std::index_sequence<Indices...> is) noexcept {
	return pack(s ^ p.template get<Indices>()...);
}

template<class ...Structs, class Arg, std::size_t ...Indices>
constexpr auto pass_pack(pack<Structs...> s, to_each<Arg> p, [[maybe_unused]] std::index_sequence<Indices...> is) noexcept {
	return pack(s.template get<Indices>() ^ p...);
}

} // namespace helpers

template<class T>
concept IsStructOrProtoStruct = IsStruct<T> || IsProtoStruct<T>;

template<bool PreservesLayout = false, class F>
[[nodiscard("Constructs a new proto-structure")]]
constexpr auto make_proto(F f) noexcept {
	return helpers::make_proto_impl<F, PreservesLayout>(f);
}

template<class Struct, class ProtoStruct> requires (IsStruct<Struct> && IsProtoStruct<ProtoStruct>)
[[nodiscard("Constructs a new structure")]]
constexpr auto operator ^(Struct s, ProtoStruct p) noexcept {
	return p.instantiate_and_construct(s);
}

template<class ...Structs, class ProtoStruct> requires (IsProtoStruct<ProtoStruct> && ... && IsStruct<Structs>)
[[nodiscard("Constructs a new structure")]]
constexpr auto operator ^(pack<Structs...> s, ProtoStruct p) noexcept {
	return helpers::pass_pack(s, p, std::make_index_sequence<sizeof...(Structs)>());
}

template<class Arg, class ...Args>
[[nodiscard("Constructs a new pack of structures")]]
constexpr auto operator ^(Arg s, pack<Args...> p) noexcept {
	return helpers::pass_pack(s, p, std::make_index_sequence<sizeof...(Args)>());
}

template<class ...Structs, class Arg> requires (... && IsStruct<Structs>)
[[nodiscard("Constructs a new pack of structures")]]
constexpr auto operator ^(pack<Structs...> s, to_each<Arg> p) noexcept {
	return helpers::pass_pack(s, p, std::make_index_sequence<sizeof...(Structs)>());
}

struct neutral_proto {
	static constexpr bool proto_preserves_layout = true;

	template<class Struct>
	[[nodiscard("Constructs a new proto-structure")]]
	constexpr auto instantiate_and_construct(Struct s) const noexcept { return s; }
};

template<class InnerProtoStructPack, class OuterProtoStruct>
struct compose_proto;

template<class ...InnerProtoStructs, class OuterProtoStruct>
struct compose_proto<pack<InnerProtoStructs...>, OuterProtoStruct> : flexible_contain<pack<InnerProtoStructs...>, OuterProtoStruct> {
	using base = flexible_contain<pack<InnerProtoStructs...>, OuterProtoStruct>;
	using base::base;

	static constexpr bool proto_preserves_layout = (OuterProtoStruct::proto_preserves_layout && ... && InnerProtoStructs::proto_preserves_layout);

	template<class Struct>
	[[nodiscard("Constructs a new proto-structure")]]
	constexpr auto instantiate_and_construct(Struct s) const noexcept {
		return s ^ this->template get<0>() ^ this->template get<1>();
	}

	template<class ...Structs> requires (sizeof...(Structs) != 1)
	[[nodiscard("Constructs a new proto-structure")]]
	constexpr auto instantiate_and_construct(Structs ...s) const noexcept {
		return pack(s...) ^ this->template get<0>() ^ this->template get<1>();
	}
};

template<class InnerProtoStruct, class OuterProtoStruct> requires (IsProtoStruct<InnerProtoStruct> && IsProtoStruct<OuterProtoStruct>)
[[nodiscard("Constructs a new proto-structure")]]
constexpr compose_proto<pack<InnerProtoStruct>, OuterProtoStruct> operator ^(InnerProtoStruct i, OuterProtoStruct o) noexcept {
	return compose_proto<pack<InnerProtoStruct>, OuterProtoStruct>(pack(i), o);
}

template<class ...InnerProtoStructs, class OuterProtoStruct> requires (IsProtoStruct<OuterProtoStruct> && ... && IsProtoStruct<InnerProtoStructs>)
[[nodiscard("Constructs a new proto-structure")]]
constexpr compose_proto<pack<InnerProtoStructs...>, OuterProtoStruct> operator ^(pack<InnerProtoStructs...> i, OuterProtoStruct o) noexcept {
	return compose_proto<pack<InnerProtoStructs...>, OuterProtoStruct>(i, o);
}

} // namespace noarr

#endif // NOARR_STRUCTURES_STRUCTS_COMMON_HPP
