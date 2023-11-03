#ifndef NOARR_STRUCTURES_PADDING_HPP
#define NOARR_STRUCTURES_PADDING_HPP

#include "../base/contain.hpp"
#include "../base/state.hpp"
#include "../base/structs_common.hpp"
#include "../base/utility.hpp"

namespace noarr {

/**
 * @brief padding wrapper
 * 
 * @tparam T: type of the substructure the padding wrapper contains
 * @tparam Pre: type of the padding before the substructure
 * @tparam Post: type of the padding after the substructure
 */
template<class T, class Pre, class Post>
struct padding_t : contain<T, Pre, Post> {
	static constexpr char name[] = "padding_t";
	using params = struct_params<
		structure_param<T>,
		type_param<Pre>,
		type_param<Post>>;

	explicit constexpr padding_t(T sub_structure, Pre pre, Post post) noexcept : contain<T, Pre, Post>(sub_structure, pre, post) {}

	constexpr T sub_structure() const noexcept { return contain<T, Pre, Post>::template get<0>(); }
	constexpr auto sub_state(IsState auto state) const noexcept { return state; }

	constexpr Pre pre() const noexcept { return contain<T, Pre, Post>::template get<1>(); }
	constexpr Post post() const noexcept { return contain<T, Pre, Post>::template get<2>(); }

	using signature = typename T::signature;

	template<IsState State>
	constexpr auto size(State state) const noexcept {
		using namespace constexpr_arithmetic;
		return pre() + sub_structure().size(sub_state(state)) + post();
	}

	template<class Sub, IsState State>
	constexpr auto strict_offset_of(State state) const noexcept {
		using namespace constexpr_arithmetic;
		return pre() + sub_structure().template strict_offset_of<Sub>(sub_state(state));
	}

	template<IsDim auto QDim, IsState State>
	constexpr auto length(State state) const noexcept {
		return sub_structure().template length<QDim>(sub_state(state));
	}

	template<class Sub>
	constexpr auto strict_state_at(IsState auto state) const noexcept {
		return sub_structure().template strict_state_at<Sub>(sub_state(state));
	}
};

template<class Pre, class Post>
struct padding_proto : contain<Pre, Post> {
	static constexpr bool proto_preserves_layout = false;

	using contain<Pre, Post>::contain;

	constexpr Pre pre() const noexcept { return contain<Pre, Post>::template get<0>(); }
	constexpr Post post() const noexcept { return contain<Pre, Post>::template get<1>(); }

	template<class Struct>
	constexpr auto instantiate_and_construct(Struct s) const noexcept { return padding_t<Struct, Pre, Post>(s, pre(), post()); }
};

template<class Pre, class Post>
constexpr auto padding(Pre pre, Post post) noexcept {
	return padding_proto<Pre, Post>(pre, post);
}

} // namespace noarr

#endif // NOARR_STRUCTURES_PADDING_HPP
