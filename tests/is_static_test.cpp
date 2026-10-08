#include <noarr_test/macros.hpp>

#include <cstddef>

#include <noarr/structures_extended.hpp>
#include <noarr/structures/introspection/is_static.hpp>

using namespace noarr;

struct unknown_layout {};

TEST_CASE("scalar", "[is_static]") {
	STATIC_REQUIRE(IsStatic<scalar<int>>);
	STATIC_REQUIRE(is_static(scalar<int>()));
	STATIC_REQUIRE((scalar<int>() | is_static()));
	STATIC_REQUIRE(helpers::is_static<scalar<int>, state<>>::value);
}

TEST_CASE("vector", "[is_static]") {
	STATIC_REQUIRE(!IsStatic<vector_t<'x', scalar<int>>>);
	STATIC_REQUIRE(IsStatic<vector_t<'x', scalar<int>>, state<state_item<length_in<'x'>, lit_t<10>>>>);
	STATIC_REQUIRE(!IsStatic<vector_t<'x', scalar<int>>, state<state_item<length_in<'x'>, std::size_t>>>);

	STATIC_REQUIRE(!IsStatic<vector_t<'x', vector_t<'y', scalar<int>>>,
	                         state<state_item<length_in<'x'>, lit_t<10>>>>);
	STATIC_REQUIRE(!IsStatic<vector_t<'x', vector_t<'y', scalar<int>>>,
	                         state<state_item<length_in<'x'>, lit_t<10>>, state_item<length_in<'y'>, std::size_t>>>);
	STATIC_REQUIRE(IsStatic<vector_t<'x', vector_t<'y', scalar<int>>>,
	                        state<state_item<length_in<'x'>, lit_t<10>>, state_item<length_in<'y'>, lit_t<20>>>>);
}

TEST_CASE("bcast", "[is_static]") {
	STATIC_REQUIRE(!IsStatic<bcast_t<'x', scalar<int>>>);
	STATIC_REQUIRE(IsStatic<bcast_t<'x', scalar<int>>, state<state_item<length_in<'x'>, lit_t<10>>>>);
	STATIC_REQUIRE(!IsStatic<bcast_t<'x', scalar<int>>, state<state_item<length_in<'x'>, std::size_t>>>);

	STATIC_REQUIRE(!IsStatic<bcast_t<'x', vector_t<'y', scalar<int>>>,
	                         state<state_item<length_in<'x'>, lit_t<10>>>>);
	STATIC_REQUIRE(!IsStatic<bcast_t<'x', vector_t<'y', scalar<int>>>,
	                         state<state_item<length_in<'x'>, lit_t<10>>, state_item<length_in<'y'>, std::size_t>>>);
	STATIC_REQUIRE(IsStatic<bcast_t<'x', vector_t<'y', scalar<int>>>,
	                        state<state_item<length_in<'x'>, lit_t<10>>, state_item<length_in<'y'>, lit_t<20>>>>);
}

TEST_CASE("tuple", "[is_static]") {
	STATIC_REQUIRE(IsStatic<tuple_t<'t', scalar<int>, scalar<float>>>);
	STATIC_REQUIRE(IsStatic<tuple_t<'t', scalar<int>, array_t<'x', 10, scalar<int>>>>);
	STATIC_REQUIRE(!IsStatic<tuple_t<'t', scalar<int>, vector_t<'x', scalar<int>>>>);
	STATIC_REQUIRE(
		IsStatic<tuple_t<'t', scalar<int>, vector_t<'x', scalar<int>>>, state<state_item<index_in<'t'>, lit_t<0>>>>);
	STATIC_REQUIRE(
		!IsStatic<tuple_t<'t', scalar<int>, vector_t<'x', scalar<int>>>, state<state_item<index_in<'t'>, lit_t<1>>>>);
	STATIC_REQUIRE(
		!IsStatic<tuple_t<'t', scalar<int>, array_t<'x', 10, vector_t<'y', scalar<int>>>>,
		          state<state_item<index_in<'t'>, lit_t<1>>>>);
	STATIC_REQUIRE(
		IsStatic<tuple_t<'t', scalar<int>, array_t<'x', 10, vector_t<'y', scalar<int>>>>,
		         state<state_item<index_in<'t'>, lit_t<1>>, state_item<length_in<'y'>, lit_t<20>>>>);
	STATIC_REQUIRE(
		!IsStatic<tuple_t<'t', array_t<'x', 10, scalar<int>>, array_t<'x', 10, vector_t<'y', scalar<int>>>>>);
	STATIC_REQUIRE(IsStatic<tuple_t<'t'>>);
}

TEST_CASE("fix", "[is_static]") {
	STATIC_REQUIRE(IsStatic<fix_t<'x', array_t<'x', 10, scalar<int>>, lit_t<0>>>);
	STATIC_REQUIRE(!IsStatic<fix_t<'x', array_t<'x', 10, scalar<int>>, std::size_t>>);
	STATIC_REQUIRE(!IsStatic<fix_t<'x', vector_t<'y', scalar<int>>, lit_t<0>>>);
}

TEST_CASE("set_length", "[is_static]") {
	STATIC_REQUIRE(IsStatic<set_length_t<'x', vector_t<'x', scalar<int>>, lit_t<10>>>);
	STATIC_REQUIRE(!IsStatic<set_length_t<'x', vector_t<'x', scalar<int>>, std::size_t>>);
	STATIC_REQUIRE(IsStatic<array_t<'x', 10, scalar<int>>>);
	STATIC_REQUIRE(is_static(scalar<int>() ^ array<'x', 10>()));

	STATIC_REQUIRE(!IsStatic<array_t<'x', 10, vector_t<'y', scalar<int>>>>);
	STATIC_REQUIRE(!IsStatic<set_length_t<'x', vector_t<'x', vector_t<'y', scalar<int>>>, lit_t<10>>>);
	STATIC_REQUIRE(
		IsStatic<array_t<'x', 10, vector_t<'y', scalar<int>>>, state<state_item<length_in<'y'>, lit_t<20>>>>);
	STATIC_REQUIRE(
		!IsStatic<array_t<'x', 10, vector_t<'y', scalar<int>>>, state<state_item<length_in<'y'>, std::size_t>>>);
}

TEST_CASE("shift", "[is_static]") {
	STATIC_REQUIRE(IsStatic<shift_t<'x', array_t<'x', 10, scalar<int>>, lit_t<2>>>);
	STATIC_REQUIRE(!IsStatic<shift_t<'x', array_t<'x', 10, scalar<int>>, std::size_t>>);
	STATIC_REQUIRE(!IsStatic<shift_t<'x', vector_t<'x', scalar<int>>, lit_t<2>>>);
}

TEST_CASE("slice", "[is_static]") {
	STATIC_REQUIRE(IsStatic<slice_t<'x', array_t<'x', 10, scalar<int>>, lit_t<1>, lit_t<5>>>);
	STATIC_REQUIRE(!IsStatic<slice_t<'x', array_t<'x', 10, scalar<int>>, std::size_t, lit_t<5>>>);
	STATIC_REQUIRE(!IsStatic<slice_t<'x', vector_t<'x', scalar<int>>, lit_t<1>, lit_t<5>>>);
}

TEST_CASE("span", "[is_static]") {
	STATIC_REQUIRE(IsStatic<span_t<'x', array_t<'x', 10, scalar<int>>, lit_t<1>, lit_t<5>>>);
	STATIC_REQUIRE(!IsStatic<span_t<'x', array_t<'x', 10, scalar<int>>, std::size_t, lit_t<5>>>);
	STATIC_REQUIRE(!IsStatic<span_t<'x', vector_t<'x', scalar<int>>, lit_t<1>, lit_t<5>>>);
}

TEST_CASE("step", "[is_static]") {
	STATIC_REQUIRE(IsStatic<step_t<'x', array_t<'x', 10, scalar<int>>, lit_t<1>, lit_t<2>>>);
	STATIC_REQUIRE(!IsStatic<step_t<'x', array_t<'x', 10, scalar<int>>, std::size_t, lit_t<2>>>);
	STATIC_REQUIRE(!IsStatic<step_t<'x', vector_t<'x', scalar<int>>, lit_t<1>, lit_t<2>>>);
}

TEST_CASE("views", "[is_static]") {
	STATIC_REQUIRE(IsStatic<reorder_t<array_t<'x', 10, array_t<'y', 20, scalar<int>>>, 'x'>>);
	STATIC_REQUIRE(!IsStatic<reorder_t<vector_t<'y', vector_t<'x', scalar<int>>>, 'x'>>);

	STATIC_REQUIRE(IsStatic<reverse_t<'x', array_t<'x', 10, scalar<int>>>>);
	STATIC_REQUIRE(!IsStatic<reverse_t<'x', vector_t<'x', scalar<int>>>>);

	STATIC_REQUIRE(IsStatic<hoist_t<'x', array_t<'x', 10, scalar<int>>>>);
	STATIC_REQUIRE(!IsStatic<hoist_t<'x', vector_t<'x', scalar<int>>>>);

	STATIC_REQUIRE(IsStatic<rename_t<array_t<'x', 10, scalar<int>>, 'x', 'y'>>);
	STATIC_REQUIRE(!IsStatic<rename_t<vector_t<'x', scalar<int>>, 'x', 'y'>>);

	STATIC_REQUIRE(IsStatic<join_t<array_t<'x', 10, array_t<'y', 10, scalar<int>>>, 'x', 'y', 'z'>>);
	STATIC_REQUIRE(!IsStatic<join_t<vector_t<'x', vector_t<'y', scalar<int>>>, 'x', 'y', 'z'>>);
}

TEST_CASE("blocks", "[is_static]") {
	STATIC_REQUIRE(IsStatic<into_blocks_t<'x', 'y', 'z', array_t<'x', 10, scalar<int>>>>);
	STATIC_REQUIRE(!IsStatic<into_blocks_t<'x', 'y', 'z', vector_t<'x', scalar<int>>>>);

	STATIC_REQUIRE(IsStatic<into_blocks_static_t<'x', 'b', 'y', 'z', array_t<'x', 16, scalar<int>>, lit_t<4>>>);
	STATIC_REQUIRE(!IsStatic<into_blocks_static_t<'x', 'b', 'y', 'z', vector_t<'x', scalar<int>>, lit_t<4>>>);

	STATIC_REQUIRE(!IsStatic<into_blocks_dynamic_t<'x', 'y', 'z', 'b', array_t<'x', 16, scalar<int>>>>);
	STATIC_REQUIRE(!IsStatic<into_blocks_dynamic_t<'x', 'y', 'z', 'b', vector_t<'x', scalar<int>>>>);

	STATIC_REQUIRE(IsStatic<merge_blocks_t<'x', 'y', 'z', array_t<'x', 4, array_t<'y', 4, scalar<int>>>>>);
	STATIC_REQUIRE(!IsStatic<merge_blocks_t<'x', 'y', 'z', vector_t<'x', vector_t<'y', scalar<int>>>>>);
}

TEST_CASE("zcurve", "[is_static]") {
	auto aw = scalar<int>() ^ array<'w', 10>() ^ array<'x', 16>() ^ array<'y', 16>();
	auto zw = aw ^ merge_zcurve<'x', 'y', 'z'>::maxlen_alignment<16, 16>();
	STATIC_REQUIRE(IsStatic<decltype(zw)>);

	auto dyn_aw = scalar<int>() ^ vector<'w'>() ^ array<'x', 16>() ^ array<'y', 16>();
	auto dyn_zw = dyn_aw ^ merge_zcurve<'x', 'y', 'z'>::maxlen_alignment<16, 16>();
	STATIC_REQUIRE(!IsStatic<decltype(dyn_zw)>);
}

TEST_CASE("unknown_layout", "[is_static]") {
	STATIC_REQUIRE(!IsStatic<unknown_layout>);
	STATIC_REQUIRE(!helpers::is_static<unknown_layout, state<>>::value);
}
