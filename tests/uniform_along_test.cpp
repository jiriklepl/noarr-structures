#include <noarr_test/macros.hpp>

#include <cstddef>

#include <noarr/structures_extended.hpp>
#include <noarr/structures/introspection/is_static.hpp>
#include <noarr/structures/introspection/uniform_along.hpp>

using namespace noarr;

TEST_CASE("bcast_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<bcast_t<'x', scalar<int>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<bcast_t<'x', scalar<int>>, 'x', state<state_item<length_in<'x'>, std::size_t>>>);
	STATIC_REQUIRE(IsUniformAlong<bcast_t<'x', scalar<int>>, 'x', state<state_item<index_in<'x'>, std::size_t>>>);
	STATIC_REQUIRE(IsUniformAlong<bcast_t<'x', scalar<int>>, 'y', state<>>);
	STATIC_REQUIRE(is_uniform_along<'x'>(scalar<int>() ^ bcast<'x'>()));
	STATIC_REQUIRE(is_uniform_along<'y'>(scalar<int>() ^ bcast<'x'>()));
}

TEST_CASE("vector_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<vector_t<'x', scalar<int>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<vector_t<'x', scalar<int>>, 'x', state<state_item<length_in<'x'>, std::size_t>>>);
	STATIC_REQUIRE(IsUniformAlong<vector_t<'x', scalar<int>>, 'x', state<state_item<index_in<'x'>, std::size_t>>>);
	STATIC_REQUIRE(IsUniformAlong<vector_t<'x', scalar<int>>, 'y', state<>>);

	STATIC_REQUIRE(IsUniformAlong<vector_t<'x', vector_t<'y', scalar<int>>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<vector_t<'x', vector_t<'y', scalar<int>>>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<vector_t<'x', vector_t<'y', scalar<int>>>, 'z', state<>>);
}

TEST_CASE("scalar", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<scalar<int>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<scalar<int>, 'x', state<state_item<length_in<'x'>, std::size_t>>>);
	STATIC_REQUIRE(IsUniformAlong<scalar<int>, 'x', state<state_item<index_in<'x'>, std::size_t>>>);
	STATIC_REQUIRE(is_uniform_along<'x'>(scalar<int>()));
}

TEST_CASE("fix_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<fix_t<'x', scalar<int>, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<fix_t<'x', scalar<int>, std::size_t>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<fix_t<'x', vector_t<'x', scalar<int>>, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<fix_t<'x', vector_t<'y', scalar<int>>, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<fix_t<'x', vector_t<'y', scalar<int>>, std::size_t>, 'y', state<>>);
}

TEST_CASE("set_length_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<set_length_t<'x', scalar<int>, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<set_length_t<'x', bcast_t<'x', scalar<int>>, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<set_length_t<'x', vector_t<'x', scalar<int>>, std::size_t>, 'x', state<>>);
}

TEST_CASE("rename_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<rename_t<vector_t<'x', scalar<int>>, 'x', 'y'>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<rename_t<vector_t<'x', scalar<int>>, 'x', 'y'>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<rename_t<vector_t<'x', scalar<int>>, 'x', 'y'>, 'z', state<>>);
}

TEST_CASE("join_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<join_t<vector_t<'x', vector_t<'y', scalar<int>>>, 'x', 'y', 'z'>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<join_t<vector_t<'x', vector_t<'y', scalar<int>>>, 'x', 'y', 'z'>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<join_t<vector_t<'x', vector_t<'y', scalar<int>>>, 'x', 'y', 'z'>, 'z', state<>>);
	STATIC_REQUIRE(IsUniformAlong<join_t<vector_t<'x', vector_t<'y', scalar<int>>>, 'x', 'y', 'z'>, 'w', state<>>);
}

TEST_CASE("shift_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<shift_t<'x', scalar<int>, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<shift_t<'x', vector_t<'x', scalar<int>>, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<shift_t<'x', vector_t<'x', scalar<int>>, std::size_t>, 'y', state<>>);
}

TEST_CASE("slice_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<slice_t<'x', scalar<int>, std::size_t, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<slice_t<'x', vector_t<'x', scalar<int>>, std::size_t, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<slice_t<'x', vector_t<'x', scalar<int>>, std::size_t, std::size_t>, 'y', state<>>);
}

TEST_CASE("span_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<span_t<'x', scalar<int>, std::size_t, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<span_t<'x', vector_t<'x', scalar<int>>, std::size_t, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<span_t<'x', vector_t<'x', scalar<int>>, std::size_t, std::size_t>, 'y', state<>>);
}

TEST_CASE("step_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<step_t<'x', scalar<int>, std::size_t, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<step_t<'x', vector_t<'x', scalar<int>>, std::size_t, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<step_t<'x', vector_t<'x', scalar<int>>, std::size_t, std::size_t>, 'y', state<>>);
}

TEST_CASE("reverse_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<reverse_t<'x', scalar<int>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<reverse_t<'x', vector_t<'x', scalar<int>>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<reverse_t<'x', vector_t<'x', scalar<int>>>, 'y', state<>>);
}

TEST_CASE("into_blocks_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<into_blocks_t<'x', 'y', 'z', scalar<int>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_t<'x', 'y', 'z', scalar<int>>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_t<'x', 'y', 'z', scalar<int>>, 'z', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_t<'x', 'y', 'z', vector_t<'x', scalar<int>>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_t<'x', 'y', 'z', vector_t<'x', scalar<int>>>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_t<'x', 'y', 'z', vector_t<'x', scalar<int>>>, 'z', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_t<'x', 'y', 'z', vector_t<'x', scalar<int>>>, 'w', state<>>);
}

TEST_CASE("into_blocks_static_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<into_blocks_static_t<'x', 'x', 'y', 'z', scalar<int>, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_static_t<'x', 'x', 'y', 'z', scalar<int>, std::size_t>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_static_t<'x', 'x', 'y', 'z', scalar<int>, std::size_t>, 'z', state<>>);

	STATIC_REQUIRE(IsUniformAlong<into_blocks_static_t<'x', 'x', 'y', 'z', vector_t<'x', scalar<int>>, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_static_t<'x', 'x', 'y', 'z', vector_t<'x', scalar<int>>, std::size_t>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_static_t<'x', 'x', 'y', 'z', vector_t<'x', scalar<int>>, std::size_t>, 'z', state<>>);

	STATIC_REQUIRE(IsUniformAlong<into_blocks_static_t<'x', 'b', 'y', 'z', vector_t<'x', scalar<int>>, std::size_t>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_static_t<'x', 'b', 'y', 'z', vector_t<'x', scalar<int>>, std::size_t>, 'b', state<>>);
}

TEST_CASE("into_blocks_dynamic_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<into_blocks_dynamic_t<'x', 'y', 'z', 'w', scalar<int>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_dynamic_t<'x', 'y', 'z', 'w', scalar<int>>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_dynamic_t<'x', 'y', 'z', 'w', scalar<int>>, 'z', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_dynamic_t<'x', 'y', 'z', 'w', scalar<int>>, 'w', state<>>);

	STATIC_REQUIRE(IsUniformAlong<into_blocks_dynamic_t<'x', 'y', 'z', 'w', vector_t<'x', scalar<int>>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_dynamic_t<'x', 'y', 'z', 'w', vector_t<'x', scalar<int>>>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_dynamic_t<'x', 'y', 'z', 'w', vector_t<'x', scalar<int>>>, 'z', state<>>);
	STATIC_REQUIRE(IsUniformAlong<into_blocks_dynamic_t<'x', 'y', 'z', 'w', vector_t<'x', scalar<int>>>, 'w', state<>>);
}

TEST_CASE("merge_blocks_t", "[uniform_along]") {
	STATIC_REQUIRE(IsUniformAlong<merge_blocks_t<'x', 'y', 'z', scalar<int>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<merge_blocks_t<'x', 'y', 'z', scalar<int>>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<merge_blocks_t<'x', 'y', 'z', scalar<int>>, 'z', state<>>);

	STATIC_REQUIRE(IsUniformAlong<merge_blocks_t<'x', 'y', 'z', set_length_t<'x', set_length_t<'y', vector_t<'y', vector_t<'x', scalar<int>>>, std::size_t>, std::size_t>>, 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<merge_blocks_t<'x', 'y', 'z', set_length_t<'x', set_length_t<'y', vector_t<'y', vector_t<'x', scalar<int>>>, std::size_t>, std::size_t>>, 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<merge_blocks_t<'x', 'y', 'z', set_length_t<'x', set_length_t<'y', vector_t<'y', vector_t<'x', scalar<int>>>, std::size_t>, std::size_t>>, 'z', state<>>);
}

TEST_CASE("merge_zcurve_t", "[uniform_along]") {
	auto aw = scalar<int>() ^ array<'w', 10>() ^ array<'x', 16>() ^ array<'y', 16>();
	auto zw = aw ^ merge_zcurve<'x', 'y', 'z'>::maxlen_alignment<16, 16>();

	STATIC_REQUIRE(IsUniformAlong<decltype(zw), 'z', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(zw), 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(zw), 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(zw), 'w', state<>>);
}

TEST_CASE("tuple_t", "[uniform_along]") {
	auto tup_hetero = pack(scalar<int>(), scalar<int>() ^ array<'x', 10>()) ^ tuple<'t'>();
	STATIC_REQUIRE(!IsUniformAlong<decltype(tup_hetero), 't', state<>>);
	STATIC_REQUIRE(!IsUniformAlong<decltype(tup_hetero), 'x', state<>>);

	auto tup_fixed0 = tup_hetero ^ fix<'t'>(lit<0>);
	STATIC_REQUIRE(IsUniformAlong<decltype(tup_fixed0), 't', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(tup_fixed0), 'x', state<>>);

	auto tup_fixed1 = tup_hetero ^ fix<'t'>(lit<1>);
	STATIC_REQUIRE(IsUniformAlong<decltype(tup_fixed1), 't', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(tup_fixed1), 'x', state<>>);

	auto tup_homo = pack(scalar<int>(), scalar<int>()) ^ tuple<'t'>();
	STATIC_REQUIRE(IsUniformAlong<decltype(tup_homo), 't', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(tup_homo), 'x', state<>>);

	auto tup_homo_fixed = tup_homo ^ fix<'t'>(lit<0>);
	STATIC_REQUIRE(IsUniformAlong<decltype(tup_homo_fixed), 't', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(tup_homo_fixed), 'x', state<>>);

	auto v1 = scalar<int>() ^ vector<'x'>() ^ set_length<'x'>(10);
	auto v2 = scalar<int>() ^ vector<'x'>() ^ set_length<'x'>(20);
	auto tup_dyn = pack(v1, v2) ^ tuple<'t'>();
	STATIC_REQUIRE(!IsUniformAlong<decltype(tup_dyn), 't', state<>>);
	STATIC_REQUIRE(!IsUniformAlong<decltype(tup_dyn), 'x', state<>>);

	auto a1 = scalar<int>() ^ array<'x', 10>();
	auto a2 = scalar<int>() ^ array<'x', 10>();
	auto tup_arr = pack(a1, a2) ^ tuple<'t'>();
	STATIC_REQUIRE(IsUniformAlong<decltype(tup_arr), 't', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(tup_arr), 'x', state<>>);

	auto empty_tup = tuple_t<'t'>();
	STATIC_REQUIRE(IsUniformAlong<decltype(empty_tup), 't', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(empty_tup), 'x', state<>>);

	auto vec_hetero = tup_hetero ^ vector<'v'>();
	STATIC_REQUIRE(!IsUniformAlong<decltype(vec_hetero), 't', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(vec_hetero), 'v', state<>>);
}

TEST_CASE("reorder_t", "[uniform_along]") {
	auto s = scalar<int>() ^ array<'y', 20>() ^ array<'x', 10>() ^ reorder<'x', 'y'>();
	STATIC_REQUIRE(IsUniformAlong<decltype(s), 'x', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(s), 'y', state<>>);
	STATIC_REQUIRE(IsUniformAlong<decltype(s), 'z', state<>>);
}

struct unknown_layout {};

TEST_CASE("unknown_layout", "[uniform_along]") {
	STATIC_REQUIRE(!helpers::is_uniform_along<'x', unknown_layout, state<>>::value);
	STATIC_REQUIRE(!IsUniformAlong<unknown_layout, 'x', state<>>);
}
