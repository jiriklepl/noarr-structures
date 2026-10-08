#include <noarr_test/macros.hpp>

#include <noarr/structures_extended.hpp>

using namespace noarr;

TEST_CASE("Array constexpr arithmetic", "[cearithm]") {
	auto s = scalar<int>() ^ array<'x', 10>() ^ array<'y', 20>();
	STATIC_REQUIRE((s | get_size()).value == 10 * 20 * sizeof(int));
	STATIC_REQUIRE((s | get_length<'x'>()).value == 10);
	STATIC_REQUIRE((s | get_length<'y'>()).value == 20);
	STATIC_REQUIRE((s | offset<'y', 'x'>(lit<15>, lit<5>)).value == (15*10 + 5) * sizeof(int));
}

TEST_CASE("Vector constexpr arithmetic", "[cearithm]") {
	auto s = scalar<int>() ^ vector<'x'>() ^ vector<'y'>() ^ set_length<'x'>(lit<10>) ^ set_length<'y'>(lit<20>);
	STATIC_REQUIRE((s | get_size()).value == 10 * 20 * sizeof(int));
	STATIC_REQUIRE((s | get_length<'x'>()).value == 10);
	STATIC_REQUIRE((s | get_length<'y'>()).value == 20);
	STATIC_REQUIRE((s | offset<'y', 'x'>(lit<15>, lit<5>)).value == (15*10 + 5) * sizeof(int));
}

TEST_CASE("Block split constexpr arithmetic", "[cearithm]") {
	auto s = scalar<int>() ^ array<'i', 200>() ^ into_blocks<'i', 'y', 'x'>(lit<10>);
	STATIC_REQUIRE((s | get_size()).value == 200 * sizeof(int));
	STATIC_REQUIRE((s | get_length<'x'>()).value == 10);
	STATIC_REQUIRE((s | get_length<'y'>()).value == 200/10);
	STATIC_REQUIRE((s | offset<'y', 'x'>(lit<15>, lit<5>)).value == (15*10 + 5) * sizeof(int));
}

TEST_CASE("Block merge constexpr arithmetic", "[cearithm]") {
	auto s = scalar<int>() ^ array<'x', 10>() ^ array<'y', 20>() ^ merge_blocks<'y', 'x', 'i'>();
	STATIC_REQUIRE((s | get_size()).value == 10 * 20 * sizeof(int));
	STATIC_REQUIRE((s | get_length<'i'>()).value == 10 * 20);
	STATIC_REQUIRE((s | offset<'i'>(lit<155>)).value == 155 * sizeof(int));
}

TEST_CASE("Shift outer constexpr arithmetic", "[cearithm]") {
	auto s = scalar<int>() ^ array<'x', 10>() ^ array<'y', 20>() ^ shift<'y'>(lit<3>);
	STATIC_REQUIRE((s | get_size()).value == 10 * 20 * sizeof(int));
	STATIC_REQUIRE((s | get_length<'x'>()).value == 10);
	STATIC_REQUIRE((s | get_length<'y'>()).value == 17);
	STATIC_REQUIRE((s | offset<'y', 'x'>(lit<12>, lit<5>)).value == (15*10 + 5) * sizeof(int));
}

TEST_CASE("Shift inner constexpr arithmetic", "[cearithm]") {
	auto s = scalar<int>() ^ array<'x', 10>() ^ array<'y', 20>() ^ shift<'x'>(lit<3>);
	STATIC_REQUIRE((s | get_size()).value == 10 * 20 * sizeof(int));
	STATIC_REQUIRE((s | get_length<'x'>()).value == 7);
	STATIC_REQUIRE((s | get_length<'y'>()).value == 20);
	STATIC_REQUIRE((s | offset<'y', 'x'>(lit<15>, lit<2>)).value == (15*10 + 5) * sizeof(int));
}

TEST_CASE("Constexpr arithmetic multiplication by make_const<1>", "[cearithm]") {
	using namespace noarr::constexpr_arithmetic;
	int val = 42;
	REQUIRE(make_const<1>() * val == 42);
	REQUIRE(val * make_const<1>() == 42);
	STATIC_REQUIRE((make_const<1>() * 42) == 42);
	STATIC_REQUIRE((42 * make_const<1>()) == 42);

	STATIC_REQUIRE(noarr::is_integral_constant_v<make_const<1>>);
	STATIC_REQUIRE(noarr::is_integral_constant_v<std::integral_constant<int, 5>>);
	STATIC_REQUIRE(!noarr::is_integral_constant_v<int>);
}

TEST_CASE("Constexpr arithmetic min and max", "[cearithm]") {
	using namespace noarr::constexpr_arithmetic;
	STATIC_REQUIRE(std::is_same_v<decltype(max(make_const<4>(), make_const<8>())), make_const<8>>);
	STATIC_REQUIRE(std::is_same_v<decltype(max(make_const<8>(), make_const<4>())), make_const<8>>);
	STATIC_REQUIRE(std::is_same_v<decltype(min(make_const<4>(), make_const<8>())), make_const<4>>);
	STATIC_REQUIRE(std::is_same_v<decltype(min(make_const<8>(), make_const<4>())), make_const<4>>);

	std::size_t val = 6;
	REQUIRE(max(make_const<4>(), val) == 6);
	REQUIRE(max(val, make_const<8>()) == 8);
	REQUIRE(min(make_const<4>(), val) == 4);
	REQUIRE(min(val, make_const<8>()) == 6);
}
