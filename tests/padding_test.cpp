#include <noarr_test/macros.hpp>

#include <noarr/structures.hpp>
#include <noarr/structures/structs/blocks.hpp>
#include <noarr/structures/structs/padding.hpp>
#include <noarr/structures/extra/shortcuts.hpp>

TEST_CASE("trivial padding", "[padding]") {
	auto m = noarr::scalar<float>()
		^ noarr::array<'x', 10'000>()
		^ noarr::array<'y', 20'000>()
		^ noarr::padding(noarr::lit<0>, noarr::lit<0>);

	REQUIRE((m | noarr::offset<'y', 'x'>(10, 3333)) == (10*10'000L + 3333) * sizeof(float));
	REQUIRE(decltype(m | noarr::offset<'y', 'x'>(noarr::lit<10>, noarr::lit<3333>))::value == (10*10'000L + 3333) * sizeof(float));
	REQUIRE(decltype(m | noarr::get_size())::value == 10'000L * 20'000L * sizeof(float));
}

TEST_CASE("padding with pre", "[padding]") {
	auto m = noarr::scalar<float>()
		^ noarr::array<'x', 10'000>()
		^ noarr::array<'y', 20'000>()
		^ noarr::padding(noarr::lit<10>, noarr::lit<0>);

	REQUIRE((m | noarr::offset<'y', 'x'>(10, 3333)) == (10*10'000L + 3333) * sizeof(float) + 10);
	REQUIRE(decltype(m | noarr::offset<'y', 'x'>(noarr::lit<10>, noarr::lit<3333>))::value == (10*10'000L + 3333) * sizeof(float) + 10);
	REQUIRE(decltype(m | noarr::get_size())::value == 10'000L * 20'000L * sizeof(float) + 10);
}

TEST_CASE("padding with post", "[padding]") {
	auto m = noarr::scalar<float>()
		^ noarr::array<'x', 10'000>()
		^ noarr::array<'y', 20'000>()
		^ noarr::padding(noarr::lit<0>, noarr::lit<10>);

	REQUIRE((m | noarr::offset<'y', 'x'>(10, 3333)) == (10*10'000L + 3333) * sizeof(float));
	REQUIRE(decltype(m | noarr::offset<'y', 'x'>(noarr::lit<10>, noarr::lit<3333>))::value == (10*10'000L + 3333) * sizeof(float));
	REQUIRE(decltype(m | noarr::get_size())::value == 10'000L * 20'000L * sizeof(float) + 10);
}

TEST_CASE("padding with pre and post", "[padding]") {
	auto m = noarr::scalar<float>()
		^ noarr::array<'x', 10'000>()
		^ noarr::array<'y', 20'000>()
		^ noarr::padding(noarr::lit<10>, noarr::lit<10>);

	REQUIRE((m | noarr::offset<'y', 'x'>(10, 3333)) == (10*10'000L + 3333) * sizeof(float) + 10);
	REQUIRE(decltype(m | noarr::offset<'y', 'x'>(noarr::lit<10>, noarr::lit<3333>))::value == (10*10'000L + 3333) * sizeof(float) + 10);
	REQUIRE(decltype(m | noarr::get_size())::value == 10'000L * 20'000L * sizeof(float) + 20);
}
