#include <noarr_test/macros.hpp>
#include <noarr_test/defs.hpp>

#include <noarr/structures_extended.hpp>

using namespace noarr;

// This test case tests the situation when the original length of the structure is not known
TEST_CASE("into_blocks post test", "[has_test]") {
	constexpr auto testee = scalar<int>() ^ vector<'x'>() ^ into_blocks_dynamic<'x', 'M', 'm', 'g'>();

	STATIC_REQUIRE(!(testee | has_size()));
	STATIC_REQUIRE(!(testee | has_length<'x'>()));
	STATIC_REQUIRE(!(testee | has_length<'M'>()));
	STATIC_REQUIRE(!(testee | has_length<'m'>()));
	STATIC_REQUIRE(!(testee | has_length<'g'>()));
	STATIC_REQUIRE(!(testee | has_offset()));

	STATIC_REQUIRE(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) | has_size());
	STATIC_REQUIRE((testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) | get_size()) == 10 * 2 * sizeof(int));

	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) | has_length<'x'>()));
	STATIC_REQUIRE(testee ^ set_length<'m'>(10) | has_length<'m'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(10) | get_length<'m'>()) == 10);
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) | has_length<'M'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) | has_length<'g'>()));

	STATIC_REQUIRE(!(testee ^ set_length<'M'>(2) | has_length<'x'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(2) | has_length<'m'>()));
	STATIC_REQUIRE(testee ^ set_length<'M'>(2) | has_length<'M'>());
	STATIC_REQUIRE((testee ^ set_length<'M'>(2) | get_length<'M'>()) == 2);
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(2) | has_length<'g'>()));

	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) | has_offset()));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(2) | has_offset()));

	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) | has_length<'x'>()));
	STATIC_REQUIRE(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) | has_length<'m'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) | get_length<'m'>()) == 10);
	STATIC_REQUIRE(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) | has_length<'M'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) | get_length<'M'>()) == 2);
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) | has_length<'g'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) | has_offset()));

	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) | has_length<'x'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) | has_length<'m'>()));
	STATIC_REQUIRE(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) | has_length<'M'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) | get_length<'M'>()) == 2);
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) | has_length<'g'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) | has_offset()));

	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'M'>(1) | has_length<'x'>()));
	STATIC_REQUIRE(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'M'>(1) | has_length<'m'>());
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'M'>(1) | has_length<'M'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'M'>(1) | has_length<'g'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'M'>(1) | has_offset()));

	STATIC_REQUIRE(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_length<'g'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) ^ fix<'M'>(1) | get_length<'g'>()) == 1);
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_offset()));

	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) ^ fix<'M'>(1) ^ fix<'g'>(0) | has_length<'g'>()));
	STATIC_REQUIRE(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) ^ fix<'M'>(1) ^ fix<'g'>(0) | has_offset());
	STATIC_REQUIRE((testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(4) ^ fix<'M'>(1) ^ fix<'g'>(0) | offset()) == 1 * 10 * sizeof(int) + 4 * sizeof(int));

	STATIC_REQUIRE((testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(10) ^ fix<'M'>(1) | has_length<'g'>()));
	STATIC_REQUIRE((testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(10) ^ fix<'M'>(1) | get_length<'g'>()) == 0);
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(10) ^ fix<'M'>(1) | has_offset()));

	STATIC_REQUIRE((testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(9) ^ fix<'M'>(2) | has_length<'g'>()));
	STATIC_REQUIRE((testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(9) ^ fix<'M'>(2) | get_length<'g'>()) == 0);
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(10) ^ set_length<'M'>(2) ^ fix<'m'>(9) ^ fix<'M'>(2) | has_offset()));
}

// This test case tests the situation when the original length of the structure is known
// - only one of the blocking dimensions is to be set
TEST_CASE("into_blocks pre test", "[has_test]") {
	constexpr auto testee = scalar<int>() ^ vector<'x'>(42) ^ into_blocks_dynamic<'x', 'M', 'm', 'g'>();

	STATIC_REQUIRE((testee | has_size()));
	STATIC_REQUIRE((testee | get_size()) == 42 * sizeof(int));
	STATIC_REQUIRE(!(testee | has_length<'x'>()));
	STATIC_REQUIRE(!(testee | has_length<'M'>()));
	STATIC_REQUIRE(!(testee | has_length<'m'>()));
	STATIC_REQUIRE(!(testee | has_length<'g'>()));
	STATIC_REQUIRE(!(testee | has_offset()));

	STATIC_REQUIRE(testee ^ set_length<'m'>(6) | has_size());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) | get_size()) == 6 * 7 * sizeof(int));

	STATIC_REQUIRE(testee ^ set_length<'M'>(7) | has_size());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) | get_size()) == 6 * 7 * sizeof(int));

	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) | has_length<'x'>()));
	STATIC_REQUIRE(testee ^ set_length<'m'>(6) | has_length<'m'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) | get_length<'m'>()) == 6);
	STATIC_REQUIRE(testee ^ set_length<'m'>(6) | has_length<'M'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) | get_length<'M'>()) == 7);
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) | has_length<'g'>()));

	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) | has_length<'x'>()));
	STATIC_REQUIRE(testee ^ set_length<'M'>(7) | has_length<'m'>());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) | get_length<'m'>()) == 6);
	STATIC_REQUIRE(testee ^ set_length<'M'>(7) | has_length<'M'>());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) | get_length<'M'>()) == 7);
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) | has_length<'g'>()));

	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) | has_offset()));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) | has_offset()));

	STATIC_REQUIRE(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) | has_size());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) ^ fix<'m'>(4) | get_size()) == 6 * 7 * sizeof(int));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) | has_length<'x'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) | has_length<'m'>()));
	STATIC_REQUIRE(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) | has_length<'M'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) ^ fix<'m'>(4) | get_length<'M'>()) == 7);
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) | has_length<'g'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) | has_offset()));

	STATIC_REQUIRE(testee ^ set_length<'m'>(6) ^ fix<'M'>(1) | has_size());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) ^ fix<'M'>(1) | get_size()) == 6 * 7 * sizeof(int));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'M'>(1) | has_length<'x'>()));
	STATIC_REQUIRE(testee ^ set_length<'m'>(6) ^ fix<'M'>(1) | has_length<'m'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) ^ fix<'M'>(1) | get_length<'m'>()) == 6);
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'M'>(1) | has_length<'M'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'M'>(1) | has_length<'g'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'M'>(1) | has_offset()));

	STATIC_REQUIRE(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_size());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) ^ fix<'m'>(4) ^ fix<'M'>(1) | get_size()) == 6 * 7 * sizeof(int));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_length<'x'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_length<'m'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_length<'M'>()));
	STATIC_REQUIRE(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_length<'g'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) ^ fix<'m'>(4) ^ fix<'M'>(1) | get_length<'g'>()) == 1);
	STATIC_REQUIRE(testee ^ set_length<'m'>(6) ^ fix<'m'>(4) ^ fix<'M'>(1) ^ fix<'g'>(0) | has_offset());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) ^ fix<'m'>(4) ^ fix<'M'>(1) ^ fix<'g'>(0) | offset()) == 1 * 6 * sizeof(int) + 4 * sizeof(int));

	STATIC_REQUIRE(testee ^ set_length<'m'>(6) ^ fix<'m'>(5) ^ fix<'M'>(7) | has_length<'g'>());
	STATIC_REQUIRE((testee ^ set_length<'m'>(6) ^ fix<'m'>(5) ^ fix<'M'>(7) | get_length<'g'>()) == 0);
	STATIC_REQUIRE(!(testee ^ set_length<'m'>(6) ^ fix<'m'>(5) ^ fix<'M'>(7) | has_offset()));

	STATIC_REQUIRE(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) | has_size());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) ^ fix<'m'>(4) | get_size()) == 6 * 7 * sizeof(int));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) | has_length<'x'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) | has_length<'m'>()));
	STATIC_REQUIRE(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) | has_length<'M'>());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) ^ fix<'m'>(4) | get_length<'M'>()) == 7);
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) | has_length<'g'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) | has_offset()));

	STATIC_REQUIRE(testee ^ set_length<'M'>(7) ^ fix<'M'>(1) | has_size());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) ^ fix<'M'>(1) | get_size()) == 6 * 7 * sizeof(int));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'M'>(1) | has_length<'x'>()));
	STATIC_REQUIRE(testee ^ set_length<'M'>(7) ^ fix<'M'>(1) | has_length<'m'>());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) ^ fix<'M'>(1) | get_length<'m'>()) == 6);
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'M'>(1) | has_length<'M'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'M'>(1) | has_length<'g'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'M'>(1) | has_offset()));

	STATIC_REQUIRE(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_size());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) ^ fix<'m'>(4) ^ fix<'M'>(1) | get_size()) == 6 * 7 * sizeof(int));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_length<'x'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_length<'m'>()));
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_length<'M'>()));
	STATIC_REQUIRE(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) ^ fix<'M'>(1) | has_length<'g'>());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) ^ fix<'m'>(4) ^ fix<'M'>(1) | get_length<'g'>()) == 1);
	STATIC_REQUIRE(testee ^ set_length<'M'>(7) ^ fix<'m'>(4) ^ fix<'M'>(1) ^ fix<'g'>(0) | has_offset());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) ^ fix<'m'>(4) ^ fix<'M'>(1) ^ fix<'g'>(0) | offset()) == 1 * 6 * sizeof(int) + 4 * sizeof(int));

	STATIC_REQUIRE(testee ^ set_length<'M'>(7) ^ fix<'m'>(6) ^ fix<'M'>(6) | has_length<'g'>());
	STATIC_REQUIRE((testee ^ set_length<'M'>(7) ^ fix<'m'>(6) ^ fix<'M'>(6) | get_length<'g'>()) == 0);
	STATIC_REQUIRE(!(testee ^ set_length<'M'>(7) ^ fix<'m'>(6) ^ fix<'M'>(6) | has_offset()));
}
