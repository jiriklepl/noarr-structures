#include <noarr_test/macros.hpp>

#include <noarr/structures_extended.hpp>
#include <noarr/structures/structs/zcurve.hpp>

TEST_CASE("Z curve", "[zcurve]") {
	auto a = noarr::array_t<'y', 4, noarr::array_t<'x', 4, noarr::scalar<int>>>();
	auto z = a ^ noarr::merge_zcurve<'y', 'x', 'z'>::maxlen_alignment<4, 2>();

	REQUIRE((z | noarr::offset<'z'>( 0)) == (a | noarr::offset<'x', 'y'>(0, 0)));
	REQUIRE((z | noarr::offset<'z'>( 1)) == (a | noarr::offset<'x', 'y'>(1, 0)));
	REQUIRE((z | noarr::offset<'z'>( 2)) == (a | noarr::offset<'x', 'y'>(0, 1)));
	REQUIRE((z | noarr::offset<'z'>( 3)) == (a | noarr::offset<'x', 'y'>(1, 1)));

	REQUIRE((z | noarr::offset<'z'>( 4)) == (a | noarr::offset<'x', 'y'>(2, 0)));
	REQUIRE((z | noarr::offset<'z'>( 5)) == (a | noarr::offset<'x', 'y'>(3, 0)));
	REQUIRE((z | noarr::offset<'z'>( 6)) == (a | noarr::offset<'x', 'y'>(2, 1)));
	REQUIRE((z | noarr::offset<'z'>( 7)) == (a | noarr::offset<'x', 'y'>(3, 1)));

	REQUIRE((z | noarr::offset<'z'>( 8)) == (a | noarr::offset<'x', 'y'>(0, 2)));
	REQUIRE((z | noarr::offset<'z'>( 9)) == (a | noarr::offset<'x', 'y'>(1, 2)));
	REQUIRE((z | noarr::offset<'z'>(10)) == (a | noarr::offset<'x', 'y'>(0, 3)));
	REQUIRE((z | noarr::offset<'z'>(11)) == (a | noarr::offset<'x', 'y'>(1, 3)));

	REQUIRE((z | noarr::offset<'z'>(12)) == (a | noarr::offset<'x', 'y'>(2, 2)));
	REQUIRE((z | noarr::offset<'z'>(13)) == (a | noarr::offset<'x', 'y'>(3, 2)));
	REQUIRE((z | noarr::offset<'z'>(14)) == (a | noarr::offset<'x', 'y'>(2, 3)));
	REQUIRE((z | noarr::offset<'z'>(15)) == (a | noarr::offset<'x', 'y'>(3, 3)));
}

TEST_CASE("Z curve misaligned", "[zcurve]") {
	auto a = noarr::array_t<'y', 6, noarr::array_t<'x', 6, noarr::scalar<int>>>();
	auto z = a ^ noarr::merge_zcurve<'y', 'x', 'z'>::maxlen_alignment<8, 2>();

	REQUIRE((z | noarr::offset<'z'>( 0)) == (a | noarr::offset<'x', 'y'>(0, 0)));
	REQUIRE((z | noarr::offset<'z'>( 1)) == (a | noarr::offset<'x', 'y'>(1, 0)));
	REQUIRE((z | noarr::offset<'z'>( 2)) == (a | noarr::offset<'x', 'y'>(0, 1)));
	REQUIRE((z | noarr::offset<'z'>( 3)) == (a | noarr::offset<'x', 'y'>(1, 1)));

	REQUIRE((z | noarr::offset<'z'>( 4)) == (a | noarr::offset<'x', 'y'>(2, 0)));
	REQUIRE((z | noarr::offset<'z'>( 5)) == (a | noarr::offset<'x', 'y'>(3, 0)));
	REQUIRE((z | noarr::offset<'z'>( 6)) == (a | noarr::offset<'x', 'y'>(2, 1)));
	REQUIRE((z | noarr::offset<'z'>( 7)) == (a | noarr::offset<'x', 'y'>(3, 1)));

	REQUIRE((z | noarr::offset<'z'>( 8)) == (a | noarr::offset<'x', 'y'>(0, 2)));
	REQUIRE((z | noarr::offset<'z'>( 9)) == (a | noarr::offset<'x', 'y'>(1, 2)));
	REQUIRE((z | noarr::offset<'z'>(10)) == (a | noarr::offset<'x', 'y'>(0, 3)));
	REQUIRE((z | noarr::offset<'z'>(11)) == (a | noarr::offset<'x', 'y'>(1, 3)));

	REQUIRE((z | noarr::offset<'z'>(12)) == (a | noarr::offset<'x', 'y'>(2, 2)));
	REQUIRE((z | noarr::offset<'z'>(13)) == (a | noarr::offset<'x', 'y'>(3, 2)));
	REQUIRE((z | noarr::offset<'z'>(14)) == (a | noarr::offset<'x', 'y'>(2, 3)));
	REQUIRE((z | noarr::offset<'z'>(15)) == (a | noarr::offset<'x', 'y'>(3, 3)));

	REQUIRE((z | noarr::offset<'z'>(16)) == (a | noarr::offset<'x', 'y'>(4, 0)));
	REQUIRE((z | noarr::offset<'z'>(17)) == (a | noarr::offset<'x', 'y'>(5, 0)));
	REQUIRE((z | noarr::offset<'z'>(18)) == (a | noarr::offset<'x', 'y'>(4, 1)));
	REQUIRE((z | noarr::offset<'z'>(19)) == (a | noarr::offset<'x', 'y'>(5, 1)));

	REQUIRE((z | noarr::offset<'z'>(20)) == (a | noarr::offset<'x', 'y'>(4, 2)));
	REQUIRE((z | noarr::offset<'z'>(21)) == (a | noarr::offset<'x', 'y'>(5, 2)));
	REQUIRE((z | noarr::offset<'z'>(22)) == (a | noarr::offset<'x', 'y'>(4, 3)));
	REQUIRE((z | noarr::offset<'z'>(23)) == (a | noarr::offset<'x', 'y'>(5, 3)));

	REQUIRE((z | noarr::offset<'z'>(24)) == (a | noarr::offset<'x', 'y'>(0, 4)));
	REQUIRE((z | noarr::offset<'z'>(25)) == (a | noarr::offset<'x', 'y'>(1, 4)));
	REQUIRE((z | noarr::offset<'z'>(26)) == (a | noarr::offset<'x', 'y'>(0, 5)));
	REQUIRE((z | noarr::offset<'z'>(27)) == (a | noarr::offset<'x', 'y'>(1, 5)));

	REQUIRE((z | noarr::offset<'z'>(28)) == (a | noarr::offset<'x', 'y'>(2, 4)));
	REQUIRE((z | noarr::offset<'z'>(29)) == (a | noarr::offset<'x', 'y'>(3, 4)));
	REQUIRE((z | noarr::offset<'z'>(30)) == (a | noarr::offset<'x', 'y'>(2, 5)));
	REQUIRE((z | noarr::offset<'z'>(31)) == (a | noarr::offset<'x', 'y'>(3, 5)));

	REQUIRE((z | noarr::offset<'z'>(32)) == (a | noarr::offset<'x', 'y'>(4, 4)));
	REQUIRE((z | noarr::offset<'z'>(33)) == (a | noarr::offset<'x', 'y'>(5, 4)));
	REQUIRE((z | noarr::offset<'z'>(34)) == (a | noarr::offset<'x', 'y'>(4, 5)));
	REQUIRE((z | noarr::offset<'z'>(35)) == (a | noarr::offset<'x', 'y'>(5, 5)));
}

TEST_CASE("Z curve 32-bit shift", "[zcurve]") {
	// 2D Morton with SpecialLevel 16 (shift = 16 * 2 = 32 bits)
	// Exercises that shift == 32 does not overflow 32-bit uint (1U << 32 UB)
	auto s = noarr::scalar<int>() ^ noarr::vector<'y'>() ^ noarr::vector<'x'>()
	       ^ noarr::set_length<'y'>(1ULL << 16) ^ noarr::set_length<'x'>(1ULL << 16)
	       ^ noarr::merge_zcurve<'y', 'x', 'z'>::maxlen_alignment<(1ULL << 16), (1ULL << 16)>();

	// Test z = 0b1001 (x bit 0 = 1, y bit 0 = 0, x bit 1 = 0, y bit 1 = 1 -> x = 1, y = 2)
	auto st1 = s.sub_state(noarr::empty_state.with<noarr::index_in<'z'>>(0b1001ULL));
	REQUIRE(st1.get<noarr::index_in<'x'>>() == 1);
	REQUIRE(st1.get<noarr::index_in<'y'>>() == 2);

	// Test with bits near bit 30 and 31 (within 32-bit special level):
	// bit 30 = x bit 15
	// bit 31 = y bit 15
	std::size_t z_high = (1ULL << 31) | (1ULL << 30) | 0b1001ULL;
	auto st2 = s.sub_state(noarr::empty_state.with<noarr::index_in<'z'>>(z_high));
	REQUIRE(st2.get<noarr::index_in<'x'>>() == ((1ULL << 15) | 1ULL));
	REQUIRE(st2.get<noarr::index_in<'y'>>() == ((1ULL << 15) | 2ULL));
}

TEST_CASE("Z curve 64-bit shift constexpr branch", "[zcurve]") {
	// 2D Morton with SpecialLevel 32 (shift = 32 * 2 = 64 bits >= 64)
	auto s = noarr::scalar<int>() ^ noarr::vector<'y'>() ^ noarr::vector<'x'>()
	       ^ noarr::set_length<'y'>(1ULL << 32) ^ noarr::set_length<'x'>(1ULL << 32)
	       ^ noarr::merge_zcurve<'y', 'x', 'z'>::maxlen_alignment<(1ULL << 32), (1ULL << 32)>();

	// Test lower bits
	auto st1 = s.sub_state(noarr::empty_state.with<noarr::index_in<'z'>>(0b1001ULL));
	REQUIRE(st1.get<noarr::index_in<'x'>>() == 1);
	REQUIRE(st1.get<noarr::index_in<'y'>>() == 2);

	// Test upper bits near bit 62 and 63:
	// bit 62 = x bit 31
	// bit 63 = y bit 31
	std::size_t z_high = (1ULL << 63) | (1ULL << 62) | 0b1001ULL;
	auto st2 = s.sub_state(noarr::empty_state.with<noarr::index_in<'z'>>(z_high));
	REQUIRE(st2.get<noarr::index_in<'x'>>() == ((1ULL << 31) | 1ULL));
	REQUIRE(st2.get<noarr::index_in<'y'>>() == ((1ULL << 31) | 2ULL));
}
