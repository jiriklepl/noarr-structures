#include <noarr_test/macros.hpp>

#include <cstddef>
#include <cstdint>

#include <limits>
#include <string>
#include <type_traits>
#include <utility>

#include <noarr/structures_extended.hpp>
#include <noarr/structures/extra/mangle.hpp>

template<char... Chars>
static std::string cp2a(std::integer_sequence<char, Chars...> /*is*/) { return {Chars...}; }

static const std::string int_name = "int" + std::to_string(sizeof(int) * 8) + "_t";
static const std::string size_name = "uint" + std::to_string(sizeof(std::size_t) * 8) + "_t";

TEST_CASE("Mangle scalar", "[mangle]") {
	using s = noarr::scalar<int>;
	using m = noarr::mangle<s>;
	REQUIRE(cp2a(m()) == "scalar<"+int_name+">");
}

TEST_CASE("Mangle array", "[mangle]") {
	using namespace noarr;

	using s = array_t<'x', 42, scalar<int>>;
	using m = mangle<s>;
	REQUIRE(cp2a(m()) == "set_length_t<'x',vector_t<'x',scalar<" + int_name + ">>,std::integral_constant<" + size_name + ",42>>");
}

TEST_CASE("Mangle expr", "[mangle]") {
	using namespace noarr;
	const auto &sn = size_name; // shorten name for alignment

	const auto structure =        set_length_t<'y',vector_t<'y',set_length_t<'x',vector_t<'x',scalar<int32_t>>,std::integral_constant<size_t,42>>>,size_t>{vector_t<'y',set_length_t<'x',vector_t<'x',scalar<int32_t>>,std::integral_constant<size_t,42>>>{set_length_t<'x',vector_t<'x',scalar<int32_t>>,std::integral_constant<size_t,42>>{vector_t<'x',scalar<int32_t>>{scalar<int32_t>{},},lit<42>,},},size_t{24},};
	const std::string expected = "set_length_t<'y',vector_t<'y',set_length_t<'x',vector_t<'x',scalar<int32_t>>,std::integral_constant<"+sn+",42>>>,"+sn+">{vector_t<'y',set_length_t<'x',vector_t<'x',scalar<int32_t>>,std::integral_constant<"+sn+",42>>>{set_length_t<'x',vector_t<'x',scalar<int32_t>>,std::integral_constant<"+sn+",42>>{vector_t<'x',scalar<int32_t>>{scalar<int32_t>{},},lit<42>,},},"+sn+"{24},}";
	const std::string actual = noarr::mangle_expr<std::string>(structure);

	[[maybe_unused]] const auto structure_pretty = noarr::array_t<'x', 42, noarr::scalar<int>>() ^ noarr::vector<'y'>() ^ noarr::set_length<'y'>(24);
	static_assert(std::is_same_v<decltype(structure), decltype(structure_pretty)>, "Test sanity check");

	REQUIRE(expected == actual);
}

TEST_CASE("Mangle negative integral values", "[mangle][regression]") {
	REQUIRE(noarr::mangle_expr<std::string>(std::int32_t{-123}) == "int32_t{-123}");
	REQUIRE(noarr::mangle_expr<std::string>(std::int32_t{-1}) == "int32_t{-1}");
	REQUIRE(noarr::mangle_expr<std::string>(std::numeric_limits<std::int32_t>::min()) ==
	        "int32_t{-2147483648}");
}
