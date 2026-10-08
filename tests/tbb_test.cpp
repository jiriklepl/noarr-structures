#include <noarr_test/macros.hpp>

#include <cstddef>
#include <vector>

#include <noarr/structures_extended.hpp>
#include <noarr/structures/interop/bag.hpp>
#include <noarr/structures/interop/tbb.hpp>

TEST_CASE("oneTBB - tbb_for_each", "[tbb]") {
	auto s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto t = noarr::traverser(s);

	std::size_t count = 0;
	noarr::tbb_for_each(t, [&](auto state) {
		REQUIRE(state.template get<noarr::index_in<'x'>>() == count);
		count++;
	});

	REQUIRE(count == 10);
}

TEST_CASE("oneTBB - tbb_for_sections", "[tbb]") {
	auto s = noarr::scalar<int>() ^ noarr::array<'y', 3>() ^ noarr::array<'x', 4>();
	auto t = noarr::traverser(s);

	std::size_t section_count = 0;
	noarr::tbb_for_sections(t, [&](auto sub_trav) {
		std::size_t inner_count = 0;
		sub_trav.for_each([&](auto state) {
			REQUIRE(state.template get<noarr::index_in<'x'>>() == section_count);
			inner_count++;
		});
		REQUIRE(inner_count == 3);
		section_count++;
	});

	REQUIRE(section_count == 4);
}

TEST_CASE("oneTBB - tbb_reduce with shared destination", "[tbb]") {
	auto in_s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto out_s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	std::vector<int> out_data(10, 0);

	noarr::tbb_reduce(
		noarr::traverser(in_s),
		[](auto /*state*/, void * /*ptr*/) {},
		[](auto state, void *ptr) {
			const auto idx = state.template get<noarr::index_in<'x'>>();
			static_cast<int *>(ptr)[idx] = static_cast<int>(idx) * 2;
		},
		[](auto /*state*/, void * /*dst*/, const void * /*src*/) {},
		out_s,
		out_data.data()
	);

	for (std::size_t i = 0; i < 10; ++i) {
		REQUIRE(out_data[i] == static_cast<int>(i * 2));
	}
}

TEST_CASE("oneTBB - tbb_reduce with privatized destination", "[tbb]") {
	auto in_s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto out_s = noarr::scalar<int>();
	int sum = 0;

	noarr::tbb_reduce(
		noarr::traverser(in_s),
		[](auto /*state*/, void *ptr) {
			*static_cast<int *>(ptr) = 0;
		},
		[](auto state, void *ptr) {
			*static_cast<int *>(ptr) += static_cast<int>(state.template get<noarr::index_in<'x'>>());
		},
		[](auto /*state*/, void *dst, const void *src) {
			*static_cast<int *>(dst) += *static_cast<const int *>(src);
		},
		out_s,
		&sum
	);

	REQUIRE(sum == 45);
}

TEST_CASE("oneTBB - tbb::split traverser range", "[tbb]") {
	auto s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto range1 = noarr::traverser(s).range();

	decltype(range1) range2(range1, tbb::split{});

	std::vector<std::size_t> r1_indices;
	range1.for_each([&](auto state) {
		r1_indices.push_back(state.template get<noarr::index_in<'x'>>());
	});

	std::vector<std::size_t> r2_indices;
	range2.for_each([&](auto state) {
		r2_indices.push_back(state.template get<noarr::index_in<'x'>>());
	});

	REQUIRE(r1_indices == std::vector<std::size_t>{0, 1, 2, 3, 4});
	REQUIRE(r2_indices == std::vector<std::size_t>{5, 6, 7, 8, 9});
}

TEST_CASE("oneTBB - planner_tbb_execute", "[tbb]") {
	std::vector<int> data(10, 0);
	auto s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto b = noarr::make_bag(s, data.data());

	auto p = noarr::planner(b).for_each_elem([](auto state, auto &&elem) {
		elem = static_cast<int>(state.template get<noarr::index_in<'x'>>()) * 5;
	});

	p | noarr::planner_tbb_execute();

	for (std::size_t i = 0; i < 10; ++i) {
		REQUIRE(data[i] == static_cast<int>(i * 5));
	}
}

TEST_CASE("oneTBB - combinable API validation", "[tbb]") {
	tbb::combinable<int> c([] { return 42; });
	REQUIRE(c.local() == 42);

	bool exists = false;
	REQUIRE(c.local(exists) == 42);
	REQUIRE(exists);

	c.local() = 100;
	REQUIRE(c.combine([](int a, int b) { return a + b; }) == 200);

	int combined_val = 0;
	c.combine_each([&](int val) { combined_val += val; });
	REQUIRE(combined_val == 100);

	c.clear();
	REQUIRE(c.local() == 0);
}
