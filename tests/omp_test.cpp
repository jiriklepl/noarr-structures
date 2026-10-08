#include <noarr_test/macros.hpp>

#ifndef _OPENMP
#define _OPENMP 201511
#endif

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunknown-pragmas"
#endif

#include <cstddef>
#include <vector>

#include <noarr/structures_extended.hpp>
#include <noarr/structures/interop/bag.hpp>
#include <noarr/structures/interop/omp.hpp>

TEST_CASE("OpenMP - omp_for_each", "[omp]") {
	auto s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto t = noarr::traverser(s);

	std::size_t count = 0;
	noarr::omp_for_each(t, [&](auto state) {
		REQUIRE(state.template get<noarr::index_in<'x'>>() == count);
		count++;
	});

	REQUIRE(count == 10);
}

TEST_CASE("OpenMP - omp_for_sections", "[omp]") {
	auto s = noarr::scalar<int>() ^ noarr::array<'y', 3>() ^ noarr::array<'x', 4>();
	auto t = noarr::traverser(s);

	std::size_t section_count = 0;
	noarr::omp_for_sections(t, [&](auto sub_trav) {
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

TEST_CASE("OpenMP - planner_omp_execute", "[omp]") {
	std::vector<int> data(10, 0);
	auto s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto b = noarr::make_bag(s, data.data());

	auto p = noarr::planner(b).for_each_elem([](auto state, auto &&elem) {
		elem = static_cast<int>(state.template get<noarr::index_in<'x'>>()) * 10;
	});

	p | noarr::planner_omp_execute();

	for (std::size_t i = 0; i < 10; ++i) {
		REQUIRE(data[i] == static_cast<int>(i * 10));
	}
}

TEST_CASE("OpenMP - runtime API validation", "[omp]") {
	REQUIRE(omp_get_num_threads() == 1);
	REQUIRE(omp_get_max_threads() == 1);
	REQUIRE(omp_get_thread_num() == 0);
	REQUIRE(!omp_in_parallel());
}

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic pop
#endif
