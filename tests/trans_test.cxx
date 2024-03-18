#include <chrono>

#include <noarr_test/macros.hpp>

#include <noarr/structures_extended.hpp>
#include <noarr/structures/interop/bag.hpp>

using namespace noarr;

namespace {

[[gnu::flatten]]
constexpr void transpose(size_t width, const float* a, float* b) {
	const auto as = noarr::scalar<float>() ^ noarr::sized_vector<'x'>(width) ^ noarr::sized_vector<'y'>(width);
	const auto bs = noarr::scalar<float>() ^ noarr::sized_vector<'y'>(width) ^ noarr::sized_vector<'x'>(width);

	for(size_t i = 0; i < (as | noarr::get_length<'x'>()); i++) {
		for(size_t j = 0; j < (as | noarr::get_length<'y'>()); j++) {
			const auto state = noarr::idx<'x'>(i) & noarr::idx<'y'>(j);

			b[(bs | noarr::offset(state)) / sizeof(float)] = a[(as | noarr::offset(state)) / sizeof(float)];
		}
	}
}

}

TEST_CASE("Transposition", "[transposition]") {
	size_t size = 16384 * 16384;

	auto a = std::make_unique<float[]>(size);
	auto b = std::make_unique<float[]>(size);

	for(size_t i = 0; i < size; i++) {
		a[i] = std::rand();
	}

	int last = 0;

	for(size_t i = 1024; i < 16384; i += 256) {
		auto start = std::chrono::high_resolution_clock::now();

		transpose(i, a.get(), b.get());

		auto end = std::chrono::high_resolution_clock::now();

		int sum = 0;

		for(size_t j = 0; j < size; j++) {
			sum += b[j];
		}

		int duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();

		std::cout << std::format("{} x {}: {} ms (increase: {} ms; sum: {})\n", i, i, duration, duration - last, sum);

		last = duration;
	}
}
