#ifndef NOARR_TEST_DUMMY_TBB_H
#define NOARR_TEST_DUMMY_TBB_H

/**
 * @file tbb.h
 * @brief Dummy implementation of Intel oneAPI Threading Building Blocks (oneTBB) for host-only C++ unit tests.
 *
 * Official oneTBB API References (validated live against the oneAPI Specification v1.4-rev-1):
 * - oneTBB Specification:
 *   https://oneapi-spec.uxlfoundation.org/specifications/oneapi/v1.4-rev-1/elements/onetbb/source/nested-index
 * - tbb::split:
 *   oneAPI Specification, Section "split"
 *   https://oneapi-spec.uxlfoundation.org/specifications/oneapi/v1.4-rev-1/elements/onetbb/source/algorithms/split_tags/split_cls
 * - tbb::parallel_for:
 *   oneAPI Specification, Section "parallel_for"
 *   https://oneapi-spec.uxlfoundation.org/specifications/oneapi/v1.4-rev-1/elements/onetbb/source/algorithms/functions/parallel_for_func
 * - tbb::combinable:
 *   oneAPI Specification, Section "combinable"
 *   https://oneapi-spec.uxlfoundation.org/specifications/oneapi/v1.4-rev-1/elements/onetbb/source/thread_local_storage/combinable_cls
 */

#include <utility>

namespace tbb {

struct split {};

template<class Range, class Body>
inline void parallel_for(const Range &range, const Body &body) {
	static_cast<void>(range.is_divisible());
	static_cast<void>(range.empty());
	body(range);
}

template<class T>
class combinable {
	T val = {};

public:
	constexpr combinable() = default;

	template<class FInit>
	explicit constexpr combinable(FInit finit) : val(finit()) {}

	T &local() noexcept {
		return val;
	}

	T &local(bool &exists) noexcept {
		exists = true;
		return val;
	}

	template<class UnaryFunc>
	void combine_each(UnaryFunc f) {
		f(val);
	}

	template<class BinaryFunc>
	T combine(BinaryFunc f) {
		return f(val, val);
	}

	void clear() noexcept {
		val = T{};
	}
};

} // namespace tbb

#endif // NOARR_TEST_DUMMY_TBB_H
