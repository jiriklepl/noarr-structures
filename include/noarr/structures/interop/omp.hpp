#ifndef NOARR_STRUCTURES_OMP_HPP
#define NOARR_STRUCTURES_OMP_HPP

#include "../interop/traverser_iter.hpp"
#include "../interop/planner_iter.hpp"

#if !defined(_OPENMP)
#error "This file should only be included when OpenMP is enabled"
#else
#include <omp.h>
#endif


namespace noarr {

template<class Traverser, class F>
inline auto omp_for_each(const Traverser &t, F &&f) -> decltype(f) {
	#pragma omp parallel for
	for(auto t_inner : t) {
		t_inner.for_each(std::forward<F>(f));
	}

	return std::forward<F>(f);
}

template<class Traverser, class F>
inline auto omp_for_sections(const Traverser &t, F &&f) -> decltype(f) {
	#pragma omp parallel for
	for(auto t_inner : t) {
		f(t_inner);
	}
}

struct planner_omp_execute_t {};

constexpr planner_omp_execute_t planner_omp_execute() noexcept {
	return planner_omp_execute_t();
}

template<IsPlanner Planner>
inline void operator|(const Planner &planner, planner_omp_execute_t) {
	#pragma omp parallel for
	for(auto inner_planner : planner) {
		inner_planner.execute();
	}
}

} // namespace noarr

#endif // NOARR_STRUCTURES_OMP_HPP
