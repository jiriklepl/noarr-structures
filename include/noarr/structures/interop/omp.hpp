#ifndef NOARR_STRUCTURES_OMP_HPP
#define NOARR_STRUCTURES_OMP_HPP

#include "../interop/traverser_iter.hpp"

namespace noarr {

template<class Traverser, class F>
inline auto omp_for_each(const Traverser &t, F &&f) -> decltype(f) {
	#pragma omp parallel for
	for(auto t_inner : t) {
		f(t_inner);
	}

	return std::forward<F>(f);
}

} // namespace noarr

#endif // NOARR_STRUCTURES_OMP_HPP
