#ifndef NOARR_TEST_DUMMY_OMP_H
#define NOARR_TEST_DUMMY_OMP_H

/**
 * @file omp.h
 * @brief Dummy implementation of OpenMP runtime library header for host-only C++ unit tests.
 *
 * Official OpenMP API References (validated live against the OpenMP Architecture Review Board specification):
 * - OpenMP Application Programming Interface Specification (Version 5.0):
 *   https://www.openmp.org/spec-html/5.0/openmp.html
 * - Section 3.1 "Runtime Library Definitions" (<omp.h>):
 *   https://www.openmp.org/spec-html/5.0/openmpse29.html
 * - Section 3.2 "Execution Environment Routines":
 *   https://www.openmp.org/spec-html/5.0/openmpse30.html
 */

extern "C" {

inline int omp_get_num_threads(void) {
	return 1;
}

inline int omp_get_max_threads(void) {
	return 1;
}

inline int omp_get_thread_num(void) {
	return 0;
}

inline int omp_in_parallel(void) {
	return 0;
}

} // extern "C"

#endif // NOARR_TEST_DUMMY_OMP_H
