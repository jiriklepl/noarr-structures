#ifndef NOARR_TEST_DUMMY_CUDA_DUMMY_HPP
#define NOARR_TEST_DUMMY_CUDA_DUMMY_HPP

/**
 * @file cuda_dummy.hpp
 * @brief Dummy implementation of CUDA built-in keywords, types, and variables for host-only C++ unit tests.
 *
 * Official NVIDIA CUDA API References (validated against CUDA Toolkit header <vector_types.h> and CUDA documentation):
 * - Execution Space Specifiers (__device__, __host__):
 *   NVIDIA CUDA Programming Guide, Section 5.4.1.1 "Execution Space Specifiers"
 *   https://docs.nvidia.com/cuda/cuda-programming-guide/05-appendices/cpp-language-extensions.html#execution-space-specifiers
 * - Built-in Variables (threadIdx, blockIdx, blockDim, gridDim):
 *   NVIDIA CUDA Programming Guide, Section 2.3.2 "Thread Hierarchy"
 *   https://docs.nvidia.com/cuda/cuda-programming-guide/02-basics/writing-cuda-kernels.html#thread-hierarchy
 * - dim3 type:
 *   NVIDIA CUDA Toolkit Header: <vector_types.h>, struct dim3
 *   NVIDIA CUDA C++ Programming Guide Archive, Section "dim3"
 *   https://docs.nvidia.com/cuda/archive/12.4.1/cuda-c-programming-guide/index.html#dim3
 */

#define __device__
#define __host__

typedef unsigned int uint;

struct dim3 {
	unsigned int x = 1;
	unsigned int y = 1;
	unsigned int z = 1;

	constexpr dim3(unsigned int vx = 1, unsigned int vy = 1, unsigned int vz = 1) noexcept : x(vx), y(vy), z(vz) {}
};

[[maybe_unused]] static dim3 threadIdx;
[[maybe_unused]] static dim3 blockIdx;
[[maybe_unused]] static dim3 blockDim;
[[maybe_unused]] static dim3 gridDim;

#endif // NOARR_TEST_DUMMY_CUDA_DUMMY_HPP

