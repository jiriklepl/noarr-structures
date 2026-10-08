#ifndef NOARR_TEST_DUMMY_COOPERATIVE_GROUPS_H
#define NOARR_TEST_DUMMY_COOPERATIVE_GROUPS_H

/**
 * @file cooperative_groups.h
 * @brief Dummy implementation of CUDA Cooperative Groups for host-only C++ unit tests.
 *
 * Official NVIDIA CUDA API References (validated against CUDA Toolkit header <cooperative_groups.h> and CUDA documentation):
 * - Cooperative Groups Programming Model:
 *   NVIDIA CUDA Programming Guide, Section 4.4 "Cooperative Groups"
 *   https://docs.nvidia.com/cuda/cuda-programming-guide/04-special-topics/cooperative-groups.html
 * - CUDA Toolkit Header:
 *   <cooperative_groups.h>
 *   Defines cooperative_groups::thread_block, cooperative_groups::grid_group,
 *   cooperative_groups::this_thread_block(), cooperative_groups::this_grid().
 * - Legacy Documentation Archive:
 *   NVIDIA CUDA C++ Programming Guide Archive, Section "Cooperative Groups"
 *   https://docs.nvidia.com/cuda/archive/12.4.1/cuda-c-programming-guide/index.html#cooperative-groups
 */

#include <cstddef>

#include "cuda_dummy.hpp"

namespace cooperative_groups {

/**
 * @brief Represents the thread block group.
 * Reference: <cooperative_groups.h>, class thread_block
 */
class thread_block {
public:
	constexpr thread_block() noexcept = default;

	[[nodiscard]] static unsigned int thread_rank() noexcept {
		return static_cast<unsigned int>(threadIdx.z) * blockDim.y * blockDim.x
			+ static_cast<unsigned int>(threadIdx.y) * blockDim.x
			+ threadIdx.x;
	}

	[[nodiscard]] static unsigned int num_threads() noexcept {
		return static_cast<unsigned int>(blockDim.x) * blockDim.y * blockDim.z;
	}

	[[nodiscard]] static unsigned int size() noexcept {
		return num_threads();
	}

	[[nodiscard]] static dim3 group_index() noexcept {
		return dim3(blockIdx.x, blockIdx.y, blockIdx.z);
	}

	[[nodiscard]] static dim3 thread_index() noexcept {
		return dim3(threadIdx.x, threadIdx.y, threadIdx.z);
	}

	[[nodiscard]] static dim3 group_dim() noexcept {
		return dim3(blockDim.x, blockDim.y, blockDim.z);
	}

	[[nodiscard]] static dim3 dim_threads() noexcept {
		return dim3(blockDim.x, blockDim.y, blockDim.z);
	}

	static void sync() noexcept {}
};

/**
 * @brief Represents the grid-wide thread group.
 * Reference: <cooperative_groups.h>, class grid_group
 */
class grid_group {
public:
	constexpr grid_group() noexcept = default;

	[[nodiscard]] static unsigned long long num_blocks() noexcept {
		return static_cast<unsigned long long>(gridDim.x) * (static_cast<unsigned long long>(gridDim.y) * gridDim.z);
	}

	[[nodiscard]] static unsigned long long num_threads() noexcept {
		return num_blocks() * static_cast<unsigned long long>(thread_block::num_threads());
	}

	[[nodiscard]] static unsigned long long size() noexcept {
		return num_threads();
	}

	[[nodiscard]] static unsigned long long block_rank() noexcept {
		return static_cast<unsigned long long>(blockIdx.z) * gridDim.y * gridDim.x
			+ static_cast<unsigned long long>(blockIdx.y) * gridDim.x
			+ blockIdx.x;
	}

	[[nodiscard]] static unsigned long long thread_rank() noexcept {
		return block_rank() * static_cast<unsigned long long>(thread_block::num_threads())
			+ thread_block::thread_rank();
	}

	[[nodiscard]] static dim3 dim_blocks() noexcept {
		return dim3(gridDim.x, gridDim.y, gridDim.z);
	}

	[[nodiscard]] static dim3 block_index() noexcept {
		return dim3(blockIdx.x, blockIdx.y, blockIdx.z);
	}

	[[nodiscard]] static dim3 group_dim() noexcept {
		return dim3(gridDim.x, gridDim.y, gridDim.z);
	}

	[[nodiscard]] static dim3 dim_threads() noexcept {
		return dim3(gridDim.x * blockDim.x, gridDim.y * blockDim.y, gridDim.z * blockDim.z);
	}

	[[nodiscard]] static dim3 thread_index() noexcept {
		return dim3(blockIdx.x * blockDim.x + threadIdx.x,
		            blockIdx.y * blockDim.y + threadIdx.y,
		            blockIdx.z * blockDim.z + threadIdx.z);
	}

	[[nodiscard]] static bool is_valid() noexcept {
		return true;
	}

	static void sync() noexcept {}
};

/**
 * @brief Constructs a thread_block group representing the calling thread's block.
 * Reference: <cooperative_groups.h>, this_thread_block()
 */
inline thread_block this_thread_block() noexcept {
	return {};
}

/**
 * @brief Constructs a grid_group representing all threads launched in the grid.
 * Reference: <cooperative_groups.h>, this_grid()
 */
inline grid_group this_grid() noexcept {
	return {};
}

} // namespace cooperative_groups

#endif // NOARR_TEST_DUMMY_COOPERATIVE_GROUPS_H
