#include <noarr_test/macros.hpp>

#include <cstddef>
#include <vector>

#include <noarr/structures_extended.hpp>

#include <cuda_dummy.hpp>

#include <noarr/structures/interop/cuda_step.cuh>

TEST_CASE("CUDA step - thread block with explicit dimension tag", "[cuda]") {
	blockDim = {4, 1, 1};
	threadIdx = {1, 0, 0};

	auto s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto stepped = s ^ noarr::cuda_step_block<'x'>();
	REQUIRE((stepped | noarr::get_length<'x'>()) == 3);
	REQUIRE((stepped | noarr::offset<'x'>(0)) == 1 * sizeof(int));
	REQUIRE((stepped | noarr::offset<'x'>(1)) == 5 * sizeof(int));
	REQUIRE((stepped | noarr::offset<'x'>(2)) == 9 * sizeof(int));
}

TEST_CASE("CUDA step - thread block with inferred dimension tag", "[cuda]") {
	blockDim = {4, 1, 1};
	threadIdx = {1, 0, 0};

	auto s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto stepped = s ^ noarr::cuda_step_block();
	REQUIRE((stepped | noarr::get_length<'x'>()) == 3);
	REQUIRE((stepped | noarr::offset<'x'>(0)) == 1 * sizeof(int));
	REQUIRE((stepped | noarr::offset<'x'>(1)) == 5 * sizeof(int));
	REQUIRE((stepped | noarr::offset<'x'>(2)) == 9 * sizeof(int));
}

TEST_CASE("CUDA step - pass thread_block instance explicitly", "[cuda]") {
	blockDim = {4, 1, 1};
	threadIdx = {1, 0, 0};

	auto s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto tb = cooperative_groups::this_thread_block();
	auto stepped1 = s ^ noarr::cuda_step<'x'>(tb);
	auto stepped2 = s ^ noarr::cuda_step(tb);
	REQUIRE((stepped1 | noarr::get_length<'x'>()) == 3);
	REQUIRE((stepped1 | noarr::offset<'x'>(0)) == 1 * sizeof(int));
	REQUIRE((stepped1 | noarr::offset<'x'>(1)) == 5 * sizeof(int));
	REQUIRE((stepped2 | noarr::get_length<'x'>()) == 3);
	REQUIRE((stepped2 | noarr::offset<'x'>(0)) == 1 * sizeof(int));
	REQUIRE((stepped2 | noarr::offset<'x'>(1)) == 5 * sizeof(int));
}

TEST_CASE("CUDA step - thread block type as template argument", "[cuda]") {
	blockDim = {4, 1, 1};
	threadIdx = {1, 0, 0};

	auto s = noarr::scalar<int>() ^ noarr::array<'x', 10>();
	auto stepped1 = s ^ noarr::cuda_step<'x', cooperative_groups::thread_block>();
	auto stepped2 = s ^ noarr::cuda_step<cooperative_groups::thread_block>();
	REQUIRE((stepped1 | noarr::get_length<'x'>()) == 3);
	REQUIRE((stepped1 | noarr::offset<'x'>(0)) == 1 * sizeof(int));
	REQUIRE((stepped1 | noarr::offset<'x'>(1)) == 5 * sizeof(int));
	REQUIRE((stepped2 | noarr::get_length<'x'>()) == 3);
	REQUIRE((stepped2 | noarr::offset<'x'>(0)) == 1 * sizeof(int));
	REQUIRE((stepped2 | noarr::offset<'x'>(1)) == 5 * sizeof(int));
}

TEST_CASE("CUDA step - grid group with explicit dimension tag", "[cuda]") {
	gridDim = {2, 1, 1};
	blockDim = {4, 1, 1};
	blockIdx = {1, 0, 0};
	threadIdx = {2, 0, 0};

	// Thread rank in grid: 1 * 4 + 2 = 6
	// Total threads in grid: 2 * 4 = 8
	auto s = noarr::scalar<int>() ^ noarr::array<'x', 20>();
	auto stepped = s ^ noarr::cuda_step_grid<'x'>();
	REQUIRE((stepped | noarr::get_length<'x'>()) == 2);
	REQUIRE((stepped | noarr::offset<'x'>(0)) == 6 * sizeof(int));
	REQUIRE((stepped | noarr::offset<'x'>(1)) == 14 * sizeof(int));
}

TEST_CASE("CUDA step - grid group with inferred dimension tag", "[cuda]") {
	gridDim = {2, 1, 1};
	blockDim = {4, 1, 1};
	blockIdx = {1, 0, 0};
	threadIdx = {2, 0, 0};

	auto s = noarr::scalar<int>() ^ noarr::array<'x', 20>();
	auto stepped = s ^ noarr::cuda_step_grid();
	REQUIRE((stepped | noarr::get_length<'x'>()) == 2);
	REQUIRE((stepped | noarr::offset<'x'>(0)) == 6 * sizeof(int));
	REQUIRE((stepped | noarr::offset<'x'>(1)) == 14 * sizeof(int));
}

TEST_CASE("CUDA step - pass grid_group instance explicitly", "[cuda]") {
	gridDim = {2, 1, 1};
	blockDim = {4, 1, 1};
	blockIdx = {1, 0, 0};
	threadIdx = {2, 0, 0};

	auto s = noarr::scalar<int>() ^ noarr::array<'x', 20>();
	auto gg = cooperative_groups::this_grid();
	auto stepped1 = s ^ noarr::cuda_step<'x'>(gg);
	auto stepped2 = s ^ noarr::cuda_step(gg);
	REQUIRE((stepped1 | noarr::get_length<'x'>()) == 2);
	REQUIRE((stepped1 | noarr::offset<'x'>(0)) == 6 * sizeof(int));
	REQUIRE((stepped1 | noarr::offset<'x'>(1)) == 14 * sizeof(int));
	REQUIRE((stepped2 | noarr::get_length<'x'>()) == 2);
	REQUIRE((stepped2 | noarr::offset<'x'>(0)) == 6 * sizeof(int));
	REQUIRE((stepped2 | noarr::offset<'x'>(1)) == 14 * sizeof(int));
}

TEST_CASE("CUDA step - grid group type as template argument", "[cuda]") {
	gridDim = {2, 1, 1};
	blockDim = {4, 1, 1};
	blockIdx = {1, 0, 0};
	threadIdx = {2, 0, 0};

	auto s = noarr::scalar<int>() ^ noarr::array<'x', 20>();
	auto stepped1 = s ^ noarr::cuda_step<'x', cooperative_groups::grid_group>();
	auto stepped2 = s ^ noarr::cuda_step<cooperative_groups::grid_group>();
	REQUIRE((stepped1 | noarr::get_length<'x'>()) == 2);
	REQUIRE((stepped1 | noarr::offset<'x'>(0)) == 6 * sizeof(int));
	REQUIRE((stepped1 | noarr::offset<'x'>(1)) == 14 * sizeof(int));
	REQUIRE((stepped2 | noarr::get_length<'x'>()) == 2);
	REQUIRE((stepped2 | noarr::offset<'x'>(0)) == 6 * sizeof(int));
	REQUIRE((stepped2 | noarr::offset<'x'>(1)) == 14 * sizeof(int));
}

TEST_CASE("CUDA step - 3D grid and block geometry", "[cuda]") {
	gridDim = {2, 2, 1};
	blockDim = {2, 3, 4};
	blockIdx = {1, 1, 0};
	threadIdx = {1, 2, 1};

	// Block rank: 1 + 1 * 2 + 0 = 3
	// Block size: 2 * 3 * 4 = 24
	// Thread rank in block: 1 + 2 * 2 + 1 * (2 * 3) = 1 + 4 + 6 = 11
	// Grid rank: 3 * 24 + 11 = 83
	// Grid size: 24 * (2 * 2 * 1) = 96

	auto tb = cooperative_groups::this_thread_block();
	REQUIRE(tb.thread_rank() == 11);
	REQUIRE(tb.num_threads() == 24);
	REQUIRE(tb.size() == 24);
	REQUIRE(tb.group_index().x == 1);
	REQUIRE(tb.group_index().y == 1);
	REQUIRE(tb.thread_index().x == 1);
	REQUIRE(tb.thread_index().y == 2);
	REQUIRE(tb.thread_index().z == 1);
	REQUIRE(tb.group_dim().x == 2);
	REQUIRE(tb.dim_threads().x == 2);

	// Also validate static member function access per CUDA spec
	REQUIRE(cooperative_groups::thread_block::thread_rank() == 11);
	REQUIRE(cooperative_groups::thread_block::num_threads() == 24);
	REQUIRE(cooperative_groups::thread_block::size() == 24);

	auto gg = cooperative_groups::this_grid();
	REQUIRE(gg.thread_rank() == 83);
	REQUIRE(gg.num_threads() == 96);
	REQUIRE(gg.size() == 96);
	REQUIRE(gg.is_valid());
	REQUIRE(gg.num_blocks() == 4);
	REQUIRE(gg.block_rank() == 3);
	REQUIRE(gg.dim_blocks().x == 2);
	REQUIRE(gg.dim_blocks().y == 2);
	REQUIRE(gg.dim_threads().x == 4); // 2 blocks * 2 threads
	REQUIRE(gg.dim_threads().y == 6); // 2 blocks * 3 threads
	REQUIRE(gg.dim_threads().z == 4); // 1 block * 4 threads

	// Also validate static member function access per CUDA spec
	REQUIRE(cooperative_groups::grid_group::thread_rank() == 83);
	REQUIRE(cooperative_groups::grid_group::num_threads() == 96);
	REQUIRE(cooperative_groups::grid_group::size() == 96);

	auto s = noarr::scalar<int>() ^ noarr::array<'x', 200>();

	auto stepped_b = s ^ noarr::cuda_step_block<'x'>();
	REQUIRE((stepped_b | noarr::offset<'x'>(0)) == 11 * sizeof(int));
	REQUIRE((stepped_b | noarr::offset<'x'>(1)) == (11 + 24) * sizeof(int));

	auto stepped_g = s ^ noarr::cuda_step_grid<'x'>();
	REQUIRE((stepped_g | noarr::offset<'x'>(0)) == 83 * sizeof(int));
	REQUIRE((stepped_g | noarr::offset<'x'>(1)) == (83 + 96) * sizeof(int));
}

namespace {

struct custom_group_instance {
	[[nodiscard]] unsigned int thread_rank() const noexcept { return 3; }
	[[nodiscard]] unsigned int num_threads() const noexcept { return 7; }
};

struct custom_group_static {
	[[nodiscard]] static unsigned int thread_rank() noexcept { return 2; }
	[[nodiscard]] static unsigned int num_threads() noexcept { return 5; }
};

} // namespace

TEST_CASE("CUDA step - custom instance group", "[cuda]") {
	auto s = noarr::scalar<int>() ^ noarr::array<'x', 30>();

	custom_group_instance cg;
	auto stepped1 = s ^ noarr::cuda_step<'x'>(cg);
	auto stepped2 = s ^ noarr::cuda_step(cg);

	REQUIRE((stepped1 | noarr::offset<'x'>(0)) == 3 * sizeof(int));
	REQUIRE((stepped1 | noarr::offset<'x'>(1)) == 10 * sizeof(int));
	REQUIRE((stepped2 | noarr::offset<'x'>(0)) == 3 * sizeof(int));
	REQUIRE((stepped2 | noarr::offset<'x'>(1)) == 10 * sizeof(int));
}

TEST_CASE("CUDA step - custom static group type", "[cuda]") {
	auto s = noarr::scalar<int>() ^ noarr::array<'x', 30>();

	auto stepped1 = s ^ noarr::cuda_step<'x', custom_group_static>();
	auto stepped2 = s ^ noarr::cuda_step<custom_group_static>();

	REQUIRE((stepped1 | noarr::offset<'x'>(0)) == 2 * sizeof(int));
	REQUIRE((stepped1 | noarr::offset<'x'>(1)) == 7 * sizeof(int));
	REQUIRE((stepped2 | noarr::offset<'x'>(0)) == 2 * sizeof(int));
	REQUIRE((stepped2 | noarr::offset<'x'>(1)) == 7 * sizeof(int));
}

TEST_CASE("CUDA step - multidimensional structure inner dim and traverser", "[cuda]") {
	blockDim = {4, 1, 1};
	threadIdx = {2, 0, 0};

	auto matrix = noarr::scalar<int>() ^ noarr::array<'y', 5>() ^ noarr::array<'x', 16>();

	auto stepped = matrix ^ noarr::cuda_step_block<'x'>();
	REQUIRE((stepped | noarr::get_length<'y'>()) == 5);
	REQUIRE((stepped | noarr::get_length<'x'>()) == 4);

	std::vector<std::size_t> visited_x;
	noarr::traverser(matrix).order(noarr::cuda_step_block<'x'>()).for_dims<'x'>([&](auto trav) {
		visited_x.push_back(trav.state().template get<noarr::index_in<'x'>>());
	});

	REQUIRE(visited_x == std::vector<std::size_t>{2, 6, 10, 14});
}

TEST_CASE("CUDA step - multidimensional structure outer dim by default", "[cuda]") {
	blockDim = {4, 1, 1};
	threadIdx = {2, 0, 0};

	auto matrix = noarr::scalar<int>() ^ noarr::array<'y', 5>() ^ noarr::array<'x', 16>();

	auto stepped = matrix ^ noarr::cuda_step_block();
	REQUIRE((stepped | noarr::get_length<'y'>()) == 5);
	REQUIRE((stepped | noarr::get_length<'x'>()) == 4); // outer dimension 'x', length 16, thread 2 of 4 -> indices 2, 6, 10, 14

	std::vector<std::size_t> visited_x;
	noarr::traverser(matrix).order(noarr::cuda_step_block()).for_dims<'x'>([&](auto trav) {
		visited_x.push_back(trav.state().template get<noarr::index_in<'x'>>());
	});

	REQUIRE(visited_x == std::vector<std::size_t>{2, 6, 10, 14});
}

TEST_CASE("CUDA step - with merge_blocks and traverser", "[cuda]") {
	auto matrix = noarr::scalar<int>() ^ noarr::array<'y', 5>() ^ noarr::array<'x', 16>();

	gridDim = {2, 1, 1};
	blockDim = {4, 1, 1};
	blockIdx = {1, 0, 0};
	threadIdx = {1, 0, 0};
	// Grid rank: 1 * 4 + 1 = 5, total threads = 8

	std::vector<std::size_t> visited_t;
	noarr::traverser(matrix).order(
		noarr::merge_blocks<'y', 'x', 't'>() ^ noarr::cuda_step_grid<'t'>()
	).for_dims<'t'>([&](auto trav) {
		const auto y = trav.state().template get<noarr::index_in<'y'>>();
		const auto x = trav.state().template get<noarr::index_in<'x'>>();
		visited_t.push_back(y * 16 + x);
	});

	// Total size is 5 * 16 = 80. Rank 5, step 8 -> indices: 5, 13, 21, 29, 37, 45, 53, 61, 69, 77
	REQUIRE(visited_t == std::vector<std::size_t>{5, 13, 21, 29, 37, 45, 53, 61, 69, 77});
}
