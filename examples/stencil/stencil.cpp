#include <iomanip>
#include <iostream>
#include <string>
#include <string_view>

#include <noarr/noarr.hpp>

// =============================================================================
// Layout Definitions
// =============================================================================
// 2D Grid with dimensions 'x' and 'y'
template<class T>
auto make_grid_layout(std::size_t width, std::size_t height) {
	return noarr::scalar<T>() ^ noarr::vector<'x'>(width) ^ noarr::vector<'y'>(height);
}

// =============================================================================
// Grid Printing Helper
// =============================================================================
template<class GridBag>
void print_grid(std::string_view name, const GridBag &grid) {
	auto width = grid | noarr::get_length<'x'>();
	auto height = grid | noarr::get_length<'y'>();
	std::cout << name << " (" << width << "x" << height << "):\n";

	noarr::traverser(grid).template for_dims<'y'>([&](auto row) {
		row.for_each(
			[&](auto s) { std::cout << std::fixed << std::setprecision(1) << std::setw(6) << grid[s] << " "; });
		std::cout << "\n";
	});
	std::cout << "\n";
}

// =============================================================================
// 2D 5-Point Stencil (Heat Diffusion / 5-point Laplacian smoothing)
// =============================================================================
// Applies one step of smoothing:
// output(x, y) = 0.25 * (input(x-1, y) + input(x+1, y) + input(x, y-1) + input(x, y+1))
// for all interior cells 1 <= x < width-1, 1 <= y < height-1.
template<class GridBagIn, class GridBagOut>
void apply_stencil_step(const GridBagIn &in, GridBagOut &out) {
	std::size_t width = (in | noarr::get_length<'x'>());
	std::size_t height = (in | noarr::get_length<'y'>());

	// Copy boundaries from input to output
	noarr::traverser(out).for_each([&](auto s) {
		auto [x, y] = noarr::get_indices<'x', 'y'>(s);
		if (x == 0 || x == width - 1 || y == 0 || y == height - 1) {
			out[s] = in[s];
		}
	});

	// Define a slice for the interior (excluding 1-pixel boundary)
	auto make_interior = noarr::slice<'x'>(1, width - 2) ^ noarr::slice<'y'>(1, height - 2);
	auto in_view = in.get_ref() ^ make_interior;
	auto out_view = out.get_ref() ^ make_interior;

	// Traverse the interior grid
	noarr::traverser(in_view).for_each([&](auto s) {
		// Inside the slice, indices start at 0; add 1 to get original coordinates
		float left = in_view[s - noarr::idx<'x'>(1)];
		float right = in_view[s + noarr::idx<'x'>(1)];
		float top = in_view[s - noarr::idx<'y'>(1)];
		float bottom = in_view[s + noarr::idx<'y'>(1)];

		out_view[s] = 0.25f * (left + right + top + bottom);
	});
}

int main(int argc, char *argv[]) {
	std::size_t size = 7;
	std::size_t steps = 3;

	if (argc >= 2) {
		try {
			size = std::stoul(argv[1]);
		} catch (...) {
			size = 7;
		}
	}
	if (argc >= 3) {
		try {
			steps = std::stoul(argv[2]);
		} catch (...) {
			steps = 3;
		}
	}

	std::cout << "Running Noarr 2D Stencil Example (" << size << "x" << size << ", " << steps << " steps)\n";
	std::cout << "============================================================\n\n";

	auto current = noarr::bag(make_grid_layout<float>(size, size));
	auto next = noarr::bag(make_grid_layout<float>(size, size));

	// Initialize: boundary heat source at top edge (100.0), 0.0 elsewhere
	noarr::traverser(current).for_each([&](auto s) {
		auto [x, y] = noarr::get_indices<'x', 'y'>(s);
		current[s] = (y == 0) ? 100.0f : 0.0f;
	});

	print_grid("Initial State (Heat at top boundary)", current);

	for (std::size_t step = 1; step <= steps; ++step) {
		apply_stencil_step(current, next);

		// Swap data between steps
		noarr::traverser(current, next).for_each([&](auto s) { current[s] = next[s]; });

		std::cout << "--- After Step " << step << " ---\n";
		print_grid("Current Grid", current);
	}
}
