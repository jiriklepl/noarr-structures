#include <cmath>
#include <cstdint>

#include <iomanip>
#include <iostream>
#include <string>
#include <vector>

#include <noarr/noarr.hpp>

// =============================================================================
// Layout Definitions
// =============================================================================
// 2D Grayscale image layout (width 'x', height 'y')
template<class PixelType = std::uint8_t>
auto make_image_layout(std::size_t width, std::size_t height) {
	return noarr::scalar<PixelType>() ^ noarr::vector<'x'>(width) ^ noarr::vector<'y'>(height);
}

// 1D Histogram layout with 'b' bins
template<class CountType = std::size_t>
auto make_histogram_layout(std::size_t num_bins) {
	return noarr::scalar<CountType>() ^ noarr::vector<'b'>(num_bins);
}

// =============================================================================
// Histogram Computation
// =============================================================================
template<class ImageBag, class HistBag>
void compute_histogram(const ImageBag &image, HistBag &hist, std::size_t bin_range) {
	// Zero-initialize the histogram bins
	noarr::traverser(hist).for_each([&](auto s) { hist[s] = 0; });

	// Traverse every pixel in the 2D image and accumulate into histogram
	noarr::traverser(image).for_each([&](auto s) {
		auto pixel_val = image[s];
		std::size_t bin = static_cast<std::size_t>(pixel_val) / bin_range;
		if (bin < (hist | noarr::get_length<'b'>())) {
			hist[noarr::idx<'b'>(bin)]++;
		}
	});
}

// =============================================================================
// ASCII Bar Chart Printer
// =============================================================================
template<class HistBag>
void print_histogram(const HistBag &hist, std::size_t bin_range) {
	std::size_t num_bins = (hist | noarr::get_length<'b'>());
	std::size_t max_count = 0;

	noarr::traverser(hist).for_each([&](auto s) {
		if (hist[s] > max_count) {
			max_count = hist[s];
		}
	});

	std::cout << "Histogram (" << num_bins << " bins, max count = " << max_count << "):\n";
	std::cout << "------------------------------------------------------------\n";

	const std::size_t max_bar_width = 40;
	noarr::traverser(hist).for_each([&](auto s) {
		auto bin_idx = noarr::get_index<'b'>(s);
		std::size_t count = hist[s];
		std::size_t bar_len = (max_count > 0) ? (count * max_bar_width / max_count) : 0;

		std::size_t range_low = bin_idx * bin_range;
		std::size_t range_high = range_low + bin_range - 1;

		std::cout << "[" << std::setw(3) << range_low << ".." << std::setw(3) << range_high << "] " << std::setw(5)
		          << count << " | " << std::string(bar_len, '#') << "\n";
	});
	std::cout << "\n";
}

int main(int argc, char *argv[]) {
	std::size_t width = 32;
	std::size_t height = 32;
	std::size_t num_bins = 8;

	if (argc >= 2) {
		try {
			width = height = std::stoul(argv[1]);
		} catch (...) {
			width = height = 32;
		}
	}

	std::cout << "Running Noarr Histogram Example (" << width << "x" << height << " image)\n";
	std::cout << "============================================================\n\n";

	auto image = noarr::bag(make_image_layout(width, height));
	auto hist = noarr::bag(make_histogram_layout(num_bins));

	// Generate synthetic pixel data (e.g. radial gradient centered in the image)
	float cx = static_cast<float>(width) / 2.0f;
	float cy = static_cast<float>(height) / 2.0f;
	float max_dist = std::hypot(cx, cy);

	noarr::traverser(image).for_each([&](auto s) {
		auto [x, y] = noarr::get_indices<'x', 'y'>(s);
		float dist = std::hypot(static_cast<float>(x) - cx, static_cast<float>(y) - cy);
		float normalized = (max_dist > 0.0f) ? (dist / max_dist) : 0.0f;
		image[s] = static_cast<std::uint8_t>(normalized * 255.0f);
	});

	std::size_t bin_range = 256 / num_bins;
	compute_histogram(image, hist, bin_range);
	print_histogram(hist, bin_range);
}
