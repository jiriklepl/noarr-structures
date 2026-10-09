#include <cassert>

#include <iomanip>
#include <iostream>
#include <string>
#include <string_view>

#include <noarr/noarr.hpp>
#include <noarr/structures/extra/traverser.hpp>

// =============================================================================
// Matrix Layout Definitions
// =============================================================================
// In Noarr, composition operator (^) wraps structures from inside to outside.
// The outermost dimension in the pipeline is the slowest varying (outer) index.

// Row-major: 'j' (columns) is innermost (stride 1), 'i' (rows) is outer.
template<class T>
auto make_row_major_layout(std::size_t rows, std::size_t cols) {
	return noarr::scalar<T>() ^ noarr::vector<'j'>(cols) ^ noarr::vector<'i'>(rows);
}

// Column-major: 'i' (rows) is innermost (stride 1), 'j' (columns) is outer.
template<class T>
auto make_col_major_layout(std::size_t rows, std::size_t cols) {
	// using an alternative, more verbose syntax to demonstrate the flexibility:
	return noarr::scalar<T>() ^ noarr::vector<'i'>() ^ noarr::vector<'j'>() ^ noarr::set_length<'i'>(rows) ^
	       noarr::set_length<'j'>(cols);
}

// =============================================================================
// Layout-Agnostic Matrix Printing
// =============================================================================
template<class Bag>
void print_matrix(std::string_view name, const Bag &matrix) {
	auto rows = matrix | noarr::get_length<'i'>();
	auto cols = matrix | noarr::get_length<'j'>();
	std::cout << name << " (" << rows << "x" << cols << "):\n";

	// Fix the row dimension 'i' and iterate over column dimension 'j'
	noarr::traverser(matrix).template for_dims<'i'>([&](auto row) {
		row.for_each([&](auto state) { std::cout << std::setw(6) << matrix[state] << " "; });
		std::cout << "\n";
	});
	std::cout << "\n";
}

// =============================================================================
// Zero-Copy Transposed View
// =============================================================================
template<class Bag>
auto make_transposed_view(const Bag &matrix) {
	// Reassigns dimension roles ('i' <-> 'j') without copying memory.
	return matrix.get_ref() ^ noarr::rename<'i', 'j', 'j', 'i'>();

	// Alternative, more flexible and verbose approach using explicit bag construction (commented out):
	// return noarr::bag(matrix.structure() ^
	//     noarr::rename<'i', 't'>() ^ noarr::rename<'j', 'i'>() ^ noarr::rename<'t', 'j'>(), matrix.data());
}

// =============================================================================
// Layout-Agnostic Matrix Multiplication (GEMM)
// =============================================================================
// Multiplies matrix A by matrix B and stores the result in C.
// A has dimensions ('i', 'j'), B has dimensions ('i', 'j'), C has ('i', 'j').
// The contracted inner dimension is renamed to 'k'.
//
// This single implementation works identically whether A, B, and C are
// row-major, column-major, or any custom layout.
template<class BagA, class BagB, class BagC>
void matrix_multiply(const BagA &A, const BagB &B, BagC &C) {
	assert((A | noarr::get_length<'j'>()) == (B | noarr::get_length<'i'>()));
	assert((C | noarr::get_length<'i'>()) == (A | noarr::get_length<'i'>()));
	assert((C | noarr::get_length<'j'>()) == (B | noarr::get_length<'j'>()));

	// Create views renaming the contracting dimensions to 'k'
	auto A_k = A.get_ref() ^ noarr::rename<'j', 'k'>();
	auto B_k = B.get_ref() ^ noarr::rename<'i', 'k'>();

	// Alternative, more flexible approach using explicit bag construction (commented out):
	// auto A_k = noarr::bag(A.structure() ^ noarr::rename<'j', 'k'>(), A.data());
	// auto B_k = noarr::bag(B.structure() ^ noarr::rename<'i', 'k'>(), B.data());

	// Zero out accumulator C
	noarr::traverser(C).for_each([&](auto state) { C[state] = 0; });

	// Traverser joins dimensions 'i', 'j', and 'k' across all three structures.
	// For each (i, j), iterate over k and accumulate: C(i, j) += A(i, k) * B(k, j)
	noarr::traverser(A_k, B_k, C).template for_dims<'i', 'j'>([&](auto inner) {
		inner.for_each([&](auto state) { C[state] += A_k[state] * B_k[state]; });
	});
}

// =============================================================================
// Main Demo
// =============================================================================
void run_demo(std::size_t size, bool use_col_major_c = false) {
	std::cout << "Running Noarr Matrix Example (size " << size << "x" << size << ")\n";
	std::cout << "------------------------------------------------------------\n";

	// 1. Create matrix A (Row-major)
	auto A = noarr::bag(make_row_major_layout<int>(size, size));
	noarr::traverser(A).for_each([&](auto state) {
		auto [i, j] = noarr::get_indices<'i', 'j'>(state);
		A[state] = static_cast<int>(i + 2 * j + 1);
	});
	print_matrix("Matrix A (Row-Major)", A);

	// 2. Create matrix B as an Identity matrix (Column-major)
	auto B = noarr::bag(make_col_major_layout<int>(size, size));
	noarr::traverser(B).for_each([&](auto state) {
		auto [i, j] = noarr::get_indices<'i', 'j'>(state);
		B[state] = (i == j) ? 1 : 0;
	});
	print_matrix("Matrix B (Identity, Column-Major)", B);

	// 3. Helper to run multiplication, display, and validation
	auto run_with_c = [&](auto C, std::string_view c_name) {
		matrix_multiply(A, B, C);
		print_matrix(c_name, C);

		// Validate C == A (since B is Identity)
		bool ok = true;
		noarr::traverser(A, C).for_each([&](auto state) {
			if (A[state] != C[state]) {
				ok = false;
			}
		});
		assert(ok && "Validation failed: A * Identity != A");
		std::cout << "Validation successful: A * B == A (" << c_name << ")\n\n";
	};

	if (use_col_major_c) {
		run_with_c(noarr::bag(make_col_major_layout<int>(size, size)), "Matrix C = A * B (Column-Major)");
	} else {
		run_with_c(noarr::bag(make_row_major_layout<int>(size, size)), "Matrix C = A * B (Row-Major)");
	}

	// 5. Demonstrate zero-copy transposed view
	auto A_T = make_transposed_view(A);
	print_matrix("Matrix A^T (Transposed View of A, Zero-Copy)", A_T);
}

int main(int argc, char *argv[]) {
	std::size_t size = 4;
	bool col_major = false;

	// Support both legacy syntax ("rows 10", "columns 10") and direct size argument
	if (argc >= 3) {
		std::string_view layout = argv[1];
		col_major = (layout == "columns");
		try {
			size = std::stoul(argv[2]);
		} catch (...) {
			size = 4;
		}
	} else if (argc == 2) {
		try {
			size = std::stoul(argv[1]);
		} catch (...) {
			size = 4;
		}
	}

	if (size < 1) {
		size = 1;
	}

	run_demo(size, col_major);
}
