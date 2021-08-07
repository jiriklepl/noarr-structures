#include <iostream>
#include <array>
#include <utility>
#include <vector>
#include <cassert>
#include <tuple>
#include <iostream>
#include <string>

// IMPORTANT:
// raw c++ matrix implementation is from now on a "classic matrix"
// matrix implementation in noarr will be referecend as "noarr matrix"
// whole example assumes int matrices

// definion of z-curve data stricture
#include "z_curve.hpp"
// definitions of basic matrix functions: matrix multiplication, scalar multiplication, copy and matrix transpose
#include "noarr_matrix_functions.hpp"

// definitions of noarr layouts
using matrix_rows = noarr::vector<'n', noarr::vector<'m', noarr::scalar<int>>>;
using matrix_columns = noarr::vector<'n', noarr::vector<'m', noarr::scalar<int>>>;
using matrix_zcurve = noarr::z_curve<'n', 'm', noarr::sized_vector<'a', noarr::scalar<int>>>;

// raw c++ structure, which implements matrix ("classic matrix")
struct classic_matrix
{
	// constructors
	classic_matrix(int X, int Y, std::vector<int>&& Ary) : n(X), m(Y), ary(std::move(Ary)) {};
	classic_matrix(int X, int Y, std::vector<int>& Ary) : n(X), m(Y), ary(Ary) {};

	// width
	int n;
	// heigth
	int m;
	// data vector (flattened matrix by rows into std::vector<int>)
	std::vector<int> ary;

	// element access functions (returns reference into data vector based on input indexes)
	int& at(int n_, int m_) { return ary[n_ + m_ * n]; }
	const int& at(int n_, int m_) const { return ary[n_ + m_ * n]; }

	// printing function which prints the whole matrix into the standard output
	// it takes the name parameter and prints it at the beginning to make input clearer
	void print(std::string name)
	{
		std::cout << name << ":" << std::endl;

		for (int i = 0; i < n; i++)
		{
			for (int j = 0; j < m; j++)
				std::cout << at(i, j) << " ";

			std::cout << std::endl;
		}

		std::cout << std::endl;
	}
};

// function returng random classic matrix with values in range [0 to 9] with size n x m. 
classic_matrix get_clasic_matrix(int n, int m)
{
	// data container inicialization
	const int length = n * m;
	std::vector<int> ary;

	// random values generation
	for (int i = 0; i < length; i++)
		ary.push_back(rand() % 10);

	// matrix construction
	return classic_matrix(n, m, std::move(ary));
}

// function which compares two classic matrices for value equality
bool are_equal_classic_matrices(classic_matrix& m1, classic_matrix& m2)
{
	// n size must be the same
	if (m1.n != m2.n)
		return false;

	// m size must be the same
	if (m1.m != m2.m)
		return false;

	// data container has to be the same size, we will compare values for equality
	const int length = m1.n * m1.m;
	for (int i = 0; i < length; i++)
		if (m1.ary[i] != m2.ary[i])
			return false;

	// if all tests were passed up until now the matrices have to be value-equal
	return true;
}

// function converting noarr matrix to classic matrix
// it takes source noarr matrix as argument
template<typename Structure>
classic_matrix noarr_matrix_to_clasic(noarr::bag<Structure>& source)
{
	// we will cache matrix size values
	int n_size = source.structure().template get_length<'n'>();
	int m_size = source.structure().template get_length<'m'>();

	// we will allocate target classic matrix
	classic_matrix target = get_clasic_matrix(n_size, m_size);

	// we will go through the matrix and copy noarr matrix into a classic matrix
	for (int i = 0; i < n_size; i++)
		for (int j = 0; j < m_size; j++)
			target.at(i, j) = source.template at<'n', 'm'>(i, j);

	return target;
}

// function converting classic matrix to noarr matrix
// it takes source classic matrix and target noarr matrix structure (we need to know what structure should be used)
template<typename Structure>
noarr::bag<Structure> clasic_matrix_to_noarr(classic_matrix& source, Structure structure)
{
	// we will allocate target noarr matrix
	auto target = noarr::bag(structure);

	// we will go through the classic matrix and copy it into noarr the matrix
	for (int i = 0; i < source.n; i++)
		for (int j = 0; j < source.m; j++)
			target.template at<'n', 'm'>(i, j) = source.at(i, j);

	return target;
}

// function multiplying classic matrices
// it takes 2 source classic matrices and returns multiplied matrix
classic_matrix clasic_matrix_multiply(classic_matrix& matrix1, classic_matrix& matrix2)
{
	// some of the sizes have to be equal
	assert(matrix1.n == matrix2.m);

	// we will allocate result classic matrix
	classic_matrix result = get_clasic_matrix(matrix2.n, matrix1.m);

	// standart matrix multiplication
	for (int i = 0; i < matrix2.n; i++)
		for (int j = 0; j < matrix1.m; j++)
		{
			int sum = 0;

			for (int k = 0; k < matrix1.n; k++)
				sum += matrix1.at(k, j) * matrix2.at(i, k);

			result.at(i, j) = sum;
		}

	return result;
}

// !core function of the example!
// it multiplies classic matrices, same noarr matrices, and checks if the results produced are equal
template<typename Structure>
void matrix_demo(int size, Structure structure)
{
	// generating random classic matrix 1
	classic_matrix classic_1 = get_clasic_matrix(size, size);
	classic_1.print("Matrix 1");

	// generating random classic matrix 2
	classic_matrix classic_2 = get_clasic_matrix(size, size);
	classic_2.print("Matrix 2");

	// copying 2 classic matrices to 2 noarr matrices
	auto noarr_1 = clasic_matrix_to_noarr(classic_1, structure);
	auto noarr_2 = clasic_matrix_to_noarr(classic_2, structure);

	// multiplying 2 classic matrices, result is classic matrix
	classic_matrix classic_result = clasic_matrix_multiply(classic_1, classic_2);
	classic_result.print("Classic multiplication result");

	// multiplying 2 noarr matrices, result is noarr matrix
	auto noarr_result = noarr_matrix_multiply(noarr_1, noarr_2, structure);

	// converting noarr result matrix into a classic matrix
	classic_matrix classic_noarr_result = noarr_matrix_to_clasic(noarr_result);
	classic_noarr_result.print("Noarr multiplication result");

	// check if noarr returned correct result
	assert(are_equal_classic_matrices(classic_result, classic_noarr_result));
}

// print help function
void print_help_and_exit()
{
	std::cout << "Programm takes 2 parameters. First, you choose one of the following layouts:" << std::endl;
	std::cout << "1) rows" << std::endl;
	std::cout << "2) columns" << std::endl;
	std::cout << "3) z_curve (the size has to be a power of 2)" << std::endl;
	std::cout << "Then you input integer matrix size. The size of the matrix have to be at least one (for example simplicity, only square matrices are supported)" << std::endl;

	// exit the program
	exit(1);
}

// main function called from command line
int main(int argc, char* argv[])
{
	// there have to be two arguments
	if (argc != 2)
		print_help_and_exit();

	// parse the second argument (size) into int
	int size;
	try {
		size = std::stoi(argv[1]);
	}
	catch (...) {
		print_help_and_exit();
	}

	// size has to be at least 1
	if (size < 1)
		print_help_and_exit();

	// if the first argument matches some of the supported layouts, run the example, otherwise print help
	if (!strcmp(argv[0], "rows"))
		matrix_demo(size, matrix_rows() | noarr::set_length<'n'>(size) | noarr::set_length<'m'>(size));
	else if (!strcmp(argv[0], "columns"))
		matrix_demo(size, matrix_columns() | noarr::set_length<'n'>(size) | noarr::set_length<'m'>(size));
	else if (!strcmp(argv[0], "z_curve"))
		matrix_demo(size, matrix_zcurve(noarr::sized_vector<'a', noarr::scalar<int>>(noarr::scalar<int>(), size * size), noarr::helpers::z_curve_bottom<'n'>(size), noarr::helpers::z_curve_bottom<'m'>(size)));
	else
		print_help_and_exit();

	return 0;
}
