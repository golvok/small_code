
#include <armadillo>

#include <iostream>
#include <vector>

int main(int argc, char** argv) {
	if (argc != 2) {
		std::cerr << "expects one argement, the data file\n";
		return -1;
	}

	const double learning_rate = 0.01;
	const size_t num_iters = 400;

	const char* const data_filename = argv[1];

	arma::mat data;
	data.load(data_filename, arma::auto_detect);

	// std::cout << "data = \n" << data;

	const size_t num_data_points = data.n_rows;
	const size_t num_features = data.n_cols-1;

	const arma::vec y = data.col(num_features); // last col is y values
	arma::mat X = join_horiz(
		arma::ones(num_data_points), // add first col of ones
		data.cols(0, num_features-1) // all except last col are th x values
	);

	// normalize, but not first col
	for (size_t col_index = 1; col_index < X.n_cols; ++col_index) {
		auto col_expr = X.col(col_index);
		auto mean = arma::mean(col_expr);
		auto stddev = arma::stddev(col_expr);
		col_expr = (col_expr-mean)/stddev;
	}

	// std::cout << "y =\n" << y;
	// std::cout << "X =\n" << X;

	arma::vec weights(num_features + 1); // + 1 for intercept feature
	weights.zeros();
	for (size_t iter = 0; iter < num_iters; ++iter) {
		weights = weights - (learning_rate/num_data_points)*trans(X)*(X*weights - y);
	}

	std::cout << "final weights = \n" << weights;

}