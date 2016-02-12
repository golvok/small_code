
#include "../C++_experiments/generator.h++"

#include <plplot/plstream.h>

#include <algorithm>
#include <cassert>
#include <iostream>
#include <memory>
#include <random>
#include <thread>
#include <vector>

using OrdinateList = std::vector<double>;
using MeanCount = long unsigned int;

template<typename Integral>
void plot(
	const std::vector<Integral>& xes, const std::vector<Integral>& yes,
	const std::string& xlabel, const std::string& ylabel,
	const std::string& plot_label
) {
	std::thread plot_thread([=](){
		const auto data_size = xes.size();

		assert(xes.size() == yes.size());
		auto pls = std::make_unique<plstream>();

		// Initialize plplot
		pls->sdev("xcairo");
		pls->init();

		pls->env(
			*std::min_element(xes.begin(), xes.end()),
			*std::max_element(xes.begin(), xes.end()),
			*std::min_element(yes.begin(), yes.end()),
			*std::max_element(yes.begin(), yes.end()),
			0,
			0
		);
		pls->lab(xlabel.c_str(), ylabel.c_str(), plot_label.c_str());

		pls->poin(data_size, xes.data(), yes.data(), 2);
	});

	plot_thread.detach();
}

void plot_means_and_points(
	const OrdinateList& xes,
	const OrdinateList& yes,
	const OrdinateList& mean_xes,
	const OrdinateList& mean_yes
) {
	std::thread plot_thread([=](){
		const auto data_size = xes.size();

		assert(xes.size() == yes.size());
		auto pls = std::make_unique<plstream>();

		// Initialize plplot
		pls->sdev("xcairo");
		pls->init();

		pls->env(
			*std::min_element(xes.begin(), xes.end()),
			*std::max_element(xes.begin(), xes.end()),
			*std::min_element(yes.begin(), yes.end()),
			*std::max_element(yes.begin(), yes.end()),
			0,
			0
		);
		pls->lab("x", "y", "with means");

		pls->poin(data_size, xes.data(), yes.data(), 2);

		pls->col0(2); // yellow
		pls->poin(mean_xes.size(), mean_xes.data(), mean_yes.data(), 3);
	});

	plot_thread.detach();
}

std::pair<OrdinateList,OrdinateList> k_means_clustering(
	const OrdinateList& xes,
	const OrdinateList& yes,
	OrdinateList mean_xes,
	OrdinateList mean_yes
) {
	while (true) {
		std::vector<double> closest_sums_x(mean_xes.size(), 0);
		std::vector<double> closest_sums_y(mean_xes.size(), 0);
		std::vector<MeanCount> point_count(mean_xes.size(), 0);

		for (auto point_index : xrange<size_t>((size_t)0, xes.size()-1)) {
			size_t closest_mean = -1;
			double best_distance = std::numeric_limits<double>::max();
			for (auto mean_index : xrange<size_t>((size_t)0, mean_xes.size()-1)) {
				double distance = (
					  std::pow(xes[point_index] - mean_xes[mean_index], 2)
					+ std::pow(yes[point_index] - mean_yes[mean_index], 2)
				);
				if (distance < best_distance) {
					closest_mean = mean_index;
					best_distance = distance;
				}
			}
			assert(closest_mean != (size_t)-1);
			closest_sums_x[closest_mean] += xes[point_index];
			closest_sums_y[closest_mean] += yes[point_index];
			point_count[closest_mean] += 1;
		}

		double total_diff = 0;

		std::cout << mean_xes.size() << "\n";
		for (auto mean_index : xrange<size_t>((size_t)0, mean_xes.size()-1)) {
			std::cout << point_count[mean_index] << ", ";
			if (point_count[mean_index] == 0) {
				continue; // no divide by zero
			}

			auto new_mean_x = closest_sums_x[mean_index]/point_count[mean_index];
			auto new_mean_y = closest_sums_y[mean_index]/point_count[mean_index];
			total_diff += std::abs(mean_xes[mean_index] - new_mean_x) + std::abs(mean_yes[mean_index] - new_mean_y);
			mean_xes[mean_index] = new_mean_x;
			mean_yes[mean_index] = new_mean_y;
		}

		std::cout << "\ntd=" << total_diff << '\n';
		plot_means_and_points(xes, yes, mean_xes, mean_yes);
		getchar();

		if (total_diff < 0.01) {
			break;
		}

	}

	return {std::move(mean_xes), std::move(mean_yes)};
}

int main(int argc, char** argv) {
	if (argc != 2 && argc != 3) {
		std::cerr
			<< "expects one or two argements.\n"
			<< "usage: " << argv[0] << " min_means [max_means]\n"
		;
		return -1;
	}

	struct Point {
		double x; double y;
	};

	const MeanCount min_means = [&]() {
		const long int input = std::stoi(argv[1]);
		assert(input > 0);
		return input;
	}();

	const MeanCount max_means = [&]() {
		const long int input = ( argc == 3 ) ? ( std::stoi(argv[2]) ) : ( min_means );
		assert(input > 0);
		assert(input >= (int)min_means);
		return input;
	}();

	const int num_data_points = 100;
	const double min_x = 0;
	const double max_x = 100;
	const double min_y = 0;
	const double max_y = 100;

	OrdinateList xes;
	OrdinateList yes;

	std::mt19937 gen(std::random_device{}());
	std::uniform_real_distribution<double> xdist(min_x, max_x);
	std::uniform_real_distribution<double> ydist(min_y, max_y);

	for (auto i : xrange<size_t>(1,num_data_points)) {
		(void)i;
		xes.push_back(xdist(gen));
		yes.push_back(ydist(gen));
	}

	plot(xes, yes, "x", "y", "plot");
	getchar();

	for (auto mean_count : xrange<MeanCount>(min_means, max_means)) {
		OrdinateList mean_xes;
		OrdinateList mean_yes;
		for (auto i : xrange<size_t>(1,(int)mean_count)) {
			(void)i;
			mean_xes.push_back(xdist(gen));
			mean_yes.push_back(ydist(gen));

		}

		auto new_means = k_means_clustering(xes, yes, mean_xes, mean_yes);
		(void)new_means;
	}

}
