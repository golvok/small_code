#include "common.h"

const size_t END_NUMBER = 20;

int main () {
	std::vector<size_t> primes{};
	{
		std::vector<bool> is_prime(END_NUMBER+1, true);
		is_prime[0] = false;
		is_prime[1] = false;
		for (size_t i = 2; i < is_prime.size(); ++i) {
			if (is_prime[i] == true) {
				primes.push_back(i);
				for (size_t j = i*2; j < is_prime.size(); j += i) {
					is_prime[j] = false;
				}
			}
		}
	}

	std::cout << "primes: ";
	for (size_t prime : primes) {
		std::cout << prime << ' ';
	}
	std::cout << '\n';

	std::vector<size_t> greatest_prime_factor_counts(END_NUMBER+1, 0);

	for (size_t i = 2; i < END_NUMBER+1; ++i) {
		std::cout << i << " : {  ";
		size_t current_num = i;
		for (size_t prime : primes) {
			size_t current_prime_factor_count = 0;
			while (current_num % prime == 0) {
				current_num /= prime;
				current_prime_factor_count += 1;
			}
			std::cout << prime << ':' << current_prime_factor_count; 
			if (current_prime_factor_count > greatest_prime_factor_counts[prime]) {
				std::cout << '^';
				greatest_prime_factor_counts[prime] = current_prime_factor_count;
			} else { std::cout << ' '; }
			std::cout << ' ';
		}
		std::cout << "}\n";
	}

	std::cout << "final : { ";

	size_t result = 1;
	for (size_t i = 2; i < greatest_prime_factor_counts.size(); ++i) {
		std::cout << i << ':' << greatest_prime_factor_counts[i] << ' ';
		if (greatest_prime_factor_counts[i] != 0) {
			for (size_t j = 0; j < greatest_prime_factor_counts[i]; ++j) {
				result *= i;
			}
		}
	}

	std::cout << "}\n";

	std::cout << "smallest multiple = " << result << std::endl;
} 