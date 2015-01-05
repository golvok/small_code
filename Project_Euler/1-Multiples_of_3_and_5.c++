#include "common.h"

const size_t BOUNDED_BY = 1000;

int main() {
	size_t i = 0;
	size_t sum = 0;

	while (i < (BOUNDED_BY - (BOUNDED_BY % 30) - 1)) {
		sum += i;
		i += 3;
		sum += i;
		i += 2;
		sum += i;
		i += 1;
		sum += i;
		i += 3;
		sum += i;
		i += 1;
		sum += i;
		i += 2;
		sum += i;
		i += 3;
		sum += i;
		i += 3;
		sum += i;
		i += 2;
		sum += i;
		i += 1;
		sum += i;
		i += 3;
		sum += i;
		i += 1;
		sum += i;
		i += 2;
		sum += i;
		i += 3;
	}

	do {
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 3;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 2;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 1;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 3;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 1;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 2;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 3;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 3;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 2;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 1;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 3;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 1;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 2;
		if (i >= BOUNDED_BY) {break;};
		sum += i;
		i += 3;
	} while (0);

	std::cout << sum << std::endl;
}