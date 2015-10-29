#include "generator.h++"

#include <iostream>

int main() {
	for (const auto& i : make_generator<int>(1,11,[](const auto& i){ return i+1; })) {
		std::cout << i << ' ';
	}

	std::cout << '\n';

	struct my_gen {
		int operator()(int i) {
			return i*2;
		}
	};

	for (const auto& i : make_generator<uint>(1,1024,my_gen())) {
		std::cout << i << ' ';
	}

	std::cout << '\n';

	for (const auto& i : make_generator<long>(1,[](const auto& i){ return i == 5; },[](const auto& i){ return i+1; })) {
		std::cout << i << ' ';
	}

	std::cout << '\n';

	struct my_gen2 {
		using index_type = float;
		size_t max_calls;
		size_t calls_so_far;

		my_gen2(size_t max_calls)
			: max_calls(max_calls)
			, calls_so_far(0)
		{ }

		index_type initial() const {
			return 3.0;
		}

		index_type next(const index_type& current) {
			calls_so_far += 1;
			return calls_so_far*4.5 + current;
		}

		bool done(const index_type& current) const {
			(void)current; // don't use
			return max_calls <= calls_so_far;
		}

		auto transform(index_type index) {
			return 3*index;
		}
	};

	for (const auto& f : make_generator(my_gen2(7))) {
		std::cout << f << ' ';
	}
	std::cout << '\n';

	struct my_gen3 : public generator_base<size_t> {
		size_t max_value;

		my_gen3(size_t max_value)
			: max_value(max_value)
		{ }

		index_type initial() const {
			return 2.0f;
		}

		index_type next(const index_type& current) {
			return current*current;
		}

		bool done(const index_type& current) const {
			return max_value <= current;
		}
	};

	for (const auto& f2 : make_generator(my_gen3(300))) {
		std::cout << f2 << ' ';
	}
	std::cout << '\n';

	for (const auto& g : xrange<int>(1,5)) {
		std::cout << g << ' ';
	}
	std::cout << '\n';

	for (const auto& h : xrange<long>(5)) {
		std::cout << h << ' ';
	}
	std::cout << '\n';

	for (const auto& l : xrange<size_t>(1,3,[](const auto& index) -> float { return index*0.5; })) {
		std::cout << l << ' ';
	}
	std::cout << '\n';

}
