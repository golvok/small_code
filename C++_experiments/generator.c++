#include <iostream>

template<typename VALUE_TYPE, typename NEXT, typename DONE>
class generator_iterator {
private:
	VALUE_TYPE current;
	NEXT next;
	DONE done;
	bool is_end_iterator;

public:
	generator_iterator(VALUE_TYPE current, NEXT next, DONE done, bool is_end_iterator)
		: current(current)
		, next(next)
		, done(done)
		, is_end_iterator(is_end_iterator)
	{ }

	generator_iterator& operator++() {
		current = next(current);
		return *this;
	}

	bool operator==(const generator_iterator& rhs) const {
		if (rhs.is_end_iterator && is_end_iterator) {
			return true;
		} else if (rhs.is_end_iterator) {
			return rhs.done(current);
		} else if (is_end_iterator) {
			return done(rhs.current);
		} else {
			return current == rhs.current;
		}
	}

	bool operator!=(const generator_iterator& rhs) const { return !(*this == rhs); }

	VALUE_TYPE operator*() const {
		return current;
	}
};

template<typename VALUE_TYPE, typename NEXT, typename DONE>
class generator {
public:
	using iter_type = generator_iterator<VALUE_TYPE,NEXT,DONE>;

private:
	VALUE_TYPE current;
	DONE done;
	NEXT next;

public:
	generator(VALUE_TYPE initial, DONE done, NEXT next)
		: current(initial)
		, done(done)
		, next(next)
	{ }

	iter_type begin() {
		return iter_type(current,next,done,false);
	}

	iter_type end() {
		return iter_type(current,next,done,true);
	}
};

template<typename VALUE_TYPE, typename NEXT, typename DONE>
auto make_generator(VALUE_TYPE initial, DONE done, NEXT next) {
	return generator<VALUE_TYPE,NEXT,DONE>(
		initial,
		done,
		next
	);
}

template<typename VALUE_TYPE, typename NEXT>
auto make_generator(VALUE_TYPE initial, VALUE_TYPE past_end, NEXT next) {
	return make_generator(
		initial,
		[=](const VALUE_TYPE& current ) { return current == past_end; },
		next
	);
}

template<typename GEN>
auto make_generator(GEN&& gen) {
	using value_type = typename GEN::value_type;
	return make_generator(
		gen.initial(),
		[&](const value_type& current) { return gen.done(current); },
		[&](const value_type& current) { return gen.next(current); }
	);
}

int main() {
	for (const auto& i : make_generator(1,11,[](const auto& i){ return i+1; })) {
		std::cout << i << ' ';
	}

	std::cout << '\n';

	struct my_gen {
		int operator()(int i) {
			return i*2;
		}
	};

	for (const auto& i : make_generator(1,1024,my_gen())) {
		std::cout << i << ' ';
	}

	std::cout << '\n';

	for (const auto& i : make_generator(1,[](const auto& i){ return i == 5; },[](const auto& i){ return i+1; })) {
		std::cout << i << ' ';
	}

	std::cout << '\n';

	struct my_gen2 {
		using value_type = float;
		size_t max_calls;
		size_t calls_so_far;

		my_gen2(size_t max_calls)
			: max_calls(max_calls)
			, calls_so_far(0)
		{ }

		value_type initial() const {
			return 3.0;
		}

		value_type next(const value_type& current) {
			calls_so_far += 1;
			return calls_so_far*4.5 + current;
		}

		bool done(const value_type& current) const {
			(void)current; // don't use
			return max_calls <= calls_so_far;
		}
	};

	for (const auto& f : make_generator(my_gen2(7))) {
		std::cout << f << ' ';
	}
	std::cout << '\n';
}
