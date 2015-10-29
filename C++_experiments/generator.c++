#include <iostream>

namespace detail {
	struct identity {
	template<typename U>
		constexpr auto operator()(U&& v) const noexcept -> decltype(std::forward<U>(v)) {
			return std::forward<U>(v);
		}
	};
}

template<typename INDEX_TYPE>
struct generator_base {
	using index_type = INDEX_TYPE;
	auto transform(const index_type& index) { return detail::identity()(index); }
};

template<typename INDEX_TYPE, typename NEXT, typename DONE, typename TRANSFORM>
class generator_iterator {
private:
	INDEX_TYPE current;
	NEXT next;
	DONE done;
	TRANSFORM transform;
	bool is_end_iterator;

public:
	generator_iterator(INDEX_TYPE current, NEXT next, DONE done, TRANSFORM transform, bool is_end_iterator)
		: current(current)
		, next(next)
		, done(done)
		, transform(transform)
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

	auto operator*() const {
		return transform(current);
	}
};

template<typename INDEX_TYPE, typename NEXT, typename DONE, typename TRANSFORM>
class generator {
public:
	using iter_type = generator_iterator<INDEX_TYPE,NEXT,DONE,TRANSFORM>;

private:
	INDEX_TYPE current;
	DONE done;
	NEXT next;
	TRANSFORM transform;

public:
	generator(INDEX_TYPE initial, DONE done, NEXT next, TRANSFORM transform)
		: current(initial)
		, done(done)
		, next(next)
		, transform(transform)
	{ }

	iter_type begin() {
		return iter_type(current,next,done,transform,false);
	}

	iter_type end() {
		return iter_type(current,next,done,transform,true);
	}
};

template<typename INDEX_TYPE, typename PTYPE1, typename NEXT, typename DONE, typename TRANSFORM = detail::identity>
auto make_generator(PTYPE1 initial, DONE done, NEXT next, TRANSFORM transform = TRANSFORM(), decltype(transform(initial),done(initial),next(initial))* = nullptr) {
	return generator<INDEX_TYPE,NEXT,DONE,TRANSFORM>(
		initial,
		done,
		next,
		transform
	);
}

template<
	typename INDEX_TYPE, typename PTYPE1, typename PTYPE2, typename NEXT, typename TRANSFORM = detail::identity,
	typename = std::enable_if_t<
		std::is_convertible<PTYPE1,INDEX_TYPE>::value && std::is_convertible<PTYPE2,INDEX_TYPE>::value
	>
>
auto make_generator(PTYPE1 initial, PTYPE2 past_end, NEXT next, TRANSFORM transform = TRANSFORM(), decltype(transform(initial),next(initial))* = nullptr) {
	return make_generator<INDEX_TYPE>(
		initial,
		[=](const INDEX_TYPE& current ) { return current == (INDEX_TYPE)past_end; },
		next,
		transform
	);
}

template<typename GEN>
auto make_generator(GEN&& gen) {
	using index_type = typename GEN::index_type;
	return make_generator<index_type>(
		gen.initial(),
		[&](const index_type& current) { return gen.done(current); },
		[&](const index_type& current) { return gen.next(current); },
		[&](const index_type& current) { return gen.transform(current); }
	);
}

template<typename INDEX_TYPE, typename PTYPE1, typename PTYPE2, typename TRANSFORM = detail::identity>
auto xrange(const PTYPE1& start, const PTYPE2& end, TRANSFORM transform = TRANSFORM()) {
    return make_generator<INDEX_TYPE>(
        start,
        end + 1,
        [](INDEX_TYPE i) { return i + 1; },
        transform
    );
}

template<typename INDEX_TYPE, typename PTYPE1>
auto xrange(const PTYPE1& end) {
    return xrange<INDEX_TYPE>(0,end);
}

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
