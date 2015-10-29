#include <functional>

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

template<
	typename INDEX_TYPE, typename PTYPE1, typename PTYPE2, typename TRANSFORM = detail::identity,
	typename = std::enable_if_t<
		std::is_convertible<PTYPE1,INDEX_TYPE>::value && std::is_convertible<PTYPE2,INDEX_TYPE>::value
	>
>
auto xrange(const PTYPE1& start, const PTYPE2& end, TRANSFORM transform = TRANSFORM()) {
	return make_generator<INDEX_TYPE>(
		start,
		end + 1,
		[](INDEX_TYPE i) { return i + 1; },
		transform
	);
}

template<typename INDEX_TYPE, typename PTYPE1, typename TRANSFORM = detail::identity>
auto xrange(const PTYPE1& end, TRANSFORM transform = TRANSFORM(), decltype(transform(end),-1)* = nullptr) {
	return xrange<INDEX_TYPE>(0,end,transform);
}
