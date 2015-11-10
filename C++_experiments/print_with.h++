#include <iosfwd>
#include <functional>

namespace detail {
	template<typename THING>
	struct with_helper {
		THING& thing;
		with_helper(THING& thing) : thing(thing) { }
	};

	template<typename THING>
	auto operator<<(std::ostream& stream, const detail::with_helper<THING>& helper) {
		return std::pair<std::ostream&,THING&>(stream, helper.thing);
	}
}

template<typename THING>
auto with(THING& thing) {
	return detail::with_helper<THING>(thing);
}
