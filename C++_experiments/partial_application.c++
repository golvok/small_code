#include <iostream>
#include <tuple>

template<typename F, typename... FirstArgs>
struct PartialApplier {
	F func;
	std::tuple<FirstArgs...> first_args;
	
	template<typename... SecondArgs>
	decltype(auto) operator()(SecondArgs&&... second_args) {
		return std::apply(func, tuple_cat(first_args, std::forward_as_tuple<SecondArgs...>(second_args...)));
	}
};
	

template<typename F, typename... Args>
auto partial_apply(F&& f, Args&&... args) {
	return PartialApplier<F,Args...>{std::forward<F>(f), {std::forward<Args>(args)...}};
}

int main() {
	partial_apply([](auto&& fb, auto&& os) {
		os << 'a';
		fb(os);
	},(
		partial_apply([](auto&& fc, auto&& os){
			os << 'b';
			fc(os);
		},(
			partial_apply([](auto&& fn, auto&& os){
				os << 'c';
				fn(os);
			},(
				[](auto&& os){
					(void)(os);
				}
			))
		))
	))
	(
		std::cout
	);
}
