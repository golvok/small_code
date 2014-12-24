#include <iostream>

template <typename T>
class is_reference
{
    struct yes { char a, b; };
    typedef char no;

    template <typename U> static no test(T y) { return 0; }
    template <typename U> static yes test(...) { return {0,0}; }

public:
    static const bool value = sizeof(test<T>(0)) == sizeof(yes);
};

class TracerClass1 {
public:
	TracerClass1()                         { std::cout << "TracerClass1()\n";                        }
	TracerClass1(TracerClass1&&)           { std::cout << "TracerClass1(TracerClass1&&)\n";           }
	TracerClass1(const TracerClass1&)      { std::cout << "TracerClass1(const TracerClass1&)\n";      }
	TracerClass1(int)                      { std::cout << "TracerClass1(int)\n";                     }
	// TracerClass1(int&&)                   { std::cout << "TracerClass1(int&&)\n";                   }
	TracerClass1(int, TracerClass1&&)      { std::cout << "TracerClass1(int, TracerClass1&&)\n";      }
	TracerClass1(int, const TracerClass1&) { std::cout << "TracerClass1(int, const TracerClass1&)\n"; }

	~TracerClass1() { std::cout << "~TracerClass1()\n"; }

	TracerClass1& operator=(const TracerClass1&) {
		std::cout << "operator=(const TracerClass1&)\n";
		return *this;
	}
	TracerClass1& operator=(TracerClass1&&) {
		std::cout << "operator=(TracerClass1&&)\n";
		return *this;
	}
};

class TracerClass2 {
private:
	TracerClass1 tc;
public:
	// TracerClass2()                         : tc() { std::cout << "TracerClass2()\n";                         }
	TracerClass2(TracerClass2&&)           : tc() { std::cout << "TracerClass2(TracerClass2&&)\n";           }
	TracerClass2(const TracerClass2&)      : tc() { std::cout << "TracerClass2(const TracerClass2&)\n";      }
	TracerClass2(int)                      : tc() { std::cout << "TracerClass2(int)\n";                      }
	TracerClass2(int, TracerClass2&&)      : tc() { std::cout << "TracerClass2(int, TracerClass2&&)\n";      }
	TracerClass2(int, const TracerClass2&) : tc() { std::cout << "TracerClass2(int, const TracerClass2&)\n"; }

	TracerClass2(TracerClass1&& tc_)           : tc(std::move(tc_)) { std::cout << "TracerClass2(TracerClass1&& tc_) - with std::move\n"; }
	TracerClass2(const TracerClass1& tc_)      : tc(tc_)            { std::cout << "TracerClass2(const TracerClass1& tc_)\n"; }
	TracerClass2(int, TracerClass1&& tc_)      : tc((tc_))          { std::cout << "TracerClass2(int, TracerClass1&& tc_) - without std::move \n"; }
	TracerClass2(int, const TracerClass1& tc_) : tc(tc_)            { std::cout << "TracerClass2(int, const TracerClass1& tc_)\n"; }

	~TracerClass2() { std::cout << "~TracerClass2()\n"; }

	template<typename T>
	T& setTC_withforward(T&& t) {
		std::cout << "setTC_withforward(T = " << typeid(T).name() << (is_reference<T>::value ? "&" : "" ) << " t)\n";
		tc = std::forward<T>(t);
		return t;
	}

	template<typename T>
	T& setTC_withnothing(T&& t) {
		std::cout << "setTC_withnothing(T = " << typeid(T).name() << (is_reference<T>::value ? "&" : "" ) << " t)\n";
		tc = t;
		return t;
	}

	template<typename T>
	T& setTC_withmove(T&& t) {
		std::cout << "setTC_withmove(T = " << typeid(T).name() << (is_reference<T>::value ? "&" : "" ) << " t)\n";
		tc = std::move(t);
		return t;
	}

	TracerClass2& operator=(const TracerClass2&) { std::cout << "operator=(const TracerClass1&)\n"; return *this; }
	TracerClass2& operator=(TracerClass2&&) { std::cout << "operator=(TracerClass1&&)\n"; return *this; }
};

#define SCOPED_TEST(code) std::cout << '{'<< #code << "} :\n>>>\n"; { code } std::cout << "<<< --- --- --- ---\n";

int main() {
	TracerClass2 tc2(4);
	TracerClass1 tc1;
	std::cout << "----- START -----\n";
	SCOPED_TEST(
		TracerClass1(4, TracerClass1(4));
	)
	SCOPED_TEST(
		TracerClass2(TracerClass1(4));
	)
	SCOPED_TEST(
		tc2 = TracerClass2(2, TracerClass1(4));
	)
	SCOPED_TEST(
		tc2 = TracerClass2(tc2);
		std::cout << "destroy, now.\n";
	)
	SCOPED_TEST(
		TracerClass2(6,tc1);
	)

	std::cout
		<< "\n"
		<< "try forward, nothing and move with a rvalue reference\n"
		<< "\n"
	;

	SCOPED_TEST(
		tc2.setTC_withforward(TracerClass1());
	);
	SCOPED_TEST(
		tc2.setTC_withnothing(TracerClass1());
	);
	SCOPED_TEST(
		tc2.setTC_withmove(TracerClass1());
	);

	std::cout
		<< "\n"
		<< "try forward, nothing and move with a lvalue\n"
		<< "\n"
	;

	SCOPED_TEST(
		tc2.setTC_withforward(tc1);
	);
	SCOPED_TEST(
		tc2.setTC_withnothing(tc1);
	);
	SCOPED_TEST(
		tc2.setTC_withmove(tc1);
	);
	return 0;
}