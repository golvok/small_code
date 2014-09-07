#include <iostream>
#include <stdlib.h>

template<typename T>
class SetRedirect {
public:
	SetRedirect(T& var_) : var(var_) { }

	template<typename V>
	SetRedirect<T>& operator=(const V& new_val) {
		var = new_val;
		std::cout << "set to " << new_val << "\n";
		return *this;
	}

	T* operator->() { return &var; }
	T& operator*() { return var; }
	operator T&() { return var; }
private:
	T& var;
	void operator=(const SetRedirect<T>&) = delete;
};

class SmallObj {
public:
	void set(int i) { this->i = i; std::cout << "so set to " << i << '\n'; }
	SmallObj() : i(5) { }
	int getI() { return i; }
private:
	int i;
	void operator=(const SmallObj&) = delete;
};

class Assignable {
public:
	Assignable() : i(6) { }
	Assignable& operator=(const Assignable& src) {
		return this->operator=(src.i);
	}
	Assignable& operator=(int new_i) {
		this->i = new_i;
		return *this;
	}
	operator int() { return i; }
private:
	int i;
	friend std::ostream& operator<<(std::ostream& os, const Assignable& a);
};

std::ostream& operator<<(std::ostream& os, const Assignable& a) {
	os << "{\n\ti = " << a.i << ",\n}\n";
	return os;
}

class TestClass {
public:
	TestClass() : test_integer(1), small_obj(), a() { }
	SetRedirect<int> test_int() { return SetRedirect<int>(test_integer); }
	SetRedirect<SmallObj> so() { return SetRedirect<SmallObj>(small_obj); }
	SetRedirect<Assignable> assignable() { return SetRedirect<Assignable>(a); }
private:
	int test_integer;
	SmallObj small_obj;
	Assignable a;
	void operator=(const TestClass&) = delete;
};

int main() {
	TestClass tc;

	// There an implicit conversion to the assigned type available.
	// Also, Calls to operator=(T) will work for any type T where ContainedType.operator=(T) exists.
	std::cout << tc.test_int() << '\n';
	tc.test_int() = 4;
	std::cout << tc.test_int() << '\n';

	std::cout << "---\n";

	// Methods of the contained type can be called using operator->
	std::cout << tc.so()->getI() << '\n';
	tc.so()->set(7);
	std::cout << tc.so()->getI() << '\n';

	// "Dereferenceing" ( operator*() ) will give the contained object
	(*tc.so()).set(24);
	std::cout << (*tc.so()).getI() << '\n';

	std::cout << "---\n";

	// the implicit conversion can also be invoked explictly:
	static_cast<SmallObj&>(tc.so()).set(33);
	std::cout << static_cast<SmallObj&>(tc.so()).getI() << '\n';

	std::cout << "---\n";

	// The SetRedirect is converted to it's contained type (Assignable) , and that is printed
	std::cout << tc.assignable();
	// Assignable's operator=(int) is called, through SetRedirect's operator=
	tc.assignable() = 41;
	std::cout << tc.assignable();

	return 0;
}
