#include <iostream>
#include <stdlib.h>

/**
 * the default setTo. It just assigns the two params.
 */
template<typename T, typename V>
void setTo(T& dest, const V& src) {
	std::cout << "calling default setTo\n";
	dest = src;
}

/**
 * A specalized setTo, to demonstrate that you can define one for build-in types
 */
template<>
void setTo(int& dest, const int& src) {
	std::cout << "calling specialized int,int setTo\n";
	dest = src;
}

/**
 * A class that holds an reference to some type, and will call a setter on the
 * held reference for you, if you try to assign to this class.
 */
template<typename T>
class SetRedirect {
public:
	SetRedirect(T& var_) : var(var_) { }

	template<typename V>
	SetRedirect<T>& operator=(const V& new_val) {
		setTo(var,new_val);
		std::cout << "set to " << new_val << "\n";
		return *this;
	}

	T* operator->() { return &var; }
	T& operator*() { return var; }
	operator T&() { return var; }
	T* operator->() const { return &var; }
	T& operator*() const { return var; }
	operator T&() const { return var; }
private:
	T& var;
	void operator=(const SetRedirect<T>&) = delete;
};

/**
 * A small class to demonstrate custom types and the various ways of accessing and setting
 */
class SmallObj {
public:
	void set(int i) { this->i = i; std::cout << "so.set(" << i << ")\n"; }
	SmallObj() : i(5) { }
	int getI() { return i; }
private:
	int i;
	void operator=(const SmallObj&) = delete;
};

/**
 * A specialization of setTo for SmallObj, to demonstrate custom setters for custom types.
 */
template<>
void setTo(SmallObj& so, const int& i) {
	std::cout << "calling specialized SmallObj,int setTo\n";
	so.set(i);
}

/**
 * A class with an operator=(some other type) to demonstrate the assignment operator is
 * called, through SetRedirect's assignment operator, and that SetRedirect may be
 * implicty converted to it's contained type.
 */
class Assignable {
public:
	Assignable() : i(6) { }
	Assignable(int i_) : i(i_) { }
	Assignable& operator=(const Assignable& src) {
		std::cout << "Assignable.operator=((Assignable)" << src << ") called\n";
		this->i = src.i;
		return *this;
	}
	Assignable& operator=(int new_i) {
		std::cout << "Assignable.operator=((int)" << new_i << ") called\n";
		this->i = new_i;
		return *this;
	}
	operator int() { return i; }
	operator int() const { return i; }
private:
	int i;
	friend std::ostream& operator<<(std::ostream& os, const Assignable& a);
};

std::ostream& operator<<(std::ostream& os, const Assignable& a) {
	os << "i = " << a.i;
	return os;
}

/**
 * A test class to return the SetRedirect objects.
 */
class TestClass {
public:
	TestClass() : test_float(0.5), test_integer(1), small_obj(), a() { }
	SetRedirect<float> test_flt() { return SetRedirect<float>(test_float); }
	SetRedirect<int> test_int() { return SetRedirect<int>(test_integer); }
	SetRedirect<SmallObj> so() { return SetRedirect<SmallObj>(small_obj); }
	SetRedirect<Assignable> assignable() { return SetRedirect<Assignable>(a); }
private:
	float test_float;
	int test_integer;
	SmallObj small_obj;
	Assignable a;
	void operator=(const TestClass&) = delete;
};

int main() {
	TestClass tc;

	// There an implicit conversion to the assigned type available.
	// Also, Calls to operator=(T) will work for any type T where ContainedType.operator=(T) exists,
	// or wher there is a custom setter
	std::cout << tc.test_flt() << '\n';
	tc.test_flt() = 2.3;
	std::cout << tc.test_flt() << '\n';

	std::cout << "---\n";

	// Methods of the contained type can be called using operator->
	std::cout << tc.so()->getI() << '\n';
	tc.so()->set(21);
	std::cout << tc.so()->getI() << '\n';

	// custom setters can be declared by providing a specialization of
	// setTo for you types. Notic that SmallObj has no assignment operators defined.
	tc.so() = 7;
	std::cout << tc.so()->getI() << '\n';

	// "Dereferenceing" ( operator*() ) will give the contained object
	(*tc.so()).set(24);
	std::cout << (*tc.so()).getI() << '\n';

	// the implicit conversion can also be invoked explictly:
	static_cast<SmallObj&>(tc.so()).set(33);
	std::cout << static_cast<SmallObj&>(tc.so()).getI() << '\n';

	std::cout << "---\n";

	// you can make a custom setTo for any type
	std::cout << tc.test_int() << '\n';
	tc.test_int() = 4;
	std::cout << tc.test_int() << '\n';

	std::cout << "---\n";

	// The SetRedirect is converted to it's contained type (Assignable) , and that is printed
	std::cout << tc.assignable() << '\n';

	// Assignable's operator=(int) is called, through SetRedirect's operator=
	tc.assignable() = 18;
	std::cout << tc.assignable() << '\n';

	// Assignable's operator=(Assignable) is called, through SetRedirect's operator=
	tc.assignable() = Assignable(33);
	std::cout << tc.assignable() << '\n';

	std::cout << "---\n";

	return 0;
}
