#include <iostream>
#include <memory>

template<typename Gen>
auto lazy(Gen&& g) {
    std::unique_ptr<decltype(g())> storage;
    return [storage=std::move(storage),g=std::forward<Gen>(g)]() mutable -> decltype(auto) {
        if (not storage) { storage = std::make_unique<decltype(g())>(g()); }
        return *storage;
    };
}

int main() {
    auto l = lazy([](){ std::cout << "computed!\n";
        struct S { int i; };
        return S { 4 };
    });
    std::cout << l().i << l().i << l().i;
    l().i = 5;
    std::cout << l().i << l().i << l().i;
}
    
