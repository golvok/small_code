#include <catch2/catch_test_macros.hpp>

#include "../memory_allocator.h++"

TEST_CASE("test1") {
	auto data = std::vector<std::byte>(1024);
	auto a = golvok::allocator::Allocator(data.data(), data.size());
	const auto p1 = a.alloc(1); // p1|free(1023)
	const auto p2 = a.alloc(1); // p1|p2|free(1022)
	CHECK(p1 != p2);
	CHECK(p1 == data.data());
	CHECK(p2 == data.data() + 1);

	a.free(p1); // free(1)|p2|free(1022)
	const auto p3 = a.alloc(2); // free(1)|p2|p3|free(1020)
	CHECK(p3 != p1);
	CHECK(p3 != p2);
	CHECK(p3 == data.data() + 2);

	a.free(p2); // merge backwards: free(2)|p3|free(1020)
	const auto p4 = a.alloc(2); // p4|p3|free(1020)
	CHECK(p4 == p1);

	const auto p5 = a.alloc(2); // p4|p3|p5|free(1018)
	CHECK(p5 == data.data() + 4);
	a.free(p4); // free(2)|p3|p5|free(1018)
	a.free(p5); // free(2)|p3|p5|free(1018)
	const auto p6 = a.alloc(1018); // free(2)|p3|p6
	CHECK(p6 == p5);
	a.free(p6); // free(2)|p3|free(1020)
	a.free(p3); // free(1024)

	a.alloc(1024);
}