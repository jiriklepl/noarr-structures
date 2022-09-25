#include <catch2/catch.hpp>

#include <sstream>

#include "noarr/structures_extended.hpp"
#include "noarr/structures/stream_lifting.hpp"
#include "noarr/structures/reorder.hpp"

TEST_CASE("Lift", "[stream_lifting]") {
	noarr::array<'x', 3, noarr::array<'y', 3, noarr::scalar<int>>> structure;
	auto uptr = std::make_unique<char[]>(structure | noarr::get_size());
	void *ptr = uptr.get();
	std::stringstream stream("111 222 333 444 555\n666 777 888 999");
	bool ok = !!stream_lift(stream, structure, ptr);

	REQUIRE(ok);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 0, 0)) == 111);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 0, 1)) == 222);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 0, 2)) == 333);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 1, 0)) == 444);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 1, 1)) == 555);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 1, 2)) == 666);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 2, 0)) == 777);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 2, 1)) == 888);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 2, 2)) == 999);
}

TEST_CASE("Lift reordered", "[stream_lifting]") {
	noarr::array<'x', 3, noarr::array<'y', 3, noarr::scalar<int>>> structure;
	auto uptr = std::make_unique<char[]>(structure | noarr::get_size());
	void *ptr = uptr.get();
	std::stringstream stream("111 222 333 444 555\n666 777 888 999\n");
	bool ok = !!stream_lift(stream, structure ^ noarr::reorder<'y', 'x'>(), ptr);

	REQUIRE(ok);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 0, 0)) == 111);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 1, 0)) == 222);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 2, 0)) == 333);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 0, 1)) == 444);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 1, 1)) == 555);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 2, 1)) == 666);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 0, 2)) == 777);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 1, 2)) == 888);
	REQUIRE((structure | noarr::get_at<'x', 'y'>(ptr, 2, 2)) == 999);
}

TEST_CASE("Lift incomplete", "[stream_lifting]") {
	noarr::array<'x', 3, noarr::array<'y', 3, noarr::scalar<int>>> structure;
	auto uptr = std::make_unique<char[]>(structure | noarr::get_size());
	void *ptr = uptr.get();
	std::stringstream stream("111 222 333 444 555");
	bool ok = !!stream_lift(stream, structure, ptr);

	REQUIRE(!ok);
}

TEST_CASE("Unlift", "[stream_lifting]") {
	noarr::array<'x', 3, noarr::array<'y', 3, noarr::scalar<int>>> structure;
	auto uptr = std::make_unique<char[]>(structure | noarr::get_size());
	void *ptr = uptr.get();
	(structure | noarr::get_at<'x', 'y'>(ptr, 0, 0)) = 111;
	(structure | noarr::get_at<'x', 'y'>(ptr, 0, 1)) = 222;
	(structure | noarr::get_at<'x', 'y'>(ptr, 0, 2)) = 333;
	(structure | noarr::get_at<'x', 'y'>(ptr, 1, 0)) = 444;
	(structure | noarr::get_at<'x', 'y'>(ptr, 1, 1)) = 555;
	(structure | noarr::get_at<'x', 'y'>(ptr, 1, 2)) = 666;
	(structure | noarr::get_at<'x', 'y'>(ptr, 2, 0)) = 777;
	(structure | noarr::get_at<'x', 'y'>(ptr, 2, 1)) = 888;
	(structure | noarr::get_at<'x', 'y'>(ptr, 2, 2)) = 999;

	std::stringstream stream;
	bool ok = !!stream_unlift(stream, structure, ptr);
	REQUIRE(ok);
	REQUIRE(stream.str() == "111\n222\n333\n444\n555\n666\n777\n888\n999\n");
}

TEST_CASE("Unlift reordered", "[stream_lifting]") {
	noarr::array<'x', 3, noarr::array<'y', 3, noarr::scalar<int>>> structure;
	auto uptr = std::make_unique<char[]>(structure | noarr::get_size());
	void *ptr = uptr.get();
	(structure | noarr::get_at<'x', 'y'>(ptr, 0, 0)) = 111;
	(structure | noarr::get_at<'x', 'y'>(ptr, 1, 0)) = 222;
	(structure | noarr::get_at<'x', 'y'>(ptr, 2, 0)) = 333;
	(structure | noarr::get_at<'x', 'y'>(ptr, 0, 1)) = 444;
	(structure | noarr::get_at<'x', 'y'>(ptr, 1, 1)) = 555;
	(structure | noarr::get_at<'x', 'y'>(ptr, 2, 1)) = 666;
	(structure | noarr::get_at<'x', 'y'>(ptr, 0, 2)) = 777;
	(structure | noarr::get_at<'x', 'y'>(ptr, 1, 2)) = 888;
	(structure | noarr::get_at<'x', 'y'>(ptr, 2, 2)) = 999;

	std::stringstream stream;
	bool ok = !!stream_unlift(stream, structure ^ noarr::reorder<'y', 'x'>(), ptr);
	REQUIRE(ok);
	REQUIRE(stream.str() == "111\n222\n333\n444\n555\n666\n777\n888\n999\n");
}