#include <array>
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace Trie {
using namespace std;
#include "../Trie.h++"
}

#include <catch.hpp>

SCENARIO("Three") {
  Trie::Trie t;
  t.insert("ab");
  // t.search("abc");
  // t.search("ab");
  // t.startsWith("abc");
  // t.startsWith("ab");
  // t.insert("ab");
  // t.search("abc");
  // t.startsWith("abc");
  t.insert("abc");
  t.search("abc");
  t.startsWith("abc");
}

SCENARIO("Two") {
  Trie::Trie t;
  t.insert("hello");
  REQUIRE(not t.search("hell"));
  REQUIRE(not t.search("helloa"));
  REQUIRE(t.search("hello"));
  REQUIRE(t.startsWith("hell"));
  REQUIRE(not t.startsWith("helloa"));
  REQUIRE(t.startsWith("hello"));
}

SCENARIO("One") {
  Trie::Trie t;
  t.insert("apple");
  REQUIRE(t.search("apple"));
  t.insert("appla");
  REQUIRE(t.search("apple"));
  REQUIRE(t.search("appla"));
  REQUIRE(not t.search("pple"));
  REQUIRE(not t.startsWith("pple"));
  REQUIRE(not t.search("app"));
  t.insert("app");
  REQUIRE(t.search("app"));
  REQUIRE(t.startsWith("a"));
  REQUIRE(t.startsWith("ap"));
  REQUIRE(t.startsWith("app"));
  REQUIRE(t.startsWith("appl"));
  REQUIRE(t.startsWith("apple"));
}