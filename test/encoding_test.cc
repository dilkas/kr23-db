#define BOOST_TEST_MODULE EncodingTest

#include "encoding.h"

#include <boost/test/unit_test.hpp>

#include "misc.h"
#include "variable_positions.h"

BOOST_AUTO_TEST_CASE(test_equal) {
  Encoding e1, e2;
  e1.Set(VariablePositions("xyy"));
  e2.Set(VariablePositions("xbb"));
  BOOST_CHECK(e1.IsSubsetOf(e2).quality == Match::Quality::kEqual);
}

BOOST_AUTO_TEST_CASE(test_subset) {
  Encoding e1, e2;
  e1.Set(VariablePositions("xyz"));
  e2.Set(VariablePositions("aab"));
  Match match = e1.IsSubsetOf(e2);
  BOOST_CHECK(match.quality == Match::Quality::kSubset);
  BOOST_CHECK_EQUAL(match.diff_in_variables, 1);
}

BOOST_AUTO_TEST_CASE(test_NotASubset) {
  Encoding e1, e2;
  e1.Set(VariablePositions("xxxy"));
  e2.Set(VariablePositions("xxyz"));
  BOOST_CHECK(e1.IsSubsetOf(e2).quality == Match::Quality::kNotASubset);
}

BOOST_AUTO_TEST_CASE(test_CountRedundantPositions) {
  Encoding e;
  e.Set(VariablePositions("xyyzzz"));
  BOOST_CHECK_EQUAL(e.CountRedundantPositions(), 3);
}

BOOST_AUTO_TEST_CASE(test_MatchAString) {
  Encoding e;
  e.Set(VariablePositions("xxy")); // encoding: 110
  auto match = e.MatchAString("abc");
  BOOST_CHECK_EQUAL(match.size(), 2);
  std::set<std::string> answer1 = {"c"};
  BOOST_CHECK_EQUAL_COLLECTIONS(match[0].begin(), match[0].end(),
                                answer1.begin(), answer1.end());
  std::set<std::string> answer2 = {"a", "b"};
  BOOST_CHECK_EQUAL_COLLECTIONS(match[1].begin(), match[1].end(),
                                answer2.begin(), answer2.end());
}
