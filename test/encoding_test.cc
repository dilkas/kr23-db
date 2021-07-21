#define BOOST_TEST_MODULE EncodingTest

#include "encoding.h"

#include <boost/test/unit_test.hpp>

#include "match_quality.h"
#include "variable_positions.h"

BOOST_AUTO_TEST_CASE(test_equal) {
  Encoding e1, e2;
  e1.Set(VariablePositions("xyy"));
  e2.Set(VariablePositions("xbb"));
  BOOST_CHECK(e1.IsSubsetOf(e2) == MatchQuality::kEqual);
}

BOOST_AUTO_TEST_CASE(test_subset) {
  Encoding e1, e2;
  e1.Set(VariablePositions("xyz"));
  e2.Set(VariablePositions("aab"));
  BOOST_CHECK(e1.IsSubsetOf(e2) == MatchQuality::kSubset);
}

BOOST_AUTO_TEST_CASE(test_NotASubset) {
  Encoding e1, e2;
  e1.Set(VariablePositions("xxxy"));
  e2.Set(VariablePositions("xxyz"));
  BOOST_CHECK(e1.IsSubsetOf(e2) == MatchQuality::kNotASubset);
}

BOOST_AUTO_TEST_CASE(test_CountRedundantPositions) {
  Encoding e;
  e.Set(VariablePositions("xyyzzz"));
  BOOST_CHECK_EQUAL(e.CountRedundantPositions(), 3);
}
