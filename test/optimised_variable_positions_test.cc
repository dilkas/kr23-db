#define BOOST_TEST_MODULE OptimisedVariablePositionsTest

#include "optimised_variable_positions.h"

#include <boost/test/unit_test.hpp>

#include "match_quality.h"
#include "variable_positions.h"

BOOST_AUTO_TEST_CASE(test_equal) {
  OptimisedVariablePositions p1;
  p1.Set(VariablePositions("xyy"));
  OptimisedVariablePositions p2;
  p2.Set(VariablePositions("xbb"));
  BOOST_CHECK(p1.IsSubsetOf(p2) == MatchQuality::kEqual);
}

BOOST_AUTO_TEST_CASE(test_subset) {
  OptimisedVariablePositions p1;
  p1.Set(VariablePositions("xyz"));
  OptimisedVariablePositions p2;
  p2.Set(VariablePositions("aab"));
  BOOST_CHECK(p1.IsSubsetOf(p2) == MatchQuality::kSubset);
}

BOOST_AUTO_TEST_CASE(test_NotASubset) {
  OptimisedVariablePositions p1;
  p1.Set(VariablePositions("xxxy"));
  OptimisedVariablePositions p2;
  p2.Set(VariablePositions("xxyz"));
  BOOST_CHECK(p1.IsSubsetOf(p2) == MatchQuality::kNotASubset);
}
