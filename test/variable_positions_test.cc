#define BOOST_TEST_MODULE VariablePositionsTest

#include "variable_positions.h"

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE(test_OccursOnlyOnce) {
  VariablePositions positions("xyx");
  BOOST_CHECK(!positions.OccursOnlyOnce("x"));
  BOOST_CHECK(positions.OccursOnlyOnce("y"));
}

BOOST_AUTO_TEST_CASE(test_RespectTheMap) {
  VariablePositions positions("wxyz");
  VariablePositions updated = positions.RespectTheMap({{0, {"a", "y"}},
                                                       {1, {"x", "z"}}});
  BOOST_CHECK(updated.OccursOnlyOnce("w"));
  BOOST_CHECK(updated.OccursOnlyOnce("y"));
  BOOST_CHECK(!updated.OccursOnlyOnce("z"));
  // and x is no longer there as it's replaced by z
}
