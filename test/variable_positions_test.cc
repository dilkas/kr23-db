#define BOOST_TEST_MODULE VariablePositionsTest

#include "variable_positions.h"

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE(test_OccursOnlyOnce) {
  VariablePositions positions("xyx");
  BOOST_CHECK(!positions.OccursOnlyOnce("x"));
  BOOST_CHECK(positions.OccursOnlyOnce("y"));
}
