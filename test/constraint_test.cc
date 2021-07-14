#define BOOST_TEST_MODULE ConstraintTest

#include <boost/test/unit_test.hpp>

#include "constraint.h"
#include "hasse_diagram.h"

// TODO: add an actual test
BOOST_AUTO_TEST_CASE(test_UnionsOfPositions) {
  Constraint constraint({{1}, {2}});
  HasseDiagram diagram = constraint.UnionsOfPositions();
}
