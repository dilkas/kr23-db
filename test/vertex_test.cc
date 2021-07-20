#define BOOST_TEST_MODULE VertexTest

#include <boost/test/unit_test.hpp>

#include "vertex.h"

// TODO: turn this into a VariablePositions test

// BOOST_AUTO_TEST_CASE(test_constructor) {
//   Vertex vertex("xxy");
//   VariablePositions correct_positions = {{"x", {0, 1}}, {"y", {2}}};
//   auto positions = vertex.positions();
//   BOOST_CHECK_EQUAL(positions.size(), correct_positions.size());
//   BOOST_CHECK_EQUAL_COLLECTIONS(positions["x"].begin(), positions["x"].end(),
//                                 correct_positions["x"].begin(),
//                                 correct_positions["x"].end());
//   BOOST_CHECK_EQUAL_COLLECTIONS(positions["y"].begin(), positions["y"].end(),
//                                 correct_positions["y"].begin(),
//                                 correct_positions["y"].end());
// }
