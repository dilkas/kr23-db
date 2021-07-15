#define BOOST_TEST_MODULE HasseDiagramTest

#include <set>

#include <boost/test/unit_test.hpp>

#include "hasse_diagram.h"

BOOST_AUTO_TEST_CASE(test_adding_single_position) {
  HasseDiagram diagram(1);
  auto vertex = diagram.AddVertexClass({0});
  BOOST_CHECK_EQUAL(diagram.CorrespondingVertexClass(0), vertex);
  BOOST_CHECK(diagram.Positions(vertex).empty());
}

BOOST_AUTO_TEST_CASE(test_adding_multiple_positions) {
  std::set<int> original_positions{0, 1};
  HasseDiagram diagram(1);
  auto vertex = diagram.AddVertexClass(original_positions);
  auto positions = diagram.Positions(vertex);
  BOOST_CHECK_EQUAL_COLLECTIONS(positions.begin(), positions.end(),
                                original_positions.begin(),
                                original_positions.end());
}

// TODO: turn into a real test
BOOST_AUTO_TEST_CASE(test_InstantiateSizes) {
  HasseDiagram diagram(1);
  auto vertex = diagram.AddVertexClass({0, 1});
  diagram.InstantiateSizes(5, 3);
}
