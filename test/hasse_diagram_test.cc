#define BOOST_TEST_MODULE HasseDiagramTest

#include <boost/test/unit_test.hpp>

#include "hasse_diagram.h"

BOOST_AUTO_TEST_CASE(test_adding_existing_set) {
  HasseDiagram diagram;
  BOOST_CHECK_EQUAL(diagram.AddVertexClass({}),
                    boost::graph_traits<HasseDiagram::Graph>::null_vertex());
}

BOOST_AUTO_TEST_CASE(test_AddVertexClass) {
  HasseDiagram diagram;
  std::set<int> positions{1};
  auto positions2 = diagram.positions(diagram.AddVertexClass(positions));
  BOOST_CHECK_EQUAL_COLLECTIONS(positions2.begin(), positions2.end(),
                                positions.begin(), positions.end());
}
