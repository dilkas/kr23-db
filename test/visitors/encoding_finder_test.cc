#define BOOST_TEST_MODULE EncodingFinderTest

#include "visitors/encoding_finder.h"

#include <boost/graph/depth_first_search.hpp>
#include <boost/test/unit_test.hpp>

#include "encoding.h"
#include "hasse_diagram.h"
#include "misc.h"
#include "variable_positions.h"

BOOST_AUTO_TEST_CASE(test_empty_graph) {
  // // input
  // Encoding encoding;
  // encoding.Set(VariablePositions("x"));
  // HasseDiagram graph;

  // // output
  // Match match;
  // match.quality = Match::Quality::kEqual;
  // HasseDiagram::Vertex vertex =
  //   boost::graph_traits<HasseDiagram::Graph>::null_vertex();

  // // run & check
  // visitors::EncodingFinder<HasseDiagram::Vertex, HasseDiagram::FilteredGraph>
  //   finder(encoding, match.quality, finding);
  // boost::depth_first_search(graph, finder);
  // BOOST_CHECK(match.quality == Match::Quality::kNotASubset);
  // BOOST_CHECK(vertex == boost::graph_traits<HasseDiagram::Graph>::null_vertex());
}
