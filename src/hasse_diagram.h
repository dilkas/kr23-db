#ifndef HASSE_DIAGRAM_H
#define HASSE_DIAGRAM_H

#include <set>
#include <vector>

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>

#include "vertex_class.h"

// An edge from A to B means that A is a subset of B. Visually, arrows point
// upwards.
class HasseDiagram {
 public:
  typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS,
                                VertexClass> Graph;
  typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;

  HasseDiagram() {}
  HasseDiagram(int num_position_sets);
  std::set<int> Positions(Vertex vertex);
  Vertex AddVertexClass(std::set<int> position_set);
  Vertex CorrespondingVertexClass(int position_set_index);

 private:
  Graph diagram_;
  // For convenient construction
  std::set<Vertex> tops_;
  Vertex bot_;
  // Keep track of the vertex class that directly corresponds to each position
  // set in the constraint
  std::vector<Vertex> corresponding_vertex_class_;
  int num_position_sets_;
};

#endif
