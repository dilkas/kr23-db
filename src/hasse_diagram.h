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
  HasseDiagram();
  std::set<int> positions(Vertex vertex);
  void set_tops(std::vector<Vertex> tops) { tops_ = tops; };
  Vertex AddVertexClass(std::set<int> position_set);

 private:
  Graph diagram_;
  std::vector<Vertex> tops_; // For convenient construction
  // TODO: maybe a linked list would be more suitable
};

#endif
