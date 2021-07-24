#ifndef GFODD_H
#define GFODD_H

#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graph_utility.hpp>

#include "variable_positions.h"
#include "vertex.h"

class Gfodd {
public:
  struct Edge {
    bool positive;
  };

  typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS,
    Vertex, Edge> Graph;
  typedef boost::graph_traits<Graph>::vertex_descriptor VertexDescriptor;
  typedef boost::graph_traits<Graph>::vertex_iterator VertexIterator;

  Gfodd();

  VariablePositions Positions(VertexDescriptor v) {
    return diagram_[v].positions();
  }
  int NumNewVariables(VertexDescriptor v) { // TODO: test
    return diagram_[v].new_variables();
  }

  int NumInternalEdges() { return internal_edges_.size(); }
  std::vector<VertexDescriptor> Atoms();
  VertexDescriptor NullVertex() {
    return boost::graph_traits<Graph>::null_vertex();
  }
  std::pair<VertexDescriptor, VertexDescriptor>
  Incident(int internal_edge_index) {
    return boost::incident(internal_edges_[internal_edge_index], diagram_);
  }

private:
  typedef boost::graph_traits<Graph>::edge_descriptor EdgeDescriptor;

  Graph diagram_;
  std::vector<EdgeDescriptor> internal_edges_;
};

#endif
