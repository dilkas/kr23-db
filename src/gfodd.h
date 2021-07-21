#ifndef GFODD_H
#define GFODD_H

#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <boost/graph/adjacency_list.hpp>

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
  int NumInternalEdges() { return internal_edges_.size(); }
  VariablePositions Positions(VertexDescriptor v) {
    return diagram_[v].positions();
  }
  std::vector<VertexDescriptor> Atoms();
  VertexDescriptor NullVertex() {
    return boost::graph_traits<Graph>::null_vertex();
  }
  std::pair<VariablePositions,
            VariablePositions> IncidentAtomInfo(int internal_edge_index);
  // TODO: replace with incidentVertices() and update the test

private:
  typedef boost::graph_traits<Graph>::edge_descriptor EdgeDescriptor;

  Graph diagram_;
  std::vector<EdgeDescriptor> internal_edges_;
};

#endif
