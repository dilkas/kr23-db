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
  typedef boost::graph_traits<Graph>::edge_descriptor EdgeDescriptor;
  typedef boost::graph_traits<Graph>::vertex_iterator VertexIterator;

  Gfodd();
  double Evaluate(std::map<Gfodd::EdgeDescriptor, int> edge_counts);
  int NumInternalEdges() { return internal_edges_.size(); }

  VariablePositions Positions(VertexDescriptor v) {
    return diagram_[v].positions();
  }

  int NumNewVariables(VertexDescriptor v) { // TODO: test
    return diagram_[v].new_variables();
  }

  std::vector<VertexDescriptor> Atoms();
  VertexDescriptor NullVertex() {
    return boost::graph_traits<Graph>::null_vertex();
  }

  std::pair<VertexDescriptor, VertexDescriptor>
  Incident(int internal_edge_index) {
    return boost::incident(internal_edges_[internal_edge_index], diagram_);
  }

private:
  Graph diagram_;
  VertexDescriptor source_;
  std::vector<EdgeDescriptor> internal_edges_;
  std::vector<VertexDescriptor> sinks_;
  std::vector<std::vector<std::vector<EdgeDescriptor>>> paths_; // for each sink

  VertexDescriptor AddVertex(std::string variables, int new_variables);
  VertexDescriptor AddVertex(double weight);
  void FindPaths();
};

#endif
