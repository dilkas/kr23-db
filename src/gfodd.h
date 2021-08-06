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
    int weight;
  };

  typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS,
                                Vertex, Edge> Graph;
  typedef boost::graph_traits<Graph>::vertex_descriptor VertexDescriptor;
  typedef boost::graph_traits<Graph>::edge_descriptor EdgeDescriptor;
  typedef boost::graph_traits<Graph>::vertex_iterator VertexIterator;

  Gfodd();

  int NumInternalEdges() { return internal_edges_.size(); }
  EdgeDescriptor InternalEdge(int index) { return internal_edges_[index]; }

  int NumNewVariables(VertexDescriptor v) {
    return diagram_[v].new_variables();
  }

  VariablePositions Positions(VertexDescriptor v) {
    return diagram_[v].positions();
  }

  std::vector<VertexDescriptor> Atoms();  // called only once
  VertexDescriptor NullVertex() {
    return boost::graph_traits<Graph>::null_vertex();
  }

  std::pair<VertexDescriptor, VertexDescriptor>
  Incident(int internal_edge_index) {
    return boost::incident(internal_edges_[internal_edge_index], diagram_);
  }

  // Computes the weight of an interpretation
  double Evaluate(std::vector<int> internal_edge_counts);

 private:
  Graph diagram_;
  VertexDescriptor source_;
  std::vector<EdgeDescriptor> internal_edges_;
  std::vector<VertexDescriptor> sinks_;

  // for each sink
  std::vector<std::vector<std::vector<EdgeDescriptor>>> paths_;

  VertexDescriptor AddVertex(std::string variables, int new_variables);
  VertexDescriptor AddVertex(double weight);
  void FindPaths();
  void ComputeEdgeWeights(std::vector<int> internal_edge_counts);
};

#endif
