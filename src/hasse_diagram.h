#ifndef HASSE_DIAGRAM_H
#define HASSE_DIAGRAM_H

#include <set>
#include <vector>

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>

#include "gfodd.h"
#include "variable_positions.h"
#include "vertex_class.h"

// An edge from A to B means that the position sets of A are less restrictive
// those of B
class HasseDiagram {
 public:
  struct Edge {
    int edge_of_gfodd;
    int multiplicity;
  };

  typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS,
                                VertexClass, Edge> Graph;
  typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;

  HasseDiagram() {}
  HasseDiagram(int num_position_sets);
  Vertex AddVertexClass(VariablePositions variable_positions);
  std::vector<Vertex> corresponding_vertex_class() {
    return corresponding_vertex_class_;
  }
  Vertex CorrespondingVertexClass(int position_set_index); // TODO: rewrite
  void InstantiateSizes(int domain_size, int predicate_arity);
  void RemoveOneVertex(Vertex vertex_class);
  void RemoveEdges(); // TODO: no longer necessary
  void InitialiseVertices(Gfodd gfodd);
  // void InitialiseEdges(Gfodd gfodd);

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
