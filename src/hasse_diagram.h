#ifndef HASSE_DIAGRAM_H
#define HASSE_DIAGRAM_H

#include <set>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/filtered_graph.hpp>
#include <boost/graph/graph_traits.hpp>

#include "gfodd.h"
#include "variable_positions.h"
#include "vertex_class.h"

// An edge from A to B means that the position sets of A are less restrictive
// those of B
class HasseDiagram {
 public:
  struct Edge {
    // -1 for edges that denote subset relations. -2 for edges that count paths.
    // Non-negative integers index Gfodd::internal_edges_.
    int edge_of_gfodd;
    // For internal edges, how many identical copies of this edge there are.
    // For subset edges, the difference in free variables.
    int multiplicity;
  };

  typedef boost::adjacency_list<boost::hash_mapS, boost::vecS,
                                boost::directedS, VertexClass, Edge> Graph;
  typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;

  HasseDiagram();
  void InitialiseVertices(Gfodd gfodd);
  void InstantiateSizes(int domain_size, int predicate_arity);
  void RemoveOneVertex(Vertex vertex_class);
  void InitialiseEdges(Gfodd gfodd);

 private:
  struct SelectSubsetEdges {
    SelectSubsetEdges() {}
    SelectSubsetEdges(Graph g) : graph(g) {}
    bool operator()(Graph::edge_descriptor edge) const {
      return graph[edge].edge_of_gfodd == kSubset;
    }
    Graph graph;
  } predicate_;
  typedef boost::filtered_graph<Graph, SelectSubsetEdges> FilteredGraph;

  static const int kSubset = -1, kPredecessor = -2;
  Graph diagram_;
  FilteredGraph skeleton_;

  // For convenient construction
  Vertex bot_;
  std::set<Vertex> tops_;

  // The corresponding vertex class of each GFODD vertex
  std::map<Gfodd::VertexDescriptor, Vertex> corresponding_vertex_class_;

  Vertex AddVertexClass(VariablePositions variable_positions,
                        Gfodd::VertexDescriptor gfodd_vertex_id,
                        Gfodd::VertexDescriptor null_vertex);
  void UpdatePathCounts(Vertex from, Vertex to);
};

#endif
