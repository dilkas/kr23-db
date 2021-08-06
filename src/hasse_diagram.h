#ifndef HASSE_DIAGRAM_H
#define HASSE_DIAGRAM_H

#include <map>
#include <set>
#include <vector>

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
                                boost::bidirectionalS, VertexClass, Edge> Graph;
  typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;
  typedef boost::graph_traits<Graph>::edge_descriptor EdgeDescriptor;
  typedef boost::graph_traits<Graph>::vertex_iterator VertexIterator;

  HasseDiagram(Gfodd gfodd, int predicate_arity);

  // Initialisation
  void InitialiseVertices();
  void InstantiateSizes(int domain_size);
  void InitialiseEdges(int domain_size);

  // Iteration and Evaluation
  HasseDiagram RemoveOneVertex(Vertex vertex_class);
  std::vector<int> edge_counts() { return edge_counts_; }
  int Size(Vertex vertex_class) { return diagram_[vertex_class].size(); }
  std::pair<VertexIterator, VertexIterator> VertexClassIterator() {
    return boost::vertices(diagram_);
  }

 private:
  // TODO (later): will need to make this class a lot more compact
  struct SelectSubsetEdges {
    SelectSubsetEdges() {}
    explicit SelectSubsetEdges(Graph* g) : graph(g) {}
    bool operator()(Graph::edge_descriptor edge) const {
      return (*graph)[edge].edge_of_gfodd == kSubset;
    }
    Graph* graph;
  } predicate_;
  typedef boost::filtered_graph<Graph, SelectSubsetEdges> FilteredGraph;
  // TODO (later): kPredecessor type edges are no longer needed
  static const int kSubset = -1, kPredecessor = -2;

  int predicate_arity_;
  Graph diagram_;
  FilteredGraph skeleton_;
  Gfodd gfodd_;

  // For convenient construction
  Vertex bot_;
  std::set<Vertex> tops_;

  // The corresponding vertex class of each GFODD vertex
  std::map<Gfodd::VertexDescriptor, Vertex> corresponding_vertex_class_;

  // For each non-negative edge type, i.e., each GFODD inner edge, the sum of
  // multiplicities of all edges in the diagram that are of that type
  std::vector<int> edge_counts_;

  Vertex AddVertexClass(VariablePositions variable_positions,
                        Gfodd::VertexDescriptor gfodd_vertex_id,
                        Gfodd::VertexDescriptor null_vertex);
  void UpdatePathCounts(Vertex from, Vertex to);

  // * new_size refers to the size of the sub-vertex class without the edge.
  // * edge_to_remove is an out-edge that should be removed in one version of
  //   the vertex class but remain in the other.
  void SplitVertexClass(Vertex vertex_class, EdgeDescriptor edge_to_remove,
                        int new_size);
};

bool operator==(const HasseDiagram& a, const HasseDiagram& b);

#endif
