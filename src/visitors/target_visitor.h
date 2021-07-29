#ifndef TARGET_VISITOR_H
#define TARGET_VISITOR_H

#include <map>
#include <vector>

#include <boost/graph/depth_first_search.hpp>

#include "misc.h"

namespace visitors {

// Whenever we go down an edge, divide multiplicity_ by the multiplicity of
// the edge. Conversely, when going up an edge, multiply.
template <typename Vertex, typename Graph>
class TargetVisitor : public boost::default_dfs_visitor {
 public:
  typedef typename boost::graph_traits<Graph>::edge_descriptor Edge;

  TargetVisitor(int edge_of_gfodd, int multiplicity,
                std::map<Vertex, Change>& changes) :
      edge_of_gfodd_(edge_of_gfodd), multiplicity_(multiplicity),
      changes_(changes) {}

  void discover_vertex(Vertex target, const Graph& graph);
  void tree_edge(Edge edge, const Graph& graph);
  void finish_edge(Edge edge, const Graph& graph);

 private:
  int edge_of_gfodd_, multiplicity_;
  std::vector<Edge> stack_;  // internal TODO: make it into an actual stack
  std::map<Vertex, Change>& changes_;  // output
};

}  // namespace visitors

#endif
