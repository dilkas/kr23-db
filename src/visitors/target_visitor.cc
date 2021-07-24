#include "visitors/target_visitor.h"

#include "hasse_diagram.h"

namespace visitors {

  template <typename Vertex, typename Graph>
  void TargetVisitor<Vertex, Graph>::discover_vertex(Vertex target,
                                                     const Graph& graph) const {
    // TODO: find the real multiplicity. Initially, it should be similar to
    // size() or FullSize() but with some variables held as constants. As we
    // traverse the edges, it should be updated using tree_edge and finish_edge.
    // We should also hold the descriptor of the last edge from tree_edge and
    // skip finish_edge if it's not the same edge. Also, don't create the edge
    // if multiplicity = 0.
    int multiplicity = 1;
    changes_.push_back({source_, target, multiplicity, edge_of_gfodd_});
  }

  template class TargetVisitor<HasseDiagram::Vertex,
                               HasseDiagram::FilteredGraph>;

} // namespace visitors
