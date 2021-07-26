#include "visitors/target_visitor.h"

#include "hasse_diagram.h"

namespace visitors {

  template <typename Vertex, typename Graph>
  void TargetVisitor<Vertex, Graph>::discover_vertex(Vertex target,
                                                     const Graph&) {
    auto it = changes_.find(source_);
    if (it != changes_.end()) {
      it->second[target] = {multiplicity_, edge_of_gfodd_};
    } else {
      changes_[source_] = {{target, {multiplicity_, edge_of_gfodd_}}};
    }
  }

  template <typename Vertex, typename Graph>
  void TargetVisitor<Vertex, Graph>::tree_edge(Edge edge,
                                               const Graph& graph) {
    // TODO: skip the branch if multiplicity_ < 1 (if that ever happens)
    multiplicity_ /= graph[edge].multiplicity;
    stack_.push_back(edge);
  }

  template <typename Vertex, typename Graph>
  void TargetVisitor<Vertex, Graph>::finish_edge(Edge edge,
                                               const Graph& graph) {
    if (stack_.back() == edge) {
      multiplicity_ *= graph[edge].multiplicity;
      stack_.pop_back();
    }
  }

  template class TargetVisitor<HasseDiagram::Vertex,
                               HasseDiagram::FilteredGraph>;

} // namespace visitors
