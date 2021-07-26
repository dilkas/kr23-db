#include "visitors/multiplicity_subtractor.h"

#include "hasse_diagram.h"

namespace visitors {

  template <typename Vertex, typename Graph>
  void MultiplicitySubtractor<Vertex, Graph>::
  finish_vertex(Vertex vertex, const Graph& graph) const {
    to_subtract_[vertex] = 0;
    if (boost::out_degree(vertex, graph) == 0) return;
    for (auto out_edge :
           boost::make_iterator_range(boost::out_edges(vertex, graph))) {
      auto target = boost::target(out_edge, graph);
      to_subtract_[vertex] += to_subtract_[target] +
        changes_[target].multiplicity;
    }
  }

  template class MultiplicitySubtractor<HasseDiagram::Vertex,
                                        HasseDiagram::FilteredGraph>;

}  // namespace visitors
