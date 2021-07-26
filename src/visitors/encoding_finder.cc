#include "visitors/encoding_finder.h"

#include "hasse_diagram.h"

namespace visitors {

  template <typename Vertex, typename Graph>
  void EncodingFinder<Vertex, Graph>::
  discover_vertex(Vertex vertex, const Graph& graph) const {
    match_ = graph[vertex].IsSubsetOf(encoding_).quality;
    if (match_ == Match::Quality::kEqual) {
      finding_ = vertex;
      throw EndSearchException();
    }
  }

  template class EncodingFinder<HasseDiagram::Vertex,
                                HasseDiagram::FilteredGraph>;

}  // namespace visitors
