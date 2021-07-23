#include "visitors/encoding_finder.h"

namespace visitors {

  void EncodingFinder::discover_vertex(HasseDiagram::Vertex vertex,
                                       const HasseDiagram::Graph& graph) const {
    match_ = graph[vertex].IsSubsetOf(encoding_).quality;
    if (match_ == Match::Quality::kEqual) {
      finding_ = vertex;
      throw EndSearchException();
    }
  }

} // namespace visitors
