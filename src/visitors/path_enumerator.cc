#include "visitors/path_enumerator.h"

#include "gfodd.h"

namespace visitors {

template <typename Vertex, typename Edge, typename Graph>
void PathEnumerator<Vertex, Edge, Graph>::
finish_vertex(Vertex vertex, const Graph& graph) const {
  paths_[vertex] = {};
  if (boost::out_degree(vertex, graph) == 0) return;
  for (auto out_edge :
           boost::make_iterator_range(boost::out_edges(vertex, graph))) {
    for (auto path : paths_[boost::target(out_edge, graph)]) {
      path.push_back(out_edge);
      paths_[vertex].push_back(path);
    }
  }
}

template class PathEnumerator<Gfodd::VertexDescriptor, Gfodd::EdgeDescriptor,
                              Gfodd::Graph>;

}  // namespace visitors
