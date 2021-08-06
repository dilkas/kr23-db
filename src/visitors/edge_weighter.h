#ifndef EDGE_WEIGHTER_H
#define EDGE_WEIGHTER_H

#include <boost/graph/depth_first_search.hpp>

namespace visitors {

  template <typename Vertex, typename Graph>
      class EdgeWeighter : public boost::default_dfs_visitor {
 public:
    EdgeWeighter() {}
    void discover_vertex(Vertex vertex, const Graph& graph) const;
  };

}  // namespace visitors

#endif
