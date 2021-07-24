#ifndef TARGET_VISITOR_H
#define TARGET_VISITOR_H

#include <vector>

#include <boost/graph/depth_first_search.hpp>

#include "misc.h"

namespace visitors {

  template <typename Vertex, typename Graph>
  class TargetVisitor : public boost::default_dfs_visitor {
  public:
  TargetVisitor(int edge_of_gfodd, Vertex source,
                std::vector<Change<Vertex>>& changes) :
    edge_of_gfodd_(edge_of_gfodd), source_(source), changes_(changes) {}

    void discover_vertex(Vertex target, const Graph& graph) const;

  private:
    int edge_of_gfodd_;
    Vertex source_;
    std::vector<Change<Vertex>>& changes_;
  };

} // namespace visitors

#endif
