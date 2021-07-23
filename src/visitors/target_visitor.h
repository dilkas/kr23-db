#ifndef TARGET_VISITOR_H
#define TARGET_VISITOR_H

#include <vector>

#include <boost/graph/depth_first_search.hpp>

#include "hasse_diagram.h"
#include "misc.h"

namespace visitors {

  class TargetVisitor : public boost::default_dfs_visitor {
  public:
  TargetVisitor(int edge_of_gfodd, HasseDiagram::Vertex source,
                std::vector<Change<HasseDiagram::Vertex>>& changes) :
    edge_of_gfodd_(edge_of_gfodd), source_(source), changes_(changes) {}

    void discover_vertex(HasseDiagram::Vertex target,
                         const HasseDiagram::Graph& graph) const;

  private:
    int edge_of_gfodd_;
    HasseDiagram::Vertex source_;
    std::vector<Change<HasseDiagram::Vertex>>& changes_;
  };

} // namespace visitors

#endif
