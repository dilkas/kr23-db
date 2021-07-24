#ifndef PARENT_FINDER_H
#define PARENT_FINDER_H

#include <set>
#include <vector>

#include <boost/graph/breadth_first_search.hpp>

#include "variable_positions.h"

namespace visitors {

  template <typename Vertex, typename Graph>
  class ParentFinder : public boost::default_bfs_visitor {
  public:
  ParentFinder(std::set<Vertex>& excluded,
               std::vector<Vertex>& parents,
               std::vector<int>& differences,
               const VariablePositions& positions) :
    excluded_(excluded), parents_(parents), differences_(differences),
      positions_(positions) {}

    void examine_vertex(Vertex vertex, const Graph& graph) const;

  private:
    std::set<Vertex>& excluded_;
    std::vector<Vertex>& parents_;
    // The differences in the numbers of free variables between the vertex class
    // to be inserted and all of its parents
    std::vector<int>& differences_;
    const VariablePositions& positions_;
  };

} // namespace visitors

#endif
