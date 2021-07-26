#ifndef MULTIPLICITY_SUBTRACTOR_H
#define MULTIPLICITY_SUBTRACTOR_H

#include <map>

#include <boost/graph/depth_first_search.hpp>

#include "misc.h"

namespace visitors {

template <typename Vertex, typename Graph>
class MultiplicitySubtractor : public boost::default_dfs_visitor {
 public:
  MultiplicitySubtractor(std::map<Vertex, Change>& changes,
                         std::map<Vertex, int>& to_subtract) :
      changes_(changes), to_subtract_(to_subtract) {}
  void finish_vertex(Vertex vertex, const Graph& graph) const;

 private:
  std::map<Vertex, Change>& changes_;
  std::map<Vertex, int>& to_subtract_;
};

}  // namespace visitors

#endif
