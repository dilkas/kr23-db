#ifndef PATH_ENUMERATOR_H
#define PATH_ENUMERATOR_H

#include <map>
#include <vector>

#include <boost/graph/depth_first_search.hpp>

namespace visitors {

template <typename Vertex, typename Edge, typename Graph>
class PathEnumerator : public boost::default_dfs_visitor {
 public:
  PathEnumerator(std::map<Vertex, std::vector<std::vector<Edge>>>& paths) :
      paths_(paths) {}
  void finish_vertex(Vertex vertex, const Graph& graph) const;

 private:
  std::map<Vertex, std::vector<std::vector<Edge>>>& paths_;
};

}  // namespace visitors

#endif
