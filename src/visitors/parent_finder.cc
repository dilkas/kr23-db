#include "visitors/parent_finder.h"

#include <boost/log/trivial.hpp>

#include "misc.h"

namespace visitors {

  void ParentFinder::examine_vertex(HasseDiagram::Vertex vertex,
                                    const HasseDiagram::Graph& graph) const {
    BOOST_LOG_TRIVIAL(debug) << "ParentFinder: examining vertex " << vertex;

    if (excluded_.find(vertex) != excluded_.end()) {
      BOOST_LOG_TRIVIAL(debug) << "ParentFinder: skipping";
      return;
    }
    excluded_.insert(vertex);

    auto match = graph[vertex].IsSubsetOf(positions_);
    if (match.quality == Match::Quality::kNotASubset) return;
    if (match.quality == Match::Quality::kEqual) {
      parents_ = {vertex};
      BOOST_LOG_TRIVIAL(debug) << "ParentFinder: ending the search early";
      throw EndSearchException();
    }

    parents_.push_back(vertex);
    differences_.push_back(match.diff_in_variables);
    for (auto successor :
           boost::make_iterator_range(boost::adjacent_vertices(vertex, graph))) {
      BOOST_LOG_TRIVIAL(debug) << "ParentFinder: marking successor "
                               << successor << " for exclusion";
      excluded_.insert(successor);
    }
  }

} // namespace visitors
