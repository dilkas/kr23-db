#include <exception>

#include <boost/graph/breadth_first_search.hpp>
#include <boost/log/trivial.hpp>

#include "hasse_diagram.h"

HasseDiagram::HasseDiagram() {
  tops_.insert(boost::add_vertex(diagram_));
}

std::set<int> HasseDiagram::Positions(HasseDiagram::Vertex vertex) {
  return diagram_[vertex].positions();
}

struct EndSearchException : public std::exception {};

class ParentFinder : public boost::default_bfs_visitor {
public:
  ParentFinder(std::set<HasseDiagram::Vertex> &excluded,
               std::vector<HasseDiagram::Vertex> &parents,
               const std::set<int> &position_set) :
    excluded_(excluded), parents_(parents), position_set_(position_set) {};

  template<typename Vertex, typename Graph>
  void examine_vertex(Vertex vertex, const Graph &graph) const {
    BOOST_LOG_TRIVIAL(debug) << "ParentFinder: examining vertex " << vertex;
    if (excluded_.find(vertex) != excluded_.end() ||
        !graph[vertex].IsSubsetOf(position_set_)) {
      BOOST_LOG_TRIVIAL(debug) << "ParentFinder: skipping";
      return;
    }
    if (graph[vertex].Size() == position_set_.size()) {
      parents_ = {vertex};
      BOOST_LOG_TRIVIAL(debug) << "ParentFinder: ending the search early";
      throw EndSearchException();
    }
    parents_.push_back(vertex);
    for (auto successor :
           boost::make_iterator_range(boost::adjacent_vertices(vertex, graph))) {
      BOOST_LOG_TRIVIAL(debug) << "ParentFinder: marking successor "
                               << successor << " for exclusion";
      excluded_.insert(successor);
    }
  }

private:
  std::set<HasseDiagram::Vertex> &excluded_;
  std::vector<HasseDiagram::Vertex> &parents_;
  const std::set<int> &position_set_;
};

HasseDiagram::Vertex HasseDiagram::AddVertexClass(std::set<int> position_set) {
  BOOST_LOG_TRIVIAL(debug) << "HasseDiagram: adding a vertex class with "
                           << position_set.size() << " positions";
  std::set<HasseDiagram::Vertex> excluded;
  std::vector<HasseDiagram::Vertex> parents;
  for (auto top : tops_) {
    ParentFinder parent_finder(excluded, parents, position_set);
    try {
      boost::breadth_first_search(diagram_, top, boost::visitor(parent_finder));
    } catch (EndSearchException& exception) {
      BOOST_LOG_TRIVIAL(debug) << "HasseDiagram: this position set is already in"
                               << " the diagram";
      return parents[0];
    }
  }

  BOOST_LOG_TRIVIAL(debug) << "HasseDiagram: adding a new vertex class with "
                           << parents.size() << " parents";
  assert(!parents.empty());
  HasseDiagram::Vertex new_vertex = boost::add_vertex(diagram_);
  diagram_[new_vertex].set_positions(position_set);
  for (auto parent : parents) {
    boost::add_edge(parent, new_vertex, diagram_);
    tops_.erase(parent);
  }
  tops_.insert(new_vertex);
  return new_vertex;
}
