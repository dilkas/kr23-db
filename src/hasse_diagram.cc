#include <cmath>

#include <exception>
#include <map>

#include <boost/graph/breadth_first_search.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/log/trivial.hpp>

#include "hasse_diagram.h"

HasseDiagram::HasseDiagram(int num_position_sets) :
  num_position_sets_(num_position_sets) {
  bot_ = boost::add_vertex(diagram_);
  tops_.insert(bot_);
}

HasseDiagram::Vertex
HasseDiagram::CorrespondingVertexClass(int position_set_index) {
  assert(position_set_index >= 0 &&
         position_set_index < corresponding_vertex_class_.size());
  return corresponding_vertex_class_[position_set_index];
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
    excluded_.insert(vertex);

    if (graph[vertex].NumPositions() == position_set_.size()) {
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

  // Only add a position set to the Hasse diagram if it has at least two
  // positions
  if (position_set.size() <= 1) {
    if (corresponding_vertex_class_.size() < num_position_sets_)
      corresponding_vertex_class_.push_back(bot_);
    return bot_;
  }

  std::set<HasseDiagram::Vertex> excluded;
  std::vector<HasseDiagram::Vertex> parents;
  for (auto top : tops_) {
    ParentFinder parent_finder(excluded, parents, position_set);
    try {
      boost::breadth_first_search(diagram_, top, boost::visitor(parent_finder));
    } catch (EndSearchException& exception) {
      BOOST_LOG_TRIVIAL(debug) << "HasseDiagram: this position set is already in"
                               << " the diagram";
      if (corresponding_vertex_class_.size() < num_position_sets_)
        corresponding_vertex_class_.push_back(parents[0]);
      return parents[0];
    }
  }

  BOOST_LOG_TRIVIAL(debug) << "HasseDiagram: adding a new vertex class with "
                           << parents.size() << " parents";
  assert(!parents.empty());
  HasseDiagram::Vertex new_vertex = boost::add_vertex(diagram_);
  diagram_[new_vertex].set_positions(position_set);
  for (auto parent : parents) {
    boost::add_edge(new_vertex, parent, diagram_);
    tops_.erase(parent);
  }
  tops_.insert(new_vertex);
  corresponding_vertex_class_.push_back(new_vertex);
  return new_vertex;
}

void HasseDiagram::InstantiateSizes(int domain_size, int predicate_arity) {
  std::vector<HasseDiagram::Vertex> topological_ordering;
  boost::topological_sort(diagram_, std::back_inserter(topological_ordering));
  std::map<HasseDiagram::Vertex, int> full_size;
  for (auto vertex : topological_ordering) {
    int size = std::pow((double)domain_size, predicate_arity -
                        diagram_[vertex].NumPositions());
    full_size[vertex] = size;
    for (auto successor :
           boost::make_iterator_range(boost::adjacent_vertices(vertex,
                                                               diagram_))) {
      size -= full_size[successor];
    }
    diagram_[vertex].set_size(size);
  }
}

// TODO: test this!
void HasseDiagram::RemoveOneVertex(HasseDiagram::Vertex vertex_class) {
  int size = diagram_[vertex_class].size();
  assert(size > 0);
  diagram_[vertex_class].set_size(size - 1);
}

// TODO: test this!
void HasseDiagram::RemoveEdges() {
  auto edges = boost::edges(diagram_);
  for (auto edge = edges.first; edge != edges.second; ++edge)
    boost::remove_edge(*edge, diagram_);
}

// TODO: implement
// void HasseDiagram::InitialiseEdges(Gfodd gfodd) {
  // 1. For each edge of the GFODD:
  // a) from..?
  // b) to..?
  // c) how many?
// }
