#include "hasse_diagram.h"

#include <math.h>

#include <exception>
#include <map>

#include <boost/graph/breadth_first_search.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/log/trivial.hpp>

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
  ParentFinder(std::set<HasseDiagram::Vertex>& excluded,
               std::vector<HasseDiagram::Vertex>& parents,
               const VariablePositions& positions) :
    excluded_(excluded), parents_(parents), positions_(positions) {};

  template<typename Vertex, typename Graph>
  void examine_vertex(Vertex vertex, const Graph& graph) const {
    BOOST_LOG_TRIVIAL(debug) << "ParentFinder: examining vertex " << vertex;

    if (excluded_.find(vertex) != excluded_.end()) {
      BOOST_LOG_TRIVIAL(debug) << "ParentFinder: skipping";
      return;
    }
    excluded_.insert(vertex);

    auto match = graph[vertex].IsSubsetOf(positions_);
    if (match == MatchQuality::kNotASubset) return;
    if (match == MatchQuality::kEqual) {
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
  std::set<HasseDiagram::Vertex>& excluded_;
  std::vector<HasseDiagram::Vertex>& parents_;
  const VariablePositions& positions_;
};

HasseDiagram::Vertex
HasseDiagram::AddVertexClass(VariablePositions variable_positions) {
  std::set<HasseDiagram::Vertex> excluded;
  std::vector<HasseDiagram::Vertex> parents;
  for (auto top : tops_) {
    ParentFinder parent_finder(excluded, parents, variable_positions);
    try {
      boost::breadth_first_search(diagram_, top, boost::visitor(parent_finder));
    } catch (EndSearchException& exception) {
      BOOST_LOG_TRIVIAL(debug) << "HasseDiagram: absorbed into another vertex";
      if (corresponding_vertex_class_.size() < num_position_sets_)
        corresponding_vertex_class_.push_back(parents[0]);
      return parents[0];
    }
  }

  BOOST_LOG_TRIVIAL(debug) << "HasseDiagram: adding a new vertex class with "
                           << parents.size() << " parents";
  assert(!parents.empty());
  HasseDiagram::Vertex new_vertex = boost::add_vertex(diagram_);
  diagram_[new_vertex].set_positions(variable_positions);
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
    int size = diagram_[vertex].FullSize(domain_size, predicate_arity);
    full_size[vertex] = size;
    for (auto successor :
           boost::make_iterator_range(boost::adjacent_vertices(vertex,
                                                               diagram_))) {
      size -= full_size[successor];
    }
    diagram_[vertex].set_size(size);
  }
}

// TODO: update
void HasseDiagram::RemoveOneVertex(HasseDiagram::Vertex vertex_class) {
  int size = diagram_[vertex_class].size();
  assert(size > 0);
  diagram_[vertex_class].set_size(size - 1);
}

// TODO: probably don't need this
void HasseDiagram::RemoveEdges() {
  auto edges = boost::edges(diagram_);
  for (auto edge = edges.first; edge != edges.second; ++edge)
    boost::remove_edge(*edge, diagram_);
}

// TODO: implement
void HasseDiagram::InitialiseVertices(Gfodd gfodd) {
  // HasseDiagram diagram(positions_.size());
  // std::vector<std::set<int>> top_layer;

  // // Add the sets themselves
  // for (auto position_set : positions_) {
  //   top_layer.push_back(position_set);
  //   diagram.AddVertexClass(position_set);
  // }

  // for (int i = 1; i < positions_.size(); ++i) {
  //   std::vector<std::set<int>> new_top_layer;
  //   int j = i; // which position to add first
  //   for (auto position_set : top_layer) {
  //     for (int k = j; k < positions_.size(); ++k) {
  //       std::set<int> new_set = position_set;
  //       new_set.insert(positions_[k].begin(), positions_[k].end());
  //       diagram.AddVertexClass(new_set);
  //       new_top_layer.push_back(new_set);
  //     }
  //     ++j;
  //   }
  //   top_layer = new_top_layer;
  // }
  // return diagram;
}

// TODO: implement (later)
// void HasseDiagram::InitialiseEdges(Gfodd gfodd) {
//   for (int i = 0; i < gfodd.NumInternalEdges(); ++i) {
    // from: CorrespondingVertexClass(gfodd.incidentVertices(i)) (and descendants?)
    // to: same and descendants?
//  }
  // 1. For each internal edge of the GFODD:
  // a) from..?
  // b) to..?
  // c) how many?
// }
