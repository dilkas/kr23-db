#include "hasse_diagram.h"

#include <math.h>

#include <exception>
#include <map>
#include <vector>

#include <boost/graph/breadth_first_search.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/log/trivial.hpp>

HasseDiagram::HasseDiagram() {
  bot_ = boost::add_vertex(diagram_);
  tops_.insert(bot_);
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
HasseDiagram::AddVertexClass(VariablePositions variable_positions,
                             Gfodd::VertexDescriptor gfodd_vertex_id,
                             Gfodd::VertexDescriptor null_vertex) {
  std::set<HasseDiagram::Vertex> excluded;
  std::vector<HasseDiagram::Vertex> parents;
  for (auto top : tops_) {
    ParentFinder parent_finder(excluded, parents, variable_positions);
    try {
      boost::breadth_first_search(diagram_, top, boost::visitor(parent_finder));
    } catch (EndSearchException& exception) {
      BOOST_LOG_TRIVIAL(debug) << "HasseDiagram: absorbed into another vertex";
      if (gfodd_vertex_id != null_vertex)
        corresponding_vertex_class_[gfodd_vertex_id] = parents[0];
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
  if (gfodd_vertex_id != null_vertex)
    corresponding_vertex_class_[gfodd_vertex_id] = new_vertex;
  return new_vertex;
}

void HasseDiagram::InitialiseVertices(Gfodd gfodd) {
  std::vector<Gfodd::VertexDescriptor> atoms = gfodd.Atoms();
  std::vector<VariablePositions> top_layer;

  // First pass
  for (auto atom : atoms) {
    auto variable_positions = gfodd.Positions(atom);
    top_layer.push_back(variable_positions);
    AddVertexClass(variable_positions, atom, gfodd.NullVertex());
  }

  for (int i = 1; i < atoms.size(); ++i) {
    std::vector<VariablePositions> new_top_layer;
    int j = i; // which one to add first
    for (auto positions : top_layer) {
      for (int k = j; k < atoms.size(); ++k) {
        VariablePositions new_set = positions;
        new_set.Insert(gfodd.Positions(atoms[k]));
        AddVertexClass(new_set, gfodd.NullVertex(), gfodd.NullVertex());
        new_top_layer.push_back(new_set);
      }
      ++j;
    }
    top_layer = new_top_layer;
  }
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

// TODO: update to update edges and perhaps create new vertex classes
void HasseDiagram::RemoveOneVertex(HasseDiagram::Vertex vertex_class) {
  int size = diagram_[vertex_class].size();
  assert(size > 0);
  diagram_[vertex_class].set_size(size - 1);
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
