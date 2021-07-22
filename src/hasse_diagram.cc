#include "hasse_diagram.h"

#include <assert.h>
#include <math.h>

#include <exception>
#include <map>
#include <vector>

#include <boost/graph/breadth_first_search.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/log/trivial.hpp>

#include "match.h"

struct EndSearchException : public std::exception {};

class ParentFinder : public boost::default_bfs_visitor {
public:
  ParentFinder(std::set<HasseDiagram::Vertex>& excluded,
               std::vector<HasseDiagram::Vertex>& parents,
               std::vector<int>& differences,
               const VariablePositions& positions) :
    excluded_(excluded), parents_(parents), differences_(differences),
    positions_(positions) {}

  template<typename Vertex, typename Graph>
  void examine_vertex(Vertex vertex, const Graph& graph) const {
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

private:
  std::set<HasseDiagram::Vertex>& excluded_;
  std::vector<HasseDiagram::Vertex>& parents_;
  // The differences in the numbers of free variables between the vertex class
  // to be inserted and all of its parents
  std::vector<int>& differences_;
  const VariablePositions& positions_;
};

HasseDiagram::Vertex
HasseDiagram::AddVertexClass(VariablePositions variable_positions,
                             Gfodd::VertexDescriptor gfodd_vertex_id,
                             Gfodd::VertexDescriptor null_vertex) {
  std::set<HasseDiagram::Vertex> excluded;
  std::vector<HasseDiagram::Vertex> parents;
  std::vector<int> differences;
  for (auto top : tops_) {
    ParentFinder parent_finder(excluded, parents, differences,
                               variable_positions);
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
  for (int i = 0; i < parents.size(); i++) {
    auto edge = boost::add_edge(new_vertex, parents[i], diagram_).first;
    diagram_[edge].edge_of_gfodd = -1;
    diagram_[edge].multiplicity = differences[i];
    tops_.erase(parents[i]);
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

// First we discover a vertex and record its relationship to the encoding. If
// they're equal, immediately exit the search. If they're unrelated, the
// terminator skips all out-edges.
class EncodingFinder : public boost::default_dfs_visitor {
public:
  EncodingFinder(Encoding& encoding, Match::Quality &match,
                 HasseDiagram::Vertex& finding) :
    encoding_(encoding), match_(match), finding_(finding) {}
  template<typename Vertex, typename Graph>
  void discover_vertex(Vertex vertex, const Graph& graph) const {
    match_ = graph[vertex].IsSubsetOf(encoding_).quality;
    if (match_ == Match::Quality::kEqual) {
      finding_ = vertex;
      throw EndSearchException();
    }
  }

private:
  Encoding& encoding_;
  HasseDiagram::Vertex& finding_;
  Match::Quality& match_;
};

class TargetVisitor : public boost::default_dfs_visitor {
public:
  TargetVisitor(HasseDiagram::Vertex source) : source_(source) {}
  template<typename Vertex, typename Graph>
  void discover_vertex(Vertex vertex, const Graph& graph) const {
    // TODO: add 'multiplicities' to the non-GFODD edges (AddVertexClass? can I make it efficient?)
    // TODO: check if this edge already exists
    // don't create the edge if multiplicity = 0
    // auto edge = boost::add_edge(source, target, diagram_);
    // edge.edge_of_gfodd = i;
    // edge.multiplicity = ...;
  }

private:
  HasseDiagram::Vertex source_;
};

// For each possible source (i.e., all descendants of the first source),
// determine all possible targets.
//
// Find equality constraints on variables that transfer from one GFODD vertex to
// another. These constraints occur when considering descendants of from source
// vertex.
class SourceVisitor : public boost::default_dfs_visitor {
public:
  SourceVisitor(VariablePositions source_variables,
                HasseDiagram::Vertex source_vertex,
                VariablePositions target_variables,
                HasseDiagram::Vertex parent_of_target) :
    source_variables_(source_variables), source_vertex_(source_vertex),
    target_variables_(target_variables), parent_of_target_(parent_of_target) {}

  template<typename Vertex, typename Graph>
  void discover_vertex(Vertex vertex, const Graph& graph) const {
    // Identify matching variables from source_variables and source_vertex
    auto decoding = graph[source_vertex_].
      MatchAString(source_variables_.string_representation());
    // Transform target_variables to match these constraints
    auto new_variables = target_variables_.RespectTheMap(decoding);
    // Encode them
    Encoding encoding;
    encoding.Set(new_variables);

    // Find the descendant of parent_of_target_ that matches the encoding, i.e.,
    // find the target
    Match::Quality last_match;
    HasseDiagram::Vertex target =
      boost::graph_traits<HasseDiagram::Graph>::null_vertex();
    EncodingFinder encoding_finder(encoding, last_match, target);
    std::vector<boost::default_color_type> colors(boost::num_vertices(graph));
    const auto terminator = [last_match](HasseDiagram::Vertex vertex,
                                         const Graph& graph) {
      return last_match == Match::Quality::kNotASubset;
    };
    try {
      // TODO: can I avoid supplying a color map?
      boost::depth_first_visit(graph, parent_of_target_,
                               encoding_finder, colors.data(),
                               terminator);
    } catch (EndSearchException& exception) {}
    assert(target != boost::graph_traits<HasseDiagram::Graph>::null_vertex());

    TargetVisitor visitor(vertex);
    boost::depth_first_search(graph,
                              boost::visitor(visitor).root_vertex(target));
  }

private:
  VariablePositions source_variables_;
  HasseDiagram::Vertex source_vertex_;
  VariablePositions target_variables_;
  HasseDiagram::Vertex parent_of_target_;
};

void HasseDiagram::InitialiseEdges(Gfodd gfodd) {
  for (int i = 0; i < gfodd.NumInternalEdges(); ++i) {
    auto incident_vertices = gfodd.Incident(i);
    auto source = corresponding_vertex_class_[incident_vertices.first];
    SourceVisitor visitor(gfodd.Positions(incident_vertices.first), source,
                          gfodd.Positions(incident_vertices.second),
                          corresponding_vertex_class_[incident_vertices.second]);
    boost::depth_first_search(diagram_,
                              boost::visitor(visitor).root_vertex(source));
  }
}

// TODO (later): update to update edges and perhaps create new vertex classes
void HasseDiagram::RemoveOneVertex(HasseDiagram::Vertex vertex_class) {
  int size = diagram_[vertex_class].size();
  assert(size > 0);
  diagram_[vertex_class].set_size(size - 1);
}
