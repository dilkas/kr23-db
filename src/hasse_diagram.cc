#include "hasse_diagram.h"

#include <assert.h>
#include <math.h>

#include <map>
#include <vector>

#include <boost/graph/copy.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/log/trivial.hpp>

#include "misc.h"
#include "visitors/multiplicity_subtractor.h"
#include "visitors/parent_finder.h"
#include "visitors/source_visitor.h"

HasseDiagram::HasseDiagram(Gfodd gfodd, int predicate_arity) :
    predicate_(&diagram_), predicate_arity_(predicate_arity),
    skeleton_(diagram_, predicate_), gfodd_(gfodd),
    edge_counts_(gfodd.NumInternalEdges()) {
  auto vertex = boost::add_vertex(diagram_);
  diagram_[vertex].set_positions(predicate_arity);
  tops_.insert(vertex);
}

void HasseDiagram::UpdatePathCounts(HasseDiagram::Vertex from,
                                    HasseDiagram::Vertex to) {
  for (auto from_edge :
           boost::make_iterator_range(boost::out_edges(from, diagram_))) {
    if (diagram_[from_edge].edge_of_gfodd == kPredecessor) {
      HasseDiagram::Vertex predecessor = boost::target(from_edge, diagram_);
      bool skip = false;
      auto edges = boost::edge_range(to, predecessor, diagram_);
      for (auto to_edge : boost::make_iterator_range(edges)) {
        if (diagram_[to_edge].edge_of_gfodd == kPredecessor) {
          diagram_[to_edge].multiplicity += diagram_[from_edge].multiplicity;
          skip = true;
          break;
        }
      }
      if (!skip) {
        auto edge = boost::add_edge(to, predecessor, diagram_).first;
        diagram_[edge].edge_of_gfodd = kPredecessor;
        diagram_[edge].multiplicity = diagram_[from_edge].multiplicity;
      }
    }
  }
}

HasseDiagram::Vertex
HasseDiagram::AddVertexClass(VariablePositions variable_positions,
                             Gfodd::VertexDescriptor gfodd_vertex_id,
                             Gfodd::VertexDescriptor null_vertex) {
  std::set<HasseDiagram::Vertex> excluded;
  std::vector<HasseDiagram::Vertex> parents;
  std::vector<int> differences;
  for (auto top : tops_) {
    visitors::ParentFinder<HasseDiagram::Vertex, HasseDiagram::FilteredGraph>
        parent_finder(excluded, parents, differences, variable_positions);
    try {
      boost::breadth_first_search(skeleton_, top,
                                  boost::visitor(parent_finder));
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
  for (std::vector<HasseDiagram::Vertex>::size_type i = 0;
       i < parents.size(); ++i) {
    UpdatePathCounts(parents[i], new_vertex);
    auto edge = boost::add_edge(new_vertex, parents[i], diagram_).first;
    diagram_[edge].edge_of_gfodd = kSubset;
    diagram_[edge].multiplicity = differences[i];
    tops_.erase(parents[i]);
  }
  tops_.insert(new_vertex);
  if (gfodd_vertex_id != null_vertex)
    corresponding_vertex_class_[gfodd_vertex_id] = new_vertex;
  return new_vertex;
}

void HasseDiagram::InitialiseVertices() {
  std::vector<Gfodd::VertexDescriptor> atoms = gfodd_.Atoms();
  std::vector<VariablePositions> top_layer;

  // First pass
  for (auto atom : atoms) {
    auto variable_positions = gfodd_.Positions(atom);
    top_layer.push_back(variable_positions);
    AddVertexClass(variable_positions, atom, gfodd_.NullVertex());
  }

  for (std::vector<Gfodd::VertexDescriptor>::size_type i = 1;
       i < atoms.size(); ++i) {
    std::vector<VariablePositions> new_top_layer;
    auto j = i;  // which one to add first
    for (auto positions : top_layer) {
      for (auto k = j; k < atoms.size(); ++k) {
        VariablePositions new_set = positions;
        new_set.Insert(gfodd_.Positions(atoms[k]));
        AddVertexClass(new_set, gfodd_.NullVertex(), gfodd_.NullVertex());
        new_top_layer.push_back(new_set);
      }
      ++j;
    }
    top_layer = new_top_layer;
  }
}

void HasseDiagram::InstantiateSizes(int domain_size) {
  std::vector<HasseDiagram::Vertex> topological_ordering;
  boost::topological_sort(diagram_, std::back_inserter(topological_ordering));
  std::map<HasseDiagram::Vertex, int> full_size;
  for (auto vertex : topological_ordering) {
    int size = diagram_[vertex].FullSize(domain_size, predicate_arity_);
    full_size[vertex] = size;
    for (auto successor :
             boost::make_iterator_range(boost::adjacent_vertices(vertex,
                                                                 diagram_))) {
      size -= full_size[successor];
    }
    diagram_[vertex].set_size(size);
  }
}

void HasseDiagram::InitialiseEdges(int domain_size) {
  // TODO (later): maybe merge them into one
  // Construct initial multiplicities of the new edges
  std::map<HasseDiagram::Vertex,
           std::map<HasseDiagram::Vertex, Change>> changes;
  std::map<HasseDiagram::Vertex, HasseDiagram::Vertex> top_targets;
  for (int i = 0; i < gfodd_.NumInternalEdges(); ++i) {
    auto incident_vertices = gfodd_.Incident(i);
    auto source = corresponding_vertex_class_[incident_vertices.first];
    auto source_variables = gfodd_.Positions(incident_vertices.first);
    auto target_variables = gfodd_.Positions(incident_vertices.second);
    BOOST_LOG_TRIVIAL(debug) << "HasseDiagram::InitialiseEdges: source = "
                             << source_variables.string_representation()
                             << ", target = "
                             << target_variables.string_representation();
    visitors::SourceVisitor<HasseDiagram::Vertex, HasseDiagram::FilteredGraph>
        visitor(i, std::pow(domain_size,
                            gfodd_.NumNewVariables(incident_vertices.second)),
                source_variables, target_variables, source,
                corresponding_vertex_class_[incident_vertices.second], changes,
                top_targets);
    boost::depth_first_search(skeleton_,
                              boost::visitor(visitor).root_vertex(source));
  }

  // Update the multiplicities and add them to the graph
  for (auto [source, remaining_map] : changes) {
    std::map<HasseDiagram::Vertex, int> to_subtract;
    visitors::MultiplicitySubtractor<HasseDiagram::Vertex,
                                     HasseDiagram::FilteredGraph>
        subtractor(remaining_map, to_subtract);
    boost::depth_first_search(skeleton_, boost::visitor(subtractor).
                              root_vertex(top_targets[source]));
    for (auto [target, delta] : to_subtract) {
      assert(to_subtract.size() == remaining_map.size());
      // TODO (later): I could do this in one line (here and elsewhere)
      auto edge = boost::add_edge(source, target, diagram_).first;
      diagram_[edge].edge_of_gfodd = remaining_map[target].edge_of_gfodd;
      diagram_[edge].multiplicity = remaining_map[target].multiplicity - delta;
    }
  }
}

// TODO: maybe create some private methods that simplify all that
// make_iterator_range boilerplate

void HasseDiagram::SplitVertexClass(HasseDiagram::Vertex vertex_class,
                                    HasseDiagram::EdgeDescriptor edge_to_remove,
                                    int new_size) {
  // Update the size of the old vertex
  int old_size = diagram_[vertex_class].size();
  assert(old_size > new_size);
  diagram_[vertex_class].set_size(old_size - new_size);

  // Add a new vertex
  auto new_vertex = boost::add_vertex(diagram_);
  diagram_[new_vertex].set_size(new_size);

  // Copy outgoing edges
  for (auto edge :
           boost::make_iterator_range(boost::out_edges(vertex_class,
                                                       diagram_))) {
    if (edge != edge_to_remove) {
      auto new_edge = boost::add_edge(new_vertex,
                                      boost::target(edge, diagram_),
                                      diagram_).first;
      diagram_[new_edge].edge_of_gfodd = diagram_[edge].edge_of_gfodd;
      diagram_[new_edge].multiplicity = diagram_[edge].multiplicity;
    }
  }

  // Incoming edges split their multiplicities
  for (auto edge :
           boost::make_iterator_range(boost::in_edges(vertex_class,
                                                      diagram_))) {
    assert((diagram_[edge].multiplicity * new_size) % old_size == 0);
    int new_edge_size = diagram_[edge].multiplicity * new_size / old_size;
    diagram_[edge].multiplicity -= new_edge_size;
    auto new_edge = boost::add_edge(boost::source(edge, diagram_), new_vertex,
                                    diagram_).first;
    diagram_[new_edge].edge_of_gfodd = diagram_[edge].edge_of_gfodd;
    diagram_[new_edge].multiplicity = new_edge_size;
  }
}

HasseDiagram HasseDiagram::RemoveOneVertex(HasseDiagram::Vertex vertex_class) {
  int size = diagram_[vertex_class].size();
  assert(size > 0);
  HasseDiagram copy(gfodd_, predicate_arity_);
  boost::copy_graph(diagram_, copy.diagram_);
  if (size == 1) {
    boost::clear_vertex(vertex_class, copy.diagram_);
    boost::remove_vertex(vertex_class, copy.diagram_);
  } else {
    copy.diagram_[vertex_class].set_size(size - 1);
    // TODO: this could be buggy if the set of in-edges is modified inside the
    // for-loop
    for (auto edge :
             boost::make_iterator_range(boost::in_edges(vertex_class,
                                                        copy.diagram_))) {
      auto source = boost::source(edge, copy.diagram_);
      assert((copy.diagram_[source].size() *
              copy.diagram_[edge].multiplicity) % size);
      SplitVertexClass(source, edge, copy.diagram_[source].size() *
                       copy.diagram_[edge].multiplicity / size);
    }
  }
  return copy;
}

bool operator==(const HasseDiagram& a, const HasseDiagram& b) {
  // TODO: check for equality of diagram_
  return false;
}
