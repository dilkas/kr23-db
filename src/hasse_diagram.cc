#include "hasse_diagram.h"

#include <assert.h>
#include <math.h>

#include <map>
#include <vector>

#include <boost/graph/topological_sort.hpp>
#include <boost/log/trivial.hpp>

#include "misc.h"
#include "visitors/parent_finder.h"
#include "visitors/source_visitor.h"

HasseDiagram::HasseDiagram(Gfodd gfodd) :
  predicate_(diagram_), skeleton_(diagram_, predicate_),
  edge_counts_(gfodd.NumInternalEdges()) {
  tops_.insert(boost::add_vertex(diagram_));
  // TODO: gfodd should be a field
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
      boost::breadth_first_search(skeleton_, top, boost::visitor(parent_finder));
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
  for (int i = 0; i < parents.size(); ++i) {
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

// TODO: should this take into account path counts?
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

void HasseDiagram::InitialiseEdges(int domain_size, Gfodd gfodd) {
  std::vector<Change<HasseDiagram::Vertex>> changes;
  for (int i = 0; i < gfodd.NumInternalEdges(); ++i) {
    auto incident_vertices = gfodd.Incident(i);
    auto source = corresponding_vertex_class_[incident_vertices.first];
    visitors::SourceVisitor<HasseDiagram::Vertex, HasseDiagram::FilteredGraph>
      visitor(i, std::pow(domain_size,
                          gfodd.NumNewVariables(incident_vertices.second)),
              gfodd.Positions(incident_vertices.first), source,
              gfodd.Positions(incident_vertices.second),
              corresponding_vertex_class_[incident_vertices.second], changes);
    boost::depth_first_search(skeleton_,
                              boost::visitor(visitor).root_vertex(source));
  }

  for (auto change : changes) {
    auto edge = boost::add_edge(change.source, change.target, diagram_).first;
    diagram_[edge].edge_of_gfodd = change.edge_of_gfodd;
    diagram_[edge].multiplicity = change.multiplicity;
    edge_counts_[change.edge_of_gfodd] += change.multiplicity;
    // TODO: but these multiplicities are incorrect
  }
}

// TODO (later): update to update edges and perhaps create new vertex classes
void HasseDiagram::RemoveOneVertex(HasseDiagram::Vertex vertex_class) {
  int size = diagram_[vertex_class].size();
  assert(size > 0);
  diagram_[vertex_class].set_size(size - 1);
}
