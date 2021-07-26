#include "gfodd.h"

#include <assert.h>
#include <math.h>

#include <algorithm>

#include <boost/graph/depth_first_search.hpp>
#include <boost/graph/graph_utility.hpp>

#include "visitors/path_enumerator.h"

Gfodd::Gfodd() {
  Gfodd::VertexDescriptor v1 = AddVertex("xy", 2);
  Gfodd::VertexDescriptor v2 = AddVertex("yz", 1);
  Gfodd::VertexDescriptor w1 = AddVertex(0.1);
  Gfodd::VertexDescriptor w2 = AddVertex(0.2);
  Gfodd::VertexDescriptor w3 = AddVertex(0.3);

  auto i1 = boost::add_edge(v1, v2, diagram_).first;
  diagram_[i1].positive = true;

  auto e1 = boost::add_edge(v1, w1, diagram_).first;
  diagram_[e1].positive = false;
  auto e2 = boost::add_edge(v2, w2, diagram_).first;
  diagram_[e2].positive = false;
  auto e3 = boost::add_edge(v2, w3, diagram_).first;
  diagram_[e3].positive = true;

  internal_edges_ = {i1};
  source_ = v1;
}

Gfodd::VertexDescriptor Gfodd::AddVertex(std::string variables,
                                         int new_variables) {
  auto vertex = boost::add_vertex(diagram_);
  diagram_[vertex] = Vertex(variables, new_variables);
  return vertex;
}

Gfodd::VertexDescriptor Gfodd::AddVertex(double weight) {
  auto vertex = boost::add_vertex(diagram_);
  diagram_[vertex] = Vertex(weight);
  sinks_.push_back(vertex);
  return vertex;
}

std::vector<Gfodd::VertexDescriptor> Gfodd::Atoms() {
  std::vector<Gfodd::VertexDescriptor> atoms;
  for (auto [vi, vi_end] = boost::vertices(diagram_); vi != vi_end; ++vi)
    if (!diagram_[*vi].sink()) atoms.push_back(*vi);
  return atoms;
}

void Gfodd::FindPaths() {
  std::map<Gfodd::VertexDescriptor,
           std::vector<std::vector<Gfodd::EdgeDescriptor>>> path_map;
  visitors::PathEnumerator<Gfodd::VertexDescriptor, Gfodd::EdgeDescriptor,
                           Gfodd::Graph> visitor(path_map);
  boost::depth_first_search(diagram_,
                            boost::visitor(visitor).root_vertex(source_));
  for (auto sink : sinks_) paths_.push_back(path_map[sink]);
}

// The power of a weight is the sum across all paths in GFODD of the product
// of all edges in the path
double Gfodd::Evaluate(std::vector<int> internal_edge_counts) {
  assert(sinks_.size() == paths_.size());
  assert(internal_edge_counts.size() == internal_edges_.size());

  std::map<Gfodd::EdgeDescriptor, int> edge_counts;
  for (std::vector<int>::size_type i = 0; i < internal_edge_counts.size(); ++i)
    edge_counts[internal_edges_[i]] = internal_edge_counts[i];

  double answer = 1;
  for (std::vector<Gfodd::VertexDescriptor>::size_type i = 0;
       i < sinks_.size(); ++i) {
    int power = 0;
    for (auto path : paths_[i]) {
      int path_contribution = 1;
      for (auto edge : path) {
        auto it = edge_counts.find(edge);
        if (it != edge_counts.end()) path_contribution *= it->second;
      }
      power += path_contribution;
    }
    answer *= pow(diagram_[sinks_[i]].weight(), power);
  }
  return answer;
}
