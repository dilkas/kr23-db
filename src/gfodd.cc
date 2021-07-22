#include "gfodd.h"

#include <assert.h>

#include <boost/graph/graph_utility.hpp>

Gfodd::Gfodd() {
  auto v1 = boost::add_vertex(diagram_);
  diagram_[v1] = Vertex("xy");
  auto v2 = boost::add_vertex(diagram_);
  diagram_[v2] = Vertex("yz");

  auto w1 = boost::add_vertex(diagram_);
  diagram_[w1] = Vertex(0.1);
  auto w2 = boost::add_vertex(diagram_);
  diagram_[w2] = Vertex(0.2);
  auto w3 = boost::add_vertex(diagram_);
  diagram_[w3] = Vertex(0.3);

  auto i1 = boost::add_edge(v1, v2, diagram_).first;
  diagram_[i1].positive = true;

  auto e1 = boost::add_edge(v1, w1, diagram_).first;
  diagram_[e1].positive = false;
  auto e2 = boost::add_edge(v2, w2, diagram_).first;
  diagram_[e2].positive = false;
  auto e3 = boost::add_edge(v2, w3, diagram_).first;
  diagram_[e3].positive = true;

  internal_edges_ = {i1};
}

std::vector<Gfodd::VertexDescriptor> Gfodd::Atoms() {
  std::vector<Gfodd::VertexDescriptor> atoms;
  for (auto [vi, vi_end] = boost::vertices(diagram_); vi != vi_end; ++vi)
    if (!diagram_[*vi].sink()) atoms.push_back(*vi);
  return atoms;
}
