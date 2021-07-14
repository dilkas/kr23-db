#include "hasse_diagram.h"

HasseDiagram::HasseDiagram() {
  tops_.push_back(boost::add_vertex(diagram_));
}

std::set<int> HasseDiagram::positions(HasseDiagram::Vertex vertex) {
  return diagram_[vertex].positions();
}

HasseDiagram::Vertex HasseDiagram::AddVertexClass(std::set<int> position_set) {
  std::vector<HasseDiagram::Vertex> parents;
  for (auto top : tops_) {
    if (diagram_[top].IsSubsetOf(position_set)) {
      if (diagram_[top].Size() == position_set.size())
        return boost::graph_traits<Graph>::null_vertex();
      parents.push_back(top);
    }
  }
  assert(!parents.empty());
  HasseDiagram::Vertex new_vertex = boost::add_vertex(diagram_);
  diagram_[new_vertex].set_positions(position_set);
  for (auto parent : parents) boost::add_edge(parent, new_vertex, diagram_);
  return new_vertex;
}
