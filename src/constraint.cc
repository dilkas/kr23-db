#include "constraint.h"

HasseDiagram Constraint::UnionsOfPositions() {
  HasseDiagram diagram;
  std::vector<HasseDiagram::Vertex> top_layer;

  // Add the sets themselves
  for (auto position_set : positions_)
    top_layer.push_back(diagram.AddVertexClass(position_set));
  diagram.set_tops(top_layer);

  for (int i = 1; i < positions_.size(); ++i) {
    std::vector<HasseDiagram::Vertex> new_top_layer;
    int j = i; // which position to add first
    for (auto vertex : top_layer) {
      for (int k = j; k < positions_.size(); ++k) {
        std::set<int> new_set = diagram.positions(vertex);
        new_set.insert(positions_[k].begin(), positions_[k].end());
        new_top_layer.push_back(diagram.AddVertexClass(new_set));
      }
      ++j;
    }
    top_layer = new_top_layer;
    diagram.set_tops(top_layer);
  }
  return diagram;
}
