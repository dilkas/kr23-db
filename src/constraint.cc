#include "constraint.h"

HasseDiagram Constraint::UnionsOfPositions() {
  HasseDiagram diagram;
  std::vector<std::set<int>> top_layer;

  // Add the sets themselves
  for (auto position_set : positions_) {
    top_layer.push_back(position_set);
    // Only add a position set to the Hasse diagram if it has at least two
    // positions
    if (position_set.size() > 1) {
      diagram.AddVertexClass(position_set);
    }
  }

  for (int i = 1; i < positions_.size(); ++i) {
    std::vector<std::set<int>> new_top_layer;
    int j = i; // which position to add first
    for (auto position_set : top_layer) {
      for (int k = j; k < positions_.size(); ++k) {
        std::set<int> new_set = position_set;
        new_set.insert(positions_[k].begin(), positions_[k].end());
        diagram.AddVertexClass(new_set);
        new_top_layer.push_back(new_set);
      }
      ++j;
    }
    top_layer = new_top_layer;
  }
  return diagram;
}
