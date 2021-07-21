#include "variable_positions.h"

VariablePositions::VariablePositions(std::string variables) :
  string_representation_(variables) {
  for (int i = 0; i < variables.length(); ++i)
    map_[std::string(1, variables[i])].insert(i);
}

// TODO: test (and make sure that this is the right thing to do)
void VariablePositions::Insert(VariablePositions other) {
  for (auto [variable, positions] : other.map_) {
    auto it = map_.find(variable);
    if (it == map_.end()) {
      map_[variable] = positions;
    } else {
      it->second.insert(positions.begin(), positions.end());
    }
  }
}
