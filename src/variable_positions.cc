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

VariablePositions
VariablePositions::RespectTheMap(std::map<int, std::set<std::string>> decoding) {
  VariablePositions new_version;
  for (auto& [cell_name, cell] : decoding) {
    if (cell_name == 0)
      for (auto variable : cell) new_version.map_[variable] = map_[variable];

    std::set<int> positions;
    std::string variable_name;
    for (auto variable : cell) {
      variable_name = variable;
      positions.insert(map_[variable].begin(), map_[variable].end());
    }
    new_version.map_[variable_name] = positions;
  }
  return new_version;
}
