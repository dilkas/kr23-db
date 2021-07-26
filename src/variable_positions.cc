#include "variable_positions.h"

#include <boost/log/trivial.hpp>

VariablePositions::VariablePositions(std::string variables) :
    string_representation_(variables) {
  for (std::string::size_type i = 0; i < variables.length(); ++i)
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
VariablePositions::RespectTheMap(std::map<int, std::set<std::string>> decoding)
    const {
  BOOST_LOG_TRIVIAL(debug) << "VariablePositions::RespectTheMap: this = "
                           << string_representation();
  VariablePositions new_version;
  for (auto& [cell_name, cell] : decoding) {
    if (cell_name == 0) {
      BOOST_LOG_TRIVIAL(debug) << "VariablePositions::RespectTheMap: The "
                               << "following variables stay as they are:";
      for (auto variable : cell) {
        auto it = map_.find(variable);
        if (it != map_.end()) {
          BOOST_LOG_TRIVIAL(debug) << "VariablePositions::RespectTheMap: "
                                   << variable;
          new_version.map_[variable] = it->second;
        }
      }
    }

    std::set<int> positions;
    std::string variable_name;
    for (auto variable : cell) {
      auto it = map_.find(variable);
      if (it != map_.end()) {
        variable_name = variable;
        positions.insert(it->second.begin(), it->second.end());
      }
    }
    new_version.map_[variable_name] = positions;
  }
  return new_version;
}
