#include "variable_positions.h"

#include <boost/log/trivial.hpp>

VariablePositions::VariablePositions(std::string variables) :
    string_representation_(variables) {
  for (std::string::size_type i = 0; i < variables.length(); ++i)
    map_[std::string(1, variables[i])].insert(i);
}

// TODO: should probably test this
VariablePositions::VariablePositions(const VariablePositions &other) :
    string_representation_(other.string_representation_) {
  for (auto [variable, positions] : other.map_) map_[variable] = positions;
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

// TODO: should this be a constructor that accepts an instance of
// VariablePositions and a decoding map?
VariablePositions
VariablePositions::RespectTheMap(std::map<int, std::set<std::string>> decoding)
    const {
  // TODO: do I actually need the '0' cell?
  BOOST_LOG_TRIVIAL(debug) << "VariablePositions::RespectTheMap: this = "
                           << string_representation();
  VariablePositions new_version(*this);

  for (auto& [cell_name, cell] : decoding) {
    if (cell_name != 0) {
      BOOST_LOG_TRIVIAL(debug) << "VariablePositions::RespectTheMap: The "
                               << "following variables are getting merged:";
      std::set<int> positions;
      std::string variable_name;
      for (auto variable : cell) {
        auto it = new_version.map_.find(variable);
        if (it != new_version.map_.end()) {
          BOOST_LOG_TRIVIAL(debug) << "VariablePositions::RespectTheMap: "
                                   << variable;
          variable_name = variable;
          positions.insert(it->second.begin(), it->second.end());
        }
        new_version.map_.erase(it);
      }
      new_version.map_[variable_name] = positions;
    }
  }
  return new_version;
}
