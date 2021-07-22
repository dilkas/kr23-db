#ifndef VARIABLE_POSITIONS_H
#define VARIABLE_POSITIONS_H

#include <map>
#include <set>
#include <string>

// A map from variable names to sets of indices
class VariablePositions {
 public:
  VariablePositions() {}
  VariablePositions(std::string variables);
  // NOTE: can be outdated if Insert() is called
  std::string string_representation() const { return string_representation_; };
  bool OccursOnlyOnce(std::string variable) const {
    return map_.at(variable).size() == 1;
  }
  void Insert(VariablePositions other);
  // Merge variables that are associated with the same integer
  VariablePositions RespectTheMap(std::map<int, std::set<std::string>> decoding);

 private:
  std::string string_representation_;
  std::map<std::string, std::set<int>> map_;
};

#endif
