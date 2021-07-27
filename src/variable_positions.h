#ifndef VARIABLE_POSITIONS_H
#define VARIABLE_POSITIONS_H

#include <map>
#include <set>
#include <string>

// A map from variable names to sets of indices
class VariablePositions {
 public:
  VariablePositions() {}
  explicit VariablePositions(std::string variables);
  VariablePositions(const VariablePositions &other);
  VariablePositions& operator=(const VariablePositions&) = default;

  // NOTE: This representation is never updated, so it may be uninitialised or
  // outdated (e.g., if Insert() is called)
  std::string string_representation() const { return string_representation_; }

  void Insert(VariablePositions other);

  bool OccursOnlyOnce(std::string variable) const {
    return map_.at(variable).size() == 1;
  }

  // Merge variables that are associated with the same integer
  VariablePositions
  RespectTheMap(std::map<int, std::set<std::string>> decoding) const;

 private:
  std::string string_representation_;
  std::map<std::string, std::set<int>> map_;
};

#endif
