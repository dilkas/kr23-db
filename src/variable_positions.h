#ifndef VARIABLE_POSITIONS_H
#define VARIABLE_POSITIONS_H

#include <map>
#include <set>
#include <string>

// A map from variable names to sets of indices (for non-sink vertices)
class VariablePositions {
 public:
  VariablePositions() {}
  VariablePositions(std::string variables);

 private:
  std::map<std::string, std::set<int>> map_;
};

#endif
