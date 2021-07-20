#ifndef OPTIMISED_VARIABLE_POSITIONS_H
#define OPTIMISED_VARIABLE_OPSITIONS_H

#include <vector>

#include "variable_positions.h"

class OptimisedVariablePositions {
 public:
  OptimisedVariablePositions() {}
  void Set(VariablePositions &positions);
 private:
  std::vector<int> representation_;
};

#endif
