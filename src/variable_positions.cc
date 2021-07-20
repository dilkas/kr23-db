#include "variable_positions.h"

VariablePositions::VariablePositions(std::string variables) {
  for (int i = 0; i < variables.length(); ++i)
    map_[std::string(1, variables[i])].insert(i);
}
