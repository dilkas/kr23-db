#include <map>

#include "optimised_variable_positions.h"

// TODO: reserve zero for all variables with single occurrences
// TODO: test
void OptimisedVariablePositions::Set(VariablePositions &positions) {
  representation_.clear();
  std::map<char, int> variable_encodings;
  int next_int = 0;
  for (char variable : positions.string_representation()) {
    auto it = variable_encodings.find(variable);
    if (it == variable_encodings.end()) {
      variable_encodings[variable] = next_int;
      representation_.push_back(next_int++);
    } else {
      representation_.push_back(it->second);
    }
  }
}
