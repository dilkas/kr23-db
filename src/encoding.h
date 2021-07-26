#ifndef ENCODING_H
#define ENCODING_H

#include <map>
#include <set>
#include <string>
#include <vector>

#include "misc.h"
#include "variable_positions.h"

// Standardised representation of variable positions, discarding variable names
// and variables that occur only once.
//
// Variables that occur only once are represented by a zero. All other variables
// are mapped to positive integers in the order in which they occur in the
// string.
class Encoding {
 public:
  Encoding() {}
  Match IsSubsetOf(Encoding other) const;  // i.e., is less restrictive than
  int CountRedundantPositions();
  std::string AsString() const;  // only for logging/testing

  void Set(const VariablePositions& positions);
  void Set(int num_vars) {
    for (int i = 1; i <= num_vars; ++i) representation_.push_back(0);
  }

  std::map<int, std::set<std::string>>
  MatchAString(std::string variables) const;

 private:
  std::vector<int> representation_;
};

#endif
