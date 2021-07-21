#ifndef ENCODING_H
#define ENCODING_H

#include <vector>

#include "match_quality.h"
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
  void Set(const VariablePositions& positions);
  MatchQuality IsSubsetOf(Encoding other) const;
 private:
  std::vector<int> representation_;
};

#endif
