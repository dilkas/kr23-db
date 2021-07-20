#ifndef VERTEX_CLASS_H
#define VERTEX_CLASS_H

#include <set>

#include "match_quality.h"
#include "optimised_variable_positions.h"
#include "variable_positions.h"

class VertexClass {
 public:
  VertexClass() {}

  int size() { return size_; }
  void set_size(int size) { size_ = size; }

  void set_positions(VariablePositions positions) { positions_.Set(positions); }
  int FullSize(int domain_size, int predicate_arrity);
  MatchQuality IsSubsetOf(VariablePositions other) const;

 private:
  OptimisedVariablePositions positions_;
  int size_;
};

#endif
