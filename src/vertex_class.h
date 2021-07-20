#ifndef VERTEX_CLASS_H
#define VERTEX_CLASS_H

#include <set>

#include "optimised_variable_positions.h"
#include "variable_positions.h"

// TODO: rework this to handle multiple variables
class VertexClass {
 public:
  VertexClass() {}

  int size() { return size_; }
  void set_size(int size) { size_ = size; }

  void set_positions(VariablePositions positions) { positions_.Set(positions); }
  bool IsSubsetOf(std::set<int> other_set) const;
  int FullSize(int domain_size, int predicate_arrity);

 private:
  // Standard representation of variable positions, discarding variable names
  // and variables that occur only once
  OptimisedVariablePositions positions_;
  int size_;
};

#endif
