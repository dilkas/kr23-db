#ifndef VERTEX_CLASS_H
#define VERTEX_CLASS_H

#include <map>
#include <set>
#include <string>

#include "encoding.h"
#include "misc.h"
#include "variable_positions.h"

class VertexClass {
 public:
  VertexClass() {}

  int size() { return size_; }
  void set_size(int size) { size_ = size; }

  void set_positions(VariablePositions positions) { positions_.Set(positions); }
  int FullSize(int domain_size, int predicate_arity);
  Match IsSubsetOf(Encoding other) const { return positions_.IsSubsetOf(other); }
  Match IsSubsetOf(VariablePositions other) const;

  std::map<int, std::set<std::string>>
  MatchAString(std::string variables) const {
    return positions_.MatchAString(variables);
  }

 private:
  Encoding positions_;
  int size_;
};

#endif
