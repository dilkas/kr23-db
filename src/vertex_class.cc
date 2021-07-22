#include "vertex_class.h"

#include "math.h"

int VertexClass::FullSize(int domain_size, int predicate_arity) {
  return std::pow((double)domain_size,
                  predicate_arity - positions_.CountRedundantPositions());
}

Match VertexClass::IsSubsetOf(VariablePositions other) const {
  Encoding encoding;
  encoding.Set(other);
  return positions_.IsSubsetOf(encoding);
}
