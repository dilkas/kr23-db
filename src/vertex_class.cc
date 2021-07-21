#include "vertex_class.h"

// TODO: implement
int VertexClass::FullSize(int domain_size, int predicate_arrity) {
  return 0;
  //return std::pow((double)domain_size, predicate_arity - positions_.RedundantPositionCount());
}

MatchQuality VertexClass::IsSubsetOf(VariablePositions other) const {
  Encoding encoding;
  encoding.Set(other);
  return positions_.IsSubsetOf(encoding);
}
