#include "vertex_class.h"

// TODO: implement
bool VertexClass::IsSubsetOf(std::set<int> other_set) const {
  // for (auto element : positions_)
  //   if (other_set.find(element) == other_set.end()) return false;
  return true;
}

// TODO: implement
int VertexClass::FullSize(int domain_size, int predicate_arrity) {
  return 0;
  //return std::pow((double)domain_size, predicate_arity - positions_.RedundantPositionCount());
}
