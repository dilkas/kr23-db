#include "vertex_class.h"

bool VertexClass::IsSubsetOf(std::set<int> other_set) const {
  for (auto element : positions_)
    if (other_set.find(element) == other_set.end()) return false;
  return true;
}
