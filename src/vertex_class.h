#ifndef VERTEX_CLASS_H
#define VERTEX_CLASS_H

#include <set>

class VertexClass {
 public:
  VertexClass() {}
  VertexClass(std::set<int> initial_positions) : positions_(initial_positions) {}
  std::set<int> positions() { return positions_; }
  void set_positions(std::set<int> positions) { positions_ = positions; }
  bool IsSubsetOf(std::set<int> other_set);
  int Size() { return positions_.size(); }

 private:
  std::set<int> positions_;
};

#endif
