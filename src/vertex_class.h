#ifndef VERTEX_CLASS_H
#define VERTEX_CLASS_H

#include <set>

// TODO: rework this to handle multiple variables
class VertexClass {
 public:
  VertexClass() {}
  VertexClass(std::set<int> initial_positions) : positions_(initial_positions) {}
  std::set<int> positions() { return positions_; }
  void set_positions(std::set<int> positions) { positions_ = positions; }
  int size() { return size_; }
  void set_size(int size) { size_ = size; }
  bool IsSubsetOf(std::set<int> other_set) const;
  int NumPositions() const { return positions_.size(); }

 private:
  std::set<int> positions_;
  int size_;
};

#endif
