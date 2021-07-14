#ifndef EDGE_H
#define EDGE_H

#include <vector>

class Edge {
 public:
  Edge();

private:
  int count_;
  std::vector<int> vertex_classes_;
  // Used to indicate if the edge is incident with the same vertex multiple
  // times
  std::vector<int> ids_;
};

#endif
