#ifndef STRUCTURED_GRAPH_H
#define STRUCTURED_GRAPH_H

#include <vector>

#include "constraint.h"
#include "edge.h"

class StructuredGraph {
 public:
  StructuredGraph(int domain_size, int predicate_arity, Constraint constraint);

private:
  HasseDiagram vertex_classes_;
  std::vector<Edge> edges_;
};

#endif
