#include "structured_graph.h"

StructuredGraph::StructuredGraph(int domain_size, int predicate_arity,
                                 Constraint constraint) {
  vertex_classes_ = constraint.UnionsOfPositions();
  vertex_classes_.InstantiateSizes(domain_size, predicate_arity);
  edges_ = {Edge(vertex_classes_.corresponding_vertex_class())};
}
