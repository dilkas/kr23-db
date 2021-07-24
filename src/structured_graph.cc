#include "structured_graph.h"

StructuredGraph::StructuredGraph(int domain_size, int predicate_arity) {
  // TODO: initialise the vertices of vertex_classes_
  vertex_classes_.InstantiateSizes(domain_size, predicate_arity);
  Gfodd gfodd;
  vertex_classes_.InitialiseEdges(domain_size, gfodd);
}
