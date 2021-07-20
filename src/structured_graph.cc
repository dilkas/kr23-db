#include "structured_graph.h"

#include <math.h>

#include <limits>

StructuredGraph::StructuredGraph(int domain_size, int predicate_arity) {
  // TODO: initialise the vertices of vertex_classes_
  vertex_classes_.InstantiateSizes(domain_size, predicate_arity);
  vertex_classes_.RemoveEdges();
  Gfodd gfodd;
  // vertex_classes_.InitialiseEdges(gfodd);
}
