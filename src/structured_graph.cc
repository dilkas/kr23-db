#include <cmath>
#include <limits>

#include "structured_graph.h"

StructuredGraph::StructuredGraph(int domain_size, int predicate_arity,
                                 Constraint constraint) {
  vertex_classes_ = constraint.UnionsOfPositions(); // TODO: update this line
  vertex_classes_.InstantiateSizes(domain_size, predicate_arity);
  vertex_classes_.RemoveEdges();
  Gfodd gfodd;
  // vertex_classes_.InitialiseEdges(gfodd);
}
