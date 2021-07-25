#include "structured_graph.h"

StructuredGraph::StructuredGraph(int domain_size, int predicate_arity) {
  Gfodd gfodd;
  HasseDiagram graph(gfodd);
  graph.InitialiseVertices(gfodd);
  graph.InstantiateSizes(domain_size, predicate_arity);
  graph.InitialiseEdges(domain_size, gfodd);
}
