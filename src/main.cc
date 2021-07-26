#include <iostream>

#include "gfodd.h"
#include "hasse_diagram.h"

int main() {
  int domain_size = 10;
  int predicate_arity = 2;
  Gfodd gfodd;
  HasseDiagram graph(gfodd, predicate_arity);
  graph.InitialiseVertices();
  graph.InstantiateSizes(domain_size);
  graph.InitialiseEdges(domain_size);
  std::cout << gfodd.Evaluate(graph.edge_counts()) << std::endl;
}
