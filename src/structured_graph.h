#ifndef STRUCTURED_GRAPH_H
#define STRUCTURED_GRAPH_H

#include <vector>

#include "hasse_diagram.h"

class StructuredGraph {
 public:
  StructuredGraph(int domain_size, int predicate_arity);

private:
  HasseDiagram vertex_classes_;
};

#endif
