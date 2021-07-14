#include "structured_graph.h"

StructuredGraph::StructuredGraph(int domain_size, int predicate_arity,
                                 Constraint constraint) {
  vertex_classes_ = constraint.UnionsOfPositions();
  // TODO: add edges from the constraint
  // 1. For each part of the constraint
  // 1.1. Identify the lowest VC such that constraint is a subset of VC if |constraint| > 1 else bot
  // 2. The edge will connect these VCs
}
