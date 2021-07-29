#include <iostream>
#include <set>
#include <stack>

#include "gfodd.h"
#include "hasse_diagram.h"

struct PartialProblem {
  int coefficient, next_divisor;
  HasseDiagram graph;
};

double run(Gfodd gfodd, int domain_size, int predicate_arity) {
  // Initialise everything
  HasseDiagram initial_graph(gfodd, predicate_arity);
  initial_graph.InitialiseVertices();
  initial_graph.InstantiateSizes(domain_size);
  initial_graph.InitialiseEdges(domain_size);

  // Compute the WFOMC
  double answer = 0;
  std::stack<PartialProblem> remaining;
  // std::set<HasseDiagram> visited;  // TODO: implement equality, hashing, and caching
  remaining.push({1, 1, initial_graph});
  while (!remaining.empty()) {
    PartialProblem instance = remaining.top();
    remaining.pop();
    answer += instance.coefficient *
              gfodd.Evaluate(instance.graph.edge_counts());
    for (auto vertex_class :
             boost::make_iterator_range(instance.graph.VertexClassIterator())) {
      HasseDiagram removed = instance.graph.RemoveOneVertex(vertex_class);
      // if (visited.find(removed) == visited.end()) {
      // visited.insert(removed);
        remaining.push({instance.coefficient * instance.next_divisor *
            instance.graph.Size(vertex_class), instance.next_divisor + 1,
            removed});
        // }
    }
  }
  return answer;
}

int main() {
  Gfodd gfodd;
  std::cout << run(gfodd, 10, 2) << std::endl;
}
