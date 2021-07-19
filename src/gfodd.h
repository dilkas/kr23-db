#ifndef GFODD_H
#define GFODD_H

#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <boost/graph/adjacency_list.hpp>

#include "vertex.h"

class Gfodd {
public:
  Gfodd();
  // double WeightOfInterpretation(HasseDiagram interpretation);
  int NumInternalEdges() { return internal_edges_.size(); }
  std::pair<Vertex::VariablePositions,
            Vertex::VariablePositions> IncidentAtomInfo(int internal_edge_index);

private:
  struct Edge {
    bool positive;
  };

  typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS,
                                Vertex, Edge> Graph;
  typedef boost::graph_traits<Graph>::edge_descriptor EdgeDescriptor;

  Graph diagram_;
  std::vector<EdgeDescriptor> internal_edges_;
};

#endif
