#ifndef EDGE_H
#define EDGE_H

#include <vector>

class Edge {
public:
  Edge(std::vector<HasseDiagram::Vertex> vertex_classes) :
    vertex_classes_(vertex_classes) {}

private:
  std::vector<HasseDiagram::Vertex> vertex_classes_;
};

#endif
