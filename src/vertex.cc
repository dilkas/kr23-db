#include "vertex.h"

Vertex::Vertex(std::string variables) {
  sink_ = false;
  for (int i = 0; i < variables.length(); ++i)
    positions_[std::string(1, variables[i])].insert(i);
}

Vertex::VariablePositions Vertex::positions() {
  assert(!sink_);
  return positions_;
}
