#include "vertex.h"

int Vertex::new_variables() {
  assert(!sink_);
  return new_variables_;
}

VariablePositions Vertex::positions() {
  assert(!sink_);
  return positions_;
}
