#include "vertex.h"

VariablePositions Vertex::positions() {
  assert(!sink_);
  return positions_;
}
