#ifndef VERTEX_H
#define VERTEX_H

#include <assert.h>

#include <string>

#include "variable_positions.h"

class Vertex {
 public:
  Vertex() {}
  Vertex(std::string variables) : sink_(false), positions_(variables) {}
  Vertex(double weight) : sink_(true), weight_(weight) {};
  VariablePositions positions();
  bool sink() { return sink_; }

 private:
  bool sink_;
  VariablePositions positions_;
  double weight_; // for sinks
};

#endif
