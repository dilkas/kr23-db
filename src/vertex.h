#ifndef VERTEX_H
#define VERTEX_H

#include <cassert>

#include <string>

#include "variable_positions.h"

class Vertex { // TODO: consider using a union type
 public:
  Vertex() {}
  Vertex(std::string variables) : sink_(false), positions_(variables) {}
  Vertex(double weight) : sink_(true), weight_(weight) {};
  VariablePositions positions();

 private:
  bool sink_;
  VariablePositions positions_;
  double weight_; // for sinks
};

#endif
