#ifndef VERTEX_H
#define VERTEX_H

#include <assert.h>

#include <string>

#include "variable_positions.h"

class Vertex {
 public:
  Vertex() {}
  Vertex(std::string variables, int new_variables) :
      sink_(false), new_variables_(new_variables), positions_(variables) {}
  explicit Vertex(double weight) : sink_(true), weight_(weight) {}

  bool sink() { return sink_; }
  int new_variables();
  VariablePositions positions();
  double weight();

 private:
  bool sink_;
  int new_variables_;
  VariablePositions positions_;
  double weight_;  // for sinks
};

#endif
