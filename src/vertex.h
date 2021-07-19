#ifndef VERTEX_H
#define VERTEX_H

#include <cassert>

#include <map>
#include <set>
#include <string>

class Vertex { // TODO: consider using a union type
 public:
  using VariablePositions = std::map<std::string, std::set<int>>;
  Vertex() {}
  Vertex(std::string variables);
  Vertex(double weight) : sink_(true), weight_(weight) {};
  VariablePositions positions();

 private:
  bool sink_;
  // A map from variable names to sets of indices (for non-sink vertices)
  VariablePositions positions_;
  double weight_; // for sinks
};

#endif
