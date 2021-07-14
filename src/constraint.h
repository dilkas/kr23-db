#ifndef CONSTRAINT_H
#define CONSTRAINT_H

#include <set>
#include <vector>

#include "hasse_diagram.h"

// NOTE: A constant constraint across n positions is equivalent to a variable
// constraint across n+1 positions.
class Constraint {
public:
  Constraint(std::vector<std::set<int>> initial_positions) :
    positions_(initial_positions) {}
  HasseDiagram UnionsOfPositions();

 private:
  // For each 'part' of the edge, we hold a set of positions that are occupied
  // by that constant/variable.
  std::vector<std::set<int>> positions_;
};

#endif
