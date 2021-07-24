#ifndef MISC_H
#define MISC_H

#include <exception>

template <typename Vertex>
struct Change {
  Vertex source, target;
  int multiplicity, edge_of_gfodd;
};

struct EndSearchException : public std::exception {};

struct Match {
  enum class Quality { kNotASubset, kSubset, kEqual };

  Quality quality;
  int diff_in_variables;
};

#endif