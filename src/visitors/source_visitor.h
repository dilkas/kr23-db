#ifndef SOURCE_VISITOR_H
#define SOURCE_VISITOR_H

#include <map>
#include <vector>

#include <boost/graph/depth_first_search.hpp>

#include "misc.h"
#include "variable_positions.h"

namespace visitors {

// For each possible source (i.e., all descendants of the first source),
// determine all possible targets.
//
// Find equality constraints on variables that transfer from one GFODD vertex
// to another. These constraints occur when considering descendants of from
// source vertex.
//
// NOTE: This visitor and other subsequently called visitors only traverse
// using non-GFODD edges because at this stage GFODD edges are not added to
// the graph yet.
template <typename Vertex, typename Graph>
class SourceVisitor : public boost::default_dfs_visitor {
 public:
  SourceVisitor(int edge_of_gfodd, int total_multiplicity,
                VariablePositions source_variables,
                VariablePositions target_variables, Vertex source_vertex,
                Vertex parent_of_target,
                std::map<Vertex, std::map<Vertex, Change>>& changes,
                std::map<Vertex, Vertex>& top_target) :
      edge_of_gfodd_(edge_of_gfodd), total_multiplicity_(total_multiplicity),
      source_variables_(source_variables), target_variables_(target_variables),
      source_vertex_(source_vertex), parent_of_target_(parent_of_target),
      changes_(changes), top_target_(top_target) {}

  void discover_vertex(Vertex vertex, const Graph& graph) const;

 private:
  // inputs
  int edge_of_gfodd_, total_multiplicity_;
  VariablePositions source_variables_, target_variables_;
  Vertex source_vertex_, parent_of_target_;

  // outputs
  std::map<Vertex, std::map<Vertex, Change>>& changes_;
  std::map<Vertex, Vertex>& top_target_;
};

}  // namespace visitors

#endif
