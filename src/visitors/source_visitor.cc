#include "source_visitor.h"

#include "encoding.h"
#include "hasse_diagram.h"
#include "visitors/encoding_finder.h"
#include "visitors/target_visitor.h"

namespace visitors {

template <typename Vertex, typename Graph>
void SourceVisitor<Vertex, Graph>::discover_vertex(Vertex vertex,
                                                   const Graph& graph) const {
  // Identify matching variables from source_variables and source_vertex
  auto decoding = graph[source_vertex_].
                  MatchAString(source_variables_.string_representation());

  // Transform target_variables to match these constraints
  auto new_variables = target_variables_.RespectTheMap(decoding);

  // Encode them
  Encoding encoding;
  encoding.Set(new_variables);

  // Find the descendant of parent_of_target_ that matches the encoding, i.e.,
  // find the target
  Match::Quality last_match;
  Vertex target = boost::graph_traits<Graph>::null_vertex();
  visitors::EncodingFinder<Vertex, Graph> encoding_finder(encoding, target,
                                                          last_match);
  std::vector<boost::default_color_type> colors(boost::num_vertices(graph));

  const auto terminator = [last_match](Vertex, const Graph&) {
    return last_match == Match::Quality::kNotASubset;
  };

  try {
    boost::depth_first_visit(graph, parent_of_target_,
                             encoding_finder, colors.data(),
                             terminator);
  } catch (EndSearchException& exception) {}
  assert(target != boost::graph_traits<Graph>::null_vertex());
  top_target_[vertex] = target;

  // Run the second half the edge construction algorithm
  visitors::TargetVisitor<Vertex, Graph>
      visitor(edge_of_gfodd_, total_multiplicity_, vertex, changes_);
  boost::depth_first_search(graph,
                            boost::visitor(visitor).root_vertex(target));
}

template class SourceVisitor<HasseDiagram::Vertex,
                             HasseDiagram::FilteredGraph>;

}  // namespace visitors
