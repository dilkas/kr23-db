#include "source_visitor.h"

#include "encoding.h"
#include "visitors/encoding_finder.h"
#include "visitors/target_visitor.h"

namespace visitors {

  void SourceVisitor::discover_vertex(HasseDiagram::Vertex vertex,
                                      const HasseDiagram::Graph& graph) const {
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
    HasseDiagram::Vertex target =
      boost::graph_traits<HasseDiagram::Graph>::null_vertex();
    visitors::EncodingFinder encoding_finder(encoding, last_match, target);
    std::vector<boost::default_color_type> colors(boost::num_vertices(graph));
    const auto terminator = [last_match](HasseDiagram::Vertex vertex,
                                         const HasseDiagram::Graph& graph) {
      return last_match == Match::Quality::kNotASubset;
    };
    try {
      boost::depth_first_visit(graph, parent_of_target_,
                               encoding_finder, colors.data(),
                               terminator);
    } catch (EndSearchException& exception) {}
    assert(target != boost::graph_traits<HasseDiagram::Graph>::null_vertex());

    visitors::TargetVisitor visitor(edge_of_gfodd_, vertex, changes_);
    boost::depth_first_search(graph,
                              boost::visitor(visitor).root_vertex(target));
  }

}
