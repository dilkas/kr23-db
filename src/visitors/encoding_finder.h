#ifndef ENCODING_FINDER_H
#define ENCODING_FINDER_H

#include <boost/graph/depth_first_search.hpp>

#include "encoding.h"
#include "hasse_diagram.h"
#include "misc.h"

namespace visitors {

  // First we discover a vertex and record its relationship to the encoding. If
  // they're equal, immediately exit the search. If they're unrelated, the
  // terminator skips all out-edges.
  class EncodingFinder : public boost::default_dfs_visitor {
  public:
  EncodingFinder(Encoding& encoding, Match::Quality &match,
                 HasseDiagram::Vertex& finding) :
    encoding_(encoding), match_(match), finding_(finding) {}

    void discover_vertex(HasseDiagram::Vertex vertex,
                         const HasseDiagram::Graph& graph) const;

  private:
    Encoding& encoding_;
    HasseDiagram::Vertex& finding_;
    Match::Quality& match_;
  };

} // namespace visitors

#endif
