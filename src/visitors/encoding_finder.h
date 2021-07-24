#ifndef ENCODING_FINDER_H
#define ENCODING_FINDER_H

#include <boost/graph/depth_first_search.hpp>

#include "encoding.h"
#include "misc.h"

namespace visitors {

  // First we discover a vertex and record its relationship to the encoding. If
  // they're equal, immediately exit the search. If they're unrelated, the
  // terminator skips all out-edges.
  template <typename Vertex, typename Graph>
  class EncodingFinder : public boost::default_dfs_visitor {
  public:
  EncodingFinder(Encoding& encoding, Match::Quality &match,
                 Vertex& finding) :
    encoding_(encoding), match_(match), finding_(finding) {}

    void discover_vertex(Vertex vertex, const Graph& graph) const;

  private:
    Encoding& encoding_;
    Vertex& finding_;
    Match::Quality& match_;
  };

} // namespace visitors

#endif
