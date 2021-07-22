#ifndef MATCH_H
#define MATCH_H

struct Match {
  enum class Quality { kNotASubset, kSubset, kEqual };

  Quality quality;
  int diff_in_variables;
};

#endif
