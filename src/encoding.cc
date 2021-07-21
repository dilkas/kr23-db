#include "encoding.h"

#include <assert.h>

#include <map>

void Encoding::Set(const VariablePositions& positions) {
  representation_.clear();
  std::map<char, int> variable_encodings;
  int next_int = 1;
  for (char variable : positions.string_representation()) {
    if (positions.OccursOnlyOnce(std::string(1, variable))) {
      representation_.push_back(0);
      continue;
    }
    auto it = variable_encodings.find(variable);
    if (it == variable_encodings.end()) {
      variable_encodings[variable] = next_int;
      representation_.push_back(next_int++);
    } else {
      representation_.push_back(it->second);
    }
  }
}

MatchQuality Encoding::IsSubsetOf(Encoding other) const {
  assert(representation_.size() == other.representation_.size());
  std::map<int, int> translation;
  MatchQuality match = MatchQuality::kEqual;
  for (int i = 0; i < representation_.size(); ++i) {
    if (representation_[i] == 0) {
      if (other.representation_[i] != 0) match = MatchQuality::kSubset;
      continue;
    }
    auto it = translation.find(representation_[i]);
    if (it != translation.end()) {
      if (other.representation_[i] != it->second)
        return MatchQuality::kNotASubset;
    } else {
      if (other.representation_[i] == 0) return MatchQuality::kNotASubset;
      translation[representation_[i]] = other.representation_[i];
    }
  }
  return match;
}
