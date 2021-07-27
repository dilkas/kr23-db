#include "encoding.h"

#include <assert.h>

#include <map>
#include <sstream>

#include <boost/log/trivial.hpp>

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

Match Encoding::IsSubsetOf(Encoding other) const {
  BOOST_LOG_TRIVIAL(debug) << "Encoding::IsSubsetOf: this = " << AsString()
                           << ", other = " << other.AsString();
  assert(representation_.size() == other.representation_.size());

  std::map<int, int> translation;
  Match match = {Match::Quality::kEqual, 0};
  int max_var_this = 0, max_var_other = 0;
  for (std::vector<int>::size_type i = 0; i < representation_.size(); ++i) {
    if (representation_[i] > max_var_this) max_var_this = representation_[i];
    if (other.representation_[i] > max_var_other)
      max_var_other = other.representation_[i];

    if (representation_[i] == 0) {
      if (other.representation_[i] != 0)
        match.quality = Match::Quality::kSubset;
      continue;
    }
    auto it = translation.find(representation_[i]);
    if (it != translation.end()) {
      if (other.representation_[i] != it->second)
        return {Match::Quality::kNotASubset, 0};
    } else {
      if (other.representation_[i] == 0)
        return {Match::Quality::kNotASubset, 0};
      translation[representation_[i]] = other.representation_[i];
    }
  }
  match.diff_in_variables = max_var_other - max_var_this;
  return match;
}

int Encoding::CountRedundantPositions() {
  std::set<int> seen;
  int count = 0;
  for (auto v : representation_) {
    if (seen.find(v) != seen.end()) ++count;
    seen.insert(v);
  }
  return count;
}

std::map<int, std::set<std::string>>
Encoding::MatchAString(std::string variables) const {
  BOOST_LOG_TRIVIAL(debug) << "Encoding::MatchAString: this = "
                           << AsString() << ", variables = " << variables;
  std::map<int, std::set<std::string>> decoding;
  assert(variables.size() == representation_.size());

  for (std::vector<int>::size_type i = 0; i < variables.size(); ++i) {
    BOOST_LOG_TRIVIAL(debug) << "Encoding::MatchAString: "
                             << representation_[i] << " -> " << variables[i];
    auto it = decoding.find(representation_[i]);
    if (it == decoding.end()) {
      decoding[representation_[i]] = {std::string(1, variables[i])};
    } else {
      it->second.insert(std::string(1, variables[i]));
    }
  }
  return decoding;
}

std::string Encoding::AsString() const {
  std::stringstream ss;
  ss << "[";
  for (auto i : representation_) ss << " " << i;
  ss << " ]";
  return ss.str();
}
