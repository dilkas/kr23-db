#define BOOST_TEST_MODULE GfoddTest

#include "gfodd.h"

#include <boost/test/unit_test.hpp>

#include "variable_positions.h"

BOOST_AUTO_TEST_CASE(test_NumInternalEdges) {
  Gfodd gfodd;
  BOOST_CHECK_EQUAL(gfodd.NumInternalEdges(), 1);
}

// BOOST_AUTO_TEST_CASE(test_IncidentAtomInfo) {
//   Gfodd gfodd;
//   auto [source, target] = gfodd.IncidentAtomInfo(0);
//   VariablePositions correct_source = VariablePositions("xy");
//   VariablePositions correct_target = VariablePositions("yz");

//   BOOST_CHECK_EQUAL(source.size(), correct_source.size());
//   BOOST_CHECK_EQUAL_COLLECTIONS(source["x"].begin(), source["x"].end(),
//                                 correct_source["x"].begin(),
//                                 correct_source["x"].end());
//   BOOST_CHECK_EQUAL_COLLECTIONS(source["y"].begin(), source["y"].end(),
//                                 correct_source["y"].begin(),
//                                 correct_source["y"].end());

//   BOOST_CHECK_EQUAL(target.size(), correct_target.size());
//   BOOST_CHECK_EQUAL_COLLECTIONS(target["y"].begin(), target["y"].end(),
//                                 correct_target["y"].begin(),
//                                 correct_target["y"].end());
//   BOOST_CHECK_EQUAL_COLLECTIONS(target["z"].begin(), target["z"].end(),
//                                 correct_target["z"].begin(),
//                                 correct_target["z"].end());
// }
