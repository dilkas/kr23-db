#define BOOST_TEST_MODULE GfoddTest

#include "gfodd.h"

#include <boost/test/unit_test.hpp>

#include "variable_positions.h"

BOOST_AUTO_TEST_CASE(test_Atoms) {
  Gfodd gfodd;
  auto atoms = gfodd.Atoms();
  BOOST_CHECK_EQUAL(atoms.size(), 2);
  BOOST_CHECK_EQUAL(gfodd.Positions(atoms[0]).string_representation(), "xy");
  BOOST_CHECK_EQUAL(gfodd.NumNewVariables(atoms[0]), 2);
  BOOST_CHECK_EQUAL(gfodd.Positions(atoms[1]).string_representation(), "yz");
  BOOST_CHECK_EQUAL(gfodd.NumNewVariables(atoms[1]), 1);
}

BOOST_AUTO_TEST_CASE(test_Incident) {
  Gfodd gfodd;
  BOOST_CHECK_EQUAL(gfodd.NumInternalEdges(), 1);
  auto [source, target] = gfodd.Incident(0);
  BOOST_CHECK_EQUAL(gfodd.Positions(source).string_representation(), "xy");
  BOOST_CHECK_EQUAL(gfodd.NumNewVariables(source), 2);
  BOOST_CHECK_EQUAL(gfodd.Positions(target).string_representation(), "yz");
  BOOST_CHECK_EQUAL(gfodd.NumNewVariables(target), 1);
}

BOOST_AUTO_TEST_CASE(test_Evaluate) {
  Gfodd gfodd;
  gfodd.Evaluate({});
} // TODO: finish
