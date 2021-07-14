#define BOOST_TEST_MODULE VertexClassTest

#include <boost/test/unit_test.hpp>

#include "vertex_class.h"

BOOST_AUTO_TEST_CASE(test_IsSubsetOf) {
  VertexClass vc({1, 2});
  BOOST_CHECK(vc.IsSubsetOf({1, 2, 3}));
}
