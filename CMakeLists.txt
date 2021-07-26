cmake_minimum_required(VERSION 3.20)
project(WFOMC)
enable_testing()
add_compile_options(-g -Wall -Wextra -pedantic)
find_package(Boost COMPONENTS log graph unit_test_framework REQUIRED)

include_directories(src)
include_directories(${Boost_INCLUDE_DIRS})
link_directories(src)

file(GLOB_RECURSE SRCS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} src/*.cc)
list(REMOVE_ITEM SRCS "src/main.cc")
file(GLOB_RECURSE TEST_SRCS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} test/*.cc)

IF (BUILD_TESTING)
   foreach(testSrc ${TEST_SRCS})
        #Extract the filename without an extension (NAME_WE)
        get_filename_component(testName ${testSrc} NAME_WE)
        #Add compile target
        add_executable(${testName} ${testSrc} ${SRCS})
        #link to Boost libraries AND your targets and dependencies
        target_link_libraries(${testName}
                              ${Boost_LOG_LIBRARY}
                              ${Boost_GRAPH_LIBRARY}
                              ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
                              )
        set_target_properties(${testName} PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin/test)
        #Finally add it to test execution -
        #Notice the WORKING_DIRECTORY and COMMAND
        add_test(NAME ${testName}
                 WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin/test
                 COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/bin/test/${testName} )
endforeach(testSrc)
ENDIF(BUILD_TESTING)

add_executable(wfomc src/main.cc ${SRCS})
target_link_libraries(wfomc
                      ${Boost_LOG_LIBRARY}
                      ${Boost_GRAPH_LIBRARY}
                      ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
                      )
set_target_properties(wfomc PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin)
