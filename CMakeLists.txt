CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(variant-iterator
        VERSION 0.0.1
        LANGUAGES CXX)

INCLUDE(cmake/create-package.cmake)

CREATE_HEADER_ONLY_PACKAGE(AUTHOR       gharveymn
                           NAME         variant-iterator
                           HEADERS      variant-iterator.hpp)

# Options
IF(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  OPTION(VARIANT_ITERATOR_BUILD_TESTS "Set to ON to build tests" ON)
ELSE()
  OPTION(VARIANT_ITERATOR_BUILD_TESTS "Set to ON to build tests" OFF)
ENDIF()

# Only include tests if we are at the top level
IF(VARIANT_ITERATOR_BUILD_TESTS)
  ADD_SUBDIRECTORY(test)
ENDIF()
