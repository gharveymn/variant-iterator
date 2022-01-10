cmake_minimum_required (VERSION 3.15)

project (
  variant-iterator
  VERSION
    0.0.2
  LANGUAGES
    CXX
)

string (COMPARE EQUAL "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_SOURCE_DIR}" _EXTRAS_DEFAULT)

option (
  GCH_VARIANT_ITERATOR_BUILD_TESTS
  "Set to ON to build tests for gch::variant-iterator."
  ${_EXTRAS_DEFAULT}
)

include (CMakeDependentOption)
cmake_dependent_option (
  GCH_USE_LIBCXX_WITH_CLANG
  "Set to ON to use libc++ when compiling with Clang."
  OFF
  "CMAKE_CXX_COMPILER_ID MATCHES Clang"
  OFF
)

if (GCH_VARIANT_ITERATOR_BUILD_TESTS)
  enable_testing ()
endif ()

add_subdirectory (source)

export (
  EXPORT
    variant-iterator-targets
  NAMESPACE
    gch::
)
