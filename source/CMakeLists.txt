add_library (variant-iterator INTERFACE)

target_sources (
  variant-iterator
  INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/include/gch/variant-iterator.hpp>
)

target_include_directories (
  variant-iterator
  INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/include>
)

target_include_directories (
  variant-iterator
  SYSTEM INTERFACE
    $<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>
)

set_target_properties (
  variant-iterator
  PROPERTIES
  PUBLIC_HEADER
    include/gch/variant-iterator.hpp
)

add_library (gch::variant-iterator ALIAS variant-iterator)

install (
  TARGETS
    variant-iterator
  EXPORT
    variant-iterator-targets
  PUBLIC_HEADER
    DESTINATION include/gch
)

file (WRITE ${CMAKE_CURRENT_BINARY_DIR}/variant-iterator-config.cmake [=[
get_filename_component (PACKAGE_PREFIX_DIR "${CMAKE_CURRENT_LIST_DIR}/../../../" ABSOLUTE)
include ("${CMAKE_CURRENT_LIST_DIR}/variant-iterator-targets.cmake")
]=])

include (CMakePackageConfigHelpers)
write_basic_package_version_file (
  variant-iterator-version.cmake
  COMPATIBILITY
    SameMinorVersion
)

install (
  FILES
    ${CMAKE_CURRENT_BINARY_DIR}/variant-iterator-config.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/variant-iterator-version.cmake
  DESTINATION
    lib/cmake/variant-iterator
)

install (
  EXPORT
    variant-iterator-targets
  DESTINATION
    lib/cmake/variant-iterator
  NAMESPACE
    gch::
)

if (GCH_VARIANT_ITERATOR_ENABLE_TESTS)
  add_subdirectory (test)
endif ()
