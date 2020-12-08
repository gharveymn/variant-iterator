SET(CMAKE_EXPORT_COMPILE_COMMANDS ON)

ADD_EXECUTABLE(test_variant-iterator main.cpp)
TARGET_LINK_LIBRARIES(test_variant-iterator variant-iterator)

IF(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  TARGET_COMPILE_OPTIONS(test_variant-iterator PUBLIC -stdlib=libc++ -ftime-trace --coverage -Wall -Wextra)
  TARGET_LINK_OPTIONS(test_variant-iterator PUBLIC -Wl,--allow-multiple-definition --coverage)
  TARGET_LINK_LIBRARIES(test_variant-iterator c++ c++abi)
ELSEIF(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  TARGET_COMPILE_OPTIONS(test_variant-iterator PUBLIC -Wall -Wextra)
ELSEIF(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
  TARGET_COMPILE_OPTIONS(test_variant-iterator PUBLIC /W4)
ELSEIF(CMAKE_CXX_COMPILER_ID STREQUAL "Intel")

ENDIF()

SET_TARGET_PROPERTIES(test_variant-iterator PROPERTIES
                      CXX_STANDARD          17
                      CXX_STANDARD_REQUIRED NO
                      CXX_EXTENSIONS        NO)
