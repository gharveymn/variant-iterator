SET(CMAKE_EXPORT_COMPILE_COMMANDS ON)

ADD_EXECUTABLE(test main.cpp)
TARGET_LINK_LIBRARIES(test variant-iterator)

if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  TARGET_COMPILE_OPTIONS(test PUBLIC "-stdlib=libc++")
  TARGET_LINK_LIBRARIES(test -lc++ -lc++abi)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --coverage")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  # using Intel C++
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
  # using Visual Studio C++
endif()

SET_TARGET_PROPERTIES(test PROPERTIES
                      CXX_STANDARD          17
                      CXX_STANDARD_REQUIRED NO
                      CXX_EXTENSIONS        NO)
