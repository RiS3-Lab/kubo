cmake_minimum_required(VERSION 3.1)

find_package(LLVM REQUIRED CONFIG)
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

link_directories(${LLVM_LIBRARY_DIRS})

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_CXX_FLAGS "-Wno-unused-variable ")
set(CMAKE_CXX_FLAGS_DEBUG "-g")
set(CMAKE_CXX_FLAGS_RELEASE "-O1")

add_subdirectory(Utils) # For utilities

add_subdirectory(RangeAnalysis) # For Range Analysis

add_subdirectory(Customizations) # General interface for customizations.

add_subdirectory(LinuxKernelCustomizations) # customizations for linux kernel.

add_subdirectory(SoundyAliasAnalysis)

