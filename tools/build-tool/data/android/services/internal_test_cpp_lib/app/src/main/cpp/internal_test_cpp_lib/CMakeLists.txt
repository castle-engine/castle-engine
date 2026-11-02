cmake_minimum_required(VERSION 3.10)

project(random_utils)

add_library(random_utils SHARED random_utils.cpp)

set_target_properties(random_utils PROPERTIES
    CXX_STANDARD 11
    CXX_STANDARD_REQUIRED YES
)

target_include_directories(random_utils PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

