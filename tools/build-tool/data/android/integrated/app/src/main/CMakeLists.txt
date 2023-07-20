cmake_minimum_required(VERSION 3.10)

project(castle_cpp_libs)

option(ALSOFT_NO_CONFIG_UTIL ON)
option(ALSOFT_EXAMPLES OFF)
option(ALSOFT_UTILS OFF)
option(ALSOFT_TESTS OFF)
option(OBOE_SOURCE ../oboe)
option(ALSOFT_REQUIRE_OBOE ON)
option(ALSOFT_BACKEND_OPENSL ON)
option(ALSOFT_EMBED_HRTF_DATA YES)

add_subdirectory("cpp/sound/openal-soft/")

unset(ALSOFT_NO_CONFIG_UTIL CACHE)
unset(ALSOFT_EXAMPLES CACHE)
unset(ALSOFT_UTILS CACHE)
unset(ALSOFT_TESTS CACHE)
unset(OBOE_SOURCE CACHE)
unset(ALSOFT_REQUIRE_OBOE CACHE)
unset(ALSOFT_BACKEND_OPENSL CACHE)
unset(ALSOFT_EMBED_HRTF_DATA CACHE)


add_subdirectory("cpp/testlib/")


