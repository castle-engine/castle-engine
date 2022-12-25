# Compile using g++ for Windows. Works at least with g++ from Cygwin.
#
# Note: if you're compiling a 64-bit application,
# make sure that castleengine.dll is also compiled for 64-bit.
# You may want tc run ../../../src/deprecated_library/castleengine_compile.sh
# with the -Twin64 -Px86_64 options to force this (since often
# the default fpc compiles for Windows 32-bit).

# Do not link to castleengine.dll this way,
# instead use dynamic loading in ../../../src/deprecated_library/castlelib_c_loader.cpp
#LINKFLAGS:=-L../../../src/deprecated_library/ -lcastleengine

# Do not use -lWs2_32, instead use -mwindows following https://cygwin.com/cygwin-ug-net/programming.html
# This works also on 64-bit.
#LINKFLAGS:=-lWs2_32

LINKFLAGS:=-lopengl32

cpp_winapi_library_tester: main.cpp
	g++ $< ../../../src/deprecated_library/castlelib_c_loader.cpp -mwindows $(LINKFLAGS) -o $@

.PHONY: run
run:
	cp -f ../../../src/deprecated_library/castleengine.dll .
	./cpp_winapi_library_tester.exe
