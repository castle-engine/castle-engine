# Pack CGE in a library to develop simple 3D viewers

Using the code in this directory you can compile CGE into a dynamic library
that exposes a simple C API in castleengine.h .
It is a very limited API, that allows to load and display a single 3D model
(anything that can be loaded into TCastleScene).
As such, it can be used to develop 3D model viewers from other languages (like C or C++)
using CGE.

See examples from ../../examples/library/ to see this library being used from
Qt application in C++ on desktop, iOS application in Objective-C etc.
