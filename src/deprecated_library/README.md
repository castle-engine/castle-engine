# Pack CGE in a library to develop simple 3D viewers

Using the code in this directory you can compile CGE into a dynamic library
that exposes a simple C API in castleengine.h .
It is a very limited API, that allows to load and display a single 3D model
(anything that can be loaded into TCastleScene).
As such, it can be used to develop 3D model viewers from other languages (like C or C++)
using CGE.

See examples from ../../examples/deprecated_library/ to see this library being used from
Qt application in C++ on desktop, iOS application in Objective-C etc.

*Deprecated*: This approach to using CGE is deprecated, because

- It is inherently very limited.

    The C API in castleengine.h exposes only a tiny API to load a single 3D model
    and display it. It doesn't allow:
    - using the full TCastleScene API,
    - multiple TCastleScene,
    - adjust TCastleViewport,
    - use all TCastleUserInterface descendants,
    - loading designs from editor,
    - using CGE SoundEngine...

- It didn't gain much usage.

    The only significant application using it is https://www.roomarranger.com/
    (as far as Michalis is aware; if you're using this library, and rely on it,
    let me know! We can talk https://castle-engine.io/talk.php and find a solution,
    maybe even un-deprecate it.)
