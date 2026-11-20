# WOFF and WOFF2 fonts

This example uses fonts in the WOFF (1.0) format from this directory. WOFF is supported by all FreeType implementations used by _Castle Game Engine_: the FreeType library and our Pascal implementation of the FreeType API (used e.g. on the web and with Delphi on Android / iOS).

The `.woff2` files are also here, but are not used by the example. WOFF2 fonts can only be read by a FreeType library compiled with WOFF2 support (which requires the Brotli library). In particular, WOFF2 is _not_ supported by:

- our Pascal implementation of the FreeType API,
- the FreeType DLL we distribute with the engine on Windows.

See [AUTHORS.md](AUTHORS.md) for the fonts' authors and licenses.
