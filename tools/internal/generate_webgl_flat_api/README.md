# Generate "flat" WebGL API

This utility generates include file(s) that are part of `CastleInternalWebGL` unit and expose WebGL API as global constants / methods that look similar to `CastleGLES` unit.

In effect, `CastleInternalWebGL` exposes global methods that will call some class methods of `CastleInternalJobWeb`.

To do this, we parse WEBIDL file `castleinternaljobweb.webidl` which was used to generate `CastleInternalJobWeb`. This effectively tells us what methods are available in WebGL.
