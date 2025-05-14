# Kraft Physics Engine

Kraft Physics Engine is an open source Object Pascal physics engine library.

Author: Benjamin 'BeRo' Rosseaux

The original sources and demos are available on https://github.com/BeRo1985/kraft

See also movies
- https://www.youtube.com/watch?v=42Q9vp-r7Ec
- https://www.youtube.com/watch?v=Ufpn9XCz9zs

This directory contains a local copy of Kraft sources,
to easily compile them together with Castle Game Engine.

- This is a convenience, to compile Castle Game Engine with physics
  support out-of-the-box, without requiring you to download anything
  besides Castle Game Engine repository and FPC.

    It is "just a convenience", i.e. on *some* platforms, you can also just use
    original Kraft unit from https://github.com/BeRo1985/kraft .

- It also allows us to make some mods to Kraft, in particular to support
  platforms supported by CGE but not officially by Kraft.
  This means now:

  - [Web](https://castle-engine.io/web)
  - [Delphi on Linux](https://castle-engine.io/delphi_linux)

License: The Kraft Physics Engine is released under the open-source ZLib license:
https://opensource.org/licenses/zlib

Last synchronization:
- commit f6223a7bdef8fd7ac71ff4f5ea3228627e5bb0d6
- see Kraft state on https://github.com/BeRo1985/kraft/blob/f6223a7bdef8fd7ac71ff4f5ea3228627e5bb0d6/src/kraft.pas
- compare:
  ```
  COMMIT=f6223a7bdef8fd7ac71ff4f5ea3228627e5bb0d6
  wget https://raw.githubusercontent.com/BeRo1985/kraft/$COMMIT/src/kraft.pas --output-document=kraft_upstream.pas
  diff -wur kraft.pas kraft_upstream.pas
  ```