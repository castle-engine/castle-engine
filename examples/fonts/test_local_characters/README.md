The main source code is in "game.pas" file ("Game" unit).

Demo that displays strings with local characters
(Chinese, Greek, Russian, Polish...
first 3 from https://helloworldcollection.github.io/#Human ).

Strings come from various sources (XML, hardcoded in Pascal, X3D...).
This shows that internationalization support in Castle Game Engine works nicely.

This demo can also be tested on mobile (Android).
We do not package (by default) the freetype library on Android,
so ttf loading will not work, but the "embedded" fonts will work.
