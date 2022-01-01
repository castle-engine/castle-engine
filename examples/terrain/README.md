# Overview

Terrain generation and visualization toy using Castle Game Engine
( https://castle-engine.io/ ).

Compile using Castle Game Engine build tool
( https://castle-engine.io/build_tool ).
You can just run `make` to compile a normal executable for current system
(Linux, Windows etc.). Other targets are available, e.g. run `make android`
to create an Android application.

# Using in your own games

The core terrain generation algorithm is now part of the Castle Game Engine
(unit `CastleTerrain`), and it can be used in other games.
See e.g. "Wyrd Forest" for a demo game using `CastleTerrain`:

- https://github.com/castle-engine/wyrd-forest
- https://www.patreon.com/posts/15811244

# Terrain generation methods

Terrain heights may be generated in various ways:

1. First of all you can generate random terrain using various standard algorithms for a smooth noise, combined with some terrain-specific improvements (Heterogeneous).

    Some (old and random) notes about the algorithms used are in the file `OLD_TERRAIN_GENERATION_NOTES_FOR_SEMINAR.txt` in this directory.

2. There's terrain data reader from a very simple SRTM-3 *.hgt file

    See http://www2.jpl.nasa.gov/srtm/, see (linked there) http://dds.cr.usgs.gov/srtm/ for sample data for whole Earth. In you speak Polish, nice overview is also on  http://netgis.geo.uw.edu.pl/srtm/. Sample files for Poland are on http://netgis.geo.uw.edu.pl/srtm/Poland/, for Europe http://netgis.geo.uw.edu.pl/srtm/Europe/.

    You can run the program with command-line parameter to pass URL of such .hgt file to load on start.

3. You can also define terrain as an explicit function using CastleScript expression syntax ( https://castle-engine.io/castle_script.php ).

    Try e.g. function like
    - sin(x) + sin(y)
    - Sum sinusoides of various frequencies and amplitudes:
        ```
        (sin(x) + sin(x*2) / 2 + sin(x*4) / 4)  *
        (sin(y) + sin(y*2) / 2 + sin(y*4) / 4)
        ```
