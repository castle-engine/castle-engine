# Overview

Terrain generation and visualization toy using Castle Game Engine
( https://castle-engine.sourceforge.io/ ).

Compile using Castle Game Engine build tool
( https://github.com/castle-engine/castle-engine/wiki/Build-Tool ).
You can just run "make" to compile a normal executable for current system
(Linux, Windows etc.). Other targets are available, e.g. run "make android"
to create an Android application.

# Using in your own games

The core terrain generation algorithm is now part of the Castle Game Engine
(unit CastleTerrain), and it can be used in other games.
See e.g. "Wyrd Forest"
- https://github.com/castle-engine/wyrd-forest
- https://www.patreon.com/posts/15811244
for a demo game using CastleTerrain.

# Terrain data sources

Terrain data may be obtained from various sources:

1. There's terrain data reader from a very simple SRTM-3 *.hgt file

   See http://www2.jpl.nasa.gov/srtm/, see (linked there)
   http://dds.cr.usgs.gov/srtm/ for sample data for whole Earth.
   In you speak Polish, nice overview is also on
   http://netgis.geo.uw.edu.pl/srtm/.
   Sample files for Poland are on http://netgis.geo.uw.edu.pl/srtm/Poland/,
   for Europe http://netgis.geo.uw.edu.pl/srtm/Europe/.

   You can run the program with command-line parameter to pass URL
   of such .hgt file to load on start.

2. You can also define terrain as an explicit function using CastleScript
   expression syntax, [http://castle-engine.sourceforge.net/castle_script.php].
   Try e.g. function like
   - sin(x) + sin(y)
   - (sin(x) + sin(x*2) / 2 + sin(x*4) / 4)  *
     (sin(y) + sin(y*2) / 2 + sin(y*4) / 4)
     (sum sinusoides of various frequencies and amplitudes).

3. You can also (and this was actually the primary reason for this program
   in the 1st place) generate random terrain.
