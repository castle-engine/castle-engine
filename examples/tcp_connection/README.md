These examples demonstrate a cross-platform communication using TCP,
using a client and server application with Castle Game Engine user interface.

Note that they depend on `CastleClientServer` unit,
which in turn depends on Indy ( https://www.indyproject.org/ ),
which is not distributed together with Castle Game Engine.
You need to get Indy yourself :

- You can install "indy" module using fpcupdeluxe. See https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe about what is fpcupdeluxe and how to get it. You can install your own FPC and Lazarus using fpcupdeluxe, and add an "indy" module to it.

    After installing using fpcupdeluxe GUI, in Lazarus open .../ccr/indy/Lib/indylaz.lpk (the "..." in the output directory of fpcupdeluxe, by default "$HOME/fpcupdeluxe/" on Unix and "C:\fpcupdeluxe\" on Windows). Press "compile" to compile and make this package known to Lazarus. (This step shouldn't be necessary with new Lazarus version from trunk, fpcupdeluxe should automatically compile "indylaz", but it seems necessary with stable Lazarus 1.8.x for now.)

- Alternatively, do SVN checkout of https://svn.atozed.com:444/svn/Indy10/trunk/ . Similar to above, open the package "Lib/indylaz.lpk" inside in Lazarus and compile it.

In both cases, you should have an additional package "indylaz" known by Lazarus.
