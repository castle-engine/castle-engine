# TCP/IP communication in Castle Game Engine using Indy components

The examples here demonstrate a cross-platform communication using TCP/IP with a "classic" client and server approach. The _Castle Game Engine_ shows the windows and user interface.

## Compiling

* You need [Indy](https://www.indyproject.org/).

    Indy is not distributed with _Castle Game Engine_.

    FPC users: See https://castle-engine.io/manual_network.php#section_indy about installing Indy.

    Delphi users: No need to do anything, Indy is part of Delphi.

* Compiling from Delphi, or using CGE editor with Delphi compiler:

    Should work out-of-the-box. Indy is part of Delphi, there's no need to do any special setup.

* Compiling from Lazarus:

    These examples depend on the `castle_indy` package. Make sure you compile the `packages/castle_indy.lpk` in Lazarus (it depends on Indy package).

* Compiling from CGE editor, with FPC compiler:

    Edit the `client/CastleEngineManifest.xml` and `server/CastleEngineManifest.xml` files and replace the `your/path/to/Indy10` example there with actual path to Indy units on your system. It can be an absolute path (like `c:/indy/` or `/home/me/indy`) or relative path (like `../../indy/`).

## TODO

Migrate both examples to use UI designed in editor and TCastleView.
