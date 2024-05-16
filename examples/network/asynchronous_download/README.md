# Asynchronous download

Demonstrates using `TCastleDownload` to download files asynchronously, i.e. without blocking the main application while it waits for the download to finish.

Using [Castle Game Engine](https://castle-engine.io/).

![Screenshot](screenshot.png)

## On Windows, make sure to use OpenSSL DLLs to have HTTPS support

Both FPC and Delphi code paths use OpenSSL to implement HTTPS support.

We strongly advise to compile at least once using [CGE editor](https://castle-engine.io/editor) that will place the appropriate DLLs automatically alongside your EXE. Afterwards you can build using [CGE editor](https://castle-engine.io/editor) or from Delphi or Lazarus IDEs, the DLLs will be in place.

If you really must do it manually, get the 2 necessary DLLs from these locations:

- Win64: https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/external_libraries/x86_64-win64/openssl

- Win32: https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/external_libraries/i386-win32/openssl

## FPC + HTTPS + Linux

If you use FPC on Linux, note that older FPC (including 3.2.2) does not handle latest OpenSSL library versions.

If you get errors related to OpenSSL, upgrade to latest FPC (from GitLab) e.g. using [fpcupdeluxe](https://castle-engine.io/fpcupdeluxe).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `remote_logging_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
