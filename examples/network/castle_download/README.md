# Command-line downloader of URLs

This application downloads the URL (given as a command-line parameter) and writes result to the standard output (stdout).

This is a simple test that our `TCastleDownload` class allows you to grab any URL to a `TStream` in Pascal.

Writes diagnostic output to the log (see https://castle-engine.io/manual_log.php for information where is the log; note that we set `LogEnableStandardOutput = false`, which means that on Linux it is usually inside `~/.config/castle_download/`).

Try from command-line like

```
castle_download https://castle-engine.io/ > output.html
castle_download https://castle-engine.io/modern_pascal_introduction.html > output.html
castle_download https://castle-engine.io/latest.zip > output.zip
```

You can think about it as a simple replacement for `wget` or `curl` (although it's not as powerful as them, it's just a simple test of our `TCastleDownload` class).

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `castle_download.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `castle_download.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.