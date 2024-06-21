# Read KTX or DDS and decompose multiple images

Read KTX or DDS and decompose into a number of simple images (PNG).

- Reads file given as a command-line argument,
- outputs some information about it,
- and saves a sequence of PNG files `xxx-yyy.png` where `xxx` comes from the basename of $1, and `yyy` is the consecutive number from 0.

The PNG files are saved in the same directory as input KTX / DDS. Use `--no-save` option to not save files (it then only outputs information).

Try

```
./image_decompose data/marble_with_colored_mipmaps.dds
```

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
