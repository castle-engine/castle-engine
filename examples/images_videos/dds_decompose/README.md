# Read DDS and decompose multiple images

Read DDS and decompose multiple images into a number of simple images.
Reads DDS file $1, outputs some information about it,
and (if not --no-save) saves a sequence of PNG files xxx-yyy.png
(where xxx comes
from the basename of $1, and yyy is the consecutive number from 0,
and the files are saved in $1 directory).

Try

```
./dds_decompose data/marble_with_colored_mipmaps.dds
```

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
