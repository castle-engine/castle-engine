# Command-line tool to convert images

Convert one image into another.
Can resize the image and adjust colors. Syntax:

```
image_convert <inputfile> <outputfile> [<resize-x> <resize-y>]
```

This will convert image in `<inputfile>` to image in `<outputfile>`.
Format of both images will be recognized from file extension.
`<inputfile>` and `<outputfile>` may be the same file, it will be handled OK.

If optional `<resize-x>` `<resize-y>` parameters are given then output image
is scaled to that size. Size = 0 means "do not scale in this dimension",
e.g. `<resize-x> <resize-y>` = 200 0 means "scale x dimension to 200
and leave y dimension as is". (effectively, specifying
`<resize-x> <resize-y>` = 0 0 has the same effect as omitting these
parameters).

Additional parameters (use them anywhere, before or after, main parameters documented above):

- `-s <float>` or `--scale <float>`

    Valid only if input file has a float format (this is currently
    the case only for RGBE (Radiance) image format).

    Effect : before writing output image, scales each pixel color
    (its red, green and blue value) by <float>.
    Multiple --scale *cummulate* : e.g.
    "--scale 1.5 --scale 2" would have the same effect as
    "--scale 3".


- `-g <float>` or `--gamma <float>`

    Similiar to `--scale` - valid only when input has float precision,
    multiple params are cummulated, default is 1.0.

    Each component is raised to 1/<float>.

- `--grayscale`

    Convert to grayscale.

- `--convert-to-channel 0|1|2`

    Converts colors to red / green / blue channel,
    its like converting to grayscale and then
    writing output to only one channel.

- `--strip-to-channel 0|1|2`

    Strips colors to only one channel, i.e.
    sets to zero intensities of other two channels.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
