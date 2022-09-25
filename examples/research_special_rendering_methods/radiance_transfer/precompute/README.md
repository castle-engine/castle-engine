# Process arbitrary model for Precomputed Radiance Transfer.

Computes and adds "radianceTransfer"
field to all geometry nodes (descending from X3DComposedGeometryNode,
this includes most often used nodes).
See https://castle-engine.io/x3d_extensions.php#section_ext_radiance_transfer
about the radiance transfer.

Command-line usage:
- $1 is the source model,
- $2 is the output model (output is always VRML/X3D, so use .wrl/.x3dv extension).

Example:

```
./precompute/precompute_radiance_transfer data/chinchilla.wrl.gz chinchilla-output.x3dv
./radiance_transfer chinchilla-output.x3dv # test the output
```

Optional parameters:

- `--sh-basis-count / -c COUNT`

    Says how much basis SH functions to use. PRT paper advices between
    9 and 25.

- `--rays-per-vertex / -r COUNT`

    How many rays per vertex to generate. This linearly affects the speed
    of the program. Default if 1000, PRT paper advices 10 * 1000 to 30 * 1000
    for best effect.

TODO: for now, radianceTransfer is calculated for whole model.
This means that self-shadowing takes whole model into account,
but also that whole model must remain static (or radianceTransfer must
be animated along with coords).

Alternative approach is possible: calculate radianceTransfer only
for this specific shape. Then shape must stay static (or it's
radianceTransfer must be animated along with it's coords), but it can
move with respect to other shapes. But note that then self-shadowing
takes only this shape into account... TODO: make this possible,
and document on
https://castle-engine.io/x3d_extensions.php#section_ext_radiance_transfer

We compute radianceTransfer in scene space (not in local shape
space). This is important, otherwise incoming light SH (calculated
when rendering at every frame) would have to be transformed (rotated)
for each shape. Right now, it only has to be rotated once, for each scene.

Note that your geometry nodes shouldn't use DEF/USE mechanism.
If the same shape is instantiated many times, it will have the same
radianceTransfer. Which is bad, since self-shadowing may be different
on different instances.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `precompute_radiance_transfer.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
