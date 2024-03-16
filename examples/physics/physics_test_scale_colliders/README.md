# Physics scale colliders

Tests the scale applied to `TCastleCollider` in various ways:

- scaling using the `TCastleTransform.Scale` on parent (direct and indirect) of the `TCastleCollider`

- scaling using the `TCastleCollider.SizeScale`.

Correct test result (in both editor simulation and when you run):

- All objects should fall nicely on yellow bar, not "go through".

- Pressing any "Scale ..." buttons should also make them stand sensibly on top of another

- ...*except* "Scale Red Box Collider 1/2" that deliberately desynchronizes collider size with look, it should make the red box "fall through" the yellow bar partially and stop.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `physics_test_scale_colliders_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `physics_test_scale_colliders_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
