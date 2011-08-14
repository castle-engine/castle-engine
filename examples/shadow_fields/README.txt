This directory contains an example implementation of
"Precomputed Shadow Fields for Dynamic Scenes".
Video and paper from http://www.kunzhou.net/

Video notes: note that morphing objects is problematic, since shadow fields
are precalculated for given geometry. They admit this (when talking
about "each leg has it's own SF", and when video texture light has very
few FPS (400 MB according to paper)).

Explain (basically 3.1 and 3.2 describe algorithm's idea).
- First note just one light.
- Then one light combined with one shadow caster.
- Then many lights combinaed with many shadow casters: adding and multiplying.
- Then add sugar:
  - environment light (like local light but with only one env map
    (no need for whole shadow field), at infinite distance)
  - self-shadowing (just a simple env map precalculated at each vertex)

For exercise, look what happens for only pure white environment light
(no local) and only one shadow caster: then each vertex actually calculates
integral over sphere where visibility = shadow caster non-blocking.
Everything is Ok.

Describe compression of env maps.
- See demo results when it's calculated directly using env maps
  ("Program -> Use spherical harmonics" can be turned off).
  Note that currently interpolation is never done when using directly
  env maps (interpolating env maps would be very slow).

- For min/max radius and size of env maps, you can use "SF explorer"
  object in demo program. Largest sphere radius is easy to check
  visually: ideally, outside of largest sphere radius,
  OOF should be safely considered pure white, SRF pure black.

- Note ugly aliasing and the need for interpolation. Only linear-radius
  interpolation is not enough. See demo program, change interpolation.

  Note that aliasing is the result of shadow fields using too few
  env maps. Env map at a single point of shadow field doesn't produce
  aliasing, as we integrate over it anyway (and it's encoded by SH,
  that smooth it by the wat). This means that environment light
  or self-shadowing (that are single env map, not shadow field)
  are not problematic.

- compare various SHCount values (speed/accuracy balance)

My current demo implementation in shadow_fields.lpr:
- there is only one shadow caster (TODO: although may be instanced many times)
- there is only one shadow receiver (this also means that shadow casters
  and receivers are different things, no shadowing between shadow casters)
- there is only one local light
- there is only one environment light
- also, there is an additional point in 3D space that you can move
  to inspect environment maps at arbitrary points in 3D space.
  I call this "shadow fields explorer" in code.
Also, there's no rotating of caster/light source. Only move/scale.
Also, no self-shadowing.
Also, only local light or environ light works (not both).
Also, BRDF and cos are not applied.

So in my implementation the algorithm (from figure 5) degenerates to:

  First, compare whether shadow caster works
  (for env light: always true, for local light:
  when caster closer than local light).

  If shadow caster doesn't work, then
    Bp := simple integral over light (trivial to calculate, see code)
  else
    o := get env map from from OOF field of Ob, using (v - Ob.position)
    if env light then
      s := constant env map of environment light
    else
      s := get env map from SRF field of local light, using (v - local light.position)
    Bp := DoubleProduct(o, s)

Shadow fields are trivially saved to a binary file,
basically dumping all env map contents from memory,
and piping through gzip (de)compressing stream.
Gzip reduces data size extremely: for real env maps compression is > 100 times
(from 36 MB to 250 KB). (For empty testing env maps it's around 1000 times,
to 36 KB). No surprise, this type of data is highly compressible.
The SH vector factors are a little harder to compress, but still
compressed file is very small.

Source code: besides sources in this directory, take a look at
the spherical harmonics and cube env map utilities in units
  3d/sphericalharmonics.pas,
  3d/cubemap.pas,
  opengl/glcubemap.pas
Some of them were created directly for shadow_fields demo,
some of them are shared by radiance_transfer demo.

------------------------------------------------------------------------------
Note:
- how to calculate SH vector basis for constant function f(s) = c ?
  (useful in one place, see shadow_fields VectorColor when CasterVector = nil).
  Well, SH basis function 0 is constant, so only first SH factor should
  be used, rest is zero.

  f = integral over a sphere for SHBasis0 * c = just SHBasis0 * c

  This can be confirmed empirically with my code:

  var
    PureWhiteMap: TCubeMapByte;
    PureWhiteVector: TSHVectorSingle;

    InitializeSHBasisMap;

    FillChar(PureWhiteMap, SizeOf(PureWhiteMap), High(Byte));
    SHVectorFromCubeMap(PureWhiteVector, PureWhiteMap);
    Writeln(VectorToRawStr(PureWhiteVector));

  and note that resulting PureWhiteVector has only the first factor non-zero
  and equal SHBasis0 (~ 0.28).
