Implementation of Precomputed Radiance Transfer idea.

http://www.mpi-inf.mpg.de/~jnkautz/projects/prt/prtSIG02.pdf (orig paper, 2002)
http://www.tml.tkk.fi/~tmakipat/prtf_fixed.pdf (more tutorial-like into)
http://en.wikipedia.org/wiki/Precomputed_Radiance_Transfer

Many papers related to this, including full dynamic soft-shadows approaches.
- "Precomputed Shadow Fields for Dynamic Scenes" (uses SH for combining shadow fields)
  (see other demo ../shadow_fields/)

  "Real-time Soft Shadows in Dynamic Scenes using Spherical Harmonic Exponentiation"
  Both from http://www.kunzhou.net/

- "Large-Scale Data Management for PRT-Based Real-Time Rendering of Dynamically Skinned Model"
  http://graphics.cs.uiuc.edu/~wfeng2/

Idea:
- For each vertex of the 3D model, precalculate a function defined over
  a sphere around this vertex. This function contains precalculated
  self-occlusion data and material (BRDF) data.
  (But it's *not* dependent on lighting position and intensity!)
- When rendering, use this precalculated function together with
  current light(s) positions and intensity to quickly calculate lighting
  on this vertex. This will light the 3D model, with effects like
  self-shadowing.

Advantages/disadvantages:
- Your object cannot morph (this would break our precalculation,
  that stores self-occlusion). Alternatively: you can precalculate
  transfer for all frames (or only key-frames) of your object,
  and then you can animate this transfer data just like you animate coords.
  But note that transfer data may take a lot of space.
+ Your object can be translated, rotated with respect to the light source.
  (Scaled too? I see no problems, TODO check in practice.)

- Light shouldn't be too close to your object. (Position of light source
  with respect to the vertex matters, theoretically we should reproject
  light at each vertex --- but this has horrible time, and is not needed
  for typical cases. So we project light, say, few times per object --- but this
  means that light pos should be more-or-less the same wrt all vertexes.)
+ We can have many large lights. They don't hurt us. (Light geometry is only
  rendered at eath frame, but this is trivial for OpenGL.)
  In fact, algorithm wants to have large lights (otherwise SH approximation
  will loose the details of light source geometry anyway).

+ Actually, the precalculated stage may compute much more.
  Anything that depends purely on local geometry (not depends on other
  objects or lighting conditions). For example, interreflections
  and, yes, caustics.

- Base PRT is not a shadowing method, that is we do not think here about
  one object casting shadows on other objects. So e.g. a human on a ground,
  with human casting shadow on this ground, is fine (treat human + ground
  as one object), but you cannot move human with respect to the ground
  (within one object nothing can change)...
  But the ideas of PRT *can* be used for fast soft-shadow algorithms,
  see references at the beginning, one example: shadow fields, later.
+~ Well, actually PRT has straighforward extenstion to cast shadows
  (and caustics and everything else) from one object to the other:
  radiance neighborhood transfer. This is quite brutal solution
  (we simply add non-existing points to compute around our object),
  and will work only under a couple of assumptions (lighting conditions
  on one area may be affected by only one other object, so more
  than one shadow (at least one made PRT) = problems;
  neighborhood vertices must be placed and precalculated,
  so shadow/caustics receiver doesn't have full flexibility of moving).

- Model must have quite a lot of vertexes to look really nice.
  Low-poly models look ugly from close distance with dynamic lighting
  changes.

How to represent "function defined over a sphere"? Spherical harmonics
(Polish: "Harmoniki sferyczne". Poetry.)
http://en.wikipedia.org/wiki/Spherical_harmonics.
What's important for us: it's an infinite set of functions
mapping a 3D sphere position (think "direction") to a real number.
Such that, given any (square-integrable) function f
(also mapping direction -> real), we can express it as

  f(s) = l_1 * SH_1 + l_2 * SH_2 + ...

When we truncate this, we get a good approx of f, which removes some
details from f but catches general shape. So a vector (l_1, l_2, ...)
is a nice, compact and efficient way to describe function f.
(Actually SH has two indexes, see paper.)

Computation of SH --- a lot of papers on WWW, for a few simple numbers
we can just find ready results:
http://www.sjbrown.co.uk/2004/10/16/spherical-harmonic-basis/
So we don't really have to get our hands dirty.
Sensible numbers to truncate are 9, 16, 25
according to paper it's not worth trying more (TODO check),
so we only need up to L = 4).

See show_sh program.

Page 8 from Teemu (3.1.1) --- this is the core idea of the algorithm,
shows what and how the precalculation does.

Show results for 1 basis (with "show simple occlusion") first,
to show that basic self-shadowing (visibility * diffuse mat * cos)
is simply calculated in pre-processing. Explanation why this works:

  The first SH basis function is just
  constant (SHBasis0 ~= 0.28), so radianceTransfer can be scaled by
  1/SHBasis0 and then you have simple integral of visibility *  brdf * cos.
  In other words, you have simple ambient occlusion per vertex.

  This is useful for testing, since you can easily visualize and check
  the results of such calculation. radiance_transfer has special trick
  to visualize this: use "Show Simple Occlusion" menu item.
  Then vertex color is simply taken from radianceTransfer value,
  only scaled appropriately.
