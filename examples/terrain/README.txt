This directory contains an example terrain generation and visualization toy.

The rest of this file contains a draft notes for Michalis' talk at seminar
(http://www.ii.uni.wroc.pl/~anl/dyd/GKs/ at 2010).

------------------------------------------------------------------------------
Idea: make nice terrains. Randomness, but "looking terrain-like".

------------------------------------------------------------------------------
Subdivision: oldest?, relatively poor, methods for terrain gen.

Stationary subdivision: add new points between, displace randomly.
Keep old points (thus "stationary").
Problems: visible "creases", because the most
visible peaks/valleys go along the 1st subdivision edges.

Demo: Blender's "subdivide multi fractal" is a "stationary subdivision".
(well, Blender also displaces xy slightly, which actually helps a little,
although the result in no longer an exact "height field".)
Demo creases.

Non-stationary: add new points between, displace randomly, discard old points.
Looks nicer.

Problems in general: no way to make nice function "height_above_point(x,y)"
that works in O(1) without some preprocessing. You have to calculate and
keep your subdivision. This means implementation is complicated by
~memory management strategies, to keep memory usage ~low, and dynamically
add/remove terrain parts (and/or LOD levels for these parts)
when you want infinite terrain.

Extremely quick intro: see page 10 in
"Steven C. Dollins: Modeling for the Plausible Emulation of Large Worlds",
http://www.cs.brown.edu/~scd/world/dollins-thesis.pdf
from
http://www.cs.brown.edu/~scd/world/
(many other ideas and useful references there, quite nice).

------------------------------------------------------------------------------
Synthesizing noise (aka "coherent noise").
References:
blender/source/blender/blenlib/intern/noise.c
http://freespace.virgin.net/hugo.elias/models/m_perlin.htm

(Terminology warning: what I describe below is often called:

- fBm = fractional Brownian motion (e.g. Blender source code, Musgrave's dissertation):

  Most of the time, fBm in terrain generation means "fBm by summing up noise
  functions", this is what I describe below. But actually fBm is only a function
  that we try to approximate (by both below approaches, and by above
  subdivision, and by some other methods).

- Perlin noise: this is a method of making the base noise, I'll describe
  it very shortly at the end. Many publications refer to any general
  noise synthesis like below as "Perlin noise"
  (e.g. http://freespace.virgin.net/hugo.elias/models/m_perlin.htm above).

------------------------------------------------------------------------------
1D for a second:

fbm: generally, a process that has small randomization close,
large from far away. Sounds like exactly what we need.

We do not want to generate such function as a "process", we want to
"synthesise" it, which ~means we want to be able to compute f(x)
equally fast for any x. This is crucial if we want to sample infinite terrain
at any resolution, without affecting the computation (as long
as only the number of samples stays the same, but independent
from samples locations).

Simple noise synthesis:
- random sequences, but again bad --- we want to eval f(x)
  at any point with O(1)
- so, hash function. Let's base it on Trunc(x), we'll take
  care of floats later, for now --- assume we want noise on ints.

Smooth simple noise: box, tri, cosine or cubic spline
  (More precisely, http://en.wikipedia.org/wiki/Cubic_Hermite_spline#Catmull.E2.80.93Rom_spline,
  very concrete equation:
  http://www.mvps.org/directx/articles/catmull/
  (warning: the cubic interpolation code on
  http://freespace.virgin.net/hugo.elias/models/m_perlin.htm
  seems missing weights?) ).
Show demo.
  TODO: this is awfully un-optimal now, each linear/cosine samples 4 points
  and spline samples 16 points. These points are most probably sampled
  many many times. Fix, but without too much preprocessing, to keep
  Height calls nicely independent.

Idea: we want large wide mountains, and small narrow peaks.
So: 1 smoothed noise makes large mountains,
2 smoothed noise makes smaller details, etc.

f(x) = f_1(x) +
       f_2(2x) / 2 +
       f_3(4x) / 4 +
       f_4(8x) / 8 +
       etc. (usually 8 is enough)

subscripts f_i = smoothed noise with seed i (or determined by i;
the point is f_i is a family of functions, for particular i it's
a concrete defined function).

------------------------------------------------------------------------------
fBm 2D: as easy, except we have intnoise2d, smoothed a little smarter
(sample 4 neighboring int grid points).

Less vertical/horizontal look: our noise (even interpolated)
is still "blocky". This is hidden by cosine/cubic interpolation,
but still present underneath. Formally, our 2D noise is
not direction-independent, i.e. if we rotate it by 45 degs it will
have a little different properties.
We can hide it a little better by smoothing
the noise, i.e. "blurring", just like we blur images.
See demo of with/without smoothing.

Note: smooth before, or after interpolation? Doesn't matter, tests
show that results are the same. So it's marginally faster to do it before.
Useful? Not really in my tests...

Actually, this generalizes to any number of dims
3D: volumetric textures (fog, smoke, clouds etc.)
4D: animated fog, smoke, clouds etc.

------------------------------------------------------------------------------
Also: Perlin noise: gradient noise (assume on grid points values are 0,
choose gradients on grid points by hashing, calculate value between grid points
by summing gradients with appropriate weights).

To avoid showing user ever the peculiar zero grid points, we usually
take a slice from an other dimension. That is, if you want 2D noise,
then implement Perlin for 3D, and take a slice with z = 0.5.

------------------------------------------------------------------------------
Simple observation: you can manually control almost everything here
by supplying grayscale images.

- Simplest use: you can add a scaled height from the image to such
  heightmap.
- Image color may be max value (noise is clamped to this),
  to forcibly flatten some areas.
- Image color may be used to affect some noise setting. Like Smoothness
  or Heterogeneous or Amplitude. As long as change them smoothly
  on the image (usually you'll want to filter image e.g. bilinear),
  and you don't touch Frequency (that affects directly X, Y used for noise),
  resulting terrain will be nice, and you can locally vary Smoothness etc.

Some ideas from
http://http.developer.nvidia.com/GPUGems3/gpugems3_ch01.html.

------------------------------------------------------------------------------
Heterogeneous fBm:
  (References: Ken Musgrave's dissertation,
    http://www.kenmusgrave.com/dissertation.html from
    http://www.kenmusgrave.com/
  )
  Just multiply newly added noise by previous noises mult.
    Idea: instead of ading
      resuilt += amplitude * newnoise
    make
      signal = weighted_and_clamped_to_01(signal) * newnoise
      result += amplitude * signal
    Effectively, instead of

       f(x) = f_1(x) +
              f_2(2x)/2 +
              f_3(4x)/4 +
              f_4(8x)/8 +
              etc. (usually 8 is enough)

    we have ~

   f(x) = f_1(x) +
          f_1(x) * f_2(2x)/2 +
          f_1(x) * f_2(2x)/2 * f_3(4x)/4 +
          f_1(x) * f_2(2x)/2 * f_3(4x)/4 * f_4(8x)/8 +
          etc.
          (with additional weighted_and_clamped_to_01() along the way)

  Idea: higher terrain is more "noisy", because debris gathers in lower terrain.

  Demo.

------------------------------------------------------------------------------
Rendering:

Idea: ultra-simplified version of geometry clipmaps:
  http://research.microsoft.com/en-us/um/people/hoppe/geomclipmap.pdf
  (Of interest also GPU implementation of geometry clipmaps:
  http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter02.html)

  Just subdivided rectangles with holes, centered on viewer.
  Problems:

  - Creases, my dumb solution: patch with triangles
    (optimally, we could also copy verts
    positions from more detailed to next less detailed layer, to match perfectly).

    TODO: do copy verts. Fixes visible seams?

  - Common problem with LODs: you either you get visible "flowing" when
    you move camera even slightly (because every little change to Middle point
    changes all sampled positions).
    Or (when using my "cure"
      MiddleX := Round(MiddleX / RoundGridCell) * RoundGridCell;
      MiddleY := Round(MiddleY / RoundGridCell) * RoundGridCell;
    ) you get "popping" (not only when new terrain comes into view,
    this is unavoidable without fog, but also LOD of existing terrain
    features changes).

    Demo trivial fog in shader, BTW.

    Is my trivial impl Ok? For walking, with ~3 layers and fog,
    popping is not that bad. For flying, when you really want to see ~6
    layers to "feel" tha landscape, it's not acceptable.
    Demo.

Proper solution of geometry clipmaps (gpu and not) for both problems
is to introduce "blending" regions, where the higher-resolution map
values are blended with values from lower-resolution,
such that at the critical moment (when you replace some place
with different resolution) it isn't noticeable, as higher
and lower resolutions represented the same (perceived) values.

Also, this allows to avoid regenerating vbo data at each frame.
Just cache the values for all levels, and regenerate them only when needed.
My current implementation also allows this, but I would have to regenerate
all levels at once at critical moment, so it's not very smart.

------------------------------------------------------------------------------
Other rendering notes:
Texture this thing to make it look Ok! (Demo with/without shaders).
Multitexturing, blending textures based on their height.
Additionally: darken on steep faces (when normal.z is low).
Adding a hint of color is good also.

