# Dynamic Ambient Occlusion

Sample implementation of "Dynamic ambient occlusion". See below for algorithm descriptions and links.

By default we open the "peach" model from `data/` subdirectory. You can pass on command-line URL of a different model to open, like `castle-data:/chinchilla_awakens.x3dv`.

Navigate by mouse/keys like in view3dscene (see https://castle-engine.io/view3dscene.php).

Use keys s / S scale shadow more / less (for more/less dramatic effect; the default scale is anyway 2 times larger than what math suggests, to be more dramatic).

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `dynamic_ambient_occlusion_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).

## Algorithm

Dynamic ambient occlusion, based on GPU Gems 2 chapter 14, online on
http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter14.html
(PDF version on
http://http.download.nvidia.com/developer/GPU_Gems_2/GPU_Gems2_ch14.pdf).

Idea:

- Crude assumption: model is actually a number of elements, each element
  corresponding to a vertex of orig model, with normal and area correctly
  calculated.
  See menu _"Program -> Show Elements"_.

- Encode elements info into the texture.

- On each vertex do (in vertex shader): calculate AO using elements info.

Practice:

- First of all, this AO will have too much shadow (multiple shadows
  on the same item). So calculate 2nd time, multiplying by 1st result.
  The key is that "if element is already in shadow, then it also doesn't
  contribute anything to the other shadows".

  2nd pass will make the result too much light (because we multiplied
  by results of 1st pass, which were too dark...). Theoretically,
  more passes are needed. In practice: the result is between 1st
  and 2nd passes, so just at the end of 2nd pass take the results
  and average.

- Note that you cannot actually do this in vertex shader: with arch 3,
  texture2D calls in vertex shader are not allowed or
  very very horribly-terribly slow. And you want to read texture a lot
  to get all element infos.

  (I tried to do the 1st testing implementation
  of one pass on vertex shader, it simply doesn't work when tested on older
  NVidia and works with 1 frame / couple seconds (even with 1 element count!!!)
  on fglrx.)

- So do this on fragment shader. Also, render such that 1 screen pixel =
  one element, and simply write result back to buffer. Using the texture,
  1st shader passes results to the 2nd, and 2nd passes it to the CPU.

  For communication between shaders, texture data doesn't have
  to go through CPU --- glCopyTexImage2D is perfectly designed for such tricks.

  For actual rendering, I grab the colors to the CPU --- while it would be possible
  to avoid this, I would then run into trouble trying to access textures
  from vertex shader, which is practically prohibited for shaders gen <= 3,
  see above notes.

- Shader optimization notes:

  - Remember to expand constant variables for the shader, instead
    of making them uniforms. This is important, not only for speed,
    also for correctness: older GPUs need to unroll "for" loop
    (to elements_count) at compilation.

    (I didn't expand all possible vars, because fglrx doesn't tolerate
    floats with fractional part in GLSL code; so to be safe for fglrx too,
    only ints can be expanded such.)

  - Above unrolling also means that this may just get too difficult
    for older GPUs for larger models: too many instructions after unrolling.
    E.g. my NVidia GPU (kocury) "GeForce FX 5200/AGP/SSE2/3DNOW!" handles
    smaller models (simplico), but fails with
    "error: too many instructions" on larger ones (peach, chinchilla).
    More tests show that the border is somewhere between
    24 (Ok) and 42 (not Ok) verts (for this GPU, of course;
    much newer Radeon on MacBookPro can handle at least 20k (although
    naive implementation gets really slow then).

- Note that GPU Gems text gives two (different) equations for calculating
  element-to-element shadowing, both of them wrong IMO...
  Yes, I did implemented and tried them. It's a matter of changing
  "color -= ..." line.
  Math eq on page 3 is equivalent to

    ```
    color -= sqrt(sqr_distance) * cos_emitter_angle *
      max(1.0, 4.0 * cos_current_angle) /
      sqrt(element_area / pi + sqr_distance);
    ```

  Code on page 4 is (possibly --- not sure, as they put "1 - " part
  already there, so I don't see a reasonable way to use it?) equivalent to

    ```
    color -= inversesqrt((element_area/pi)/sqr_distance + 1) *
      cos_emitter_angle *
      min(1.0, 4.0 * cos_current_angle);
    ```

  If you grok reasoning behind their equations, and/or if there's an
  error in my implementations above, mail me. My implementation follows
  normal approximation for solid angle:

    ```
    element_area * cos_emitter_angle / sqr_distance
    ```

  We also multiply by cos_current_angle
  (as light coming from an angle is scaled down, so blocker for
  which cos_current_angle is small blocks less light).

Advantages:

- Since all work is done on the fly, the scene may be absolutely dynamic.
  (Although on change, list of elements must be updated, this causes CPU work.)
  For example, see:

    - data/chinchilla_awakens (timesensor animation) and
    - data/dynamic_world_ifs (interactive world changing, press left mouse down, use keys werxdf, uiojkl)

- Can make bent normals, indirect lighting almost for free.

- Although the work is done on shaders, resulting colors can be grabbed to CPU.

    While this is not efficient, it allowed us to make simple implementation, and to easily debug it too.

- Can work with practically any 3d model.

    For example, check out castle/data/levels/fountain/fountain_final.wrl

Disadvantages:

- Like PRT and shadow fields, this requires a large number of vertexes (elements) to look good, and still number vertexes is also the key thing that determines speed. (On the up side, it's not difficult to make lower LOD versions of elements, since they are so simple? Hierarchy idea will also help a lot.)

    On dynamic models, when too few vertexes are available, it can be seen as moving shadows are "sticky" to vertexes.

- Requires a good GPU. Fragment shader must do a *lot* of work, a lot of texture lookups. Simply not possible on older GPUs.

TODO: our current method of generating elements works only for nodes
with explicit vertexes, so will not work for X3D primitives (sphere,
cone and and such).
