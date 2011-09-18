The bump_mapping demo, showing off various bump mapping techniques.

- First of all, the modern bump mapping techniques using GLSL are shown.
  These are implemented right now inside the engine itself,
  this program metely loads a nice demo scene and allows you to choose
  the exact method (parallax etc.).

  You can use this bump mapping out-of-the-box in your own models,
  just add to the model appropriate normalMap declarations,
  see http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_bump_mapping.
  No special ObjectPascal code needed at all.

- Also, implementation of some older bump mapping techniques is shown:
  dot by multitexturing (works in OpenGL fixed-function pipeline)
  and emboss (a really, really old approach to bump mapping).

The rest of this file contains various notes about bump mapping,
mostly from my old talk at seminar at ii.uni.wroc.pl.

------------------------------------------------------------------------------
Bump mapping overview: brick wall.
Texturing: because obviously too costly to model each brick.
Bump mapping: increase the look, still without increasing geometric complexity.
Idea: normals change within. How about we store normals somehow in some texture
(to be able to vary them on pixel-by-pixel basis ?) ? This changes the look,
without changing the geometry.

------------------------------------------------------------------------------
Emboss idea:

Like 2D effect. Take image, shift, subtract --- this tells how to scale color
{draw image with hills, it's clear}.

More math speaking, we're calculating derivative, actually gradient
(gradient = 2d derivative) and calculating gradient is how we calculate normals
from height map. This is also the answer to "auto generation of normal
maps from height maps", useful for dot method much later.

First, overview of OpenGL multitexturing:

------------------------------------------------------------------------------
GL_ARB_multitexture:

GL_ARB_multitexture avail from OpenGL 1.2.1, incorporated as standard since
OpenGL 1.3.

http://www.opengl.org/wiki/index.php/GL_ARB_multitexture - short.
http://www.berkelium.com/OpenGL/GDC99/multitexture.html - nice intro.
Multiple textures, mixed freely - multiple uses.
Common (first ?) example: lightmaps in GLQuake.
First, historically, "ARB" extension (reviewed by ARB, as opposed to EXT
that are popular and accepted but not necessarily reviewed). ARB essentially
means "we think that this will definitely go into gl standard".

Separate texture units: separate coords, matrix (stack too, you can feel
this painfully: do push texture matrix, change active texture, pop
texture matrix -> OpenGL stack underflow error),
environment, bound unit (although the same texture can
be bound to multiple channels; and yes, we will use this for emboss).
Set active unit:

glActiveTextureARB(GL_TEXTUREn_ARB); // 0 <= n < GL_MAX_TEXTURE_UNITS_ARB

Note that glTexCoord2f still affects only 0th texture (regardless
of glActiveTexture calls). To change coords for other textures, use

glMultiTexCoord2fARB(GL_TEXTUREn_ARB, x, y);
(so glTexCoord2f(x, y); is just a shortcut for
glMultiTexCoord2fARB(GL_TEXTURE0_ARB, x, y)).

One more: for client things (notably glEnableClientState(GL_TEXTURE_COORD_ARRAY))
there's glClientActiveTextureARB(GL_TEXTURE0_ARB) (analogous to
glActiveTextureARB(GL_TEXTURE0_ARB)). E.g.

  glClientActiveTextureARB(GL_TEXTURE1_ARB);
  glTexCoordPointer(2, GL_FLOAT, 0, tp1);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);

(This will be improved soon by GL_EXT_texture_env_combine, but for now,
state of GL_ARB_multitexture:)

Textures are mixed as specified by TEXTURE_ENV_MODE.
- interpolated pixel color (from vertex colors, that already include
  lights (materials, eventually colors set explicitly by glColor), fog).
- ... is mixed with texture 0 color. This is where we stop without multitexturing.
- ... the result above is mixed with texture 1 color.
- ... and so on, for each enabled texture unit.

See glTexEnv. For example, GL_MODULATE means (for texture RGB case)
that color is multipled component-wise by tex color. GL_ADD may be useful
for 2nd and further texture units.

Note that some equations still require multipass technique, e.g.
  A * B + C * D
(where A, B, C and D are 4 textures). Without multitetexturing, this
is not doable at all with screen rendering. Multipass doesn't help:
you could render A, then mutliply by B, then add C. Nothing to do with D...
With multitexturing, you can actually do (A * B + C) in one pass.
So you can do multitexturing with 2 passes, one pass does A*B and 2nd one adds C*D.

Naturally, it turns out that multitexturing allows us to do the same things
as multipass in the past. Multitexturing is just better (you have to transform,
rasterize, z-test, stencil-test etc. only once).

------------------------------------------------------------------------------
GL_EXT_texture_env_combine:

Emboss is (Bump - shifted(at runtime) Bump) * normal scene color.

Bump is one texture unit, "shifted(at runtime) Bump" goes to 2nd texture
unit. Actually, we can also invent ourselves, so "- shifted(at runtime) Bump"
goes to 2nd texture unit. Then texture env may use GL_ADD.
Actually, we can avoid precalculating inversion (saves texture memory)
since GL 1.3 gives us GL_SUBTRACT.
Also, note that we want (...) to be in 0..1 range. Right now it's in -1..1 range.
So scale / 2. Totally we have:
  0.5 + Bump/2 + (- shifted(at runtime) Bump/2)

Still, how to do this by 1-pass multutexturing ? At the beginning we have to
input "normal scene color", so no way to multiply by combined
"(Bump - shifted(at runtime) Bump)". Here's GL_EXT_texture_env_combine
useful.

GL_EXT_texture_env_combine
later
GL_ARB_texture_env_combine
later
standard in gl 1.3
1.3 also added GL_SUBTRACT, we will use it.

Idea:
glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);
and then
glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_xxx, ...);
determine the actual computation.

GL_COMBINE_RGB,
GL_COMBINE_ALPHA,
           RGB_SCALE,
	   ALPHA_SCALE,
           SOURCE0_RGB,    OPERAND0_RGB,
	   SOURCE1_RGB,	   OPERAND1_RGB,
	   SOURCE2_RGB,	   OPERAND2_RGB,
	   SOURCE0_ALPHA,  OPERAND0_ALPHA,
	   SOURCE1_ALPHA,  OPERAND1_ALPHA,
	   SOURCE2_ALPHA   OPERAND2_ALPHA

Advantages of texture_env_combine
- you can specify RGB and ALPHA mixing orthogonally (old, classic modes for
  TEXTURE_ENV_MODE, like glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
  specified both RGB and ALPHA methods at once).

- we have more modes,  like GL_SUBTRACT, GL_INTERPOLATE (one texture unit
  specifies interpolation between two other texture units, this uses three
  arguments).

- For each of 3 arguments, we have
  GL_COMBINE_SOURCEn_RGB, SOURCEn_ALPHA,
             OPERANDn_RGB, OPERANDn_ALPHA

  SOURCEn values tell from where to take this (from fragment unmodified color
  PRIMARY_COLOR (as normal for 0th texture unit), from previous texture
  PREVIOUS (as normal for > 0th texture units), from constant color,
  explicitly selected texture unit, currently bound texture unit...).

  OPERANDn values allow you to additionally transform the value.
  Usually, it's GL_SRC_COLOR or GL_SRC_ALPHA for OPERANDn_RGB, OPERANDn_ALPHA,
  but you can also change them. This allows you to mix RGB+Alpha to, well,
  RGB and/or alpha of this texture unit.

Since SOURCEn may point to any texture unit, not only to "current unit",
we can arrange our calculation completely independent of loaded textures.
That is, enable(GL_TEXTURE_2) and set glTexEnv is one set of texture units.
And you can load actual textures (and point to them by SOURCEn) to totally
independent texture units.

Stil, one limitation: only GL_PREVIOUS can get result of previous calculation.
So we *cannot* make any calculation arrangement... this is still a pipeline
arrangement, one result goes to another. That's why many multitexturing tricks
use alpha and RGB channels for totally different jobs.

For emboss, this will allow a 1-pass method.

http://oss.sgi.com/projects/ogl-sample/registry/ARB/texture_env_combine.txt

See "Texture Mapping: Beyond the Basics"
http://www.informit.com/content/images/9780321498823/samplechapter/0321498828_CH09.pdf
for practical overview of texture extensions in OpenGL.

http://www.opengl.org/sdk/docs/man/ - glTexEnv manpage as of OpenGL 2006.

------------------------------------------------------------------------------
Emboss 1-pass:

Back to
  (0.5 + Bump/2 - shifted(at runtime) Bump/2) * normal scene color
Normal scene color is actually PRIMARY_COLOR * normal texture, so this
is still not so easy. You cannot make it 1-pass without using alpha channels.
Only on RGB, you would have to make
  PRIMARY_COLOR * normal texture
as first calculation and then multiply it by (0.5 + Bump/2 - shifted(at runtime) Bump/2)
which is not doable...

Idea is to use alpha channel. Compute "PRIMARY_COLOR * normal texture"
in RGB channel and in parallel "(0.5 + Bump/2 - shifted(at runtime) Bump/2)" in alpha
channels. This may work, since for "normal texture" and "0.5 + Bump/2" you
want the same texture coordinates, so you can put them on a single RGBA texture.

------------------------------------------------------------------------------
Light in tangent space: how to calculate the shift.
See code (LightDirectionInTangentSpace function), see Moller+Haines.

Explanation and code of this in nehe demo is rather messy...
Scan drawing from Moller+Haines ? There's a short and good expl.

------------------------------------------------------------------------------
Emboss finish:

So I have bump_amount * normal_color. normal_color = primary_color * tex_color.
The idea is that bump_amount replaces (approximates, but with bumps)
normal lighting diffuse color.
So the "bump_amount * normal_color" seems to be the correct way, assuming
that normal color is calculated with diffuse factor = always 1.0.
And with lighting off.

Actually getting rid of normal diffuse calculation is not good, bump_amount
doesn't catch any normal diffuse behavior, we miss the fact that light
doesn't shine at all on some surfaces, when it's on heir back side,
we miss the fact that light is much smaller when viewed from small angle...
So we have to turn on lighting to catch this.
Also, surfaces are darker, as now 0.5 modulation = normal lighting (shifted
bumps equal, and this occurs on most places).
Could balance by making lighter textures.
Implemented (2nd and 3rd columns).

Also, another hack would be make "(Bump/2 - shifted(at runtime) Bump/2)"
(so result may be < 0, although OpenGL will clamp to 0) and simply add
bump_amount + normal_color. Totally not related to any lighting equation,
but looks quite OK.
Implemented (4th and 5th columns).

Emboss bump mapping examples elsewhere:
http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=22
http://nehe.gamedev.net/data/lessons/extras/lesson22/EmbossBumpMapping.ppt

Some my improvements: 1-pass only method.

For "bump_amount + normal_color" approach (see below), 3 texture units
are needed.

For the modulate "bump_amount * normal_color" approach (see below)
we can get away with only 2 tex units, which are found practically
anywhere. The role of 3rd texture unit can be done by blending with
glBlendFunc(GL_SRC_ALPHA, GL_ZERO). Both approaches implemented,
and for modulate both 3rd texture unit or blending method done --- you can
see that results are exactly the same in this case.
See comments in code (when setting 3rd tex unit,
glActiveTextureARB(GL_TEXTURE2_ARB)) about this.

GL_SUBTRACT used, so no need to load bump map twice into memory.

------------------------------------------------------------------------------
Note about generating height maps from normal ("normal" as "the ones used
for normal texturing", not as "normal maps") texture:
- get image, increase brightness and contrast almost to the max,
  making it almost black and white image.
- middle row looks worse, because it contained many small white/black
  stains. In lower row these stains are removed, image much better.
So seems that automatic generation of sensible bump images is easy and doable.

If you start from highly polygonal model and you will convert to low poly,
it's straightforward to get height map from highly polygonal version
(just render depth with ortho projection set directly at the face).
There are also programs that just do this automatically, some links from
http://www.realtimerendering.com/#texture

TODO: auto generation of lighter images for emboss
------------------------------------------------------------------------------
Dot bump mapping (using multitexturing):

Requirement: texture combine function DOT3_RGB_ARB. Added by
ARB_texture_env_dot3 extension (obviously depends on ARB_multitexture
and EXT_texture_env_combine). Standard since GL 1.3. It works on RGB sources,
calculates
  4*((Arg0_r - 0.5)*(Arg1_r - 0.5) +
     (Arg0_g - 0.5)*(Arg1_g - 0.5) +
     (Arg0_b - 0.5)*(Arg1_b - 0.5))
and places the same result in all R, G, B. (There's also analogous
DOT3_RGBA_ARB, not important for us). Why "-0.5" and "4*" above ?
Because we will use R, G, B values to represent lighting direction
and normals direction. So actually we want to place -1...1 values there.
We squeeze them by (value + 1) / 2 to get into range 0..1.
For the calculations, we want to invert this, so we want to calculate

  2 * (Arg0_r - 0.5) * 2 * (Arg1_r - 0.5) +
  2 * (Arg0_g - 0.5) * 2 * (Arg1_g - 0.5) +
  2 * (Arg0_b - 0.5) * 2 * (Arg1_b - 0.5)

Yes, DOT3_RGB_ARB was explicitly "invented" for doing bump mapping.

Idea:

1. Make a texture with normals stored as 3-value vectors.
   This is called normal map, and what we used in emboss is called height map
   (height map when used for bump mapping is sometimes confusingly called just bump map).

2. Pass direction of light-to-vertex (in vertex space, i.e. calculated
   by transforming light pos to tangent space just like for emboss) as
   some 3f value that will be interpolated and available for texture unit
   operations.

   a) It can be passed as color3f (with smooth shading this is interpolated),
      and lighting may be off, then texture unit can access it simply as
      PRIMARY_COLOR.

      Advantage: simple. Disadvantages: not normalized light vector
      (see below), also you loose any material/light parameters in
      calculation. Practically, material is like 0 ambient, 1 diffuse,
      0 specular, and light properties also.

      Although you could add ambient by another texture unit, adding
      some constant color.

      And this method also allows to calculate speculars: probably 2nd pass needed
      for this, and some tricks with textures or pixel shader to calculate
      exponent in texture unit, newer pixel shader has calculating exps
      built-in. I didn't do it, but Haines+Moller say it's doable all right,
      and in fact this is one of advantages of dot product over emboss.

   b) texCoord3f (texture coords are obviously interpolated by gl).
      Then texture unit can use
      this to access actual values using cube map, that normalizes the
      light direction (as interpolation between two normalized vectors
      easily leads to not normalized vectors (although quite close),
      {draw simple example}).

      This way we get more correct method, at the cost
      of one additional texture unit needed and we have to prepare normalizing
      cube map.

      See below.

Use DOT3_RGB_ARB to calculate dot between light direction and normal
at this point of the screen. Effectively, this calculates the diffuse
factor of the lighting equation, so we can use this just like
bump_amount in emboss bump mapping.

http://nehe.gamedev.net/data/articles/article.asp?article=20

Nice explanation of dot product bump mapping (using multitex):
http://www.paulsprojects.net/tutorials/simplebump/simplebump.html
(testing shell_normalmap from there)
(although there code does 2-pass, while we did 1-pass with 3 texture units
or even 2 texture units if we ignore normalization).

------------------------------------------------------------------------------
Cube map:

Idea: we have a 3D cube, with size 2, centered around the origin.
We can easily project any (x, y, z) point on this cube:
1. choose component with largest absolute value.
   Let's say, we have (33, 44, -100). So we have -100, which says
   "take -Z side of the cube".
2. divide other 2 components by abs value of this. IOW, simply
   scale down the vector such that it's exactly on the -Z cube face:
   (0.33, 0.44, -1). So take from -Z side of the cube vertex corresponding
   to 0.33, 0.44. If you think about texture coordinates in 0..1 range,
   you can take (x+1)/2.

Invented for environment mapping: an object reflects some surroundings
(assumption: surroundings are far, far away from this object; e.g. sky).
We have viewer direction, normal of the surface, calculate reflection vector,
use this as (x, y, z) to see surrounding color at this place of the cube.

TODO: demo program.

Our use of cube mapping will be more "creative" :) Namely: we want to
use cube maps as precalculated array of normals. Initialize colors of cube map
textures such that they specify normalized vector (x, y, z).
This way, lookup in cube map normalizes any vector... without any nasty sqrs
(but at the cost of memory and aliasing). What's more, OpenGL does all the work
for us, we only initialize cube map.

Show the normal map colors, view3dscene normalization_cube_display.wrl.

See code how we init and use it for dot3. 3rd texture unit is needed
(no escape from this, we need 1 RGB unit for original texture,
1 RGB unit for 3D normals, 1 cube map unit for cube map; if you don't
have 3 texture units, you must do 2-pass... or just ignore the whole
"normalization" issue, which is quite reasonable).

Compare normalized and not normalized results: not normalized are a little
darker, as expected. When the light moves over the surface, the lighting
seems dimmer right under the light, this is exactly the result of not
normalizing. So, this normalizing is needed in pratice, if there's a chance
that user will be able to clearly see this bad artifact of non-normalizing.
If the light is not directly near and over the surface, the difference
between these two methods is not noticeable at all (no wonder, interpolating
normals (almost) preserves their length 1 then).

Oh, this is OpenGL extension ARB_texture_cube_map (and standard since gl 1.3,
as almost every other gl feature we were talking about here...).

------------------------------------------------------------------------------
Auto generation of normal maps seems doable easily,
just calc gradients on both sides, e.g. by Sobel filter.

Actually, it's already done as GIMP plugin:
http://nifelheim.dyndns.org/%7ecocidius/normalmap/
(some info on
http://developer.valvesoftware.com/wiki/Normal_Map_Creation_in_The_GIMP
also). It can even try to do the other way around, i.e. restore height map from
normal map.

For this demo:

shell_normalmap was taken from
http://www.paulsprojects.net/tutorials/simplebump/simplebump.html
(for emboss, height map was generated by gimp-normalmap).
Y was inverted.
shell.jpg is totally unrelated texture from http://lemog.fr/

brick_* heightmaps were made from original textures, and normal maps were
done using gimp-normalmap (also from original textures).

Important notes:

- *Do not* generate normal maps from the height maps that
  you used for emboss bump mapping. Height maps for emboss bump mapping
  have little noise, this was needed for emboss effect to look OK
  (compare brick_1_bump and brick_1_bump_better). However, for dot3 method,
  detailed (noisy) normal maps are good, and non-detailed in fact give
  "too subtle" effect (compare brick_1_normalmap, done from original brick_1,
  with brick_1_normalmap_worse, done from brick_1_bump_better).
  So generate normal maps from original detailed textures, if possible.

- Remember to check "invert y" for generating. We expect normals to be oriented
  such that
    +X follows texture S and
    +Y follows texture T and
    +Z sticks out of the surface.
  This seems, well, obvious, but remember that image coordinates in GIMP
  (and every other 2D image editing program) are such that Y grows *down*
  (contrary to maths standard). So select "invert y" when making normal maps,
  or your normalmaps will be bad.

------------------------------------------------------------------------------
Practical implementation in VRML: (show VRML demo)

How to calculate STangent and TTangent from normal vertex data + tex coord
data that we usually get in 3D model. See paper (I mean, a real piece
of paper with my drawing that I'll hopefully remember to take tomorrow).

(Speed notes below are obsolete, from 2007-11. In 2007-12 I implemented
bump mapping in VRML using GLSL shaders, this allows to use normal
VRML optimizations (not poor roNone) and speed is much greater,
pretty much a non-issue with test model.)

Speed: FPS around 30 on chantal. When viewed by
$ view3dscene \
  ../../../../castle/data/levels/fountain/fountain_bumpdemo_final.wrl \
  --renderer-optimization=none
(i.e., without bump mapping) it's around 45.
Sure, when viewed with optimizations, it's around ~100, or ~300 etc...
Current implementation of this in VRML is lousy, it forces me to turn
off optimizations that precalculate many things (for example, normal
vectors are recalculated each frame with roNone optimization,
which is quite stupid... but I have to use roNone, because texture coords
change depending on light position). IOW, better optimization should be
done, to precalculate everything as it should, but specify tex coords
for bump mapping dynamically. Current optimizations assume that parts
of model (at least untransformed shape+states) are constant.

Also, currently I even calculate STangent, TTangent at each frame.
Although tests (when commented out and calculation replaced by dummy
  STangent := Vector3Single(1, 0, 0);
  TTangent := Vector3Single(0, 1, 0);
suggest that calculation of tangents at each frame doesn't hurt noticeably.
Still, it's just ugly.

In other words: looks like performance penalty is not a problem.
Current performance problems are results of my lousy implementation
of this in VRML renderer, which should be remedied quickly...

Still, this scene is only 6300 triangles. Test on larger scene could
be useful.

Final, practical note: I used normal maps and height maps the same
size as original textures. That's a waste of texture memory,
according to various articles these maps are OK being 2 times
(i.e. 4 times area) smaller than original textures. Or even more smaller.

I added bump mapping to VRML by inventing normalMap extension,
see [http://castle-engine.sourceforge.net/kambi_vrml_extensions.php].
Other implementations in VRML of this are X3D multitexture node
(see [http://www.web3d.org/x3d/specifications/ISO-IEC-19775-X3DAbstractSpecification/Part01/components/texturing.html#MultiTexture]
with DOTPRODUCT3 mode) and Cortona's BumpMap extension for VRML 97
[http://www.parallelgraphics.com/developer/products/cortona/extensions/bumpmap/].
Both X3D and Cortona ways allow you to do bump mapping just by allowing
you to do multitexturing with Dot3 operation, I felt that this exposes
implementation details too much (and you still have to implement in VRML script things
like calculating light direction in tangent space, eventually normalizing it
on each pixel etc.). I feel that my normalMap field is much more comfortable
for VRML authors (you just specify normalMap texture, and VRML engine is
supposed to do everything else to handle this normal map somehow).

------------------------------------------------------------------------------
Notes about filtering: I simply filtered normal maps just like normal
textures, LINEAR_MIMAP_LINEAR (2 mipmaps mixed, both with bilinear)
and LINEAR for mag. Other options are simply
not acceptable, the usual ugly aliasing occurs.
I mention it because articles about bump mapping sometimes scare that
filtering too smoothly may result in bad artifacts. In my experience,
normal maps and height maps require filtering for the same reason as
normal material textures, no escape from this.

------------------------------------------------------------------------------
See
../../glsl_bump_mapping.vs
../../glsl_bump_mapping.fs
for implementation of bump mapping using GLSL.

------------------------------------------------------------------------------
Parallax mapping:

Basic idea is just to do h * E.xy / E.z.

Offset limiting:
http://www.cs.cmu.edu/afs/cs/academic/class/15462/web.06s/asst/project3/parallax_mapping.pdf
The above paper proposes "offset limiting", which means simply to drop "/ E.z".
Demo GLSL implementation.

Problems with parallax mapping: texture must be smooth.
Demo brick_1_height_map and brick_1_height_map_smoother for classic parallax.
With brick_1_height_map_smoother, the artifacts (strange texture stripes)
disappear, but it's clearly visible that heightmap is too smooth,
perspective looks a little strange.

Steep height differences cause problems, as we quickly hit something else
(we reach "too far" with texture offset).
Solution: "steep parallax mapping".
http://graphics.cs.brown.edu/games/SteepParallax/index.html
http://graphics.cs.brown.edu/games/SteepParallax/mcguire-steepparallax.pdf
http://www-static.cc.gatech.edu/grads/d/davidp/parallax_mapping/

Also called "parallax occlusion mapping" (this is *exactly* the same algorithm,
as far as I read).
http://ati.amd.com/developer/gdc/2006/GDC06-Tatarchuk-Parallax_Occlusion_Mapping.pdf

Steep parallax allows us also to do self-shadowing within the surface.
Trivial, just repeat the mini-ray-tracer algorithm,
tracing the direction to the light.

See
../../glsl_parallax_bump_mapping.vs
../../glsl_parallax_bump_mapping.fs
for implementation of this in GLSL.

------------------------------------------------------------------------------
End:

- Environment bump mapping (normal map says how to transform environment
  texture coord at each point; this way environment-mapped texture is
  modified with bumps from normal map)

- Displacement: actually change geometry. But smartly (like, depending
  on the camera position and dir with respect to surface plane).
  Don't ask me how...

------------------------------------------------------------------------------
Not really related to bump mapping info:

A good thing to remember for multi-pass implementations,
mentioned in from http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=22:

  Note also that this cube CAN NOT be done using a display list, since
  display-lists seem to use an internal floating point accuracy
  different from GLfloat. Since this leads to several nasty effects,
  generally referred to as "decaling"-problems, I kicked display
  lists. I assume that a general rule for multipass algorithms is to do
  the entire geometry with or without display lists.

------------------------------------------------------------------------------
