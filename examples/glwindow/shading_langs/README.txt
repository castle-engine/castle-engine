------------------------------------------------------------------------------
What are shading languages, in short:

OpenGL program is basically

  glLoadMatrix, glMultMatrix (and shortcuts like glLoadIdentity, glTranslate, glScale...)

  glBegin(...)

    gl(... some vertex attrib, like glTexCoord, glMultiTexCoord, glMaterial...)
    glVertex(...);

    gl(... some vertex attrib, like glTexCoord, glMultiTexCoord, glMaterial...)
    glVertex(...);

    gl(... some vertex attrib, like glTexCoord, glMultiTexCoord, glMaterial...)
    glVertex(...);

    ....

  glEnd();

For each glVertex, some work is done.
1. transform it by projection * modelview matrices,
2. calculate per-vertex values like color from lighting
   color =
     material emission +
     scene ambient * mat ambient +
     for each light: (
       light ambient * mat ambient +
       light diffuse * mat diffuse * diffuse factor +
       light specular .... etc.).
This work may be replaced by vertex shader, to transform in any way we like,
calculate color by any equation we like (using OpenGL lights parameters or not)
and some other.

When vertexes make primitives, each primitive is checked (for backface culling)
and is rasterized. Note that some vertexes
are shared by more than one primitive, e.g. triangle strips, fans etc.
The idea of these primitives is that the per-vertex work (whether fixed-function
or vertex shader) is done only once, and results are reused.

Fragment = same thing as pixel by default, although some operations like
glPixelZoom change this correspondence.
Rasterization calculates coordinates of fragments, each fragment gets as input
some interpolated values (for example, color results of lighting calculation from
paragraph above are interpolated for fragment).

Then for each fragment some work is done.
In fixed-function pipeline, this is mostly to mix color with textures
(all the multitexture operations set up by glTexEnv are done here) and apply
fog parameter. This work may be replaced by fragment shader.

There are also geometry shader, for later...

Notes
- Both shaders *replace* fixed-function work. There's generally
  no way to say "do exactly the same work as when fixed-function was used,
  and then pipe the results through this shader...". (there are special
  commands/variables to do inside shader some part of this, like ftransform
  and lightProducts, but that's about it).

  This is by design, the idea is that using shaders may be actually
  *faster* than letting fixed-function pipeline do it's job.
  That's because fixed-function pipeline does quite a lot of complicated
  work, and in most cases we don't need it. IOW, shaders are usually
  written with very specific environment in mind (e.g. "I will use
  only one light source, and without attenuation").

- In GLSL < 1.40:
  There's no requirement to replace both vertex and fragment shaders.
  It's Ok to replace only one of them, and let the other do it's fixed-function
  job.

  ... but it's often useful to replace both. When we write both vertex and
  fragment shaders, we're able to pass from vertex shader to
  fragment shader any value to interpolate. This is called "varying" variable.

  For example, Phong shading: trivial vertex program to just pass normal
  for interpolation, and fragment program to calculate lighting per-pixel.

- In GLSL 1.40:
  you can't mix fixed-function stages with shaders.
  E.g. if you provide fragment shader, you should also provide vertex shader.
  Mixing is deprecated in GLSL 1.30, disallowed in GLSL 1.40.

http://en.wikipedia.org/wiki/Shading_language

------------------------------------------------------------------------------
Assembly shading languages:
- really old: NVidia register combiners
  http://developer.nvidia.com/object/registercombiners.html

  In some way, this is not a new language, it's only an extension to
  multitexturing, "programmed" by issuing appropriate OpenGL API calls,
  much like normal multitexturing.
  But with much more parameters and flexibility. So much that complexity
  of this qualifies it as an initial attempt at fragment programs.

- NVidia-specific ancestors of ARB langs below (don't use, use ARB now):
  NV_fragment_program
  http://www.opengl.org/registry/specs/NV/fragment_program.txt
  NV_vertex_program.txt
  http://oss.sgi.com/projects/ogl-sample/registry/NV/vertex_program.txt
  Very similar to ARB counterparts.

- blessed by ARB (standard in gl >= 2.0):
  ARB_vertex_program
  http://oss.sgi.com/projects/ogl-sample/registry/ARB/vertex_program.txt
  ARB_fragment_program
  http://oss.sgi.com/projects/ogl-sample/registry/ARB/fragment_program.txt

  http://en.wikipedia.org/wiki/ARB_%28GPU_assembly_language%29

  Very nice overview or ARB vertex/fragment assembly programs:
  http://www.ce.chalmers.se/edu/course/EDA425/lectures/shaders.pdf

  - Show OpenGL API to init them (glProgramStringARB and friends).
  - They replace fixed-functionality (show diagrams, x3d specs about shaders also).
  - We have OpenGL state available.
  - Vectors are "primitive" types for them, most instructions work on whole
    vectors component-wise, i.e. naturally SIMD model
    (actually multiple *shaders* even run in parallel, see "no jumps" note
    later).
  - Each instruction can, by the way, negate and/or swizzle it's arguments,
    that is we expect that negation and swizzling is "free" as far as
    speed is concerned.
  - Various normal math operations (sin/cos, log/lg2/exp/ex2/pow, add/sub,
    rcp/rsq, dp3/dp4).
  - Special for fragment: texture lookup (tex and friends).

    Note that the "pipeline" constraint, present when we were
    playing with multitexturing without shaders (GL_ARB_multitexture,
    GL_EXT_texture_env_combine) is not present in shaders.
    In one program I can access any texture coordinate and sample any texture.
    E.g. for ARB vertex program see fragment.texcoord[n], texture[n],
    operations like TEX, TXP.

      TEX some_result, some_coordinate, texture[some_texture_unit_number], 2D;

    So we can avoid the previous multitexture constraint that forced the
    calculations to access only results from previous texture unit.

  - Note: no real "if" to make execution of shaders in parallel possible.
    There are only "cheats" like "CMP" --- conditional copy,
      CMP result a b c
    means
      result := if a < 0 then b else c;
    or
      SLT result a b
    means
      result := if a < b then 1 else 0;
    Sure you can do tricks with this, actually you can emulate whole "if"
    (in the worst case, just set some variable based on the result of comparison,
    then perform both branches of "if", one after the other,
    multiply result of the 1st branch with SLT result,
    multiply result of the 2nd branch with (1-SLT result)).

    You can play with assembler languages, by compiling GLSL to it by cgc:
      cgc -oglsl -profile arbfp1 glsl_test_if.fs
    .. to see that this "if" compiles to "CMP" and "SLT".
    Example:

      void main()
      {
	if (gl_TexCoord[0].x > 0.5)
	  gl_FragColor = vec4(1, 1, 1, 1); else
	  gl_FragColor = vec4(0, 0, 0, 1);
      }

    =>

      PARAM c[1] = { { 1, 0, 0.5 } };
      TEMP R0;

      SLT R0.x, c[0].z, fragment.texcoord[0];
      # So R0.x = 1 means "0.5 < gl_TexCoord[0].x", that is 1st if branch
      # should be done. Otherwise it's R0.x = 0.

      ABS R0.x, R0;
      # This is useless ? Most probably this is a common "combo" of instructions
      # together with compare below.

      CMP R0.x, -R0, c[0].y, c[0];
      # last arg "c[0]" actually is equivalent to "c[0].x", since we use
      # only the 1st component of the vector (because result is only 1 component).
      # So this is equivalent to
      #   CMP R0.x, -R0.x, 0, 1;
      # So
      #   if -R0.x < 0 then R0.x = 0 else R0.x = 1
      # so (consider R0.x is either 0 or 1)
      #   if R0.x = 1 then R0.x = 0 else R0.x = 1
      # IOW, this was *really* convoluted way to say "R0.x := 1 - R0.x;"

      CMP result.color, -R0.x, c[0].yyyx, c[0].x;
      # "-R0.x < 0" still means just "R0.x = 1". So if that's the truth,
      # set result.color to (0, 0, 0, 1) otherwise (1, 1, 1, 1).

      END

    As usual, we could write better assembler code by hand, as we see
    that "ABS" was useless and actually "R0.x := 1 - R0.x;" was useless
    too (I could just swap the operands of CMP). So I can shorten this to:

      PARAM c[1] = { { 1, 0, 0.5 } };
      TEMP R0;

      SLT R0.x, c[0].z, fragment.texcoord[0];
      CMP result.color, -R0.x, c[0].x, c[0].yyyx;

      END

  - Since no jumps, loops can be done by unrolling --- so don't depend
    on long running loops.
    Actually, simple shaders should probably avoid using loops, or use them
    only to iterate over a couple of light sources etc.

    See how glsl_test_loop.fs is compiled.

  - Interesting fragment shader instructions: KIL, so we can easily
    reject some pixels using any convoluted rule.

------------------------------------------------------------------------------
Higher-level shading languages:

They both merge fragment and vertex langs in one syntax (well, much like
assembly langs actually).

- NVidia Cg
  http://en.wikipedia.org/wiki/Cg_%28programming_language%29

  "C for graphics". High-level, also C-like. One advantage is that this is also
  usable for Direct X, assuming anyone cares.
  Although cgc can be used to compile GLSL to any other output language
  (even Direct X one, it seems, although I didn't test).

  This language is not built inside OpenGL. It's compiled into
  other languages --- like ARB_vertex_program, ARB_fragment_program,
  GLSL vertex/fragment. You can also compile *from* GLSL.
  You need NVidia Cg toolkit for this.

  - Shader may be compiled by a developer, when developing the game ---
    this means that you compile to one or several chosen hardware profiles
    that you expect that your game will run on. Like
      cgc cg_sample.cg -profile arbvp1 -o vertex_program_from_cg_sample.txt

  - Or it can be compiled at your game runtime. This means that you
    distribute your game with actual Cg shader sources, and compile
    on user machine. This means that you compile precisely for the
    hardware that will run the shader, so you may get more optimizations.
    The disadvantage is that you must program it, to link and call Cg runtime
    functions to compile from your program.

- blessed by ARB (standard in gl >= 2.0, gl >= 2.1 requires GLSL >= 1.20),
  compiled into ARB assembly langs above:
  GLSL or glslang

  Many many tutorials and references around the WWW,
  http://www.opengl.org/documentation/glsl/
  http://www.lighthouse3d.com/opengl/glsl/
  http://developer.3dlabs.com/documents/index.htm#Presentations
  http://www.opengl.org/wiki/index.php/Shading_languages:_GLSL
  http://folk.uio.no/johans/publications/Seland.2006.INF3320.pdf
  http://nehe.gamedev.net/data/articles/article.asp?article=21
  basically just
  http://www.google.com/search?q=glsl

  Show toon shading development (first on vs, _per_vertex version,
  than to per-fragment), Phong shading development.

  Although some parts of GLSL could be just compiled to ARB assembly langs
  above, but this may result in non-optimal results.
  Also, some concepts are simply not available in ARB assembly langs
  (ftransform with real correctness, matching fixed-function results?
  uniform, varying AFAIK also are not available in ARB progs ?).
  In practice, GLSL is compiled to some internal assembly for each vendor ---
  but usually very similar to ARB assembly, so it's definitely useful to know
  ARB assembly.

  See e.g. Mesa GLSL implementation notes:
  http://www.mesa3d.org/shading.html
  Demo
  - run with
      export LD_LIBRARY_PATH=/home/michalis/installed/mesa/7.0.2/lib/:"$LD_LIBRARY_PATH"
    shading_langs_demo, bump_mapping demo,
    view3dscene with shaders and bump mapping demos
  - glslcompiler --arb --linenumbers --vs vertshader.txt
    like
    ./glslcompiler -n --fs ~/sources/vrmlengine/trunk/kambi_vrml_game_engine/examples/glwindow/shading_langs/glsl_test_loop.fs
    makes true loop! BGNLOOP;, ENDLOOP;
    ./glslcompiler --fs ~/sources/vrmlengine/trunk/kambi_vrml_game_engine/examples/glwindow/shading_langs/glsl_test_if.fs
    makes true "if" !

  NVidia specific GLSL notes:
  http://developer.download.nvidia.com/opengl/glsl/glsl_release_notes.pdf
  E.g. half floats are avail in NVidia.
  EXT_Cg_shader for lazy Cg programmers owning NVidia.(TODO: test)

  GLSL is compiled on each GPU, possibly differently. This is it's strength
  vs using Cg to compile to standard ARB (vendor-specific optimizations
  may be applied), although this also means that you should test your program
  on various GPUs. With Cg->ARB, you at least know that ARB program is the same
  for each GPU.

  Note: be sure to test your GLSL not only on NVidia cards.
  NVidia GL tends to be forgiving to various malformed (but reasonable...)
  code, most probably because cgc also is forgiving and they use the same
  parser there, and cgc is forgiving because it was initially for Cg language.
  NVidia does eat every valid GLSL code, they just also often eat invalid GLSL
  code (with some Cg shortcuts / syntax).
  So if you develop on NVidia, be sure to test afterwards also on ATI/Mesa
  and fix eventual small things (like texture coords for texture2D functions
  should have ".st" qualifiers).
  * This is no longer true for newer nvidia drivers, it seems.
  They are more strict now with GLSL syntax. *

  There's GLSLvalidate
  http://developer.3dlabs.com/downloads/glslvalidate/index.htm
  (didn't test, looks like Windows-only, no ready compilation for Linux...).

  Some lang notes:
  - Communication between OpenGL and shaders:
    - uniform variables (glGetUniformLocation, glUniform)
      (settable per-object, that is only outside glBegin / glEnd)
    - attribute variables (glGetAttribLocation, glVertexAttrib)
      (settable per-vertex, that is possibly within glBegin / glEnd)

    Communication between vertex and fragment shaders:
    varying variables, read/write from vs, interpolated and readble in fs.
    That's the main reason why we have a "program" that links
    together many "shaders".

  - Just like in assembly langs, we have access to some OpenGL state as gl_Xxx
    variables. Some of it is read-only, some is write-only, some is both,
    different things available for vs and fs...
    Vertex shader must set gl_Position,
    fragment shader most usually will set at least gl_FragColor.
    See "quick ref guide" or specification, both from
    http://www.opengl.org/documentation/glsl/, for details.

  - Types: int, bool, float, vectors up to 4 of them (like ivec4, bvec4, vec4).
    Matrices of floats (various sizes, not necessarily square).

    Samplers: special type for textures, assigned from OpenGL API
    like a uniform int variable (assigned value indicated which texture
    unit to associate with this sampler, e.g. assign 1 to say that GL_TEXTURE1
    is used for this sampler). In shaders, this is an opaque type and can
    be used to query texture by functions like texture2D.

    No pointers, so this stays a true high-level lang.
    No strings also, not needed. (after all, there's no i/o obviously).

  - Vectors and matrices are primitive types.
    Well, actually matrices not so much (they are preety much just arrays of
    vectors, column major like for OpenGL API), but vectors still.
    E.g. vec4 <> float[4], vec4 is the primitive optimized type. You can access
    both arrays and vcetors using [index] syntax, but only vectors have
    special ".x" etc. syntax. E.g.

      float[4] v;
      v[0] = ...,
      v[1] = ...;

      vec4 v;
      v.xyz = ...;

    This reflects assembly shading langs underneath, and ability to "swizzle"
    vector components for free. E.g. in demo_models/shaders/glsl_some_deform.vs,
    initial code was
      v[0] += v[1];
      v[2] += v[1];
    but actually I can write this as faster
      v.xz += v.y;

  - int->float type promotion doesn't happen in all cases, e.g.
    vector * 4 incorrect (although tolerated by NVidia, watch out),
    vector * 4.0 correct.
  - functions, overloading, in, inout, out params.
  - OpenGL API for shaders (with OpenGL 2.0 standard functions,
    there are mostly equivalent functions for ARB extension):

    /* Preparation: */

    prog := glCreateProgram();

    shader := glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(shader, ... pass strings containing shader source ...);
    glCompileShader(shader);
    glAttachShader(program, shader);

    (same for GL_FRAGMENT_SHADER)

    glLinkProgram(program);


    /* Usage: */

    glUseProgram(program);
    ....
    glUseProgram(0); // back to fixed-function pipeline

    /* End: */

    /* glDetachShader, glDeleteShader(...) if you like to be clean */
    glDeleteProgram(prog);

    More details:
    - after compilation, you should check
      glGetShaderiv(shader, GL_COMPILE_STATUS, ...)
      eventual error message is in glGetShaderInfoLog

    - after linking, you should check
      glGetProgramiv(program, GL_LINK_STATUS, ...)
      eventual error message is in glGetProgramInfoLog

    See ../../glshaders.pas for full code of class to handle GLSL programs,
    using both std GL or ARB versions.

  See demo_models/shaders/ for demo how it's used from X3D (aka VRML 3.0).

  http://www.opengl.org/code/category/C20

------------------------------------------------------------------------------
More:

Demos:
- see demo_models demos:
  - shaders.x3dv
  - see specular_demo_phong_shading
  - see e.g. fountain VRML with toon shading applied

- see bump mapping in VRML engine, when bmGLSLNormal and bmGLSLParallax,
  implemented using GLSL shaders,
  see parallax bump mapping,
  examples in demo_models/bump_mapping
