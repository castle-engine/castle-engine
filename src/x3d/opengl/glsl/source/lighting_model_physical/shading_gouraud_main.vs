#ifdef LIT
  /* Two-sided lighting in Gouraud shading:
     flip the normal vector to correspond to the face side that we actually see.

     Note that we don't flip the castle_normal_eye (we only flip the
     normal_for_lighting), as castle_normal_eye may be useful also for other
     calculations, e.g. cubemap reflections, that don't want this flippping
     (testcase: demo-models/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv )

     This is commented out, because it's not perfect, and I'm not sure can
     we efficiently do artifact-free version of two-sided lighting.
     Reproduction of the problem:
     - demo-models/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv,
       look at the back side of the box.
     - demo-models/fog/fog_linear, rotate in Examine and look at the thin water
       edges.

     The problem: We base our flipping on castle_normal_eye,
     which may be a smoothed (per-vertex) normal vector.

     - We cannot calculate here reliably per-face vector (fragment shaders
       can do a trick with dFdx, see
       https://makc3d.wordpress.com/2015/09/17/alternative-to-gl_frontfacing/ ,
       but dFdx is only available in fragment shader).

     - Fully-correct solutions are inefficient:
       - To pass to vertex shader a face_normal in a special uniform
         means that we have to pass extra data, and also that we have to
         split vertexes to not share vertexes across faces.
       - Calculating light 2x times and then letting fragment shader to choose
         which side to show (this is what fixed-function does, I think).

     - If you're OK with being correct (not fast), you can use Phong shading
       where two-sided lighting works easily.
  */
  /* vec3 normal_for_lighting = (castle_normal_eye.z > 0.0 ? castle_normal_eye : -castle_normal_eye); */

  vec4 material_diffuse_alpha;

  #ifdef COLOR_PER_VERTEX
  material_diffuse_alpha = castle_ColorPerVertex;
  #else
  material_diffuse_alpha = castle_MaterialBaseAlpha;
  #endif

  castle_Color = vec4(castle_SceneColor, material_diffuse_alpha.a);

  /* PLUG: add_light (castle_Color, castle_vertex_eye, castle_normal_eye, material_diffuse_alpha) */

  /* Clamp sum of lights colors to be <= 1. See template_phong.fs for comments. */
  castle_Color.rgb = min(castle_Color.rgb, 1.0);
#else
  // Unlit case
  castle_Color = castle_UnlitColor;
  #ifdef COLOR_PER_VERTEX
  castle_Color *= castle_ColorPerVertex;
  #endif
#endif
