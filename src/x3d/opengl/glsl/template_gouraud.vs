/* Gouraud shading GLSL vertex shader. */

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
uniform mat3 castle_NormalMatrix;
attribute vec4 castle_Vertex;
attribute vec3 castle_Normal;

/* PLUG-DECLARATIONS */

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;
varying vec4 castle_Color;

uniform float castle_MaterialDiffuseAlpha;
uniform float castle_MaterialShininess;
/* Color summed with all the lights.
   Like gl_Front/BackLightModelProduct.sceneColor:
   material emissive color + material ambient color * global (light model) ambient.
*/
uniform vec3 castle_SceneColor;
uniform vec4 castle_UnlitColor;

#ifdef COLOR_PER_VERTEX
attribute vec4 castle_ColorPerVertex;
#endif

/* Include fragment shader utilities used by both Gouraud and Phong shading. */
/* CASTLE-COMMON-CODE */

void main(void)
{
  vec4 vertex_object = castle_Vertex;
  vec3 normal_object = castle_Normal;
  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  #ifdef CASTLE_BUGGY_GLSL_READ_VARYING
  /* use local variables, instead of reading + writing to varying variables,
     in this case */
  vec4 temp_castle_vertex_eye;
  vec3 temp_castle_normal_eye;
  vec4 temp_castle_Color;
  #define castle_vertex_eye temp_castle_vertex_eye
  #define castle_normal_eye temp_castle_normal_eye
  #define castle_Color      temp_castle_Color
  #endif

  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;
  castle_normal_eye = normalize(castle_NormalMatrix * normal_object);

  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */

#ifdef LIT
  castle_Color = vec4(castle_SceneColor, 1.0);

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

  #ifdef COLOR_PER_VERTEX
    /* PLUG: add_light_contribution (castle_Color, castle_vertex_eye, castle_normal_eye, castle_MaterialShininess, castle_ColorPerVertex) */
    castle_Color.a = castle_ColorPerVertex.a;
  #else
    /* PLUG: add_light_contribution (castle_Color, castle_vertex_eye, castle_normal_eye, castle_MaterialShininess, vec4(0.0)) */
    castle_Color.a = castle_MaterialDiffuseAlpha;
  #endif

  /* Clamp sum of lights colors to be <= 1. See template_phong.fs for comments. */
  castle_Color.rgb = min(castle_Color.rgb, 1.0);
#else
  castle_Color = castle_UnlitColor
#ifdef COLOR_PER_VERTEX
    /* Apply COLOR_PER_VERTEX, when unlit.
       (When lit, then the analogous multiplication is done
        inside template_light.glsl) */
    * castle_ColorPerVertex
#endif
  ;
#endif

  /* PLUG: lighting_apply (castle_Color, castle_vertex_eye, castle_normal_eye) */

  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;

  #ifdef CASTLE_BUGGY_GLSL_READ_VARYING
  #undef castle_vertex_eye
  #undef castle_normal_eye
  #undef castle_Color
  castle_vertex_eye = temp_castle_vertex_eye;
  castle_normal_eye = temp_castle_normal_eye;
  castle_Color      = temp_castle_Color;
  #endif
}
