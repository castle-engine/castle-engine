/* Parallax bump mapping shader effect.
   This is added right after bump_mapping.fs.
   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.
*/

/* Make castleTranspose() available.

   For desktop OpenGL, we could use "#version 120" and get transpose().

   But on OpenGLES, it seems easiest to just implement your own transpose().
   Or we could require version 3.00,
   https://www.khronos.org/registry/OpenGL-Refpages/es3.1/html/transpose.xhtml ,
   but this also requires other GLSL changes,
   and it seems that iOS would be troublesome anyway:
   https://stackoverflow.com/questions/18034677/transpose-a-mat4-in-opengl-es-2-0-glsl
*/
mat3 castleTranspose(const in mat3 m)
{
  return mat3(
    vec3(m[0].x, m[1].x, m[2].x),
    vec3(m[0].y, m[1].y, m[2].y),
    vec3(m[0].z, m[1].z, m[2].z)
  );
}

// avoid redeclaring variables when no "separate compilation units" available (OpenGLES)
#ifndef GL_ES
uniform mat4 castle_ModelViewMatrix;
#endif

varying vec3 castle_vertex_to_eye_in_tangent_space;

void PLUG_bump_mapping_tangent_space_calculations(
  const in vec4 vertex_eye, const in mat3 tangent_to_object_space)
{
  mat3 object_to_tangent_space = castleTranspose(tangent_to_object_space);
  mat3 eye_to_object_space = mat3(
    castle_ModelViewMatrix[0][0], castle_ModelViewMatrix[1][0], castle_ModelViewMatrix[2][0],
    castle_ModelViewMatrix[0][1], castle_ModelViewMatrix[1][1], castle_ModelViewMatrix[2][1],
    castle_ModelViewMatrix[0][2], castle_ModelViewMatrix[1][2], castle_ModelViewMatrix[2][2]);
  mat3 eye_to_tangent_space = object_to_tangent_space * eye_to_object_space;
  /* Theoretically faster implementation below, not fully correct ---
     assume that transpose is enough to invert this matrix. Tests proved:
     - results seem the same
     - but it's not really faster. */
  // mat3 eye_to_tangent_space = castleTranspose(castle_tangent_to_eye_space);
  castle_vertex_to_eye_in_tangent_space = normalize(eye_to_tangent_space * (-vec3(vertex_eye)) );

  /* PLUG: bump_mapping_parallax_tangent_space_calculations (vertex_eye, eye_to_tangent_space) */
}
