/* Generic GLSL vertex shader, used on OpenGL ES. */

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
uniform mat3 castle_NormalMatrix;
attribute vec4 castle_Vertex;
attribute vec3 castle_Normal;

/* PLUG-DECLARATIONS */

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;
varying vec4 castle_Color;

/* Light source position in eye coordinates. */
const vec3 castle_LightSource0Position = vec3(0.0, 0.0, 0.0);
uniform float castle_LightSource0SpotExponent;
/* Multiplied colors of light source and material. */
uniform vec4 castle_SideLightProduct0Ambient;
uniform vec4 castle_SideLightProduct0Diffuse;
uniform vec4 castle_SideLightProduct0Specular;

void main(void)
{
  vec4 vertex_object = castle_Vertex;
  vec3 normal_object = castle_Normal;
  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;
  castle_normal_eye = normalize(castle_NormalMatrix * normal_object);

  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */

  /* Simple lighting calculation for a single positional light source. */
  vec3 light_dir = normalize(castle_LightSource0Position - vec3(castle_vertex_eye));
  vec3 reflect_dir = reflect(-light_dir, castle_normal_eye);
  vec3 view_dir = normalize(-vec3(castle_vertex_eye));
  float diffuse = max(dot(light_dir, castle_normal_eye), 0.0);
  float spec = 0.0;
  if (diffuse > 0.0) {
      spec = max(dot(reflect_dir, view_dir), 0.0);
      spec = pow(spec, castle_LightSource0SpotExponent);
  }
  castle_Color = castle_SideLightProduct0Ambient +
    castle_SideLightProduct0Diffuse * diffuse +
    castle_SideLightProduct0Specular * spec;

  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;
}
