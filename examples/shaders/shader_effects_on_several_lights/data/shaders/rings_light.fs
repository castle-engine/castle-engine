/*
  Effect on a light source: the light shines in rings around the lamp,
  like light through a patterned shade.

  Every lamp in this example uses this same code, with a different
  number of rings (the uniform "ring_count", a field of each lamp's Effect).

  We need the world position of the lit point:
  castle_vertex_eye (position in eye space, available with Phong shading)
  times castle_CameraInverseMatrix (provided by the "EyeWorldSpace" shader library,
  enabled by an Effect on the lit scene, see gameviewmain.pas).
*/

uniform mat4 castle_CameraInverseMatrix;
varying vec4 castle_vertex_eye;

/* Lamp position in world space. */
uniform vec3 lamp_position;
/* Rings per unit of distance from the lamp. */
uniform float ring_count;

void PLUG_light_scale(inout float scale, const in vec3 normal_eye, const in vec3 light_dir)
{
  vec3 world_position = (castle_CameraInverseMatrix * castle_vertex_eye).xyz;
  float distance_xz = length(world_position.xz - lamp_position.xz);
  /* 1 in the middle of each ring, 0 between the rings */
  scale *= smoothstep(0.0, 0.6, 0.5 + 0.5 * cos(distance_xz * ring_count * 6.2831853));
}
