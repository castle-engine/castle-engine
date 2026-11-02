/*
  Effect on a light source: the light shines in rings around the lamp,
  like light through a patterned lampshade.

  PLUG_light_scale is called for every point lit by the light.
  light_dir is the direction from the lit point to the light, in eye space.
*/

/* Rings for each unit of "spread" (see below).
   A field of the Effect node, set from Pascal. */
uniform float ring_count;

/* Provided by the EyeWorldSpace shader library. */
vec3 direction_eye_to_world_space(vec3 direction_eye);

void PLUG_light_scale(inout float scale, const in vec3 normal_eye, const in vec3 light_dir)
{
  vec3 to_light = direction_eye_to_world_space(light_dir);
  /* Horizontal distance from the point right below the lamp,
     divided by the lamp height above the lit point.
     Points with the same spread make a circle around the lamp. */
  float spread = length(to_light.xz) / max(to_light.y, 0.001);
  /* 1 in the middle of each ring, 0 between the rings */
  scale *= smoothstep(0.0, 0.6, 0.5 + 0.5 * cos(spread * ring_count * 6.2831853));
}
