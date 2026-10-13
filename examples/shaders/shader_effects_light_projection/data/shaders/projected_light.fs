/*
  Effect on a light source: multiply the light by an image projected
  from the light downwards, like a projector, or sunlight through
  a (stained glass) window.

  The images are textures in the Effect's fields "stained_glass_texture",
  "window_texture" and "blinds_texture", the field "image_index" chooses one.
  The renderer binds them like textures in an Effect on a material
  (also when the light is in another scene than the lit shapes, with
  TCastleScene.CastGlobalLights).

  We use PLUG_light_color_scale, that multiplies the light's color
  by an RGB value, so the image colors the light.
  (PLUG_light_scale multiplies the light by a single number,
  which is enough for grayscale images.)
  Both start at 1, and are multiplied with the other light factors
  (spot light cone, attenuation, radius). Several effects on the same light
  combine when each one multiplies (*=).

  We need the world position of the lit point:
  castle_vertex_eye (position in eye space, available with Phong shading)
  times castle_CameraInverseMatrix (provided by the "EyeWorldSpace" shader library,
  enabled by an Effect in the lit scene, see lit_objects.x3dv).
*/

uniform mat4 castle_CameraInverseMatrix;
varying vec4 castle_vertex_eye;

uniform sampler2D stained_glass_texture;
uniform sampler2D window_texture;
uniform sampler2D blinds_texture;
uniform int image_index;
/* Light position in world space. */
uniform vec3 light_position;
/* Half size of the projected image at distance 1 below the light. */
uniform float projection_size;

/* normal_eye: the surface normal, in eye space.
   light_dir: the direction from the lit point to the light, in eye space, normalized. */
void PLUG_light_color_scale(inout vec3 color_scale, const in vec3 normal_eye, const in vec3 light_dir)
{
  vec3 world_position = (castle_CameraInverseMatrix * castle_vertex_eye).xyz;
  vec3 to_point = world_position - light_position;

  /* Distance below the light. Nothing above it is lit. */
  float depth = -to_point.y;
  if (depth <= 0.0) {
    color_scale = vec3(0.0);
    return;
  }

  /* Perspective projection, from the light down the -Y axis. */
  vec2 uv = to_point.xz / (depth * projection_size) * 0.5 + 0.5;
  if (uv.x < 0.0 || uv.x > 1.0 || uv.y < 0.0 || uv.y > 1.0) {
    color_scale = vec3(0.0);
    return;
  }

  if (image_index == 0)
    color_scale *= texture2D(stained_glass_texture, uv).rgb;
  else
  if (image_index == 1)
    color_scale *= texture2D(window_texture, uv).rgb;
  else
    color_scale *= texture2D(blinds_texture, uv).rgb;
}
