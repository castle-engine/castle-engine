/*
  Effect on a light source: multiply the light by an image projected
  from the light downwards, like a projector, or sunlight through a window.

  The images are textures in the Effect's fields "window_texture" and
  "blinds_texture", the field "image_index" chooses one.
  The renderer binds it like a texture in an Effect on a material
  (also when the light is in another scene than the lit shapes, with
  TCastleScene.CastGlobalLights).

  We need the world position of the lit point:
  castle_vertex_eye (position in eye space, available with Phong shading)
  times castle_CameraInverseMatrix (provided by the "EyeWorldSpace" shader library,
  enabled by an Effect in the lit scene, see lit_objects.x3dv).
*/

uniform mat4 castle_CameraInverseMatrix;
varying vec4 castle_vertex_eye;

uniform sampler2D window_texture;
uniform sampler2D blinds_texture;
uniform int image_index;
/* Light position in world space. */
uniform vec3 light_position;
/* Half size of the projected image at distance 1 below the light. */
uniform float projection_size;

void PLUG_light_scale(inout float scale, const in vec3 normal_eye, const in vec3 light_dir)
{
  vec3 world_position = (castle_CameraInverseMatrix * castle_vertex_eye).xyz;
  vec3 to_point = world_position - light_position;

  /* Distance below the light. Nothing above it is lit. */
  float depth = -to_point.y;
  if (depth <= 0.0) {
    scale = 0.0;
    return;
  }

  /* Perspective projection, from the light down the -Y axis. */
  vec2 uv = to_point.xz / (depth * projection_size) * 0.5 + 0.5;
  if (uv.x < 0.0 || uv.x > 1.0 || uv.y < 0.0 || uv.y > 1.0) {
    scale = 0.0;
    return;
  }

  if (image_index == 0)
    scale *= texture2D(window_texture, uv).r;
  else
    scale *= texture2D(blinds_texture, uv).r;
}
