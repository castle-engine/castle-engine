/* Fragment shader utilities used by both Gouraud and Phong shading.

   Used by ../castlerendererinternalshader.pas to construct the final shader.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

/* Wrapper for calling PLUG texture_coord_shift */
vec2 texture_coord_shifted(in vec2 tex_coord)
{
  /* PLUG: texture_coord_shift (tex_coord) */
  return tex_coord;
}

// Declare functions defined in tone_mapping.fs

/* sRGB to linear conversion.
   Leaves alpha unmodified.

   Input is returned unchanged if CASTLE_GAMMA_CORRECTION not defined.
*/
vec4 castle_texture_color_to_linear(const in vec4 srgbIn);

/* sRGB to linear conversion.
   Input is returned unchanged if CASTLE_GAMMA_CORRECTION not defined.
*/
vec3 castle_texture_color_to_linear(const in vec3 srgbIn);

/* Linear to sRGB conversion and applying tone mapping.
   Input is returned unchanged if
   neither CASTLE_GAMMA_CORRECTION nor CASTLE_TONE_MAPPING are defined.
*/
vec3 castle_linear_to_screen(const in vec3 color);
