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
