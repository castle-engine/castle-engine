attribute vec2 vertex;
attribute vec2 tex_coord;

/* Note that passing viewport_size (just 2 floats) removes the need to pass
   a projection_matrix (16 floats).

   We only pass a size of the context (width, height),
   since we know that the viewport for 2D drawing is always the full context,
   with coordinates matching pixels.
   (TCastleWindowCustom.EventRender even sets such projection matrix,
   although we don't depend on it for TGLImageCore rendering after all.)
   This allows us to simplify
     gl_Position = projection_matrix * vec4(vertex, 0.0, 1.0);
   because we know that the projection_matrix actually corresponds to
     OrthoProjection(0, viewport_size.x, 0, viewport_size.y);
*/
uniform vec2 viewport_size;
varying vec2 tex_coord_frag;

#ifdef CLIP_LINE
varying vec2 frag_coord;
#endif

/* Simple GLSL shader to apply 2D texture.
   Must be suitable also for GLES20, so don't use any deprecated gl_Xxx
   variables. */

void main(void)
{
  gl_Position = vec4(vertex * 2.0 / viewport_size - vec2(1.0), 0.0, 1.0);
  #ifdef CLIP_LINE
  frag_coord = vertex;
  #endif
  tex_coord_frag = tex_coord;
}
