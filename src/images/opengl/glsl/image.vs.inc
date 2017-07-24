{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "image.vs" }
'attribute vec2 vertex;' + LineEnding +
'attribute vec2 tex_coord;' + LineEnding +
'' + LineEnding +
'/* Note that passing viewport_size (just 2 floats) removes the need to pass' + LineEnding +
'   a projection_matrix (16 floats).' + LineEnding +
'' + LineEnding +
'   We only pass a size of the context (width, height),' + LineEnding +
'   since we know that the viewport for 2D drawing is always the full context,' + LineEnding +
'   with coordinates matching pixels.' + LineEnding +
'   (TCastleWindowCustom.EventRender even sets such projection matrix,' + LineEnding +
'   although we don''t depend on it for TGLImageCore rendering after all.)' + LineEnding +
'   This allows us to simplify' + LineEnding +
'     gl_Position = projection_matrix * vec4(vertex, 0.0, 1.0);' + LineEnding +
'   because we know that the projection_matrix actually corresponds to' + LineEnding +
'     OrthoProjection(0, viewport_size.x, 0, viewport_size.y);' + LineEnding +
'*/' + LineEnding +
'uniform vec2 viewport_size;' + LineEnding +
'varying vec2 tex_coord_frag;' + LineEnding +
'' + LineEnding +
'#ifdef CLIP_LINE' + LineEnding +
'varying vec2 frag_coord;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'/* Simple GLSL shader to apply 2D texture.' + LineEnding +
'   Must be suitable also for GLES20, so don''t use any deprecated gl_Xxx' + LineEnding +
'   variables. */' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_Position = vec4(vertex * 2.0 / viewport_size - vec2(1.0), 0.0, 1.0);' + LineEnding +
'  #ifdef CLIP_LINE' + LineEnding +
'  frag_coord = vertex;' + LineEnding +
'  #endif' + LineEnding +
'  tex_coord_frag = tex_coord;' + LineEnding +
'}' + LineEnding +
''
