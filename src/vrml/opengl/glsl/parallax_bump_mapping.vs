/*
  Copyright 2007-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/

/* GLSL vertex shader to do bump mapping.
   Version with parallax mapping.

   This is converted to glsl_parallax_bump_mapping.vs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

uniform vec3 light_position_world_space;
uniform mat4 world_space_to_object;

varying vec3 point_to_eye_in_tangent_space;

attribute mat3 object_space_to_tangent;
attribute vec2 tex_coord;

varying vec3 light_dir_tangent;

void main(void)
{
  gl_TexCoord[0] = gl_TextureMatrix[0] * vec4(tex_coord, 0.0, 1.0);

  /* Calculate light_dir_tangent, which is crucial for bump mapping. */
  vec3 light_position_object_space =
    vec3(world_space_to_object * vec4(light_position_world_space, 1));
  vec3 light_dir_object_space = light_position_object_space - vec3(gl_Vertex);
  light_dir_tangent = object_space_to_tangent * light_dir_object_space;

  /* Calculate point_to_eye_in_tangent_space */
  point_to_eye_in_tangent_space =
    /* This is how camera is translated from eye space to object space */
    vec3(gl_ModelViewMatrixInverse[3]) - vec3(gl_Vertex);
  point_to_eye_in_tangent_space = object_space_to_tangent *
    point_to_eye_in_tangent_space;

  gl_Position = ftransform();
}
