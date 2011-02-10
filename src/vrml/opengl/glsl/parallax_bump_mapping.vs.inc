{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "parallax_bump_mapping.vs" }
'/*' + LineEnding + 
'  Copyright 2007-2010 Michalis Kamburelis.' + LineEnding + 
'' + LineEnding + 
'  This file is part of "Kambi VRML game engine".' + LineEnding + 
'' + LineEnding + 
'  "Kambi VRML game engine" is free software; see the file COPYING.txt,' + LineEnding + 
'  included in this distribution, for details about the copyright.' + LineEnding + 
'' + LineEnding + 
'  "Kambi VRML game engine" is distributed in the hope that it will be useful,' + LineEnding + 
'  but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding + 
'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'/* GLSL vertex shader to do bump mapping.' + LineEnding + 
'   Version with parallax mapping.' + LineEnding + 
'' + LineEnding + 
'   This is converted to glsl_parallax_bump_mapping.vs.inc, and is them compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'uniform vec3 light_position_world_space;' + LineEnding + 
'uniform mat4 world_space_to_object;' + LineEnding + 
'' + LineEnding + 
'varying vec3 point_to_eye_in_tangent_space;' + LineEnding + 
'' + LineEnding + 
'attribute mat3 object_space_to_tangent;' + LineEnding + 
'attribute vec2 tex_coord;' + LineEnding + 
'' + LineEnding + 
'varying vec3 light_dir_tangent;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  gl_TexCoord[0] = gl_TextureMatrix[0] * vec4(tex_coord, 0.0, 1.0);' + LineEnding + 
'' + LineEnding + 
'  /* Calculate light_dir_tangent, which is crucial for bump mapping. */' + LineEnding + 
'  vec3 light_position_object_space =' + LineEnding + 
'    vec3(world_space_to_object * vec4(light_position_world_space, 1));' + LineEnding + 
'  vec3 light_dir_object_space = light_position_object_space - vec3(gl_Vertex);' + LineEnding + 
'  light_dir_tangent = object_space_to_tangent * light_dir_object_space;' + LineEnding + 
'' + LineEnding + 
'  /* Calculate point_to_eye_in_tangent_space */' + LineEnding + 
'  point_to_eye_in_tangent_space =' + LineEnding + 
'    /* This is how camera is translated from eye space to object space */' + LineEnding + 
'    vec3(gl_ModelViewMatrixInverse[3]) - vec3(gl_Vertex);' + LineEnding + 
'  point_to_eye_in_tangent_space = object_space_to_tangent *' + LineEnding + 
'    point_to_eye_in_tangent_space;' + LineEnding + 
'' + LineEnding + 
'  gl_Position = ftransform();' + LineEnding + 
'}' + LineEnding + 
''
