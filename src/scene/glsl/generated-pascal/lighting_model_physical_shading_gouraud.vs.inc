{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/shading_gouraud.vs" }
'/*' + LineEnding +
'  Copyright 2020-2020 Michalis Kamburelis and glTF-Sample-Viewer authors.' + LineEnding +
'' + LineEnding +
'  This file is part of "Castle Game Engine".' + LineEnding +
'' + LineEnding +
'  "Castle Game Engine" is free software; see the file COPYING.txt,' + LineEnding +
'  included in this distribution, for details about the copyright.' + LineEnding +
'' + LineEnding +
'  "Castle Game Engine" is distributed in the hope that it will be useful,' + LineEnding +
'  but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding +
'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.' + LineEnding +
'' + LineEnding +
'  ----------------------------------------------------------------------------' + LineEnding +
'' + LineEnding +
'  Calculate Physical (PBR) lighting model, in Gouraud shading. */' + LineEnding +
'' + LineEnding +
'uniform vec4 castle_MaterialBaseAlpha;' + LineEnding +
'uniform vec3 castle_EmissiveColor;' + LineEnding +
'' + LineEnding +
'void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'  vec4 material_base_alpha;' + LineEnding +
'' + LineEnding +
'  material_base_alpha = castle_apply_color_per_vertex(castle_MaterialBaseAlpha);' + LineEnding +
'' + LineEnding +
'  result = vec4(castle_EmissiveColor, material_base_alpha.a);' + LineEnding +
'' + LineEnding +
'  "This GLSL code should never be used, since PhysicalMaterial requires Phong shading.' + LineEnding +
'  Uncommented, to break compilation in case this is accidentally used."' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. See template_phong.fs for comments. */' + LineEnding +
'  result.rgb = min(result.rgb, 1.0);' + LineEnding +
'}' + LineEnding +
''
