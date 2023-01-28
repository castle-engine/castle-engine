{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_phong/shading_phong.fs" }
'/*' + LineEnding +
'  Copyright 2010-2020 Michalis Kamburelis.' + LineEnding +
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
'  Calculate Phong lighting model, in Phong shading. */' + LineEnding +
'' + LineEnding +
'uniform vec4 castle_MaterialDiffuseAlpha;' + LineEnding +
'uniform vec3 castle_MaterialAmbient;' + LineEnding +
'uniform vec3 castle_MaterialSpecular;' + LineEnding +
'uniform float castle_MaterialShininess;' + LineEnding +
'uniform vec3 castle_MaterialEmissive;' + LineEnding +
'uniform vec3 castle_GlobalAmbient;' + LineEnding +
'' + LineEnding +
'uniform vec4 castle_UnlitColor;' + LineEnding +
'' + LineEnding +
'/* Material ambient color * global (light model) ambient. */' + LineEnding +
'vec3 get_ambient_color()' + LineEnding +
'{' + LineEnding +
'  vec3 ambient = castle_MaterialAmbient;' + LineEnding +
'  /* PLUG: material_light_ambient (ambient) */' + LineEnding +
'  return ambient * castle_GlobalAmbient;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'#ifdef LIT' + LineEnding +
'  MaterialInfo material_info;' + LineEnding +
'' + LineEnding +
'  material_info.ambient = castle_MaterialAmbient;' + LineEnding +
'  /* PLUG: material_ambient (material_info.ambient) */' + LineEnding +
'  material_info.specular = castle_MaterialSpecular;' + LineEnding +
'  /* PLUG: material_specular (material_info.specular) */' + LineEnding +
'  material_info.shininess = castle_MaterialShininess;' + LineEnding +
'  /* PLUG: material_shininess (material_info.shininess) */' + LineEnding +
'' + LineEnding +
'  material_info.diffuse_alpha = castle_apply_color_per_vertex(castle_MaterialDiffuseAlpha);' + LineEnding +
'' + LineEnding +
'  main_texture_apply(material_info.diffuse_alpha, normal_eye);' + LineEnding +
'' + LineEnding +
'  result = vec4(get_ambient_color(), material_info.diffuse_alpha.a);' + LineEnding +
'' + LineEnding +
'  /* PLUG: add_light (result, vertex_eye, normal_eye, material_info) */' + LineEnding +
'' + LineEnding +
'  /* PLUG: material_occlusion (result) */' + LineEnding +
'' + LineEnding +
'  vec3 emissive = castle_MaterialEmissive;' + LineEnding +
'  /* PLUG: material_emissive (emissive) */' + LineEnding +
'  result.rgb += emissive;' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.' + LineEnding +
'     This isn''t really mandatory, but scenes with many lights could easily' + LineEnding +
'     have colors > 1 and then the textures will look "burned out".' + LineEnding +
'     Of course, for future HDR rendering we will turn this off. */' + LineEnding +
'  result.rgb = min(result.rgb, 1.0);' + LineEnding +
'#else' + LineEnding +
'  /* Unlit case.' + LineEnding +
'     This is only an optimization of the "lit" case' + LineEnding +
'     (e.g. no need to do per-light-source processing).' + LineEnding +
'' + LineEnding +
'     Diffuse color is zero in this case, and we know that it''s not set to non-zero' + LineEnding +
'     by castle_ColorPerVertexFragment (since that would make the shape "lit").' + LineEnding +
'     So we don''t use castle_apply_color_per_vertex,' + LineEnding +
'     and we don''t apply RGB of main_texture_apply (as it would be multiplied by zero).' + LineEnding +
'  */' + LineEnding +
'  result = castle_UnlitColor;' + LineEnding +
'' + LineEnding +
'  // Apply emissiveTexture, if exists' + LineEnding +
'  /* PLUG: material_emissive (result.rgb) */' + LineEnding +
'' + LineEnding +
'  // Apply alpha from the diffuseTexture (or Appearance.texture).' + LineEnding +
'  // We need to call main_texture_apply for this.' + LineEnding +
'  vec4 color_alpha = result;' + LineEnding +
'  main_texture_apply(color_alpha, normal_eye);' + LineEnding +
'  result.a = color_alpha.a;' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
''
