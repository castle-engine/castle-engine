{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_phong/shading_gouraud.vs" }
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
'  Calculate Phong lighting model, in Gouraud shading. */' + LineEnding +
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
'  return ambient * castle_GlobalAmbient;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'#ifdef LIT' + LineEnding +
'  /* Two-sided lighting in Gouraud shading:' + LineEnding +
'     flip the normal vector to correspond to the face side that we actually see.' + LineEnding +
'' + LineEnding +
'     Note that we don''t flip the normal_eye (we only flip the' + LineEnding +
'     normal_for_lighting), as normal_eye may be useful also for other' + LineEnding +
'     calculations, e.g. cubemap reflections, that don''t want this flippping' + LineEnding +
'     (testcase: demo-models/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv )' + LineEnding +
'' + LineEnding +
'     This is commented out, because it''s not perfect, and I''m not sure can' + LineEnding +
'     we efficiently do artifact-free version of two-sided lighting.' + LineEnding +
'     Reproduction of the problem:' + LineEnding +
'     - demo-models/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv,' + LineEnding +
'       look at the back side of the box.' + LineEnding +
'     - demo-models/fog/fog_linear, rotate in Examine and look at the thin water' + LineEnding +
'       edges.' + LineEnding +
'' + LineEnding +
'     The problem: We base our flipping on normal_eye,' + LineEnding +
'     which may be a smoothed (per-vertex) normal vector.' + LineEnding +
'' + LineEnding +
'     - We cannot calculate here reliably per-face vector (fragment shaders' + LineEnding +
'       can do a trick with dFdx, see' + LineEnding +
'       https://makc3d.wordpress.com/2015/09/17/alternative-to-gl_frontfacing/ ,' + LineEnding +
'       but dFdx is only available in fragment shader).' + LineEnding +
'' + LineEnding +
'     - Fully-correct solutions are inefficient:' + LineEnding +
'       - To pass to vertex shader a face_normal in a special uniform' + LineEnding +
'         means that we have to pass extra data, and also that we have to' + LineEnding +
'         split vertexes to not share vertexes across faces.' + LineEnding +
'       - Calculating light 2x times and then letting fragment shader to choose' + LineEnding +
'         which side to show (this is what fixed-function does, I think).' + LineEnding +
'' + LineEnding +
'     - If you''re OK with being correct (not fast), you can use Phong shading' + LineEnding +
'       where two-sided lighting works easily.' + LineEnding +
'  */' + LineEnding +
'  /* vec3 normal_for_lighting = (normal_eye.z > 0.0 ? normal_eye : -normal_eye); */' + LineEnding +
'' + LineEnding +
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
'  result = vec4(get_ambient_color(), material_info.diffuse_alpha.a);' + LineEnding +
'' + LineEnding +
'  /* PLUG: add_light (result, vertex_eye, normal_eye, material_info) */' + LineEnding +
'' + LineEnding +
'  vec3 emissive = castle_MaterialEmissive;' + LineEnding +
'  result.rgb += emissive;' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. See template_phong.fs for comments. */' + LineEnding +
'  result.rgb = min(result.rgb, 1.0);' + LineEnding +
'#else' + LineEnding +
'  /* Unlit case.' + LineEnding +
'     See shading_phong.fs for comments when this is used.' + LineEnding +
'  */' + LineEnding +
'  result = castle_UnlitColor;' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
''
