{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/terrain.fs" }
'/* OpenGL shader effect (used to enhance the Castle Game Engine shaders,' + LineEnding +
'   see https://castle-engine.io/compositing_shaders.php ),' + LineEnding +
'   applied over terrain.' + LineEnding +
'' + LineEnding +
'   This adjusts terrain color, mixing textures, based on current height. */' + LineEnding +
'' + LineEnding +
'uniform sampler2D tex_1;' + LineEnding +
'uniform sampler2D tex_2;' + LineEnding +
'uniform sampler2D tex_3;' + LineEnding +
'uniform sampler2D tex_4;' + LineEnding +
'' + LineEnding +
'uniform vec3 color_1;' + LineEnding +
'uniform vec3 color_2;' + LineEnding +
'uniform vec3 color_3;' + LineEnding +
'uniform vec3 color_4;' + LineEnding +
'' + LineEnding +
'// These values are packed in vec4, one float per layer' + LineEnding +
'uniform vec4 uv_scale;' + LineEnding +
'uniform vec4 metallic;' + LineEnding +
'uniform vec4 roughness;' + LineEnding +
'' + LineEnding +
'uniform float height_1;' + LineEnding +
'uniform float height_2;' + LineEnding +
'' + LineEnding +
'uniform float layers_influence;' + LineEnding +
'uniform float steep_emphasize;' + LineEnding +
'' + LineEnding +
'varying vec3 terrain_position;' + LineEnding +
'varying vec3 terrain_normal;' + LineEnding +
'' + LineEnding +
'// avoid redeclaring when no "separate compilation units" available (OpenGLES)' + LineEnding +
'#ifndef GL_ES' + LineEnding +
'vec4 castle_texture_color_to_linear(const in vec4 srgbIn);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void PLUG_main_texture_apply(inout vec4 fragment_color, const in vec3 normal)' + LineEnding +
'{' + LineEnding +
'  float h = terrain_position.y;' + LineEnding +
'' + LineEnding +
'  /* We flip terrain_position.z, to map texture more naturally, when viewed from above.' + LineEnding +
'     This is consistent with calculating TexCoord for TCastleTerrainData.Height.' + LineEnding +
'     We just flip the sign, because the terrain textures always have repeat = true,' + LineEnding +
'     so there''s no need to shift the texture in any way.' + LineEnding +
'  */' + LineEnding +
'  vec2 uv = vec2(terrain_position.x, -terrain_position.z);' + LineEnding +
'' + LineEnding +
'  /* What does this mean?' + LineEnding +
'     normal_slope (normal.y)' + LineEnding +
'     = 0 means a vertical face' + LineEnding +
'     = 1 means a horizontal face' + LineEnding +
'  */' + LineEnding +
'  float normal_slope = normalize(terrain_normal).y;' + LineEnding +
'' + LineEnding +
'  vec3 c1 = color_1 * castle_texture_color_to_linear(texture2D(tex_1, uv * uv_scale.x)).rgb;' + LineEnding +
'  vec3 c2 = color_2 * castle_texture_color_to_linear(texture2D(tex_2, uv * uv_scale.y)).rgb;' + LineEnding +
'  vec3 c3 = color_3 * castle_texture_color_to_linear(texture2D(tex_3, uv * uv_scale.z)).rgb;' + LineEnding +
'  vec3 c4 = color_4 * castle_texture_color_to_linear(texture2D(tex_4, uv * uv_scale.w)).rgb;' + LineEnding +
'' + LineEnding +
'  float height_mix = smoothstep(height_1, height_2, h);' + LineEnding +
'  vec3 flat_color = mix(c1, c3, height_mix);' + LineEnding +
'  vec3 steep_color = mix(c2, c4, height_mix);' + LineEnding +
'  vec3 modified_color = mix(steep_color, flat_color, pow(normal_slope, steep_emphasize));' + LineEnding +
'' + LineEnding +
'  fragment_color.rgb = mix(fragment_color.rgb, modified_color, layers_influence);' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void PLUG_material_metallic_roughness(inout float metallic_final, inout float roughness_final)' + LineEnding +
'{' + LineEnding +
'  float h = terrain_position.y;' + LineEnding +
'  float normal_slope = normalize(terrain_normal).y;' + LineEnding +
'  float height_mix = smoothstep(height_1, height_2, h);' + LineEnding +
'' + LineEnding +
'  float flat_metallic = mix(metallic.x, metallic.z, height_mix);' + LineEnding +
'  float steep_metallic = mix(metallic.y, metallic.w, height_mix);' + LineEnding +
'  float modified_metallic = mix(steep_metallic, flat_metallic, pow(normal_slope, steep_emphasize));' + LineEnding +
'  metallic_final = mix(metallic_final, modified_metallic, layers_influence);' + LineEnding +
'' + LineEnding +
'  float flat_roughness = mix(roughness.x, roughness.z, height_mix);' + LineEnding +
'  float steep_roughness = mix(roughness.y, roughness.w, height_mix);' + LineEnding +
'  float modified_roughness = mix(steep_roughness, flat_roughness, pow(normal_slope, steep_emphasize));' + LineEnding +
'  roughness_final = mix(roughness_final, modified_roughness, layers_influence);' + LineEnding +
'}' + LineEnding +
''
