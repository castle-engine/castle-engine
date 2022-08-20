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
'' + LineEnding +
'uniform vec3 color_1;' + LineEnding +
'uniform vec3 color_2;' + LineEnding +
'uniform vec3 color_3;' + LineEnding +
'' + LineEnding +
'uniform float uv_scale_1;' + LineEnding +
'uniform float uv_scale_2;' + LineEnding +
'uniform float uv_scale_3;' + LineEnding +
'' + LineEnding +
'uniform float darkness_amount;' + LineEnding +
'uniform float darkness_intensity;' + LineEnding +
'uniform float texture_mix;' + LineEnding +
'' + LineEnding +
'uniform float h0; // below is tex/color_1' + LineEnding +
'uniform float h1; // below is tex/color_2 mixed with tex/color_1' + LineEnding +
'uniform float h2; // below is tex/color_2' + LineEnding +
'uniform float h3; // below is tex/color_3 mixed with tex/color_2' + LineEnding +
'                  // above is tex/color_3' + LineEnding +
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
'  /* We flip terrain_position.z, to map texture more naturally, when viewed from above.' + LineEnding +
'     This is consistent with calculating TexCoord for TCastleTerrainData.Height.' + LineEnding +
'     We just flip the sign, because the terrain textures always have repeat = true,' + LineEnding +
'     so there''s no need to shift the texture in any way.' + LineEnding +
'  */' + LineEnding +
'  vec2 uv = vec2(terrain_position.x, -terrain_position.z);' + LineEnding +
'  float normal_slope = normalize(terrain_normal).y;' + LineEnding +
'' + LineEnding +
'  vec3 base_color;' + LineEnding +
'  float hhalf = (h1 + h2) * 0.5;' + LineEnding +
'  if (h < hhalf) {' + LineEnding +
'    float mixfactor = smoothstep(h0, h1, h);' + LineEnding +
'    base_color = mix(' + LineEnding +
'      color_1 * castle_texture_color_to_linear(texture2D(tex_1, uv * uv_scale_1)).rgb,' + LineEnding +
'      color_2 * castle_texture_color_to_linear(texture2D(tex_2, uv * uv_scale_2)).rgb,' + LineEnding +
'      mixfactor);' + LineEnding +
'  } else {' + LineEnding +
'    float mixfactor = smoothstep(h2, h3, h);' + LineEnding +
'    base_color = mix(' + LineEnding +
'      color_2 * castle_texture_color_to_linear(texture2D(tex_2, uv * uv_scale_2)).rgb,' + LineEnding +
'      color_3 * castle_texture_color_to_linear(texture2D(tex_3, uv * uv_scale_3)).rgb,' + LineEnding +
'      mixfactor);' + LineEnding +
'  }' + LineEnding +
'' + LineEnding +
'  fragment_color.rgb = mix(fragment_color.rgb, base_color, texture_mix);' + LineEnding +
'' + LineEnding +
'  /* What does this do?' + LineEnding +
'' + LineEnding +
'     normal_slope (normal.y)' + LineEnding +
'     = 0 means a vertical face' + LineEnding +
'     = 1 means a horizontal face' + LineEnding +
'' + LineEnding +
'     So' + LineEnding +
'' + LineEnding +
'     - when normal_slope <= darkness_amount (more vertical than darkness_amount)' + LineEnding +
'       then we multiply by "1.0-darkness_intensity",' + LineEnding +
'       which means we make it completely dark when darkness_intensity=1.' + LineEnding +
'' + LineEnding +
'       Smaller darkness_amount makes the area with "full darkness"' + LineEnding +
'       ("1.0-darkness_intensity") smaller, though the darkness is still applied' + LineEnding +
'       through the range, just with smaller impact.' + LineEnding +
'' + LineEnding +
'     - when normal_slope >= 1.0 (completely horizontal)' + LineEnding +
'       then we multiply by 1.0' + LineEnding +
'       which means we don''t change it.' + LineEnding +
'  */' + LineEnding +
'' + LineEnding +
'  fragment_color.rgb *= mix(1.0-darkness_intensity, 1.0, smoothstep(darkness_amount, 1.0, normal_slope));' + LineEnding +
'}' + LineEnding +
''
