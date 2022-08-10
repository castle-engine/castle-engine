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
'uniform float uv_scale_1;' + LineEnding +
'uniform float uv_scale_2;' + LineEnding +
'uniform float uv_scale_3;' + LineEnding +
'' + LineEnding +
'uniform float normal_dark;' + LineEnding +
'uniform float normal_darkening;' + LineEnding +
'uniform float texture_mix;' + LineEnding +
'' + LineEnding +
'uniform float h0; // below is tex_1' + LineEnding +
'uniform float h1; // below is tex_2 mixed with tex_1' + LineEnding +
'uniform float h2; // below is tex_2' + LineEnding +
'uniform float h3; // below is tex_3 mixed with tex_2' + LineEnding +
'                  // above is tex_3' + LineEnding +
'' + LineEnding +
'varying vec3 terrain_position;' + LineEnding +
'varying vec3 terrain_normal;' + LineEnding +
'' + LineEnding +
'void PLUG_main_texture_apply(inout vec4 fragment_color, const in vec3 normal)' + LineEnding +
'{' + LineEnding +
'  vec4 tex;' + LineEnding +
'  float h = terrain_position.y;' + LineEnding +
'  vec2 uv = terrain_position.xz;' + LineEnding +
'  float normal_slope = normalize(terrain_normal).y;' + LineEnding +
'' + LineEnding +
'  /*' + LineEnding +
'  if (h <= h0) {' + LineEnding +
'    tex = texture2D(tex_1, uv);' + LineEnding +
'  } else' + LineEnding +
'  if (h <= h1) {' + LineEnding +
'    float mixfactor = smoothstep(h0, h1, h);' + LineEnding +
'      //clamp((h - h0) / (h1 - h0), 0.0, 1.0);' + LineEnding +
'    tex = mix(texture2D(tex_1, uv), texture2D(tex_2, uv), mixfactor);' + LineEnding +
'  } else' + LineEnding +
'  if (h <= h2) {' + LineEnding +
'    tex = texture2D(tex_2, uv);' + LineEnding +
'  } else' + LineEnding +
'  if (h <= h3) {' + LineEnding +
'    float mixfactor = smoothstep(h2, h3, h);' + LineEnding +
'      //clamp((h - h2) / (h3 - h2), 0.0, 1.0);' + LineEnding +
'    tex = mix(texture2D(tex_2, uv), texture2D(tex_3, uv), mixfactor);' + LineEnding +
'  } else' + LineEnding +
'  {' + LineEnding +
'    tex = texture2D(tex_3, uv);' + LineEnding +
'  }' + LineEnding +
'  */' + LineEnding +
'' + LineEnding +
'  /* This achieves the same effect as above (because smoothstep' + LineEnding +
'     does clamp() inside), but better:' + LineEnding +
'     - one 1 "if", instead of 4 "if"s' + LineEnding +
'     - no weird artifacts when h is precisely at h0, h1, h2 or h3.' + LineEnding +
'' + LineEnding +
'       (previous code was sometimes showing a weird color at these borders,' + LineEnding +
'       possibly GPU was forcing all neighboring pixels to have the same' + LineEnding +
'       "if" outcome -- observed with' + LineEnding +
'         Renderer: GeForce GTS 450/PCIe/SSE2' + LineEnding +
'         Version: 4.5.0 NVIDIA 375.82' + LineEnding +
'         on Linux/x86_64.' + LineEnding +
'       ).' + LineEnding +
'  */' + LineEnding +
'' + LineEnding +
'  float hhalf = (h1 + h2) * 0.5;' + LineEnding +
'  if (h < hhalf) {' + LineEnding +
'    float mixfactor = smoothstep(h0, h1, h);' + LineEnding +
'    tex = mix(' + LineEnding +
'      texture2D(tex_1, uv * uv_scale_1),' + LineEnding +
'      texture2D(tex_2, uv * uv_scale_2), mixfactor);' + LineEnding +
'  } else {' + LineEnding +
'    float mixfactor = smoothstep(h2, h3, h);' + LineEnding +
'    tex = mix(' + LineEnding +
'      texture2D(tex_2, uv * uv_scale_2),' + LineEnding +
'      texture2D(tex_3, uv * uv_scale_3), mixfactor);' + LineEnding +
'  }' + LineEnding +
'' + LineEnding +
'  fragment_color.rgb = mix(fragment_color.rgb, tex.rgb, texture_mix);' + LineEnding +
'' + LineEnding +
'  fragment_color.rgb *= mix(normal_darkening, 1.0, smoothstep(normal_dark, 1.0, normal_slope));' + LineEnding +
'}' + LineEnding +
''
