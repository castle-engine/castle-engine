{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/terrain_edit_brush.fs" }
'varying vec2 tex_coord_frag;' + LineEnding +
'' + LineEnding +
'uniform sampler2D image_texture;' + LineEnding +
'uniform vec2 viewport_size;' + LineEnding +
'uniform int brush_shape; // brush shape' + LineEnding +
'uniform float strength; // strength used for alpha channel (how strong is the terrain height change)' + LineEnding +
'uniform float max_terrain_height;' + LineEnding +
'uniform float ring_thickness;' + LineEnding +
'uniform int brush_size;' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  switch (brush_shape) {' + LineEnding +
'    case 0: // return square texture' + LineEnding +
'      gl_FragColor = vec4(vec3(max_terrain_height), 1.0);' + LineEnding +
'      break;' + LineEnding +
'' + LineEnding +
'    case 1: // return square texture with alpha using strength' + LineEnding +
'      gl_FragColor = vec4(vec3(max_terrain_height), strength);' + LineEnding +
'      break;' + LineEnding +
'' + LineEnding +
'    case 2: { // cbtPyramid -  with alpha based on distance from center and strength' + LineEnding +
'      if (brush_size < 2) {' + LineEnding +
'        gl_FragColor = vec4(vec3(max_terrain_height), strength);' + LineEnding +
'        return;' + LineEnding +
'      }' + LineEnding +
'      vec2 pixelCoord = (tex_coord_frag * vec2(brush_size, brush_size));' + LineEnding +
'      vec2 center = vec2(brush_size / 2, brush_size / 2);' + LineEnding +
'      float distance_x = abs((pixelCoord.x  + pixelCoord.y) - (center.x + center.y));' + LineEnding +
'      vec4 col = vec4(vec3(max_terrain_height), strength * (1 - distance_x/(center.x/2)));' + LineEnding +
'      distance_x = abs((pixelCoord.x  - pixelCoord.y) - (center.x - center.y));' + LineEnding +
'      col += vec4(vec3(max_terrain_height), strength * (1 - distance_x/(center.x/2)));' + LineEnding +
'      gl_FragColor = col;' + LineEnding +
'      break;' + LineEnding +
'    }' + LineEnding +
'' + LineEnding +
'    case 3: { // cbtCircle - circle with alpha based on strength' + LineEnding +
'      if (brush_size < 2) {' + LineEnding +
'        gl_FragColor = vec4(vec3(max_terrain_height), strength);' + LineEnding +
'        return;' + LineEnding +
'      }' + LineEnding +
'      vec2 pixelCoord = vec2(brush_size, brush_size) * tex_coord_frag;' + LineEnding +
'      float radius = brush_size / 2;' + LineEnding +
'      vec2 center = vec2(brush_size / 2, brush_size / 2);' + LineEnding +
'      float distance = length(pixelCoord - center);' + LineEnding +
'      if (distance <= radius) {' + LineEnding +
'        gl_FragColor = vec4(vec3(max_terrain_height), strength);' + LineEnding +
'      } else' + LineEnding +
'        gl_FragColor = vec4(0.0);' + LineEnding +
'      break;' + LineEnding +
'    }' + LineEnding +
'' + LineEnding +
'    case 4: { // cbtCone - circle with alpha based on distance from center and strength' + LineEnding +
'      if (brush_size < 2) {' + LineEnding +
'        gl_FragColor = vec4(vec3(max_terrain_height), strength);' + LineEnding +
'        return;' + LineEnding +
'      }' + LineEnding +
'      vec2 pixelCoord = vec2(brush_size, brush_size) * tex_coord_frag;' + LineEnding +
'      float radius = brush_size / 2;' + LineEnding +
'      vec2 center = vec2(brush_size / 2, brush_size / 2);' + LineEnding +
'      float distance = length(pixelCoord - center);' + LineEnding +
'      if (distance <= radius) {' + LineEnding +
'        gl_FragColor = vec4(vec3(max_terrain_height), strength * (1 - distance / radius) );' + LineEnding +
'      } else' + LineEnding +
'        gl_FragColor = vec4(0.0);' + LineEnding +
'      break;' + LineEnding +
'    }' + LineEnding +
'' + LineEnding +
'    case 5: { // cbtRing - circle with alpha based on strength' + LineEnding +
'      if (brush_size < 2) {' + LineEnding +
'        gl_FragColor = vec4(vec3(max_terrain_height), strength);' + LineEnding +
'        return;' + LineEnding +
'      }' + LineEnding +
'      vec2 pixelCoord = vec2(brush_size, brush_size) * tex_coord_frag;' + LineEnding +
'      float radius = brush_size / 2;' + LineEnding +
'      vec2 center = vec2(brush_size / 2, brush_size / 2);' + LineEnding +
'      float distance = length(pixelCoord - center);' + LineEnding +
'      if ((distance <= radius) && (distance > radius - ring_thickness)) {' + LineEnding +
'        gl_FragColor = vec4(vec3(max_terrain_height), strength);' + LineEnding +
'      } else' + LineEnding +
'        gl_FragColor = vec4(0.0);' + LineEnding +
'      break;' + LineEnding +
'    }' + LineEnding +
'' + LineEnding +
'    case 6: { // cbtLyingCilinder -' + LineEnding +
'      if (brush_size < 2) {' + LineEnding +
'        gl_FragColor = vec4(vec3(max_terrain_height), strength);' + LineEnding +
'        return;' + LineEnding +
'      }' + LineEnding +
'      vec2 pixelCoord = (tex_coord_frag * vec2(brush_size, brush_size));' + LineEnding +
'      vec2 center = vec2(brush_size / 2, brush_size / 2);' + LineEnding +
'      float distance_x = abs(pixelCoord.x - center.x);' + LineEnding +
'      gl_FragColor = vec4(vec3(max_terrain_height), strength * (1 - distance_x/(center.x/2)));' + LineEnding +
'      break;' + LineEnding +
'    }' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
''
