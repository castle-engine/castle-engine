varying vec2 tex_coord_frag;

uniform sampler2D image_texture;
uniform vec2 viewport_size;
uniform int brush_shape; // brush shape
uniform float strength; // strength used for alpha channel (how strong is the terrain height change)
uniform float max_terrain_height;
uniform float ring_thickness;
uniform int brush_size;

void main(void)
{
  switch (brush_shape) {
    case 0: // return square texture
      gl_FragColor = vec4(vec3(max_terrain_height), 1.0);
      break;

    case 1: // return square texture with alpha using strength
      gl_FragColor = vec4(vec3(max_terrain_height), strength);
      break;

    case 2: { // cbtPyramid -  with alpha based on distance from center and strength
      if (brush_size < 2) {
        gl_FragColor = vec4(vec3(max_terrain_height), strength);
        return;
      }
      vec2 pixelCoord = (tex_coord_frag * vec2(brush_size, brush_size));
      vec2 center = vec2(brush_size / 2, brush_size / 2);
      float distance_x = abs((pixelCoord.x  + pixelCoord.y) - (center.x + center.y));
      vec4 col = vec4(vec3(max_terrain_height), strength * (1 - distance_x/(center.x/2)));
      distance_x = abs((pixelCoord.x  - pixelCoord.y) - (center.x - center.y));
      col += vec4(vec3(max_terrain_height), strength * (1 - distance_x/(center.x/2)));
      gl_FragColor = col;
      break;
    }

    case 3: { // cbtCircle - circle with alpha based on strength
      if (brush_size < 2) {
        gl_FragColor = vec4(vec3(max_terrain_height), strength);
        return;
      }
      vec2 pixelCoord = vec2(brush_size, brush_size) * tex_coord_frag;
      float radius = brush_size / 2;
      vec2 center = vec2(brush_size / 2, brush_size / 2);
      float distance = length(pixelCoord - center);
      if (distance <= radius) {
        gl_FragColor = vec4(vec3(max_terrain_height), strength);
      } else
        gl_FragColor = vec4(0.0);
      break;
    }

    case 4: { // cbtCone - circle with alpha based on distance from center and strength
      if (brush_size < 2) {
        gl_FragColor = vec4(vec3(max_terrain_height), strength);
        return;
      }
      vec2 pixelCoord = vec2(brush_size, brush_size) * tex_coord_frag;
      float radius = brush_size / 2;
      vec2 center = vec2(brush_size / 2, brush_size / 2);
      float distance = length(pixelCoord - center);
      if (distance <= radius) {
        gl_FragColor = vec4(vec3(max_terrain_height), strength * (1 - distance / radius) );
      } else
        gl_FragColor = vec4(0.0);
      break;
    }

    case 5: { // cbtRing - circle with alpha based on strength
      if (brush_size < 2) {
        gl_FragColor = vec4(vec3(max_terrain_height), strength);
        return;
      }
      vec2 pixelCoord = vec2(brush_size, brush_size) * tex_coord_frag;
      float radius = brush_size / 2;
      vec2 center = vec2(brush_size / 2, brush_size / 2);
      float distance = length(pixelCoord - center);
      if ((distance <= radius) && (distance > radius - ring_thickness)) {
        gl_FragColor = vec4(vec3(max_terrain_height), strength);
      } else
        gl_FragColor = vec4(0.0);
      break;
    }

    case 6: { // cbtLyingCilinder -
      if (brush_size < 2) {
        gl_FragColor = vec4(vec3(max_terrain_height), strength);
        return;
      }
      vec2 pixelCoord = (tex_coord_frag * vec2(brush_size, brush_size));
      vec2 center = vec2(brush_size / 2, brush_size / 2);
      float distance_x = abs(pixelCoord.x - center.x);
      gl_FragColor = vec4(vec3(max_terrain_height), strength * (1 - distance_x/(center.x/2)));
      break;
    }
  }
}