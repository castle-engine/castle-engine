/*
  Copyright 2023-2024 Eugene Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  Rendering font with distance field font.

  This is similar to image.fs.
  It is used as custom shader for TDrawableImage.
  Notes:

  - Just like image.fs it honors the COLOR_UNIFORM, and multiplies it
    (e.g. to honor TCastleLabel.Color).

    TODO: Hm, but TDrawableImage.CustomShader is not recompiled now
    with various COLOR_UNIFORM / not versions.
    And we cannot just hardcode "define COLOR_UNIFORM" here,
    because the uniform is not passed when color is white.
    - We need to improve how TDrawableImage.CustomShader is used?
    - Hm, but why does it work now when TCastleLabel.Color is white?
      The "color" uniform is undefined -- is it just white by accident?
      Check that we don't pass "color" uniform in this case.

  - This is in subdirectory base_rendering/.
    The subdirectory base_rendering is not the best for font stuff,
    but this way the distance_field_font.fs lives alongside image.fs
    with which it should be synchronized.

  - This is used together with vertex shader image.vs.
    So we didn't need a special vertex shader like distance_field_font.vs.
*/

varying vec2 tex_coord_frag;
uniform sampler2D image_texture;

#define COLOR_UNIFORM
#ifdef COLOR_UNIFORM
uniform vec4 color;
#endif

void main(void)
{
#ifdef COLOR_UNIFORM
  gl_FragColor = color;
#else
  gl_FragColor = vec4(1.0);
#endif

  float alpha_from_distance_texture = texture2D(image_texture, tex_coord_frag).a;
  if (alpha_from_distance_texture <= 0.495) {
    discard;
  } else
  if ( (alpha_from_distance_texture > 0.495) &&
       (alpha_from_distance_texture < 0.5)) {
    gl_FragColor.a *= (alpha_from_distance_texture - 0.495) * 200.0;
  } else {
    // leave gl_FragColor unchanged
  }
}
