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

  - This always behaves as image.fs with COLOR_UNIFORM defined,
    so it defines and uses "uniform vec4 color;".

    It can, because it is used always with a font texture that consists
    only of alpha channel. The TDrawableImage always has
    - TextureHasOnlyAlpha = true
    - and so, it has always ColorTreatment = ctColorMultipliesTextureAlpha
    - and so,
      - TDrawableImage.TImageProgram.InitializeUniformsAttributes
        queries for "color" uniform,
      - and rendering sets it using "Prog.UniformColor.SetValue(Color)".

    This means the rendering honors TDrawableImage.Color
    which in turn means it honors TCastleLabel.Color.

  - This shader is in subdirectory base_rendering/.
    The subdirectory base_rendering is not the best for font stuff,
    but this way the distance_field_font.fs lives alongside image.fs
    with which it should be synchronized.

  - This is used together with vertex shader image.vs.
    So we didn't need a special vertex shader like distance_field_font.vs.
*/

varying vec2 tex_coord_frag;
uniform sampler2D image_texture;

uniform vec4 color;

void main(void)
{
  gl_FragColor = color;

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
