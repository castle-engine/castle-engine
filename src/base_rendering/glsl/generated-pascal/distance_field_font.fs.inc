{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/distance_field_font.fs" }
'/*' + LineEnding +
'  Copyright 2023-2024 Eugene Loza, Michalis Kamburelis.' + LineEnding +
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
'  Rendering font with distance field font.' + LineEnding +
'' + LineEnding +
'  This is similar to image.fs.' + LineEnding +
'  It is used as custom shader for TDrawableImage.' + LineEnding +
'  Notes:' + LineEnding +
'' + LineEnding +
'  - This always behaves as image.fs with COLOR_UNIFORM defined,' + LineEnding +
'    so it defines and uses "uniform vec4 color;".' + LineEnding +
'' + LineEnding +
'    It can, because it is used always with a font texture that consists' + LineEnding +
'    only of alpha channel. The TDrawableImage always has' + LineEnding +
'    - TextureHasOnlyAlpha = true' + LineEnding +
'    - and so, it has always ColorTreatment = ctColorMultipliesTextureAlpha' + LineEnding +
'    - and so,' + LineEnding +
'      - TDrawableImage.TImageProgram.InitializeUniformsAttributes' + LineEnding +
'        queries for "color" uniform,' + LineEnding +
'      - and rendering sets it using "Prog.UniformColor.SetValue(Color)".' + LineEnding +
'' + LineEnding +
'    This means the rendering honors TDrawableImage.Color' + LineEnding +
'    which in turn means it honors TCastleLabel.Color.' + LineEnding +
'' + LineEnding +
'  - This shader is in subdirectory base_rendering/.' + LineEnding +
'    The subdirectory base_rendering is not the best for font stuff,' + LineEnding +
'    but this way the distance_field_font.fs lives alongside image.fs' + LineEnding +
'    with which it should be synchronized.' + LineEnding +
'' + LineEnding +
'  - This is used together with vertex shader image.vs.' + LineEnding +
'    So we didn''t need a special vertex shader like distance_field_font.vs.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'varying vec2 tex_coord_frag;' + LineEnding +
'uniform sampler2D image_texture;' + LineEnding +
'' + LineEnding +
'uniform vec4 color;' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_FragColor = color;' + LineEnding +
'' + LineEnding +
'  float alpha_from_distance_texture = texture2D(image_texture, tex_coord_frag).a;' + LineEnding +
'  if (alpha_from_distance_texture <= 0.495) {' + LineEnding +
'    discard;' + LineEnding +
'  } else' + LineEnding +
'  if ( (alpha_from_distance_texture > 0.495) &&' + LineEnding +
'       (alpha_from_distance_texture < 0.5)) {' + LineEnding +
'    gl_FragColor.a *= (alpha_from_distance_texture - 0.495) * 200.0;' + LineEnding +
'  } else {' + LineEnding +
'    // leave gl_FragColor unchanged' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
''
