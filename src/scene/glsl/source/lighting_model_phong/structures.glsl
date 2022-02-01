/*
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  Structures for Phong lighting model.
  They are included in a special way, to make GLSL code compile both with and without
  "separate compilation units".
*/

struct MaterialInfo
{
  /* Calculated color from
     Material.diffuseColor/transparency (or ColorRGBA node) * diffuse texture.
     Contains complete "diffuse/transparency" information that is independent of light source.
     In case of Gouraud shading it is not multiplied by the diffuse texture
     (because it cannot be, as we're on vertex shader). */
  vec4 diffuse_alpha;

  vec3 ambient;
  vec3 specular;
  float shininess;
};
