/*
  Copyright 2020-2020 Michalis Kamburelis and glTF-Sample-Viewer authors.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  Structures for Physical (PBR) lighting model.
  They are included in a special way, to make GLSL code compile both with and without
  "separate compilation units".

  Code below is adapted from glTF-Sample-Viewer ( Apache License 2.0 )
  https://github.com/KhronosGroup/glTF-Sample-Viewer/
  In particular src/shaders/metallic-roughness.frag .
*/

struct AngularInfo
{
  float NdotL;                  // cos angle between normal and light direction
  float NdotV;                  // cos angle between normal and view direction
  float NdotH;                  // cos angle between normal and half vector
  float LdotH;                  // cos angle between light direction and half vector

  float VdotH;                  // cos angle between view direction and half vector

  vec3 padding;
};

struct MaterialInfo
{
  float perceptualRoughness;    // roughness value, as authored by the model creator (input to shader)
  vec3 reflectance0;            // full reflectance color (normal incidence angle)

  float alphaRoughness;         // roughness mapped to a more linear change in the roughness (proposed by [2])
  vec3 diffuseColor;            // color contribution from diffuse lighting

  vec3 reflectance90;           // reflectance color at grazing angle
  vec3 specularColor;           // color contribution from specular lighting
};
