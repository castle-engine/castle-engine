{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/structures.glsl" }
'/*' + LineEnding +
'  Copyright 2020-2020 Michalis Kamburelis and glTF-Sample-Viewer authors.' + LineEnding +
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
'  Structures for Physical (PBR) lighting model.' + LineEnding +
'  They are included in a special way, to make GLSL code compile both with and without' + LineEnding +
'  "separate compilation units".' + LineEnding +
'' + LineEnding +
'  Code below is adapted from glTF-Sample-Viewer ( Apache License 2.0 )' + LineEnding +
'  https://github.com/KhronosGroup/glTF-Sample-Viewer/' + LineEnding +
'  In particular src/shaders/metallic-roughness.frag .' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'struct AngularInfo' + LineEnding +
'{' + LineEnding +
'  float NdotL;                  // cos angle between normal and light direction' + LineEnding +
'  float NdotV;                  // cos angle between normal and view direction' + LineEnding +
'  float NdotH;                  // cos angle between normal and half vector' + LineEnding +
'  float LdotH;                  // cos angle between light direction and half vector' + LineEnding +
'' + LineEnding +
'  float VdotH;                  // cos angle between view direction and half vector' + LineEnding +
'' + LineEnding +
'  vec3 padding;' + LineEnding +
'};' + LineEnding +
'' + LineEnding +
'struct MaterialInfo' + LineEnding +
'{' + LineEnding +
'  float perceptualRoughness;    // roughness value, as authored by the model creator (input to shader)' + LineEnding +
'  vec3 reflectance0;            // full reflectance color (normal incidence angle)' + LineEnding +
'' + LineEnding +
'  float alphaRoughness;         // roughness mapped to a more linear change in the roughness (proposed by [2])' + LineEnding +
'  vec3 diffuseColor;            // color contribution from diffuse lighting' + LineEnding +
'' + LineEnding +
'  vec3 reflectance90;           // reflectance color at grazing angle' + LineEnding +
'  vec3 specularColor;           // color contribution from specular lighting' + LineEnding +
'};' + LineEnding +
''
