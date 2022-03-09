{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_phong/structures.glsl" }
'/*' + LineEnding +
'  Copyright 2020-2020 Michalis Kamburelis.' + LineEnding +
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
'  Structures for Phong lighting model.' + LineEnding +
'  They are included in a special way, to make GLSL code compile both with and without' + LineEnding +
'  "separate compilation units".' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'struct MaterialInfo' + LineEnding +
'{' + LineEnding +
'  /* Calculated color from' + LineEnding +
'     Material.diffuseColor/transparency (or ColorRGBA node) * diffuse texture.' + LineEnding +
'     Contains complete "diffuse/transparency" information that is independent of light source.' + LineEnding +
'     In case of Gouraud shading it is not multiplied by the diffuse texture' + LineEnding +
'     (because it cannot be, as we''re on vertex shader). */' + LineEnding +
'  vec4 diffuse_alpha;' + LineEnding +
'' + LineEnding +
'  vec3 ambient;' + LineEnding +
'  vec3 specular;' + LineEnding +
'  float shininess;' + LineEnding +
'};' + LineEnding +
''
