{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Configuration of loading models in the glTF 2.0 format. }
unit CastleLoadGltf;

{$I castleconf.inc}

interface

var
  { Makes model loaded from glTF use Phong materials (TMaterialNode) instead of
    Physically-Based Rendering materials (TPhysicalMaterialNode).

    Phong is a worse lighting model in general (less realistic, and most authoring
    tools now expose parameters closer to PBR, like Blender).
    However Phong lighting model is cheaper to compute, and it allows both
    Gouraud and Phong shading. And Phong lighting model combined with Gouraud shading
    is very cheap to render, which in effect means that your models render fast.

    We just interpret glTF pbrMetallicRoughness parameter "baseColor" (RGB part)
    as Phong "diffuseColor". }
  GltfForcePhongMaterials: Boolean = false;

implementation

end.
