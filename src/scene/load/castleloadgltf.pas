{
  Copyright 2020-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Configuration of loading models in the glTF 2.0 format.
  This unit is pointless now -- @link(GltfForcePhongMaterials) is deprecated.
  But it may become relevant again, if we introduce a class like
  TGltfLoadOptions in the future, see TCastleSceneLoadOptions comments.
  @exclude }
unit CastleLoadGltf;

{$I castleconf.inc}

interface

var
  { @deprecated Use @link(TCastleSceneLoadOptions.GltfPhongMaterials) instead. }
  GltfForcePhongMaterials: Boolean = false;

implementation

end.
