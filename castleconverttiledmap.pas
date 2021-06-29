{
  Copyright 2020-2021 Matthias J. Molski.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert Tiled map (see https://www.mapeditor.org/) loaded by
  CastleTiledMap unit into X3D representation.

  This unit is on purpose not fully integrated with the engine yet. This makes
  debugging easier. If unit is fully functional, it should be integrated with
  the Scene.Load mechanism. Until then it works as follows:

  1. Create Tiled map (TTiledMap).
  2. Prepare X3D scene.
  3. Convert Tiled map by this unit (CastleConvertTiledMap).
  4. Load X3D scene directly by X3D representation (use Scene.Load).

  TODO:
  1. Integrate unit with Castle Game Engine

}
unit CastleConvertTiledMap;

//{$I castleconf.inc}

interface

uses
  Classes, SysUtils, Math,
  X3DNodes, CastleTiledMap, CastleVectors, CastleTransform, CastleColors,
  CastleRenderOptions, X3DLoadInternalImage;

function ConvertTiledMap(TiledMap: TTiledMap): TX3DRootNode;

implementation

function ConvertTiledMap(TiledMap: TTiledMap): TX3DRootNode;
begin
  Result := TX3DRootNode.Create;
end;

end.

