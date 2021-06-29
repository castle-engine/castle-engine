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
  1. Integrate unit with Castle Game Engine (e.g. add to pack., use castle conf.
     inc., ...)

}
unit CastleConvertTiledMap;

//{$I castleconf.inc}

interface

uses
  Classes, SysUtils, Math,
  X3DNodes, CastleTiledMap, CastleVectors, CastleTransform, CastleColors,
  CastleRenderOptions, X3DLoadInternalImage;

{ Converts a Tiled map into a X3D representation for the Castle Game Engine.
  The result can be returned to Scene.Load method. }
function ConvertTiledMap(ATiledMap: TTiledMap): TX3DRootNode;

implementation

type
  { Converter class to convert Tiled map into X3D representations. }
  TTiledMapConverter = class
  strict private
    FMap: TTiledMap;
    FMapNode: TX3DRootNode;

  public
    constructor Create;
    destructor Destroy; override;

    property Map: TTiledMap read FMap write FMap;
    { Holds the X3D representation of the Tiled map. Is not free'd
      automatically.

      TODO : What if MapNode is never returned and manually free'd?
      Improve by getter func.! }
    property MapNode: TX3DRootNode read FMapNode;
  end;

constructor TTiledMapConverter.Create;
begin
  inherited Create;

  FMapNode := TX3DRootNode.Create;
end;

destructor TTiledMapConverter.Destroy;
begin

  inherited Destroy;
end;

function ConvertTiledMap(ATiledMap: TTiledMap): TX3DRootNode;
var
  ATiledMapConverter: TTiledMapConverter;
begin
  Result := nil;

  if not Assigned(ATiledMap) then
    Exit;

  try
    ATiledMapConverter := TTiledMapConverter.Create;
    ATiledMapConverter.Map := ATiledMap;
    Result := ATiledMapConverter.MapNode;
  finally
    FreeAndNil(ATiledMapConverter);
  end;

end;

end.

