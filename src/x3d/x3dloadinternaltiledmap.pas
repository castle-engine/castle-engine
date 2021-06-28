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

{ Convert tiled map (created by Tiled; See https://www.mapeditor.org/)
  into X3D scene representation. }

{ TODO : Add this unit to castle_base.lpk when finished. }

unit X3DLoadInternalTiledMap;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, Math,
  X3DNodes, CastleTiledMap, CastleVectors, CastleTransform, CastleColors,
  CastleRenderOptions, X3DLoadInternalImage;

type
  { Converter class to convert Tiled map into X3D representation. }

  { TTiledMapX3DConverter }

  TTiledMapX3DConverter = class
  strict private
    FMap: TTiledMap;
    FMapNode: TX3DRootNode;

  public
    constructor Create(const Stream: TStream; const BaseUrl: String);
    destructor Destroy; override;

    property Map: TTiledMap read FMap;

    { Holds the X3D representation of the Tiled map. Is not free'd
      automatically.

      TODO : What if MapNode is never returned and manually free'd?
      Improve by getter func.! }
    property MapNode: TX3DRootNode read FMapNode;
  end;

function LoadTiledMap2d(const Stream: TStream; const BaseUrl: String
  ): TX3DRootNode;

implementation

function LoadTiledMap2d(const Stream: TStream; const BaseUrl: String
  ): TX3DRootNode;
var
  MapConverter: TTiledMapX3DConverter;
begin
  Result := nil;
  try
    MapConverter := TTiledMapX3DConverter.Create(Stream, BaseUrl);
    Result := MapConverter.MapNode; //BuildSceneFromTiledMap(ATiledMap);
  finally
    FreeAndNil(MapConverter);
  end;
end;

{ TTiledMapX3DConverter }

constructor TTiledMapX3DConverter.Create(const Stream: TStream;
  const BaseUrl: String);
begin
  inherited Create;

  FMap := TTiledMap.Create(Stream, BaseUrl);
  if not Assigned(FMap) then
    raise Exception.Create(BaseUrl + ' could not be loaded.');

  FMapNode := TX3DRootNode.Create; // Executed after exception?
end;

destructor TTiledMapX3DConverter.Destroy;
begin
  FreeAndNil(FMap);
  inherited Destroy;
end;

end.

