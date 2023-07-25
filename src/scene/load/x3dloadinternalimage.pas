{
  Copyright 2020-2020 Andrzej Kilijański (and3md)

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple loader of image files to TCastleScene. }
unit X3DLoadInternalImage;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils,
  X3DNodes;

function LoadImageAsNode(const Stream: TStream; const BaseUrl, MimeType: String): TX3DRootNode;

implementation

uses Generics.Collections, CastleImages, CastleLog, CastleURIUtils, CastleStringUtils,
  CastleTextureImages, CastleVectors;

type

  TImageAsX3DModelLoader = class
  strict private
    FImage: TEncodedImage;
    FBaseUrl: String;
    FDisplayUrl: String;

    FLeft: Integer;
    FBottom: Integer;
    FWidth: Integer;
    FHeight: Integer;

    FRoot: TX3DRootNode;
    FShapeCoord: TCoordinateNode;
    FShapeTexCoord: TTextureCoordinateNode;

    FCoordArray: array of TVector3;
    FTexCoordArray: array of TVector2;

    procedure ReadImportSettings;

    procedure PrepareX3DRoot;

    procedure CalculateCoords;

    procedure PrepareShape(const CoordArray: array of TVector3;
        const TexCoordArray: array of TVector2);

  public
    constructor Create(const Stream: TStream; const BaseUrl, MimeType: String);

    function Load: TX3DRootNode;
  end;

function LoadImageAsNode(const Stream: TStream; const BaseUrl, MimeType: String): TX3DRootNode;
var
  ImageLoader: TImageAsX3DModelLoader;
begin
  ImageLoader := TImageAsX3DModelLoader.Create(Stream, BaseUrl, MimeType);
  try
    Result := ImageLoader.Load;
  finally
    FreeAndNil(ImageLoader);
  end;
end;

{ TImageAsX3DModelLoader ---------------------------------------------------- }

procedure TImageAsX3DModelLoader.ReadImportSettings;
var
  SettingsMap: TStringStringMap;
  Setting: {$ifdef FPC}TStringStringMap.TDictionaryPair{$else}TPair<string, string>{$endif};
begin
  FLeft := 0;
  FBottom := 0;
  FWidth := 0;
  FHeight := 0;

  SettingsMap := TStringStringMap.Create;
  try
    URIGetSettingsFromAnchor(FBaseUrl, SettingsMap);
    for Setting in SettingsMap do
    begin
      if LowerCase(Setting.Key) = 'left' then
        FLeft := StrToInt(Setting.Value)
      else
      if LowerCase(Setting.Key) = 'bottom' then
        FBottom := StrToInt(Setting.Value)
      else
      if LowerCase(Setting.Key) = 'width' then
        FWidth := StrToInt(Setting.Value)
      else
      if LowerCase(Setting.Key) = 'height' then
        FHeight := StrToInt(Setting.Value)
      else
        WritelnWarning('LoadNode(Image)', 'Unknown setting (%s) in "%s" anchor.',
          [Setting.Key, FDisplayUrl]);
    end;
  finally
    FreeAndNil(SettingsMap);
  end;

  if FWidth = 0 then
    FWidth := FImage.Width;
  if FHeight = 0 then
    FHeight := FImage.Height;
end;

procedure TImageAsX3DModelLoader.PrepareX3DRoot;
begin
  FRoot.Meta['generator'] := 'Castle Game Engine, https://castle-engine.io';
  FRoot.Meta['source'] := ExtractURIName(FBaseUrl);
end;

procedure TImageAsX3DModelLoader.CalculateCoords;
var
  X1, X2, Y1, Y2: Single;
  AnchorX, AnchorY: Single;
begin
  AnchorX := 0.5;
  AnchorY := 0.5;

  X1 := -FWidth * AnchorX;
  X2 := FWidth * (1 - AnchorX);
  Y1 := -FHeight * AnchorY;
  Y2 := FHeight * (1 - AnchorY);

  FCoordArray[0] := Vector3(X1, Y1, 0);
  FCoordArray[1] := Vector3(X2, Y1, 0);
  FCoordArray[2] := Vector3(X2, Y2, 0);
  FCoordArray[3] := Vector3(X1, Y2, 0);

  X1 := 1 / FImage.Width * FLeft;
  X2 := 1 / FImage.Width * (FLeft + FWidth);
  Y1 := 1 / FImage.Height * FBottom;
  Y2 := 1 / FImage.Height * (FBottom + FHeight);

  FTexCoordArray[0] := Vector2(X1, Y1);
  FTexCoordArray[1] := Vector2(X2, Y1);
  FTexCoordArray[2] := Vector2(X2, Y2);
  FTexCoordArray[3] := Vector2(X1, Y2);
end;

procedure TImageAsX3DModelLoader.PrepareShape(
  const CoordArray: array of TVector3; const TexCoordArray: array of TVector2);
var
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
  Shape: TShapeNode;
  Tri: TIndexedTriangleSetNode;
  Tex: TImageTextureNode;
  TexProperties: TTexturePropertiesNode;
begin
  Material := TUnlitMaterialNode.Create;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Shape := TShapeNode.Create;
  Shape.Appearance := Appearance;

  Tex := TImageTextureNode.Create('', FBaseUrl);
  { Take FImage ownership, we will not free it here }
  Tex.LoadFromImage(FImage, true, FBaseUrl);
  { No point in adjusting RepeatS/T: TextureProperties override it.
  Tex.RepeatS := false;
  Tex.RepeatT := false; }
  Appearance.Texture := Tex;

  TexProperties := TTexturePropertiesNode.Create;
  TexProperties.MagnificationFilter := magDefault;
  TexProperties.MinificationFilter := minDefault;
  TexProperties.BoundaryModeS := bmClampToEdge;
  TexProperties.BoundaryModeT := bmClampToEdge;
  { Do not force "power of 2" size, which may prevent mipmaps.
    This seems like a better default (otherwise the resizing underneath
    may cause longer loading time, and loss of quality, if not expected). }
  TexProperties.GuiTexture := true;
  Tex.TextureProperties := TexProperties;

  Tri := TIndexedTriangleSetNode.Create;
  Tri.SetIndex([0, 1, 2, 0, 2, 3]);
  Tri.Solid := false;

  FShapeCoord := TCoordinateNode.Create('coord');
  FShapeCoord.SetPoint([
    CoordArray[0],
    CoordArray[1],
    CoordArray[2],
    CoordArray[3]
  ]);

  FShapeTexCoord := TTextureCoordinateNode.Create('texcoord');
  FShapeTexCoord.SetPoint([
    TexCoordArray[0],
    TexCoordArray[1],
    TexCoordArray[2],
    TexCoordArray[3]
  ]);

  Tri.Coord := FShapeCoord;
  Tri.TexCoord := FShapeTexCoord;
  Shape.Geometry := Tri;

  FRoot.AddChildren(Shape);
end;

constructor TImageAsX3DModelLoader.Create(const Stream: TStream; const BaseUrl, MimeType: String);
begin
  inherited Create;

  FImage := LoadEncodedImage(Stream, MimeType, []);
  FBaseUrl := BaseUrl;
  FDisplayUrl := URIDisplay(FBaseUrl);

  SetLength(FCoordArray, 4);
  SetLength(FTexCoordArray, 4);
end;

function TImageAsX3DModelLoader.Load: TX3DRootNode;
begin
  FRoot := nil;
  try
    FRoot := TX3DRootNode.Create;
    ReadImportSettings;

    CalculateCoords;
    PrepareShape(FCoordArray, FTexCoordArray);

    Result := FRoot;
  except
    FreeAndNil(FRoot);
    raise;
  end;
end;

end.
