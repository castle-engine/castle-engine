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

function LoadImageAsNode(const URL: String): TX3DRootNode;

implementation

uses CastleImages, CastleLog, CastleURIUtils, CastleStringUtils,
  CastleTextureImages, CastleVectors;

type

  TImageAsX3DModelLoader = class
  strict private
    FURL: String;
    FDisplayURL: String;

    FImageWidth, FImageHeight: Integer;
    FImagePath: String;

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

    procedure ReadImageProperties(const URL: String);

    procedure CalculateCoords;

    procedure PrepareShape(const CoordArray: array of TVector3;
        const TexCoordArray: array of TVector2);

  public
    constructor Create(const URL: String);

    function Load: TX3DRootNode;
  end;

function LoadImageAsNode(const URL: String): TX3DRootNode;
var
  ImageLoader: TImageAsX3DModelLoader;
begin
  ImageLoader := TImageAsX3DModelLoader.Create(URL);
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
  Setting: TStringStringMap.TDictionaryPair;
begin
  FLeft := 0;
  FBottom := 0;
  FWidth := 0;
  FHeight := 0;

  SettingsMap := TStringStringMap.Create;
  try
    URIExtractSettingsFromAnchor(FURL, SettingsMap);
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
        WritelnWarning('ImageAsX3DModel', 'Unknown setting (%s) in "%s" anchor.',
          [Setting.Key, FDisplayURL]);
    end;
  finally
    FreeAndNil(SettingsMap);
  end;

  ReadImageProperties(FURL);

  if FWidth = 0 then
    FWidth := FImageWidth;
  if FHeight = 0 then
    FHeight := FImageHeight;
end;

procedure TImageAsX3DModelLoader.PrepareX3DRoot;
begin
  FRoot.Meta['generator'] := 'Castle Game Engine, https://castle-engine.io';
  FRoot.Meta['source'] := ExtractURIName(FURL);
end;

procedure TImageAsX3DModelLoader.ReadImageProperties(const URL: String);
var
  Image: TCastleImage;
begin
  FImagePath := URL;

  Image := LoadImage(URL);
  try
    FImageWidth := Image.Width;
    FImageHeight := Image.Height;
  finally
    FreeAndNil(Image);
  end;
end;

procedure TImageAsX3DModelLoader.CalculateCoords;
var
  X1, X2, Y1, Y2: Single;
  AnchorX, AnchorY: Single;
begin
  AnchorX := 0.5;
  AnchorY := 0.5;

  X1 := 1 / FImageWidth * FLeft;
  Y1 := 1 / FImageHeight * (FBottom + FHeight);

  X2 := 1 / FImageWidth * (FLeft + FWidth);
  Y2 := 1 / FImageHeight * (FBottom);

  FCoordArray[0] := Vector3(-FWidth * AnchorX,
      FHeight * AnchorY, 0);

  FCoordArray[1] := Vector3(FWidth * (1 - AnchorX),
      FHeight * AnchorY, 0);

  FCoordArray[2] := Vector3(FWidth * (1 - AnchorX),
      -FHeight * (1 - AnchorY), 0);

  FCoordArray[3] := Vector3(-FWidth * AnchorX,
      FHeight * AnchorY, 0);

  FCoordArray[4] := Vector3(FWidth * (1 - AnchorX),
      -FHeight * (1 - AnchorY), 0);

  FCoordArray[5] := Vector3(-FWidth * AnchorX,
      -FHeight * (1 - AnchorY), 0);

  FTexCoordArray[0] := Vector2(X1, Y1);
  FTexCoordArray[1] := Vector2(X2, Y1);
  FTexCoordArray[2] := Vector2(X2, Y2);
  FTexCoordArray[3] := Vector2(X1, Y1);
  FTexCoordArray[4] := Vector2(X2, Y2);
  FTexCoordArray[5] := Vector2(X1, Y2);
end;

procedure TImageAsX3DModelLoader.PrepareShape(
  const CoordArray: array of TVector3; const TexCoordArray: array of TVector2);
var
  Shape: TShapeNode;
  Tri: TTriangleSetNode;
  Tex: TImageTextureNode;
begin
  Shape := TShapeNode.Create;
  Shape.Material := TUnlitMaterialNode.Create;

  Tex := TImageTextureNode.Create;
  Tex.FdUrl.Send(FImagePath);
  Tex.RepeatS := false;
  Tex.RepeatT := false;
  Shape.Texture := Tex;

  Tri := TTriangleSetNode.Create;
  Tri.Solid := false;

  FShapeCoord := TCoordinateNode.Create('coord');
  FShapeCoord.SetPoint([
      CoordArray[0],
      CoordArray[1],
      CoordArray[2],
      CoordArray[3],
      CoordArray[4],
      CoordArray[5]]);

  FShapeTexCoord := TTextureCoordinateNode.Create('texcoord');
  FShapeTexCoord.SetPoint([
       TexCoordArray[0],
       TexCoordArray[1],
       TexCoordArray[2],
       TexCoordArray[3],
       TexCoordArray[4],
       TexCoordArray[5]]);

  Tri.Coord := FShapeCoord;
  Tri.TexCoord := FShapeTexCoord;
  Shape.Geometry := Tri;

  FRoot.AddChildren(Shape);
end;

constructor TImageAsX3DModelLoader.Create(const URL: String);
begin
  inherited Create;
  FImageWidth := 0;
  FImageHeight := 0;

  FURL := URL;
  FDisplayURL := URIDisplay(FURL);

  SetLength(FCoordArray, 6);
  SetLength(FTexCoordArray, 6);
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

