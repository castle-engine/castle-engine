{
  Copyright 2017-2018 Trung Le (kagamma),
  Copyright 2020 Andrzej Kilijański (and3md)

  Based on sprite-sheet-to-x3d source code by Trung Le (kagamma).

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Starling 2D animations loader.

  Starling Texture Atlas Spec: https://doc.starling-framework.org/current/starling/textures/TextureAtlas.html
}
unit X3DLoadInternalStarling;

{$mode objfpc}{$H+}

interface

uses DOM,
  X3DNodes;

type

  TStarlingSubTexture = class
  private
    procedure PrepareCordsForX3D(ImageWidth, ImageHeight: Integer);
  public
    AnimationName: String;
    X1: Single;
    Y1: Single;
    X2: Single;
    Y2: Single;
    Width: Integer;
    Height: Integer;
    AnchorX: Single;
    AnchorY: Single;

    procedure ReadFormXMLNode(const SubTextureNode: TDOMElement; ImageWidth, ImageHeight: Integer);
  end;

var
  FramesPerSecond: Single = 4.0;

function LoadStarlingTextureAtlas(const URL: String): TX3DRootNode;

implementation

uses SysUtils, StrUtils,
  CastleImages, CastleTextureImages, CastleURIUtils, CastleVectors, CastleXMLUtils;

function LoadStarlingTextureAtlas(const URL: String): TX3DRootNode;
var
  Doc: TXMLDocument;
  AtlasNode: TDOMElement;
  I: TXMLElementIterator;
  ImageWidth, ImageHeight: Integer;
  ImagePath: String;
  SubTexture: TStarlingSubTexture;
  Root: TX3DRootNode;
  LastAnimationName: String;
  Coord: TCoordinateNode;
  TexCoord: TTextureCoordinateNode;
  CoordArray: array of TVector3;
  TexCoordArray: array of TVector2;
  CurrentAnimFrameCount: Integer;
  TimeSensor: TTimeSensorNode;
  CoordInterp: TCoordinateInterpolatorNode;
  TexCoordInterp: TCoordinateInterpolator2DNode;

procedure ReadImageProperties(const URL: String; const AtlasNode: TDOMElement);
var
  Image: TCastleImage;
begin
  ImagePath := ExtractURIPath(URL) + AtlasNode.AttributeString('imagePath');
  { Some exporters like Free Texture Packer add width and height attributes.
    In this case we don't need load image to check them. }
  if AtlasNode.HasAttribute('width') and AtlasNode.HasAttribute('height') then
  begin
    ImageWidth := AtlasNode.AttributeInteger('width');
    ImageHeight := AtlasNode.AttributeInteger('height');
  end
  else
    begin
      Image := LoadImage(ImagePath);
      try
        ImageWidth := Image.Width;
        ImageHeight := Image.Height;
      finally
        FreeAndNil(Image);
      end;
    end;
end;

procedure PrepareX3D(const Root: TX3DRootNode);
var
  Shape: TShapeNode;
  Tri: TTriangleSetNode;
  Tex: TImageTextureNode;
  Material: TUnlitMaterialNode;
begin
  Root.Meta['generator'] := 'Castle Game Engine, https://castle-engine.io';
  Root.Meta['source'] := ExtractURIName(URL);

  Shape:= TShapeNode.Create;
  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := Vector3(1, 1, 1);
  Shape.Material := Material;

  Tex := TImageTextureNode.Create;
  Tex.FdUrl.Send(ImagePath);
  Tex.RepeatS := false;
  Tex.RepeatT := false;
  Tex.TextureProperties := TTexturePropertiesNode.Create;
  Tex.TextureProperties.MinificationFilter := minNearest;
  Tex.TextureProperties.MagnificationFilter := magNearest;
  Shape.Texture := Tex;

  Tri := TTriangleSetNode.Create;
  Tri.Solid := false;
  Coord := TCoordinateNode.Create('coord');
  Coord.SetPoint([
      Vector3(-128, -128, 0),
      Vector3(128, -128, 0),
      Vector3(128, 128, 0),
      Vector3(-128, -128, 0),
      Vector3(128, 128, 0),
      Vector3(-128, 128, 0)]);
  TexCoord := TTextureCoordinateNode.Create('texcoord');
  TexCoord.SetPoint([
       Vector2(0, 0),
       Vector2(1, 0),
       Vector2(1, 1),
       Vector2(0, 0),
       Vector2(1, 1),
       Vector2(0, 1)]);
  Tri.Coord := Coord;
  Tri.TexCoord := TexCoord;
  Shape.Geometry := Tri;

  Root.AddChildren(Shape);
end;

procedure AddRoutes(const TimeSensor: TTimeSensorNode;
  const CoordInterp: TCoordinateInterpolatorNode;
  const TexCoordInterp: TCoordinateInterpolator2DNode);
var
  R1, R2, R3, R4: TX3DRoute;
begin
  { Create routes. }
  R1 := TX3DRoute.Create;
  R2 := TX3DRoute.Create;
  R3 := TX3DRoute.Create;
  R4 := TX3DRoute.Create;
  R1.SetSourceDirectly(TimeSensor.EventFraction_changed);
  R1.SetDestinationDirectly(CoordInterp.EventSet_fraction);
  R2.SetSourceDirectly(TimeSensor.EventFraction_changed);
  R2.SetDestinationDirectly(TexCoordInterp.EventSet_fraction);
  R3.SetSourceDirectly(CoordInterp.EventValue_changed);
  R3.SetDestinationDirectly(Coord.FdPoint);
  R4.SetSourceDirectly(TexCoordInterp.EventValue_changed);
  R4.SetDestinationDirectly(TexCoord.FdPoint);
  Root.AddRoute(R1);
  Root.AddRoute(R2);
  Root.AddRoute(R3);
  Root.AddRoute(R4);
end;

procedure AddFrameCoords(SubTexture: TStarlingSubTexture);
begin
  CoordArray[0] := Vector3(-SubTexture.Width * (SubTexture.AnchorX),
      SubTexture.Height * (SubTexture.AnchorY), 0);

  CoordArray[1] := Vector3(SubTexture.Width * (1 - SubTexture.AnchorX),
      SubTexture.Height * (SubTexture.AnchorY), 0);

  CoordArray[2] := Vector3(SubTexture.Width * (1 - SubTexture.AnchorX),
      -SubTexture.Height * (1 - SubTexture.AnchorY), 0);

  CoordArray[3] := Vector3(-SubTexture.Width * SubTexture.AnchorX,
      SubTexture.Height * SubTexture.AnchorY, 0);

  CoordArray[4] := Vector3(SubTexture.Width * (1 - SubTexture.AnchorX),
      -SubTexture.Height * (1 - SubTexture.AnchorY), 0);

  CoordArray[5] := Vector3(-SubTexture.Width * SubTexture.AnchorX,
      -SubTexture.Height * (1 - SubTexture.AnchorY), 0);

  TexCoordArray[0] := Vector2(SubTexture.X1, SubTexture.Y1);
  TexCoordArray[1] := Vector2(SubTexture.X2, SubTexture.Y1);
  TexCoordArray[2] := Vector2(SubTexture.X2, SubTexture.Y2);
  TexCoordArray[3] := Vector2(SubTexture.X1, SubTexture.Y1);
  TexCoordArray[4] := Vector2(SubTexture.X2, SubTexture.Y2);
  TexCoordArray[5] := Vector2(SubTexture.X1, SubTexture.Y2);

  CoordInterp.FdKeyValue.Items.AddRange(CoordArray);
  TexCoordInterp.FdKeyValue.Items.AddRange(TexCoordArray);
  { Repeat all keyValues, to avoid interpolating them smoothly between two keys }
  CoordInterp.FdKeyValue.Items.AddRange(CoordArray);
  TexCoordInterp.FdKeyValue.Items.AddRange(TexCoordArray);
end;

procedure AddAnimation(FrameCount:Integer);
var
  I: Integer;
  Key: Single;
begin
  { Set Cycle Interval becouse we know now frame count }
  TimeSensor.CycleInterval := CurrentAnimFrameCount / FramesPerSecond;

  { Generate list of keys. }
  for I := 0 to CurrentAnimFrameCount - 1 do
  begin
    Key := I / CurrentAnimFrameCount;

    CoordInterp.FdKey.Items.Add(Key);
    TexCoordInterp.FdKey.Items.Add(Key);
    if I > 0 then
    begin
      CoordInterp.FdKey.Items.Add(Key);
      TexCoordInterp.FdKey.Items.Add(Key);
    end;
  end;

  { This way, we have keys like
    0 0.333 0.333 0.666 0.666 1
    That is, all keys are repeated, except 0 and 1. }
  CoordInterp.FdKey.Items.Add(1.0);
  TexCoordInterp.FdKey.Items.Add(1.0);

  { Add TimeSensor, CoordinateInterpolatorNode,
    CoordinateInterpolator2DNode to Root node }
  Root.AddChildren(TimeSensor);
  Root.AddChildren(CoordInterp);
  Root.AddChildren(TexCoordInterp);
  AddRoutes(TimeSensor, CoordInterp, TexCoordInterp);
end;

begin
  Doc := nil;
  SubTexture := nil;
  Root := nil;
  try
    Doc := URLReadXML(URL);
    SubTexture := TStarlingSubTexture.Create;
    AtlasNode := Doc.FindNode('TextureAtlas') as TDOMElement;
    ReadImageProperties(URL, AtlasNode);

    Root := TX3DRootNode.Create;
    PrepareX3D(Root);

    SetLength(CoordArray, 6);
    SetLength(TexCoordArray, 6);
    CurrentAnimFrameCount := 0;

    I := AtlasNode.ChildrenIterator('SubTexture');
    try
      while I.GetNext do
      begin
        { Read frame from XML }
        SubTexture.ReadFormXMLNode(I.Current, ImageWidth, ImageHeight);

        if LastAnimationName <> SubTexture.AnimationName then
        begin
          { First frame of animation loaded. }

          if CurrentAnimFrameCount > 0 then
          begin
            AddAnimation(CurrentAnimFrameCount);
          end;

          { Reset variables for new animation }
          CurrentAnimFrameCount := 1;
          LastAnimationName := SubTexture.AnimationName;
          TimeSensor := TTimeSensorNode.Create(LastAnimationName);
          CoordInterp := TCoordinateInterpolatorNode.Create(LastAnimationName + '_Coord');
          TexCoordInterp := TCoordinateInterpolator2DNode.Create(LastAnimationName + '_TexCoord');

          AddFrameCoords(SubTexture);
        end
        else
          begin
            { Next frame of animation }
            Inc(CurrentAnimFrameCount);

            AddFrameCoords(SubTexture);
          end;

      end;

      if CurrentAnimFrameCount > 0 then
      begin
        AddAnimation(CurrentAnimFrameCount);
      end;

      Result := Root;

    finally
      FreeAndNil(I);
    end;

  finally
    FreeAndNil(Doc);
    FreeAndNil(SubTexture);
  end;
end;

{ TStarlingSubTexture }

procedure TStarlingSubTexture.PrepareCordsForX3D(ImageWidth, ImageHeight: Integer);
begin
  X2 := 1 / ImageWidth * (X1 + Width);
  Y2 := 1 - 1 / ImageHeight * (Y1 + Height);

  X1 := 1 / ImageWidth * X1;
  Y1 := 1 - 1 / ImageHeight * Y1;
end;

procedure TStarlingSubTexture.ReadFormXMLNode(const SubTextureNode: TDOMElement;
  ImageWidth, ImageHeight: Integer);
var
  UnderscorePos: SizeInt;
  FrameXTrim: Integer;
  FrameYTrim: Integer;
  FrameWidth: Integer;
  FrameHeight: Integer;
  Trimmed: Boolean;
  FrameAnchorX: Integer;
  FrameAnchorY: Integer;
begin
  AnimationName := SubTextureNode.AttributeString('name');
  UnderscorePos := rpos('_', AnimationName);
  if UnderscorePos > 0 then
    Delete(AnimationName, UnderscorePos, Length(AnimationName) - UnderscorePos);

  X1 := SubTextureNode.AttributeInteger('x');
  Y1 := SubTextureNode.AttributeInteger('y');
  Width := SubTextureNode.AttributeInteger('width');
  Height := SubTextureNode.AttributeInteger('height');

  Trimmed := SubTextureNode.HasAttribute('frameX');
  if Trimmed then
  begin
    FrameXTrim := -SubTextureNode.AttributeIntegerDef('frameX', 0);
    FrameYTrim := -SubTextureNode.AttributeIntegerDef('frameY', 0);
    FrameWidth := SubTextureNode.AttributeIntegerDef('frameWidth', Width);
    FrameHeight := SubTextureNode.AttributeIntegerDef('frameHeight', Height);

    // calculate anchor
    FrameAnchorX := FrameWidth div 2;
    FrameAnchorY := FrameHeight div 2;

    AnchorX := 1 / Width * (FrameAnchorX - FrameXTrim);
    AnchorY := 1 / Height * (FrameAnchorY - FrameYTrim);
  end
  else
    begin
      AnchorX := 0.5;
      AnchorY := 0.5;
    end;

  PrepareCordsForX3D(ImageWidth, ImageHeight);
end;




end.

