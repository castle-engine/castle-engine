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

uses Classes, DOM,
  X3DNodes, CastleVectors;

type
  TStarlingTextureAtlasLoader = class
    strict private
      type
        { Class that represents SubTexture from Starling xml file }
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

          procedure ReadFormXMLNode(const SubTextureNode: TDOMElement; const ImageWidth, ImageHeight: Integer);
        end;

      var
        FURL: String;

        FImageWidth, FImageHeight: Integer;
        FImagePath: String;

        FSubTexture: TStarlingSubTexture;

        FRoot: TX3DRootNode;
        FShapeCoord: TCoordinateNode;
        FShapeTexCoord: TTextureCoordinateNode;

        FCoordArray: array of TVector3;
        FTexCoordArray: array of TVector2;

        FFramesPerSecond: Single;
        { Animation list to check if the file has any mixed SubTexture nodes. }
        FAnimationList: TStringList;

        procedure ReadImportSettings(const URL: String);

        procedure PrepareX3DRoot;

        procedure ReadImageProperties(const URL: String; const AtlasNode: TDOMElement);

        procedure CalculateFrameCoords(const SubTexture: TStarlingSubTexture);

        procedure PrepareShape(const CoordArray: array of TVector3;
            const TexCoordArray: array of TVector2);

        procedure AddFrameCoords(const CoordInterp: TCoordinateInterpolatorNode;
            const TexCoordInterp: TCoordinateInterpolator2DNode);

        procedure AddAnimation(const FrameCount: Integer;
            const TimeSensor: TTimeSensorNode;
            const CoordInterp: TCoordinateInterpolatorNode;
            const TexCoordInterp: TCoordinateInterpolator2DNode);

        procedure AddRoutes(const TimeSensor: TTimeSensorNode;
            const CoordInterp: TCoordinateInterpolatorNode;
            const TexCoordInterp: TCoordinateInterpolator2DNode);

        function CheckAnimationNameAvailable(AnimationName: String): Boolean;

    public
      constructor Create(const URL: String);
      function Load: TX3DRootNode;
      destructor Destroy; override;
  end;

function LoadStarlingTextureAtlas(const URL: String): TX3DRootNode;

implementation

uses SysUtils, StrUtils,
  CastleImages, CastleTextureImages, CastleURIUtils, CastleXMLUtils, CastleLog;

function LoadStarlingTextureAtlas(const URL: String): TX3DRootNode;
var
  StarlingLoader: TStarlingTextureAtlasLoader;
begin
  StarlingLoader := TStarlingTextureAtlasLoader.Create(URL);
  try
    Result := StarlingLoader.Load;
  finally
    FreeAndNil(StarlingLoader);
  end;
end;

{ TStarlingTextureAtlasLoader }

procedure TStarlingTextureAtlasLoader.ReadImportSettings(const URL: String);
begin
  // TODO: read settings from URL anchors
  FFramesPerSecond := 4.0;
end;

procedure TStarlingTextureAtlasLoader.PrepareX3DRoot;
begin
  FRoot.Meta['generator'] := 'Castle Game Engine, https://castle-engine.io';
  FRoot.Meta['source'] := ExtractURIName(FURL);
end;

procedure TStarlingTextureAtlasLoader.ReadImageProperties(const URL: String;
  const AtlasNode: TDOMElement);
var
  Image: TCastleImage;
begin
  FImagePath := ExtractURIPath(URL) + AtlasNode.AttributeString('imagePath');
  { Some exporters like Free Texture Packer add width and height attributes.
    In this case we don't need load image to check them. }
  if AtlasNode.HasAttribute('width') and AtlasNode.HasAttribute('height') then
  begin
    FImageWidth := AtlasNode.AttributeInteger('width');
    FImageHeight := AtlasNode.AttributeInteger('height');
  end
  else
    begin
      Image := LoadImage(FImagePath);
      try
        FImageWidth := Image.Width;
        FImageHeight := Image.Height;
      finally
        FreeAndNil(Image);
      end;
    end;
end;

procedure TStarlingTextureAtlasLoader.CalculateFrameCoords(
  const SubTexture: TStarlingSubTexture);
begin
  FCoordArray[0] := Vector3(-SubTexture.Width * (SubTexture.AnchorX),
      SubTexture.Height * (SubTexture.AnchorY), 0);

  FCoordArray[1] := Vector3(SubTexture.Width * (1 - SubTexture.AnchorX),
      SubTexture.Height * (SubTexture.AnchorY), 0);

  FCoordArray[2] := Vector3(SubTexture.Width * (1 - SubTexture.AnchorX),
      -SubTexture.Height * (1 - SubTexture.AnchorY), 0);

  FCoordArray[3] := Vector3(-SubTexture.Width * SubTexture.AnchorX,
      SubTexture.Height * SubTexture.AnchorY, 0);

  FCoordArray[4] := Vector3(SubTexture.Width * (1 - SubTexture.AnchorX),
      -SubTexture.Height * (1 - SubTexture.AnchorY), 0);

  FCoordArray[5] := Vector3(-SubTexture.Width * SubTexture.AnchorX,
      -SubTexture.Height * (1 - SubTexture.AnchorY), 0);

  FTexCoordArray[0] := Vector2(SubTexture.X1, SubTexture.Y1);
  FTexCoordArray[1] := Vector2(SubTexture.X2, SubTexture.Y1);
  FTexCoordArray[2] := Vector2(SubTexture.X2, SubTexture.Y2);
  FTexCoordArray[3] := Vector2(SubTexture.X1, SubTexture.Y1);
  FTexCoordArray[4] := Vector2(SubTexture.X2, SubTexture.Y2);
  FTexCoordArray[5] := Vector2(SubTexture.X1, SubTexture.Y2);
end;

procedure TStarlingTextureAtlasLoader.PrepareShape(
  const CoordArray: array of TVector3; const TexCoordArray: array of TVector2);
var
  Shape: TShapeNode;
  Tri: TTriangleSetNode;
  Tex: TImageTextureNode;
  Material: TUnlitMaterialNode;
begin
  Shape:= TShapeNode.Create;
  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := Vector3(1, 1, 1);
  Shape.Material := Material;

  Tex := TImageTextureNode.Create;
  Tex.FdUrl.Send(FImagePath);
  Tex.RepeatS := false;
  Tex.RepeatT := false;
  Tex.TextureProperties := TTexturePropertiesNode.Create;
  Tex.TextureProperties.MinificationFilter := minNearest;
  Tex.TextureProperties.MagnificationFilter := magNearest;
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

procedure TStarlingTextureAtlasLoader.AddFrameCoords(
    const CoordInterp: TCoordinateInterpolatorNode;
    const TexCoordInterp: TCoordinateInterpolator2DNode);
begin
  CoordInterp.FdKeyValue.Items.AddRange(FCoordArray);
  TexCoordInterp.FdKeyValue.Items.AddRange(FTexCoordArray);
  { Repeat all keyValues, to avoid interpolating them smoothly between two keys }
  CoordInterp.FdKeyValue.Items.AddRange(FCoordArray);
  TexCoordInterp.FdKeyValue.Items.AddRange(FTexCoordArray);
end;

procedure TStarlingTextureAtlasLoader.AddAnimation(const FrameCount: Integer;
  const TimeSensor: TTimeSensorNode;
  const CoordInterp: TCoordinateInterpolatorNode;
  const TexCoordInterp: TCoordinateInterpolator2DNode);
var
  I: Integer;
  Key: Single;
begin
  { Set Cycle Interval becouse we know now frame count }
  TimeSensor.CycleInterval := FrameCount / FFramesPerSecond;

  { Generate list of keys. }
  for I := 0 to FrameCount - 1 do
  begin
    Key := I / FrameCount;

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
  FRoot.AddChildren(TimeSensor);
  FRoot.AddChildren(CoordInterp);
  FRoot.AddChildren(TexCoordInterp);
  AddRoutes(TimeSensor, CoordInterp, TexCoordInterp);
end;

procedure TStarlingTextureAtlasLoader.AddRoutes(
  const TimeSensor: TTimeSensorNode;
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
  R3.SetDestinationDirectly(FShapeCoord.FdPoint);
  R4.SetSourceDirectly(TexCoordInterp.EventValue_changed);
  R4.SetDestinationDirectly(FShapeTexCoord.FdPoint);
  FRoot.AddRoute(R1);
  FRoot.AddRoute(R2);
  FRoot.AddRoute(R3);
  FRoot.AddRoute(R4);
end;

function TStarlingTextureAtlasLoader.CheckAnimationNameAvailable(
    AnimationName: String): Boolean;
begin
  if FAnimationList.IndexOf(AnimationName) > -1 then
  begin
    WritelnWarning('Starling', 'Mixed animations tags (%s) in %s file', [AnimationName, FURL]);
    Exit(false);
  end;

  FAnimationList.Add(AnimationName);
  Result := true;
end;


constructor TStarlingTextureAtlasLoader.Create(const URL: String);
begin
  FURL := URL;
  FSubTexture := TStarlingSubTexture.Create;

  SetLength(FCoordArray, 6);
  SetLength(FTexCoordArray, 6);

  FAnimationList := TStringList.Create;
end;

function TStarlingTextureAtlasLoader.Load: TX3DRootNode;
var
  Doc: TXMLDocument;
  AtlasNode: TDOMElement;
  I: TXMLElementIterator;
  LastAnimationName: String;
  CurrentAnimFrameCount: Integer;
  TimeSensor: TTimeSensorNode;
  CoordInterp: TCoordinateInterpolatorNode;
  TexCoordInterp: TCoordinateInterpolator2DNode;
  FirstFrameInFirstAnimation: Boolean;

begin
  Result := nil;

  ReadImportSettings(FURL);

  FRoot := nil;
  Doc := nil;
  try
    try
      FRoot := TX3DRootNode.Create;
      Doc := URLReadXML(FURL);
      AtlasNode := Doc.FindNode('TextureAtlas') as TDOMElement;
      ReadImageProperties(FURL, AtlasNode);

      CurrentAnimFrameCount := 0;
      FirstFrameInFirstAnimation := true;

      I := AtlasNode.ChildrenIterator('SubTexture');
      try
        while I.GetNext do
        begin
          { Read frame from XML }
          FSubTexture.ReadFormXMLNode(I.Current, FImageWidth, FImageHeight);

          CalculateFrameCoords(FSubTexture);
          { After calculate first frame cords and tex cord we need create shape. }
          if FirstFrameInFirstAnimation then
          begin
            PrepareShape(FCoordArray, FTexCoordArray);
            FirstFrameInFirstAnimation := false;
          end;

          if LastAnimationName <> FSubTexture.AnimationName then
          begin
            { First frame of animation loaded. }

            if CurrentAnimFrameCount > 0 then
                AddAnimation(CurrentAnimFrameCount, TimeSensor, CoordInterp, TexCoordInterp);

            { Reset variables for new animation }
            CurrentAnimFrameCount := 0;
            LastAnimationName := FSubTexture.AnimationName;

            if not CheckAnimationNameAvailable(LastAnimationName) then
              continue;

            CurrentAnimFrameCount := 1;
            TimeSensor := TTimeSensorNode.Create(LastAnimationName);
            CoordInterp := TCoordinateInterpolatorNode.Create(LastAnimationName + '_Coord');
            TexCoordInterp := TCoordinateInterpolator2DNode.Create(LastAnimationName + '_TexCoord');

            AddFrameCoords(CoordInterp, TexCoordInterp);
          end
          else
            begin
              { Next frame of animation }
              Inc(CurrentAnimFrameCount);

              AddFrameCoords(CoordInterp, TexCoordInterp);
            end;
        end;

        { Add last animation }
        if CurrentAnimFrameCount > 0 then
          AddAnimation(CurrentAnimFrameCount, TimeSensor, CoordInterp, TexCoordInterp);

        Result := FRoot;

      finally
        FreeAndNil(I);
      end;
    except
      FreeAndNil(FRoot);
      raise;
    end;
  finally
    FreeAndNil(Doc);
  end;
end;

destructor TStarlingTextureAtlasLoader.Destroy;
begin
  FreeAndNil(FSubTexture);
  FreeAndNil(FAnimationList);
  inherited Destroy;
end;

{ TStarlingSubTexture }

procedure TStarlingTextureAtlasLoader.TStarlingSubTexture.PrepareCordsForX3D(
    ImageWidth, ImageHeight: Integer);
begin
  { The input data (X1, Y1) is the coordinates in the texture.
    We need the coordinates in the texture so we start by computing X2 and Y2. }
  X2 := 1 / ImageWidth * (X1 + Width);
  Y2 := 1 - 1 / ImageHeight * (Y1 + Height);

  X1 := 1 / ImageWidth * X1;
  Y1 := 1 - 1 / ImageHeight * Y1;
end;

procedure TStarlingTextureAtlasLoader.TStarlingSubTexture.ReadFormXMLNode(
    const SubTextureNode: TDOMElement; const ImageWidth, ImageHeight: Integer);
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

