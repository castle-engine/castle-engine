{
  Copyright 2017-2018 Trung Le (kagamma),
  Copyright 2020-2020 Andrzej Kilijański (and3md)

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

{$I castleconf.inc}

interface

uses Classes, SysUtils, DOM,
  X3DNodes,
  CastleTextureImages, CastleVectors;

type
  { Starling XML file is not correct }
  EInvalidStarlingXml = class(Exception);

  { Frame names in starling file can be named freely, but in the case of our loader,
    we have to define what is the next frame of the animation and what should
    be recognized as a separate animation. }
  TStarlingAnimationNaming = (
    { Default behavior treats as animation frames only those subtextures whose
      names ends with an underscore followed by a number.
      In this case, "walk_01", "walk_02" will be recognized as next frames of the
      same animation, but "item1", "item2" will be treated as separate entities. }
    anStrictUnderscore,
    { In many cases, the consecutive frames of one animation are named
      without underscore, eg "walk1", "walk2". To load such subtextures as one animation
      use the anTralingNumber option. }
    anTralingNumber
  );

  TStarlingTextureAtlasLoader = class
  strict private
    type
      { Class that represents SubTexture from Starling xml file }
      TStarlingSubTexture = class
      private
        FAnimationNaming: TStarlingAnimationNaming;
        procedure PrepareTexCordsForX3D(ImageWidth, ImageHeight: Integer);
        procedure ParseAnimationName(const SubTextureName: String);
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
        constructor Create;
      end;
    var
      FURL: String;
      FDisplayURL: String;

      { Load settings. }
      FFramesPerSecond: Single;

      FImageWidth, FImageHeight: Integer;
      FImagePath: String;

      FSubTexture: TStarlingSubTexture;

      FRoot: TX3DRootNode;
      FShapeCoord: TCoordinateNode;
      FShapeTexCoord: TTextureCoordinateNode;

      FCoordArray: array of TVector3;
      FTexCoordArray: array of TVector2;

      { Animation list to check if the file has any mixed SubTexture nodes. }
      FAnimationList: TStringList;

    procedure ReadImportSettings;

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

    function CheckAnimationNameAvailable(const AnimationName: String): Boolean;

  public
    constructor Create(const URL: String);
    destructor Destroy; override;

    function Load: TX3DRootNode;
  end;

function LoadStarlingTextureAtlas(const URL: String): TX3DRootNode;

implementation

uses StrUtils,
  CastleImages, CastleLog, CastleStringUtils, CastleURIUtils, CastleUtils,
  CastleXMLUtils;

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

procedure TStarlingTextureAtlasLoader.ReadImportSettings;
var
  SettingsMap: TStringStringMap;
  Setting: TStringStringMap.TDictionaryPair;
begin
  // default values
  FFramesPerSecond := 4.0;

  SettingsMap := TStringStringMap.Create;
  try
    URIExtractAnchor(FURL, SettingsMap);
    for Setting in SettingsMap do
    begin
      if LowerCase(Setting.Key) = 'fps' then
      begin
        FFramesPerSecond := StrToFloatDot(Setting.Value);
      end else
      if LowerCase(Setting.Key) = 'anim-naming' then
      begin
        if Setting.Value = 'strict-underscore' then
          FSubTexture.FAnimationNaming := anStrictUnderscore
        else if Setting.Value = 'trailing-number' then
          FSubTexture.FAnimationNaming := anTralingNumber
        else
          WritelnWarning('Starling', 'Unknown anim-naming value (%s) in "%s" anchor.', [Setting.Value, FDisplayURL]);
      end else
        WritelnWarning('Starling', 'Unknown setting (%s) in "%s" anchor.', [Setting.Key, FDisplayURL]);
    end;
  finally
    FreeAndNil(SettingsMap);
  end;
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
  end else
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
    const AnimationName: String): Boolean;
begin
  if FAnimationList.IndexOf(AnimationName) > -1 then
  begin
    WritelnWarning('Starling', 'Mixed animations tags (animation: %s) in "%s".', [AnimationName, FDisplayURL]);
    Exit(false);
  end;

  FAnimationList.Add(AnimationName);
  Result := true;
end;


constructor TStarlingTextureAtlasLoader.Create(const URL: String);
begin
  FURL := URL;
  FDisplayURL := URIDisplay(FURL);

  FSubTexture := TStarlingSubTexture.Create;
  SetLength(FCoordArray, 6);
  SetLength(FTexCoordArray, 6);
  FAnimationList := TStringList.Create;
end;

function TStarlingTextureAtlasLoader.Load: TX3DRootNode;
var
  Doc: TXMLDocument;
  Node: TDOMNode;
  AtlasNode: TDOMElement;
  I: TXMLElementIterator;
  LastAnimationName: String;
  CurrentAnimFrameCount: Integer;
  TimeSensor: TTimeSensorNode;
  CoordInterp: TCoordinateInterpolatorNode;
  TexCoordInterp: TCoordinateInterpolator2DNode;
  FirstFrameInFirstAnimation: Boolean;
begin
  ReadImportSettings;

  FRoot := nil;
  Doc := nil;
  try
    try
      FRoot := TX3DRootNode.Create;
      Doc := URLReadXML(FURL);
      Node := Doc.FindNode('TextureAtlas');
      if (Node = nil) or (Node.NodeType <> ELEMENT_NODE) then
        raise EInvalidStarlingXml.CreateFmt('Invalid Starling XML file "%s" - TextureAtlas node not found.', [FDisplayURL]);
      AtlasNode := Node as TDOMElement;

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
          end else
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

procedure TStarlingTextureAtlasLoader.TStarlingSubTexture.PrepareTexCordsForX3D(
    ImageWidth, ImageHeight: Integer);
begin
  { The input data (X1, Y1) are the coordinates in the texture.
    We need those coordinates to compute X2, Y2 and after that we
    recalculate X1, X2 for X3D. }
  X2 := 1 / ImageWidth * (X1 + Width);
  Y2 := 1 - 1 / ImageHeight * (Y1 + Height);

  X1 := 1 / ImageWidth * X1;
  Y1 := 1 - 1 / ImageHeight * Y1;
end;

procedure TStarlingTextureAtlasLoader.TStarlingSubTexture.ParseAnimationName(
  const SubTextureName: String);
var
  UnderscorePos: SizeInt;
  FrameNumberStr: String;
  FrameNumber: Integer;
  AnimationNameLength: Integer;
begin
  { Some files has file type Extensions like "walk/0001.png" or walk_1.png }
  AnimationName := DeleteFileExt(SubTextureName);

  case FAnimationNaming of
    anStrictUnderscore:
      begin
        AnimationNameLength := Length(AnimationName);
        UnderscorePos := rpos('_', AnimationName);
        { Check characters after underscore is number if not  }
        if (UnderscorePos > 0) and (AnimationNameLength > UnderscorePos) then
        begin
          FrameNumberStr := Copy(AnimationName, UnderscorePos + 1, AnimationNameLength - UnderscorePos);
          if TryStrToInt(FrameNumberStr,  FrameNumber) then
          begin
            Delete(AnimationName, UnderscorePos, AnimationNameLength - UnderscorePos + 1);
          end;
        end;
      end;
    anTralingNumber:
      begin
        RemoveTrailingChars(AnimationName, ['0'..'9']);

        AnimationNameLength := Length(AnimationName);
        if (Length(AnimationName) > 1) and (AnimationName[AnimationNameLength] = '_') then
          delete(AnimationName, AnimationNameLength, 1);
      end;
  end;

  if AnimationName = '' then
  begin
    WritelnWarning('Starling', 'Incorrect animation name (%s), I set to "unknown"',
    [SubTextureName]);
    AnimationName := 'unknown';
  end;
end;

procedure TStarlingTextureAtlasLoader.TStarlingSubTexture.ReadFormXMLNode(
    const SubTextureNode: TDOMElement; const ImageWidth, ImageHeight: Integer);
var
  FrameXTrimOffset: Integer;
  FrameYTrimOffset: Integer;
  FullFrameWidth: Integer;
  FullFrameHeight: Integer;
  Trimmed: Boolean;
  FrameAnchorX: Integer;
  FrameAnchorY: Integer;
begin
  ParseAnimationName(SubTextureNode.AttributeString('name'));

  X1 := SubTextureNode.AttributeInteger('x');
  Y1 := SubTextureNode.AttributeInteger('y');
  Width := SubTextureNode.AttributeInteger('width');
  Height := SubTextureNode.AttributeInteger('height');

  Trimmed := SubTextureNode.HasAttribute('frameX');
  if Trimmed then
  begin
    { When frame is trimmed Width and Height does not mean the full size
      of the frame, so we have to calculate the appropriate
      anchor to get the correct position because it will not be (0.5, 0.5) }

    FrameXTrimOffset := SubTextureNode.AttributeIntegerDef('frameX', 0);
    FrameYTrimOffset := SubTextureNode.AttributeIntegerDef('frameY', 0);
    FullFrameWidth := SubTextureNode.AttributeIntegerDef('frameWidth', Width);
    FullFrameHeight := SubTextureNode.AttributeIntegerDef('frameHeight', Height);

    { Anchor in pixels (Without translation to correct texture point
      because we don't need that. Just add X1, Y1 to have correct position.) }
    FrameAnchorX := FullFrameWidth div 2 + FrameXTrimOffset;
    FrameAnchorY := FullFrameHeight div 2 + FrameYTrimOffset;

    { Convert to 0.0..1.0 coordinate system }
    AnchorX := 1 / Width * FrameAnchorX;
    AnchorY := 1 / Height * FrameAnchorY;
  end else
  begin
    AnchorX := 0.5;
    AnchorY := 0.5;
  end;

  PrepareTexCordsForX3D(ImageWidth, ImageHeight);
end;

constructor TStarlingTextureAtlasLoader.TStarlingSubTexture.Create;
begin
  FAnimationNaming := anStrictUnderscore;
end;


end.

