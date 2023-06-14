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
{ Cocos2d animations loader.

  Spec:
  https://docs.cocos2d-x.org/cocos2d-x/v3/en/sprites/spritesheets.html
  https://titanwolf.org/Network/Articles/Article?AID=557a2a17-0790-4c1a-bc16-96221a21aec1#gsc.tab=0
  https://www.codeandweb.com/blog/2016/01/29/cocos2d-plist-format-explained

}
 unit X3DLoadInternalCocos2d;

{$I castleconf.inc}

interface

uses Classes, SysUtils,
  X3DNodes;

type
  { Cocos2d XML file is not correct }
  EInvalidCocos2dPlist = class(Exception);

function LoadCocos2d(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

implementation

uses Generics.Collections, StrUtils, DOM, XMLRead,
  CastleImages, CastleLog, CastleStringUtils, CastleTextureImages,
  CastleURIUtils, CastleUtils, CastleVectors, CastleXMLUtils;

type

  TCocos2dLoader = class
  strict private
    type
      { Class that represents frame from Cocos2d file }
      TCocosFrame = class
      private
        FDisplayUrl: String;
        FCocosFormat: Integer;

        FParseFrameDictionary: procedure (const DictNode: TDOMElement) of object;

        procedure PrepareTexCordsForX3D(const ImageWidth, ImageHeight: Integer);
        procedure ParseAnimationName(const FrameFileName: String);
        procedure ParseFrameDictionaryFormat2(const DictNode: TDOMElement);
        procedure ParseFrameDictionaryFormat3(const DictNode: TDOMElement);
      public
        AnimationName: String;
        { Texture coordinates. Before PrepareTexCordsForX3D() they are in pixels,
          after PrepareTexCordsForX3D() they are just UV for X3D }
        X1: Single;
        Y1: Single;
        X2: Single;
        Y2: Single;
        { Width of sprite. If sprite is trimmed this is trimmed width - not full frame width. }
        Width: Integer;
        { Height of sprite. If sprite is trimmed this is trimmed height - not full frame height. }
        Height: Integer;
        AnchorX: Single;
        AnchorY: Single;

        constructor Create(const DisplayUrl: String);
        { We support format version 2 and 3. This procedure sets suitable
          ParseFrameDictionaryFormatX procedure. }
        procedure SetCocosFormat(const Format: Integer);

        procedure ReadFormDict(const KeyNode, DictNode: TDOMElement;
          const ImageWidth, ImageHeight: Integer);
        class function ReadDual(const ASrc: String; out V1, V2: Integer): Boolean; overload;
        class function ReadDual(const ASrc: String; out V1, V2: Single): Boolean; overload;
        class function ReadQuad(const ASrc: String; out V1, V2, V3, V4: Integer): Boolean;
      end;
    var
      FStream: TStream;
      FBaseUrl: String;
      FDisplayUrl: String;

      { Load settings. }
      FFramesPerSecond: Single;

      FImageWidth, FImageHeight: Integer;
      FImagePath: String;

      { We currently support the format version 2 and 3. The frames have
        different parameters depending on the version. }
      FCocosFormat: Integer;

      FCocosFrame: TCocosFrame;

      FRoot: TX3DRootNode;
      FShapeCoord: TCoordinateNode;
      FShapeTexCoord: TTextureCoordinateNode;

      FCoordArray: array of TVector3;
      FTexCoordArray: array of TVector2;

      { Animation list to check if the file has any mixed SubTexture nodes. }
      FAnimationList: TStringList;

    procedure ReadImportSettings;

    procedure PrepareX3DRoot;

    procedure ReadMetadata(const MetadataNode: TDOMElement);

    procedure ReadFrames(const MetadataNode: TDOMElement);

    procedure CalculateFrameCoords(const CocosFrame: TCocosFrame);

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
    constructor Create(const Stream: TStream; const BaseUrl: String);
    destructor Destroy; override;

    function Load: TX3DRootNode;
  end;

function LoadCocos2d(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  Cocos2dLoader: TCocos2dLoader;
begin
  Cocos2dLoader := TCocos2dLoader.Create(Stream, BaseUrl);
  try
    Result := Cocos2dLoader.Load;
  finally
    FreeAndNil(Cocos2dLoader);
  end;
end;

{ TCocos2dLoader.TCocosFrame ------------------------------------------------ }

procedure TCocos2dLoader.TCocosFrame.PrepareTexCordsForX3D(const ImageWidth,
  ImageHeight: Integer);
begin
  { The input data (X1, Y1) are the coordinates in the texture.
    We need those coordinates to compute X2, Y2 and after that we
    recalculate X1, X2 for X3D. }
  X2 := 1 / ImageWidth * (X1 + Width);
  Y2 := 1 - 1 / ImageHeight * (Y1 + Height);

  X1 := 1 / ImageWidth * X1;
  Y1 := 1 - 1 / ImageHeight * Y1;
end;

procedure TCocos2dLoader.TCocosFrame.ParseAnimationName(const FrameFileName: String);
begin
  { Some times this names can be like "walk/0001.png" }
  AnimationName := DeleteFileExt(FrameFileName);

  {$ifdef FPC}
  RemoveTrailingChars(AnimationName, ['0'..'9']);
  {$else}
  AnimationName := AnimationName.TrimRight(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
  {$endif}

  if (Length(AnimationName) > 1) and ((AnimationName[Length(AnimationName)] = '_')
    or (AnimationName[Length(AnimationName)] = '-')
    or (AnimationName[Length(AnimationName)] = '/')
    or (AnimationName[Length(AnimationName)] = '\')) then
  delete(AnimationName, Length(AnimationName), 1);

  if AnimationName = '' then
  begin
    WritelnWarning('Cocos2d',
      'Incorrect animation name (%s), setting the name to "unknown"',
      [FrameFileName]);
    AnimationName := 'unknown';
  end;
end;

procedure TCocos2dLoader.TCocosFrame.ParseFrameDictionaryFormat2(const DictNode: TDOMElement);
var
  I: TXMLElementIterator;
  KeyNode, ValueNode: TDOMElement;

  X: Integer;
  Y: Integer;
  FrameXTrimed: Integer;
  FrameYTrimed: Integer;
  FrameWidthTrimed: Integer;
  FrameHeightTrimed: Integer;
  Trimmed: Boolean;
  FullFrameWidth: Integer;
  FullFrameHeight: Integer;

  HasAnchor: Boolean;
  FrameAnchorX: Integer;
  FrameAnchorY: Integer;

  WasFrame: Boolean;
  WasFrameSize: Boolean;
begin
  HasAnchor := false;
  WasFrame := false;
  WasFrameSize := false;
  Trimmed := false;
  I := DictNode.ChildrenIterator;
  try
    while I.GetNext do
    begin
      KeyNode := I.Current;
      if KeyNode.NodeName <> 'key' then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - key node expected.', [FDisplayUrl]);

      if not I.GetNext then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - value node expected.', [FDisplayUrl]);

      ValueNode := I.Current;

      if KeyNode.TextData = 'frame' then
      begin
        { Sprite position and size in the texture - the same as textureRect in format 3 }
        if ReadQuad(ValueNode.TextData, X, Y, Width, Height) then
        begin
          X1 := X;
          Y1 := Y;
          WasFrame := true;
        end;
      end else
      if KeyNode.TextData = 'sourceColorRect' then
      begin
        { rect of the trimmed sprite }
        ReadQuad(ValueNode.TextData, FrameXTrimed, FrameYTrimed, FrameWidthTrimed, FrameHeightTrimed);
        Trimmed := true;
      end else
      if KeyNode.TextData = 'anchor' then
      begin
        { Anchor point for the sprite in coordinates relative to the original sprite size }
        if ReadDual(ValueNode.TextData, AnchorX, AnchorY) then
          HasAnchor := true;
      end else
      if KeyNode.TextData = 'sourceSize' then
      begin
        { full size of the sprite, the same as spriteSourceSize in format 3 }
        if ReadDual(ValueNode.TextData, FullFrameWidth, FullFrameHeight) then
          WasFrameSize := true;
      end;
    end;
  finally
    FreeAndNil(I);
  end;

  if not WasFrameSize or not WasFrame then
    raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - frame data incomplete.', [FDisplayUrl]);

  if not HasAnchor then
  begin
    { When frame is trimmed Width and Height does not mean the full size
      of the frame, so we have to calculate the appropriate
      anchor to get the correct position because it will not be (0.5, 0.5) }
    if Trimmed then
    begin
      { Anchor in pixels (Without translation to correct texture point
        because we don't need that. Just add X1, Y1 to have correct position.) }
      FrameAnchorX := FullFrameWidth div 2 - FrameXTrimed;
      FrameAnchorY := FullFrameHeight div 2 - FrameYTrimed;

      { Convert to 0.0..1.0 coordinate system }
      AnchorX := 1 / Width * FrameAnchorX;
      AnchorY := 1 / Height * FrameAnchorY;
    end else
    begin
      AnchorX := 0.5;
      AnchorY := 0.5;
    end;
  end else
  begin
    { TODO: need example with anchors, in spritesheet to x3d in this case
      nothing is done but I haven't validated that }
  end;
end;

procedure TCocos2dLoader.TCocosFrame.ParseFrameDictionaryFormat3(
  const DictNode: TDOMElement);
var
  I: TXMLElementIterator;
  KeyNode, ValueNode: TDOMElement;

  X: Integer;
  Y: Integer;
  FrameXTrimOffset: Single;
  FrameYTrimOffset: Single;
  Trimmed: Boolean;
  FullFrameWidth: Integer;
  FullFrameHeight: Integer;

  HasAnchor: Boolean;
  FrameAnchorX: Single;
  FrameAnchorY: Single;

  WasTextureFrame: Boolean;
  WasFrameFullSize: Boolean;
begin
  HasAnchor := false;
  WasTextureFrame := false;
  WasFrameFullSize := false;
  Trimmed := false;
  I := DictNode.ChildrenIterator;
  try
    while I.GetNext do
    begin
      KeyNode := I.Current;
      if KeyNode.NodeName <> 'key' then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - key node expected.', [FDisplayUrl]);

      if not I.GetNext then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - value node expected.', [FDisplayUrl]);

      ValueNode := I.Current;

      if KeyNode.TextData = 'textureRect' then
      begin
        { Sprite position and size in the texture - the same as frame in format 2 }
        if ReadQuad(ValueNode.TextData, X, Y, Width, Height) then
        begin
          X1 := X;
          Y1 := Y;
          WasTextureFrame := true;
        end;
      end else
      if KeyNode.TextData = 'spriteOffset' then
      begin
        { offset in sprite }
        ReadDual(ValueNode.TextData, FrameXTrimOffset, FrameYTrimOffset);
        Trimmed := true;
      end else
      if KeyNode.TextData = 'anchor' then
      begin
        { Anchor point for the sprite in coordinates relative to the original sprite size }
        if ReadDual(ValueNode.TextData, AnchorX, AnchorY) then
          HasAnchor := true;
      end else
      if KeyNode.TextData = 'spriteSourceSize' then
      begin
        { full size of the sprite, the same as sourceSize in format 2 }
        if ReadDual(ValueNode.TextData, FullFrameWidth, FullFrameHeight) then
          WasFrameFullSize := true;
      end;
    end;
  finally
    FreeAndNil(I);
  end;

  if not WasTextureFrame or not WasFrameFullSize then
    raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - frame data incomplete.', [FDisplayUrl]);

  X1 := X;
  Y1 := Y;

  if not HasAnchor then
  begin
    { When frame is trimmed Width and Height does not mean the full size
      of the frame, so we have to calculate the appropriate
      anchor to get the correct position because it will not be (0.5, 0.5) }
    if Trimmed then
    begin
      { Anchor in pixels (Without translation to correct texture point
        because we don't need that. Just add X1, Y1 to have correct position.) }
      FrameAnchorX := FullFrameWidth / 2 - FrameXTrimOffset;
      FrameAnchorY := FullFrameHeight / 2 - FrameYTrimOffset;

      { Convert to 0.0..1.0 coordinate system }
      AnchorX := 1 / Width * FrameAnchorX;
      AnchorY := 1 / Height * FrameAnchorY;
    end else
    begin
      AnchorX := 0.5;
      AnchorY := 0.5;
    end;
  end else
  begin
    { TODO: need example with anchors, in spritesheet-to-x3d in this case
      nothing is done but I haven't validated that }
  end;
end;

constructor TCocos2dLoader.TCocosFrame.Create(const DisplayUrl: String);
begin
  inherited Create;
  FDisplayUrl := DisplayUrl;
  { By default, use format 3 }
  FParseFrameDictionary := {$ifdef FPC}@{$endif}ParseFrameDictionaryFormat3;
end;

procedure TCocos2dLoader.TCocosFrame.SetCocosFormat(const Format: Integer);
begin
  FCocosFormat := Format;
  case Format of
    2:
      FParseFrameDictionary := {$ifdef FPC}@{$endif}ParseFrameDictionaryFormat2;
    3:
      FParseFrameDictionary := {$ifdef FPC}@{$endif}ParseFrameDictionaryFormat3;
    else
      { If format is unsupported try use latest supported version and add warning. }
      FParseFrameDictionary := {$ifdef FPC}@{$endif}ParseFrameDictionaryFormat3;
      WritelnWarning('Cocos2d',
        'Unsupported format version %d in "%s", trying to load with the latest importer (format = 3).',
        [Format, FDisplayUrl]);
  end;
end;

procedure TCocos2dLoader.TCocosFrame.ReadFormDict(const KeyNode, DictNode: TDOMElement;
  const ImageWidth, ImageHeight: Integer);
begin
  ParseAnimationName(KeyNode.TextData);
  FParseFrameDictionary(DictNode);
  PrepareTexCordsForX3D(ImageWidth, ImageHeight);
end;

class function TCocos2dLoader.TCocosFrame.ReadDual(const ASrc: String; out V1,
  V2: Integer): Boolean;
var
  OpenBracePos: Integer;
  CloseBracePos: Integer;
  CommaPos: Integer;
begin
  OpenBracePos := Pos('{', ASrc);
  if OpenBracePos = 0 then
    Exit(false);

  CloseBracePos := PosEx('}', ASrc, OpenBracePos);
  if CloseBracePos = 0 then
    Exit(false);

  CommaPos := PosEx(',', ASrc, OpenBracePos);
  if CommaPos = 0 then
    Exit(false);

  V1 := StrToInt(Copy(ASrc, OpenBracePos + 1, CommaPos - (OpenBracePos + 1)));
  V2 := StrToInt(Copy(ASrc, CommaPos + 1, CloseBracePos - (CommaPos + 1)));
  Result := true;
end;

class function TCocos2dLoader.TCocosFrame.ReadDual(const ASrc: String; out V1,
  V2: Single): Boolean;
var
  OpenBracePos: Integer;
  CloseBracePos: Integer;
  CommaPos: Integer;
begin
  OpenBracePos := Pos('{', ASrc);
  if OpenBracePos = 0 then
    Exit(false);

  CloseBracePos := PosEx('}', ASrc, OpenBracePos);
  if CloseBracePos = 0 then
    Exit(false);

  CommaPos := PosEx(',', ASrc, OpenBracePos);
  if CommaPos = 0 then
    Exit(false);

  V1 := StrToFloatDot(Copy(ASrc, OpenBracePos + 1, CommaPos - (OpenBracePos + 1)));
  V2 := StrToFloatDot(Copy(ASrc, CommaPos + 1, CloseBracePos - (CommaPos + 1)));
  Result := true;
end;


class function TCocos2dLoader.TCocosFrame.ReadQuad(const ASrc: String; out V1,
  V2, V3, V4: Integer): Boolean;
var
  OpenBracePos: Integer;
  CloseBracePos: Integer;
begin
  OpenBracePos := Pos('{', ASrc);
  if OpenBracePos = 0 then
    Exit(false);

  CloseBracePos := Pos('}', ASrc);
  if CloseBracePos = 0 then
    Exit(false);

  if not ReadDual(Copy(ASrc, OpenBracePos + 1, CloseBracePos - OpenBracePos), V1, V2) then
    Exit(false);

  OpenBracePos := PosEx('{', ASrc, CloseBracePos);
  if OpenBracePos = 0 then
    Exit(false);

  CloseBracePos := PosEx('}', ASrc, CloseBracePos + 1);
  if CloseBracePos = 0 then
    Exit(false);

  if not ReadDual(Copy(ASrc, OpenBracePos, CloseBracePos - OpenBracePos + 1), V3, V4) then
    Exit(false);

  Result := true;
end;

{ TCocos2dLoader ------------------------------------------------------------ }

procedure TCocos2dLoader.ReadImportSettings;
var
  SettingsMap: TStringStringMap;
  Setting: {$ifdef FPC}TStringStringMap.TDictionaryPair{$else}TPair<String, String>{$endif};
begin
  // default values
  FFramesPerSecond := DefaultSpriteSheetFramesPerSecond;

  SettingsMap := TStringStringMap.Create;
  try
    URIGetSettingsFromAnchor(FBaseUrl, SettingsMap);
    for Setting in SettingsMap do
    begin
      if LowerCase(Setting.Key) = 'fps' then
      begin
        FFramesPerSecond := StrToFloatDot(Setting.Value);
      end else
        WritelnWarning('Starling', 'Unknown setting (%s) in "%s" anchor.',
          [Setting.Key, FDisplayUrl]);
    end;
  finally
    FreeAndNil(SettingsMap);
  end;
end;


procedure TCocos2dLoader.PrepareX3DRoot;
begin
  FRoot.Meta['generator'] := 'Castle Game Engine, https://castle-engine.io';
  FRoot.Meta['source'] := ExtractURIName(FBaseUrl);
end;

procedure TCocos2dLoader.ReadMetadata(const MetadataNode: TDOMElement);
var
  I: TXMLElementIterator;
  KeyNode: TDOMElement;
  ValueNode: TDOMElement;
  RealTextureFileName: String;
  TextureFileName: String;
  Image: TCastleImage;
begin
  I := MetadataNode.ChildrenIterator;
  try
    while I.GetNext do
    begin
      KeyNode := I.Current;
      if KeyNode.NodeName <> 'key' then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - key node expected.', [FDisplayUrl]);

      if not I.GetNext then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - value node expected.', [FDisplayUrl]);

      ValueNode := I.Current;

      if KeyNode.TextData = 'realTextureFileName' then
          RealTextureFileName := ValueNode.TextData
      else
      if KeyNode.TextData = 'textureFileName' then
        TextureFileName := ValueNode.TextData
      else
      if KeyNode.TextData = 'size' then
      begin
        if not TCocosFrame.ReadDual(ValueNode.TextData, FImageWidth, FImageHeight) then
          raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - invalid size.', [FDisplayUrl]);
      end else
      if KeyNode.TextData = 'format' then
        FCocosFormat := StrToInt(ValueNode.TextData)
      else
        continue;
    end;
  finally
    FreeAndNil(I);
  end;

  { In correct files texture file name should be in realTextureFileName,
    but there are a lot files with name in textureFileName which has often
    file name without extension when realTextureFileName exists. }
  if RealTextureFileName <> '' then
    FImagePath := ExtractURIPath(FBaseUrl) + RealTextureFileName
  else
    FImagePath := ExtractURIPath(FBaseUrl) + TextureFileName;

  { This should never be needed. }
  if (FImageWidth = 0) or (FImageHeight = 0) then
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

procedure TCocos2dLoader.ReadFrames(const MetadataNode: TDOMElement);
var
  I: TXMLElementIterator;
  KeyNode: TDOMElement;
  DictNode: TDOMElement;

  LastAnimationName: String;
  CurrentAnimFrameCount: Integer;
  TimeSensor: TTimeSensorNode;
  CoordInterp: TCoordinateInterpolatorNode;
  TexCoordInterp: TCoordinateInterpolator2DNode;
  FirstFrameInFirstAnimation: Boolean;
begin
  CurrentAnimFrameCount := 0;
  FirstFrameInFirstAnimation := true;
  LastAnimationName := '';
  TexCoordInterp := nil;
  CoordInterp := nil;
  TimeSensor := nil;

  I := MetadataNode.ChildrenIterator;
  try
    while I.GetNext do
    begin
      KeyNode := I.Current;
      if KeyNode.NodeName <> 'key' then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - key node expected.', [FDisplayUrl]);

      if not I.GetNext then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - dict node expected.', [FDisplayUrl]);

      DictNode := I.Current;
      if DictNode.NodeName <> 'dict' then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - dict node expected.', [FDisplayUrl]);

      { Read frame from plist }
      FCocosFrame.ReadFormDict(KeyNode, DictNode, FImageWidth, FImageHeight);

      CalculateFrameCoords(FCocosFrame);
      { After calculate first frame cords and tex cord we need create shape. }
      if FirstFrameInFirstAnimation then
      begin
        PrepareShape(FCoordArray, FTexCoordArray);
        FirstFrameInFirstAnimation := false;
      end;

      if LastAnimationName <> FCocosFrame.AnimationName then
      begin
        { First frame of animation loaded. }

        if CurrentAnimFrameCount > 0 then
          AddAnimation(CurrentAnimFrameCount, TimeSensor, CoordInterp, TexCoordInterp);

        if not CheckAnimationNameAvailable(FCocosFrame.AnimationName) then
        begin
          CurrentAnimFrameCount := 0;
          Continue;
        end;

        { Reset variables for new animation }
        LastAnimationName := FCocosFrame.AnimationName;
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

  finally
    FreeAndNil(I);
  end;

end;

procedure TCocos2dLoader.CalculateFrameCoords(const CocosFrame: TCocosFrame);
begin
  FCoordArray[0] := Vector3(-CocosFrame.Width * (CocosFrame.AnchorX),
      CocosFrame.Height * (CocosFrame.AnchorY), 0);

  FCoordArray[1] := Vector3(CocosFrame.Width * (1 - CocosFrame.AnchorX),
      CocosFrame.Height * (CocosFrame.AnchorY), 0);

  FCoordArray[2] := Vector3(CocosFrame.Width * (1 - CocosFrame.AnchorX),
      -CocosFrame.Height * (1 - CocosFrame.AnchorY), 0);

  FCoordArray[3] := Vector3(-CocosFrame.Width * CocosFrame.AnchorX,
      CocosFrame.Height * CocosFrame.AnchorY, 0);

  FCoordArray[4] := Vector3(CocosFrame.Width * (1 - CocosFrame.AnchorX),
      -CocosFrame.Height * (1 - CocosFrame.AnchorY), 0);

  FCoordArray[5] := Vector3(-CocosFrame.Width * CocosFrame.AnchorX,
      -CocosFrame.Height * (1 - CocosFrame.AnchorY), 0);

  FTexCoordArray[0] := Vector2(CocosFrame.X1, CocosFrame.Y1);
  FTexCoordArray[1] := Vector2(CocosFrame.X2, CocosFrame.Y1);
  FTexCoordArray[2] := Vector2(CocosFrame.X2, CocosFrame.Y2);
  FTexCoordArray[3] := Vector2(CocosFrame.X1, CocosFrame.Y1);
  FTexCoordArray[4] := Vector2(CocosFrame.X2, CocosFrame.Y2);
  FTexCoordArray[5] := Vector2(CocosFrame.X1, CocosFrame.Y2);
end;

procedure TCocos2dLoader.PrepareShape(const CoordArray: array of TVector3;
  const TexCoordArray: array of TVector2);
var
  Shape: TShapeNode;
  Appearance: TAppearanceNode;
  Material: TUnlitMaterialNode;
  Tri: TTriangleSetNode;
  Tex: TImageTextureNode;
  TexProperties: TTexturePropertiesNode;
begin
  Material := TUnlitMaterialNode.Create;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Shape := TShapeNode.Create;
  Shape.Appearance := Appearance;

  Tex := TImageTextureNode.Create;
  Tex.FdUrl.Send(FImagePath);
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
    may cause longer loading time, and loss of quality, if not expected).
    See https://github.com/castle-engine/castle-engine/issues/249 }
  TexProperties.GuiTexture := true;
  Tex.TextureProperties := TexProperties;

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

procedure TCocos2dLoader.AddFrameCoords(
  const CoordInterp: TCoordinateInterpolatorNode;
  const TexCoordInterp: TCoordinateInterpolator2DNode);
begin
  CoordInterp.FdKeyValue.Items.AddRange(FCoordArray);
  TexCoordInterp.FdKeyValue.Items.AddRange(FTexCoordArray);
  { Repeat all keyValues, to avoid interpolating them smoothly between two keys }
  CoordInterp.FdKeyValue.Items.AddRange(FCoordArray);
  TexCoordInterp.FdKeyValue.Items.AddRange(FTexCoordArray);
end;

procedure TCocos2dLoader.AddAnimation(const FrameCount: Integer;
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

procedure TCocos2dLoader.AddRoutes(const TimeSensor: TTimeSensorNode;
  const CoordInterp: TCoordinateInterpolatorNode;
  const TexCoordInterp: TCoordinateInterpolator2DNode);
begin
  { Create routes. }
  FRoot.AddRoute(TimeSensor.EventFraction_changed, CoordInterp.EventSet_fraction);
  FRoot.AddRoute(TimeSensor.EventFraction_changed, TexCoordInterp.EventSet_fraction);
  FRoot.AddRoute(CoordInterp.EventValue_changed, FShapeCoord.FdPoint);
  FRoot.AddRoute(TexCoordInterp.EventValue_changed, FShapeTexCoord.FdPoint);
end;

function TCocos2dLoader.CheckAnimationNameAvailable(
  const AnimationName: String): Boolean;
begin
  if FAnimationList.IndexOf(AnimationName) > -1 then
  begin
    WritelnWarning('Starling', 'Mixed animations tags (animation: %s) in "%s".',
      [AnimationName, FDisplayUrl]);
    Exit(false);
  end;

  FAnimationList.Add(AnimationName);
  Result := true;
end;

constructor TCocos2dLoader.Create(const Stream: TStream; const BaseUrl: String);
begin
  inherited Create;
  FStream := Stream;
  FBaseUrl := BaseUrl;
  FDisplayUrl := URIDisplay(FBaseUrl);

  FCocosFrame := TCocosFrame.Create(FDisplayUrl);
  SetLength(FCoordArray, 6);
  SetLength(FTexCoordArray, 6);
  FAnimationList := TStringList.Create;
end;

destructor TCocos2dLoader.Destroy;
begin
  FreeAndNil(FCocosFrame);
  FreeAndNil(FAnimationList);
  inherited Destroy;
end;

function TCocos2dLoader.Load: TX3DRootNode;
var
  Doc: TXMLDocument;
  Node: TDOMNode;
  PlistNode: TDOMElement;
  PlistDictNode: TDOMElement;
  FramesDictNode: TDOMElement;  // available on format ver 1, 2, 3
  MetadataDictNode: TDOMElement; // available on format ver 2, 3
  TextureDictNode: TDOMElement; // available on format 1

  KeyNode: TDOMElement;
  DictNode: TDOMElement;

  I: TXMLElementIterator;
begin
  ReadImportSettings;

  FRoot := nil;
  Doc := nil;
  PlistNode := nil;
  FramesDictNode := nil;
  MetadataDictNode := nil;
  TextureDictNode := nil;
  try
    try
      FRoot := TX3DRootNode.Create;
      ReadXMLFile(Doc, FStream);

      { Doc.FindNode('plist') can fail here because of plist Apple DOCTYPE
        above plist element. }
      {$ifdef FPC}
      Node := Doc.FirstChild;
      while Node <> nil do
      begin
        if (Node.NodeName = 'plist') and (Node.NodeType = ELEMENT_NODE) then
        begin
          PlistNode := Node as TDOMElement;
          break;
        end;
        Node := Node.NextSibling;
      end;
      {$else}
        // Should work in delphi because current we have only one element in document
        Node := Doc.DocumentElement;
        if (Node.NodeName = 'plist') and (Node.NodeType = ELEMENT_NODE) then
          PlistNode := Node as TDOMElement;
      {$endif}

      if PlistNode = nil then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - plist node not found.', [FDisplayUrl]);

      { With Required = false just return nil, because we want to raise a nicer
        exception. }
      PlistDictNode := PlistNode.Child('dict', false);
      if (PlistDictNode = nil) then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - plist dictionary node not found.', [FDisplayUrl]);

      { Iterate file to find frames, metadata, textures nodes }
      I := PlistDictNode.ChildrenIterator;
      try
        while I.GetNext do
        begin
          { Should be key node. }
          KeyNode := I.Current;
          if KeyNode.NodeName <> 'key' then
            raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - key node expected.', [FDisplayUrl]);

          if not I.GetNext then
            raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - dict node expected.', [FDisplayUrl]);

          DictNode := I.Current;

          if DictNode.NodeName <> 'dict' then
            raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - dict node expected.', [FDisplayUrl]);

          { Check KeyNode.TextData to decide what this is metadata, texture or frames}
          if KeyNode.TextData = 'frames' then
            FramesDictNode := DictNode
          else
          if KeyNode.TextData = 'textures' then
            TextureDictNode := DictNode
          else
          if KeyNode.TextData = 'metadata' then
            MetadataDictNode := DictNode;
        end;
      finally
        FreeAndNil(I);
      end;

      { TextureDictNode exists only in Cocos2d files version 1 which are
        outdated/unsupported. We can't simply check metadata because version 1
        don't have metadata dictionary. Frames also have different structure.
        The files of this version (1) are so outdated that nobody will ever need
        to use them (I think). }
      if TextureDictNode <> nil then
        raise EInvalidCocos2dPlist.CreateFmt('Cocos2d plist file "%s" - file version 1 is unsupported.', [FDisplayUrl]);

      { First read metadata - texture properties }
      if MetadataDictNode = nil then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - metadata dict node not found.', [FDisplayUrl]);

      ReadMetadata(MetadataDictNode);

      { Now when we know texture properties and format number so we can
        set CocoSFrame to use suitable ParseFrame procedure. }
      FCocosFrame.SetCocosFormat(FCocosFormat);

      { Now we can read frames }
      if FramesDictNode = nil then
        raise EInvalidCocos2dPlist.CreateFmt('Invalid Cocos2d plist file "%s" - frames dict node not found.', [FDisplayUrl]);

      ReadFrames(FramesDictNode);

      Result := FRoot;
    except
      FreeAndNil(FRoot);
      raise;
    end;
  finally
    FreeAndNil(Doc);
  end;
end;

end.
