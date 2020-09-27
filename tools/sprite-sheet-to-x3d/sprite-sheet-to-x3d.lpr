{
  Copyright 2017-2018 Trung Le (kagamma).

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert spritesheets to classic X3D files. }

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

{$I castleconf.inc}

uses Classes, SysUtils, strutils, DOM, RegExpr, Generics.Collections,
  CastleParameters, CastleImages, CastleStringUtils,
  CastleVectors, CastleUtils, CastleClassUtils, X3DNodes,
  CastleTextureImages, CastleXMLUtils;

type
  TMeta = record
    Name: string;       { Image name }
    W, H: integer;      { Image size }
  end;

  TFrame = record
    X1, Y1,             { Texture coords }
    X2, Y2: single;
    W, H  : integer;    { Frame size }
    AX, AY: single;     { Anchor }
  end;

  TFrameList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TFrame>;
  TAnimations = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<string, TFrameList>)
    destructor Destroy; override;
  end;

var
  SSFullPath,
  SSPath,
  SSExt,
  SSName,
  SSOutput: string;
  Animations: TAnimations;
  FramesPerSecond: Single = 4.0;
  Meta: TMeta;

destructor TAnimations.Destroy;
var
  F: TFrameList;
begin
  for F in Values do
    F.Free;
  inherited;
end;

{ Parse a string with XXX_YYY format, XXX is the name of the frame and YYY is
  frame number. Here we only need XXX part so that it can be used as animation
  name (key). }
procedure KeyParser(const AName: string; out AKey: string);
var
  R: TRegExpr;
begin
  R := TRegExpr.Create;
  try
    R.Expression := '(.*)_(.*)';
    if R.Exec(AName) and (R.SubExprMatchCount = 2) then
    begin
      AKey := R.Match[1];
    end;
  finally
    FreeAndNil(R);
  end;
end;

{ Add a frame to frame list, it will try to find frame list based on key,
  in case there's no key in dictionary, a new frame list with that key will
  be created. }
procedure AddFrame(const AKey: string; const AFrame: TFrame);
var
  List: TFrameList;
begin
  if not Animations.TryGetValue(AKey, List) then
  begin
    List := TFrameList.Create;
    Animations.Add(AKey, List);
  end;
  List.Add(AFrame);
end;

{ Read texture altas's width and height. }
procedure ReadMeta(var AMeta: TMeta);
var
  Image: TCastleImage;
begin
  Image := LoadImage(SSPath + AMeta.Name);
  try
    AMeta.W := Image.Width;
    AMeta.H := Image.Height;
  finally
    FreeAndNil(Image);
  end;
end;

{$I parser_utils.inc}
{$I starling_parser.inc}
{$I cocos2d_parser.inc}

procedure Parse;
begin
  case LowerCase(SSExt) of
    '.xml':
      begin
        StarlingParser;
      end;
    '.plist':
      begin
        Cocos2DParser;
      end;
    else
      begin
        Writeln(ErrOutput, 'sprite-sheet-to-x3d: Error: Unrecognized input file type: ' + SSExt);
        Halt;
      end;
  end;
end;

procedure Convert;
var
  i, j: integer;
  List: TFrameList;
  AnimationName: string;
  AnimationPair: TAnimations.TDictionaryPair;
  Frame: TFrame;
  Root: TX3DRootNode;
  Shape: TShapeNode;
  Tri: TTriangleSetNode;
  Tex: TImageTextureNode;
  Coord: TCoordinateNode;
  TexCoord: TTextureCoordinateNode;
  TimeSensor: TTimeSensorNode;
  CoordInterp: TCoordinateInterpolatorNode;
  TexCoordInterp: TCoordinateInterpolator2DNode;
  TimeSensorArray: array of TTimeSensorNode;
  CoordInterpArray: array of TCoordinateInterpolatorNode;
  TexCoordInterpArray: array of TCoordinateInterpolator2DNode;
  Key: single;
  CoordArray: array of TVector3;
  TexCoordArray: array of TVector2;
  R1, R2, R3, R4: TX3DRoute;
  Material: TUnlitMaterialNode;
begin
  for List in Animations.Values do
  begin
    { Convert sprite texture coordinates to X3D format. }
    for i := 0 to List.Count-1 do
    begin
      Frame := List[i];
      Frame.X1 := 1 / Meta.W * Frame.X1;
      Frame.Y1 := 1 - 1 / Meta.H * Frame.Y1;
      Frame.X2 := 1 / Meta.W * Frame.X2;
      Frame.Y2 := 1 - 1 / Meta.H * Frame.Y2;
      List[i] := Frame;
    end;
  end;
  Root := TX3DRootNode.Create;
  Root.Meta['generator'] :=
      'sprite-sheet-to-x3d, https://castle-engine.io';
  Root.Meta['source'] := SSName + SSExt;
  try
    Shape:= TShapeNode.Create;
    Material := TUnlitMaterialNode.Create;
    Material.EmissiveColor := Vector3(1, 1, 1);
    Shape.Material := Material;

    Tex := TImageTextureNode.Create;
    Tex.FdUrl.Send(Meta.Name);
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

    SetLength(CoordArray, 6);
    SetLength(TexCoordArray, 6);
    SetLength(TimeSensorArray, Animations.Count);
    SetLength(CoordInterpArray, Animations.Count);
    SetLength(TexCoordInterpArray, Animations.Count);

    j := 0;
    for AnimationPair in Animations do
    begin
      AnimationName := AnimationPair.Key;
      List := AnimationPair.Value;
      TimeSensor := TTimeSensorNode.Create(AnimationName);
      TimeSensor.CycleInterval := List.Count / FramesPerSecond;
      Writeln('Generating animation ' + AnimationName +
        ', frames: ', List.Count,
        ', duration: ', TimeSensor.CycleInterval:1:2);
      CoordInterp :=
          TCoordinateInterpolatorNode.Create(AnimationName + '_Coord');
      TexCoordInterp :=
          TCoordinateInterpolator2DNode.Create(AnimationName + '_TexCoord');
      TimeSensorArray[j] := TimeSensor;
      CoordInterpArray[j] := CoordInterp;
      TexCoordInterpArray[j] := TexCoordInterp;
      { Generate list of keys. }
      for i := 0 to List.Count-1 do
      begin
        Key := I / List.Count;
        CoordInterp.FdKey.Items.Add(Key);
        TexCoordInterp.FdKey.Items.Add(Key);
        if i > 0 then
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
      { Generate list of coord/texcoord key values. }
      for i := 0 to List.Count-1 do
      begin
        Frame := List[i];
        CoordArray[0] := Vector3(
            -Frame.W * (  Frame.AX),  Frame.H * (  Frame.AY), 0);
        CoordArray[1] := Vector3(
             Frame.W * (1-Frame.AX),  Frame.H * (  Frame.AY), 0);
        CoordArray[2] := Vector3(
             Frame.W * (1-Frame.AX), -Frame.H * (1-Frame.AY), 0);
        CoordArray[3] := Vector3(
            -Frame.W * (  Frame.AX),  Frame.H * (  Frame.AY), 0);
        CoordArray[4] := Vector3(
             Frame.W * (1-Frame.AX), -Frame.H * (1-Frame.AY), 0);
        CoordArray[5] := Vector3(
            -Frame.W * (  Frame.AX), -Frame.H * (1-Frame.AY), 0);
        TexCoordArray[0] := Vector2(Frame.X1, Frame.Y1);
        TexCoordArray[1] := Vector2(Frame.X2, Frame.Y1);
        TexCoordArray[2] := Vector2(Frame.X2, Frame.Y2);
        TexCoordArray[3] := Vector2(Frame.X1, Frame.Y1);
        TexCoordArray[4] := Vector2(Frame.X2, Frame.Y2);
        TexCoordArray[5] := Vector2(Frame.X1, Frame.Y2);
        CoordInterp.FdKeyValue.Items.AddRange(CoordArray);
        TexCoordInterp.FdKeyValue.Items.AddRange(TexCoordArray);
        { Repeat all keyValues, to avoid interpolating them smoothly between two keys }
        CoordInterp.FdKeyValue.Items.AddRange(CoordArray);
        TexCoordInterp.FdKeyValue.Items.AddRange(TexCoordArray);
      end;
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
      Inc(j);
    end;
    { Put everything into the scene. }
    Root.AddChildren(Shape);
    for j := 0 to Animations.Count-1 do
      Root.AddChildren(TimeSensorArray[j]);
    for j := 0 to Animations.Count-1 do
    begin
      Root.AddChildren(CoordInterpArray[j]);
      Root.AddChildren(TexCoordInterpArray[j]);
    end;
    Save3D(Root, SSOutput);
  finally
    FreeAndNil(Root);
  end;
end;

const
  Options: array [0..2] of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short:  #0; Long: 'fps'; Argument: oaRequired)
  );

  HelpText =
    'sprite-sheet-to-x3d: Convert spritesheet files into X3D files.' + NL +
    NL +
    'Usage:' + NL +
    '  sprite-sheet-to-x3d [OPTIONS]... <spritesheet> <output>' + NL +
    NL +
    'Available options are:' + NL +
    HelpOptionHelp + NL +
    VersionOptionHelp + NL +
    '  --fps=<single>        How many frames per second does the animation have.' + NL+
    '                        Determines the animations duration' + NL+
    '                        (TimeSensor.cycleInterval values in the X3D output).' + NL+
    NL +
    'Supported input file formats: ' + NL+
    NL +
    '- Starling (.xml). Fully supported.' + NL +
    NL +
    '- Cocos2D (.plist). Covered most of the important stuff.' + NL+
    '  Rare features (like rotate, polygon sprites) are not supported,' + NL+
    '  but they can be added easily, please submit a request!' + NL+
    NL +
    'Notes:' + NL+
    NL +
    'Animation frames must be named "XXX_YYY", where:' + NL+
    NL +
    '- XXX: Frame name, start with a letter, will be used as animation name.'+ NL +
    NL +
    '- YYY: Frame number.' + NL +
    NL +
    'For example: slime_01.png, slime_02.png...' + NL+
    NL +
    'By default anchor will be placed at the center of the sprite if the tool' + NL+
    'didn''t found it in spritesheet.' + NL +
    NL +
    'Developed using Castle Game Engine.' + NL +
    'See http://castle-engine.io/ for latest versions' + NL +
    'of this program, sources and documentation.'
  ;

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0:begin
        Writeln(HelpText);
        Halt;
      end;
    1:begin
        // include ApplicationName in version, good for help2man
        Writeln(ApplicationName + ' ' + CastleEngineVersion);
        Halt;
      end;
    2:FramesPerSecond := StrToFloatDot(Argument);
    else raise EInternalError.Create('OptionProc');
  end;
end;

procedure ParamHandle;
begin
  if Parameters.High = 0 then
  begin
    Writeln(HelpText);
    Halt;
  end;

  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHighAtLeast(1);

  SSFullPath := Parameters[1];
  if not FileExists(SSFullPath) then
  begin
    Writeln(ErrOutput, 'sprite-sheet-to-x3d: Error: File does not exist.');
    Halt;
  end;
  SSPath := ExtractFilePath(SSFullPath);
  SSExt := ExtractFileExt(SSFullPath);
  SSName := StringReplace(
      StringReplace(SSFullPath, SSPath, '', []), SSExt, '', []);
  if Parameters.High = 2 then
    SSOutput := Parameters[2]
  else
    SSOutput := SSPath + SSName + '.x3dv';
end;

begin
  Animations := TAnimations.Create;
  try
    try
      ParamHandle;
      Parse;
      Convert;
    except
      on E: Exception do
      begin
        Writeln(ErrOutput, ExceptMessage(E));
        {$ifdef DEBUG}
        { Dump stack only when compiled in "debug" mode.
          - The stack is not so useful in "release" mode (as by default you
            don't have line information then),
          - And also it would be confusing for end-users.
            It's normal for this tool to exit with an exception in case of invalid
            input (it's not a bug). }
        Writeln(ErrOutput, DumpExceptionBackTraceToString);
        {$endif}
        { Exit with non-zero status, so that scipts using tool can detect failure. }
        Halt(1);
      end;
    end;
  finally
    FreeAndNil(Animations);
  end;
end.
