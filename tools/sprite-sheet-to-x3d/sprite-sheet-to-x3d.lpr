{
  Copyright 2017-2017 Trung Le (kagamma).

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert spritesheets to classic X3D files. }

{$APPTYPE CONSOLE}

uses
  Classes, SysUtils, strutils, DOM, XMLRead, RegExpr, FGL,
  CastleParameters, CastleImages, CastleGenericLists, CastleStringUtils,
  CastleVectors, CastleUtils, CastleClassUtils, X3DNodes;

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

  TFrameList = specialize TGenericStructList<TFrame>;
  TAnimations = class(specialize TFPGMap<string, TFrameList>)
    destructor Destroy; override;
  end;

var
  SSFullPath,
  SSPath,
  SSExt,
  SSName,
  SSOutput: string;
  Animations: TAnimations;
  Meta: TMeta;

destructor TAnimations.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Data[I].Free;
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
  i: integer;
begin
  if Animations.Find(AKey, i) then
  begin
    List := Animations.Data[i];
  end
  else
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

procedure ParamHandle;
begin
  case Parameters.High of
    0:
      begin
        Writeln(
            'sprite-sheet-to-x3d: Convert spritesheet files into X3D files.' + LineEnding +
            '    Support file formats: Starling (.xml), Cocos2D (.plist).'+ LineEnding +
            '    Please make sure frame keys follow XXX_YYY naming convention:'+ LineEnding +
            '    - XXX: Frame name, start with a letter, will be used as animation name.'+ LineEnding +
            '    - YYY: Frame number.'+ LineEnding +
            'Usage: sprite-sheet-to-x3d <spritesheet> <output>'
        );
        Halt;
      end;
    else
      begin
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
  CoordArray: array of TVector3Single;
  TexCoordArray: array of TVector2Single;
  R1, R2, R3, R4: TX3DRoute;
begin
  for j := 0 to Animations.Count-1 do
  begin
    List := Animations.Data[j];
    List.Add(List[0]);
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
      'sprite-sheet-to-x3d, http://castle-engine.sourceforge.net';
  Root.Meta['source'] := SSName + SSExt;
  try
    Shape:= TShapeNode.Create;
    Shape.Material := TMaterialNode.Create;
    Shape.Material.DiffuseColor := Vector3Single(0, 0, 0);
    Shape.Material.SpecularColor := Vector3Single(0, 0, 0);
    Shape.Material.AmbientIntensity := 0;
    Shape.Material.EmissiveColor := Vector3Single(1, 1, 1);

    Tex := TImageTextureNode.Create;
    Tex.FdUrl.Send(Meta.Name);
    Tex.RepeatS := false;
    Tex.RepeatT := false;
    Tex.FdTextureProperties.Send(TTexturePropertiesNode.Create);
    Tex.TextureProperties.FdMinificationFilter.Send('NEAREST_PIXEL');
    Tex.TextureProperties.FdMagnificationFilter.Send('NEAREST_PIXEL');
    Shape.Texture := Tex;

    Tri := TTriangleSetNode.Create;
    Tri.Solid := false;
    Coord := TCoordinateNode.Create('coord');
    Coord.FdPoint.Items.AddArray([
        Vector3Single(-128, -128, 0),
        Vector3Single(128, -128, 0),
        Vector3Single(128, 128, 0),
        Vector3Single(-128, -128, 0),
        Vector3Single(128, 128, 0),
        Vector3Single(-128, 128, 0)]);
    TexCoord := TTextureCoordinateNode.Create('texcoord');
    TexCoord.FdPoint.Items.AddArray([
         Vector2Single(0, 0),
         Vector2Single(1, 0),
         Vector2Single(1, 1),
         Vector2Single(0, 0),
         Vector2Single(1, 1),
         Vector2Single(0, 1)]);
    Tri.FdCoord.Value := Coord;
    Tri.FdTexCoord.Value := TexCoord;
    Shape.FdGeometry.Value := Tri;

    SetLength(CoordArray, 6);
    SetLength(TexCoordArray, 6);
    SetLength(TimeSensorArray, Animations.Count);
    SetLength(CoordInterpArray, Animations.Count);
    SetLength(TexCoordInterpArray, Animations.Count);

    for j := 0 to Animations.Count-1 do
    begin
      List := Animations.Data[j];
      TimeSensor := TTimeSensorNode.Create(Animations.Keys[j]);
      TimeSensor.FdCycleInterval.Send(2);
      CoordInterp :=
          TCoordinateInterpolatorNode.Create(Animations.Keys[j] + '_Coord');
      TexCoordInterp :=
          TCoordinateInterpolator2DNode.Create(Animations.Keys[j] + '_TexCoord');
      TimeSensorArray[j] := TimeSensor;
      CoordInterpArray[j] := CoordInterp;
      TexCoordInterpArray[j] := TexCoordInterp;
      { Generate list of keys. }
      for i := 0 to List.Count-1 do
      begin
        Key := 1/(List.Count - 1) * i;
        CoordInterp.FdKey.Items.Add(Key);
        TexCoordInterp.FdKey.Items.Add(Key);
        if i > 0 then
        begin
          CoordInterp.FdKey.Items.Add(Key);
          TexCoordInterp.FdKey.Items.Add(Key);
        end;
      end;
      { Generate list of coord/texcoord key values. }
      for i := 0 to List.Count-1 do
      begin
        Frame := List[i];
        CoordArray[0] := Vector3Single(
            -Frame.W * (  Frame.AX),  Frame.H * (  Frame.AY), 0);
        CoordArray[1] := Vector3Single(
             Frame.W * (1-Frame.AX),  Frame.H * (  Frame.AY), 0);
        CoordArray[2] := Vector3Single(
             Frame.W * (1-Frame.AX), -Frame.H * (1-Frame.AY), 0);
        CoordArray[3] := Vector3Single(
            -Frame.W * (  Frame.AX),  Frame.H * (  Frame.AY), 0);
        CoordArray[4] := Vector3Single(
             Frame.W * (1-Frame.AX), -Frame.H * (1-Frame.AY), 0);
        CoordArray[5] := Vector3Single(
            -Frame.W * (  Frame.AX), -Frame.H * (1-Frame.AY), 0);
        TexCoordArray[0] := Vector2Single(Frame.X1, Frame.Y1);
        TexCoordArray[1] := Vector2Single(Frame.X2, Frame.Y1);
        TexCoordArray[2] := Vector2Single(Frame.X2, Frame.Y2);
        TexCoordArray[3] := Vector2Single(Frame.X1, Frame.Y1);
        TexCoordArray[4] := Vector2Single(Frame.X2, Frame.Y2);
        TexCoordArray[5] := Vector2Single(Frame.X1, Frame.Y2);
        CoordInterp.FdKeyValue.Items.AddArray(CoordArray);
        TexCoordInterp.FdKeyValue.Items.AddArray(TexCoordArray);
        if i < List.Count-1 then
        begin
          CoordInterp.FdKeyValue.Items.AddArray(CoordArray);
          TexCoordInterp.FdKeyValue.Items.AddArray(TexCoordArray);
        end;
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
    end;
    { Put everything into the scene. }
    Root.FdChildren.Add(Shape);
    for j := 0 to Animations.Count-1 do
      Root.FdChildren.Add(TimeSensorArray[j]);
    for j := 0 to Animations.Count-1 do
    begin
      Root.FdChildren.Add(CoordInterpArray[j]);
      Root.FdChildren.Add(TexCoordInterpArray[j]);
    end;
    Save3D(Root, SSOutput);
  finally
    FreeAndNil(Root);
  end;
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
