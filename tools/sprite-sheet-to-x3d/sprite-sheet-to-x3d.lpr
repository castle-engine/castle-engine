{
  Copyright 2017 Trung Le (kagamma).

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert spritesheets to classic X3D files. }

program sprite-sheet-to-x3d;

{$APPTYPE CONSOLE}

uses
  Classes, SysUtils, strutils,
  FPimage, FPReadPNG, FPReadJPEG,
  DOM, XMLRead,
  RegExpr,
  FGL, CastleGenericLists;

type
  TMeta = record
    Name: string;       { Image name }
    W, H: integer;      { Image size }
  end;

  TFrame = record
    X1, Y1,             { Texture coords }
    X2, Y2,
    W, H,               { Frame size }
    AX, AY: single;     { Anchor }
  end;

  TFrameList = specialize TGenericStructList<TFrame>;
  TAnimations = specialize TFPGMap<string, TFrameList>;

var
  SSFullPath,
  SSPath,
  SSExt,
  SSName: string;
  TpHeader,
  TpShape,
  TpRoute,
  TpTimeSensor,
  TpInterpolator: string;
  Animations: TAnimations;
  Meta: TMeta;

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
  Image: TFPCustomImage;
  Reader: TFPCustomImageReader;
begin
  Image := TFPMemoryImage.Create(0, 0);
  case LowerCase(ExtractFileExt(AMeta.Name)) of
    '.png':
      Reader := TFPReaderPNG.Create;           
    '.jpg', '.jpeg':
      Reader := TFPReaderJPEG.Create;
    else
      begin
        Writeln('Error: Unsupported image format.');
        Halt;
      end;
  end;
  try
    Image.LoadFromFile(AMeta.Name, Reader);
    AMeta.W := Image.Width;
    AMeta.H := Image.Height;
  finally
    FreeAndNil(Image);
    FreeAndNil(Reader);
  end;
end;

{ Load classic X3D templates. }
procedure LoadTemplates;

  function ReadFile(const AName: string): string;
  var
    FS: TFileStream;
    SS: TStringStream;
  begin
    Result := '';
    FS := TFileStream.Create(AName, fmOpenRead);
    SS := TStringStream.Create;
    try
      FS.Position := 0;
      SS.CopyFrom(FS, FS.Size);
      Result := SS.DataString;
    finally
      FreeAndNil(FS);
      FreeAndNil(SS);
    end;
  end;

begin
  TpHeader := ReadFile('data/header.txt');
  TpShape := ReadFile('data/shape.txt');
  TpTimeSensor := ReadFile('data/timesensor.txt');
  TpInterpolator := ReadFile('data/interpolator.txt');
  TpRoute := ReadFile('data/route.txt');
end;

procedure ParamHandle;
begin
  case ParamCount of
    0:
      begin
        Writeln(
            'sprite-sheet-to-x3d: Convert spritesheet files into classic X3D files.' + #10#13 +
            '         Support file formats: Starling (.xml), Cocos2D (.plist).'+ #10#13 +
            '         Please make sure frame keys follow XXX_YYY naming convention:'+ #10#13 +
            '         - XXX: Frame name, start with a letter, will be used as animation name.'+ #10#13 +
            '         - YYY: Frame number.'+ #10#13 +
            'Usage: sprite-sheet-to-x3d <spritesheet>'
        );
        Halt;
      end;
    else
      begin
        SSFullPath := ParamStr(1);
        if not FileExists(SSFullPath) then
        begin
          Writeln('Error: File not exists.');
          Halt;
        end;
        SSPath := ExtractFilePath(SSFullPath);
        SSExt := ExtractFileExt(SSFullPath);
        SSName := StringReplace(
            StringReplace(SSFullPath, SSPath, '', []), SSExt, '', []);
      end;
  end;
end;

{$I starlingparser.inc}
{$I cocos2dparser.inc}

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
  end;
end;      

procedure Convert;
var
  i, j: integer;
  List: TFrameList;
  Frame: TFrame;
  X3DV: TStringList;
  S,
  Interpolators,
  Routes,
  TimeSensors,
  NameStr,
  CoordStr,
  TexCoordStr,
  OldKeyStr,
  KeyStr,
  OldCoordKeyValueStr,
  CoordKeyValueStr,
  OldTexCoordKeyValueStr,
  TexCoordKeyValueStr: string;
begin   
  Interpolators := '';
  Routes := '';
  TimeSensors := '';
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
  X3DV := TStringList.Create;
  try
    X3DV.Add(StringReplace(TpHeader, '%SOURCE%', SSName + SSExt, [rfReplaceAll]));
    X3DV.Add('');
    X3DV.Add(StringReplace(TpShape, '%ATLAS%', Meta.Name, [rfReplaceAll]));  
    X3DV.Add('');
    for j := 0 to Animations.Count-1 do
    begin
      List := Animations.Data[j];
      NameStr := StringReplace(
          ExtractFileName(Animations.Keys[j]),
          ExtractFilePath(Animations.Keys[j]), '', []);
      CoordStr := NameStr + '_Coord';
      TexCoordStr := NameStr + '_TexCoord'; 
      KeyStr := '';
      OldKeyStr := '';
      { Generate list of keys. }
      for i := 0 to List.Count-1 do
      begin
        KeyStr += OldKeyStr + FloatToStr(1/(List.Count - 1) * i) + ' ';
        OldKeyStr := FloatToStr(1/(List.Count - 1) * (i+1) - 0.0000001) + ' ';
      end;
      CoordKeyValueStr := '';
      TexCoordKeyValueStr := '';
      OldCoordKeyValueStr := '';
      OldTexCoordKeyValueStr := '';  
      { Generate list of coord/texcoord key values. }
      for i := 0 to List.Count-1 do
      begin
        Frame := List[i];
        S := Format(
            '%.1f %.1f 0, %.1f %.1f 0, %.1f %.1f 0, ' +
            '%.1f %.1f 0, %.1f %.1f 0, %.1f %.1f 0, ' + #10,
            [-Frame.W * (  Frame.AX),  Frame.H * (  Frame.AY),
              Frame.W * (1-Frame.AX),  Frame.H * (  Frame.AY),
              Frame.W * (1-Frame.AX), -Frame.H * (1-Frame.AY), 
             -Frame.W * (  Frame.AX),  Frame.H * (  Frame.AY),
              Frame.W * (1-Frame.AX), -Frame.H * (1-Frame.AY),
             -Frame.W * (  Frame.AX), -Frame.H * (1-Frame.AY)]);
        CoordKeyValueStr += OldCoordKeyValueStr + S;
        OldCoordKeyValueStr := S;
        S := Format(
            '%.4f %.4f, %.4f %.4f, %.4f %.4f, ' +
            '%.4f %.4f, %.4f %.4f, %.4f %.4f, ' + #10,
            [Frame.X1, Frame.Y1,
             Frame.X2, Frame.Y1,
             Frame.X2, Frame.Y2,
             Frame.X1, Frame.Y1,
             Frame.X2, Frame.Y2,
             Frame.X1, Frame.Y2]);
        TexCoordKeyValueStr += OldTexCoordKeyValueStr + S;
        OldTexCoordKeyValueStr := S;
      end;
      SetLength(KeyStr, Length(KeyStr)-1);
      SetLength(CoordKeyValueStr, Length(CoordKeyValueStr)-1);
      SetLength(TexCoordKeyValueStr, Length(TexCoordKeyValueStr)-1);
      TimeSensors := TimeSensors + StringsReplace(
          TpTimeSensor,
          ['%NAME%'],
          [NameStr],
          [rfReplaceAll]) + #10#10;     
      Interpolators := Interpolators + StringsReplace(
          TpInterpolator,
          ['%NAME%',
           '%COORD%', '%COORD_KEY%', '%COORD_KEYVALUE%',
           '%TEXCOORD%', '%TEXCOORD_KEY%', '%TEXCOORD_KEYVALUE%'],
          [NameStr,
           CoordStr, KeyStr, CoordKeyValueStr,
           TexCoordStr, KeyStr, TexCoordKeyValueStr],
          [rfReplaceAll]) + #10#10;
      Routes := Routes + StringsReplace(
          TpRoute,
          ['%NAME%',
           '%COORD%',
           '%TEXCOORD%'],
          [NameStr,
           CoordStr,
           TexCoordStr],
          [rfReplaceAll]) + #10;
    end;
    X3DV.Text := X3DV.Text + TimeSensors + Interpolators + Routes;
    X3DV.SaveToFile(SSName + '.x3dv');
  finally
    FreeAndNil(X3DV);
  end;
end;

begin
  Animations := TAnimations.Create;
  try
    try
      LoadTemplates;
      ParamHandle;
      Parse;
      Convert;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    FreeAndNil(Animations);
  end;
end.


