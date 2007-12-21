{
  Copyright 2003-2007 Michalis Kamburelis.

  This file is part of "Kambi's 3dgraph Pascal units".

  "Kambi's 3dgraph Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dgraph Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dgraph Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ This is a simple demo of the recursive space filling curves implemented in the
  SpaceFillingCurves unit.

  Always run with at least 2 command-line params:
    curve-type (may be "peano" or "hilbert" for now)
    curve-level (
      for peano, reasonable example values are between 1 and 6;
      for hilbert, reasonable example values are between 1 and 9;
      be careful with larger levels ---  the curve size grows
      exponentially fast, as the levels are actually just the recursion depth)
  For example
    ./draw_space_filling_curve peano 4
    ./draw_space_filling_curve hilbert 4

  Set StepsToRedisplay variable (lower in this file) always to 1
  to get more progressive display (you will see while drawing
  in what order the curve is drawn, although the whole drawing
  will be slower).
}
program draw_space_filling_curve;

{$apptype GUI}

uses SysUtils, OpenGLh, GLWindow, GLW_Demo, KambiUtils, KambiGLUtils,
  Images, VectorMath, Math, SpaceFillingCurves, KambiStringUtils,
  GLImages;

var CurveImage: TRGBImage;
const
  CurveCol: TVector3Byte = (255, 255, 255);
  CurveImageBGCol: TVector3Byte = (0, 0, 0);

{ glwindow callbacks ------------------------------------------------------- }

procedure Draw(glwin: TGLWindow);
begin
  { If DoubleBuffer available, then use it.
    This program should work perfectly with and without DoubleBuffer. }
  if glw.DoubleBuffer then glClear(GL_COLOR_BUFFER_BIT);

  ImageDraw(CurveImage);
end;

procedure Init(glwin: TGLWindow);
begin
  glClearColor(0.5, 0.5, 0.5, 1.0);
end;

procedure CloseQueryNotAllowed(glwin: TGLWindow); begin end;

{ curve generation ------------------------------------------------------------ }

var
  LastX: Integer = 1;
  LastY: Integer = 1;
  StepNum: Int64 = 0;
  { inited in main program }
  StepXSize, StepYSize, StepsToRedisplay: Integer;

procedure Step(Angle: TSFCAngle; StepFuncData: Pointer);
var
  x, y: Integer;
begin
  x := LastX;
  y := LastY;

  case Angle of
    0: x+=StepXSize;
    1: y+=StepYSize;
    2: x-=StepXSize;
    else {3:} y-=StepYSize;
  end;

  if Between(x, 0, Integer(CurveImage.Width)-1) and
     Between(y, 0, Integer(CurveImage.Height)-1) and
     Between(LastX, 0, Integer(CurveImage.Width-1)) and
     Between(LastY, 0, Integer(CurveImage.Height-1)) then
  begin
    if x = LastX then
      CurveImage.VerticalLine(x, Min(y, LastY), Max(y, LastY), CurveCol) else
      CurveImage.HorizontalLine(Min(x, LastX), Max(x, LastX), y, CurveCol);
  end;

  LastX := x;
  LastY := y;

  Inc(StepNum);
  if StepNum mod StepsToRedisplay = 0 then
  begin
    Glw.PostRedisplay;
    Glwm.ProcessAllMessages;
    { Since we use GLW_Demo, user is able to close the window by
      pressing Escape. In this case we want to break the curve generation
      and end the program. }
    if Glw.Closed then
      ProgramBreak;
  end;
end;

var
  { vars that may (or must) be taken from params }
  DoPeano: boolean;
  Level: Cardinal;
  InitialOrient: boolean; { default value zalezy od DoPeano }
  InitialAngle: TSFCAngle = 0;

  { helper vars }
  StepsResolution, AllStepsCount: Cardinal;
begin
  { parse params }
  Parameters.CheckHighAtLeast(2);
  case ArrayPosStr(Parameters[1], ['peano', 'hilbert']) of
    0: DoPeano := true;
    1: DoPeano := false;
    else raise EInvalidParams.Create('First param must be "peano" or "hilbert"');
  end;
  Level := StrToInt(Parameters[2]);
  if Parameters.High >= 3 then
    InitialAngle := StrToInt(Parameters[3]) ;
  { if Parameters.High >= 4 then
    InitialOrient := StrToInt(Parameters[4]) else}
  if DoPeano then InitialOrient := false else InitialOrient := true;

  { setup glw, glw.Init }
  glw.OnDraw := @Draw;
  glw.OnResize := @Resize2D;
  glw.OnInit := @Init;
  glw.DoubleBuffer := true;
  glw.OnCloseQuery := @CloseQueryNotAllowed;
  glw.ParseParameters(StandardParseOptions);
  glw.Init;

  { init CurveImage }
  CurveImage := TRGBImage.Create(glw.Width, glw.Height);
  try
    CurveImage.Clear(Vector4Byte(CurveImageBGCol, 0));

    { evaluate StepsToRedisplay i StepSize pomagajac sobie StepsResolution i
      AllStepsCount }
    if DoPeano then
      StepsResolution := NatNatPower(3, Level) else
      StepsResolution := Cardinal(1) shl Level;
    AllStepsCount := Sqr(StepsResolution);
    StepXSize := Max(1, CurveImage.Width div StepsResolution);
    StepYSize := Max(1, CurveImage.Height div StepsResolution);
    StepsToRedisplay := Max(1, Round(AllStepsCount / 20));
    //StepsToRedisplay := 1;

    { draw the curve }
    if DoPeano then
      PeanoCurve(InitialOrient, InitialAngle, Level, @Step, nil) else
      HilbertCurve(InitialOrient, InitialAngle, Level, @Step, nil);

    { show the window }
    glw.PostRedisplay;
    glw.OnCloseQuery := nil;
    glw.Close_CharKey := CharEscape;
    glw.InitLoop;
  finally FreeAndNil(CurveImage) end;
end.
