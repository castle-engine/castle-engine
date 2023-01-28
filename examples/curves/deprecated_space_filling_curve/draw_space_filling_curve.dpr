{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This is a simple demo of the recursive space filling curves implemented in the
  CastleInternalSpaceFillingCurves unit.

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

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils, CastleWindow, CastleUtils, CastleGLUtils, CastleParameters,
  CastleImages, CastleVectors, Math, CastleInternalSpaceFillingCurves, CastleStringUtils, CastleGLImages,
  CastleKeysMouse, CastleColors;

var
  Window: TCastleWindow;
  CurveImage: TRGBImage;
const
  CurveCol: TVector3Byte = (X: 255; Y: 255; Z: 255);
  CurveImageBGCol: TVector4Byte = (X: 0; Y: 0; Z: 0; W: 0);

{ CastleWindow callbacks ------------------------------------------------------- }

procedure Render(Container: TCastleContainer);
var
  DrawableImage: TDrawableImage;
begin
  DrawableImage := TDrawableImage.Create(CurveImage, false, false);
  try
    DrawableImage.Draw(0, 0);
  finally FreeAndNil(DrawableImage) end;
end;

procedure CloseQueryNotAllowed(Container: TCastleContainer); begin end;

{ curve generation ------------------------------------------------------------ }

var
  LastX: Integer = 1;
  LastY: Integer = 1;
  StepNum: Int64 = 0;
  { initialized in main program }
  StepXSize, StepYSize, StepsToRedisplay: Integer;

procedure Step(Angle: TSFCAngle; StepFuncData: Pointer);
var
  x, y: Integer;
begin
  x := LastX;
  y := LastY;

  case Angle of
    0: x := x + StepXSize;
    1: y := y + StepYSize;
    2: x := x - StepXSize;
    else {3:} y := y - StepYSize;
  end;

  if Between(x, 0, Integer(CurveImage.Width)-1) and
     Between(y, 0, Integer(CurveImage.Height)-1) and
     Between(LastX, 0, Integer(CurveImage.Width-1)) and
     Between(LastY, 0, Integer(CurveImage.Height-1)) then
  begin
    if x = LastX then
      CurveImage.VerticalLine(x, Min(y, LastY), Max(y, LastY), CurveCol)
    else
      CurveImage.HorizontalLine(Min(x, LastX), Max(x, LastX), y, CurveCol);
  end;

  LastX := x;
  LastY := y;

  Inc(StepNum);
  if StepNum mod StepsToRedisplay = 0 then
  begin
    Window.Invalidate;
    Application.ProcessAllMessages;
    { Since we use TCastleWindowDemo, user is able to close the window by
      pressing Escape. In this case we want to break the curve generation
      and end the program. }
    if Window.Closed then
      Halt;
  end;
end;

var
  { vars that may be taken from params }
  DoPeano: boolean = true;
  Level: Cardinal = 4;
  InitialAngle: TSFCAngle = 0;

  InitialOrient: boolean; { default value depends on DoPeano }
  StepsResolution, AllStepsCount: Cardinal;
begin
  Window := TCastleWindow.Create(Application);

  { parse params }
  if Parameters.High >= 1 then
    case ArrayPosStr(Parameters[1], ['peano', 'hilbert']) of
      0: DoPeano := true;
      1: DoPeano := false;
      else raise EInvalidParams.Create('First param must be "peano" or "hilbert"');
    end;
  if Parameters.High >= 2 then
    Level := StrToInt(Parameters[2]);
  if Parameters.High >= 3 then
    InitialAngle := StrToInt(Parameters[3]) ;
  { if Parameters.High >= 4 then
    InitialOrient := StrToInt(Parameters[4]) else}
  if DoPeano then InitialOrient := false else InitialOrient := true;

  { setup Window, Window.Open }
  Window.OnRender := @Render;
  Window.OnResize := @Resize2D;
  Window.DoubleBuffer := true;
  Window.OnCloseQuery := @CloseQueryNotAllowed;
  Window.ParseParameters(StandardParseOptions);
  Window.SetDemoOptions(keyF11, CharEscape, true);
  Window.Open;

  { init CurveImage }
  CurveImage := TRGBImage.Create(Window.Width, Window.Height);
  try
    CurveImage.Clear(CurveImageBGCol);

    { calculate StepsToRedisplay i StepSize pomagajac sobie StepsResolution i
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
    Window.Invalidate;
    Window.OnCloseQuery := nil;
    Application.Run;
  finally FreeAndNil(CurveImage) end;
end.
