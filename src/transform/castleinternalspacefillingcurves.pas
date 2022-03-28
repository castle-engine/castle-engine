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

{ Generate space-filling curves (TSwapScanCurve, THilbertCurve, TPeanoCurve).
  These are sequences of points that completely fill some 2D space. }
unit CastleInternalSpaceFillingCurves;

{$I castleconf.inc}

interface

uses SysUtils, CastleVectors;

type
  { Angle for space-filling curves.

    Let's say 0 = 0 degrees, 1 = 90 degrees, 2 = 180, 3 = 270,
    but actually it doesn't matter much --- it all a matter of convention here. }
  TSFCAngle = 0..3;

  TSFCStepFunction = procedure (Angle: TSFCAngle; StepFuncData: Pointer);

{ Low-level procedures to generate consecutive Peano and Hilbert curve points.
  See e.g. "Graphic Gems II" (gem I.8) or
  http://en.wikipedia.org/wiki/Space-filling_curve for nice description.

  Step is a function that goes to a neighbor 2D point, and "marks" it
  (whatever it means for your usage). If the starting point is "marked"
  at the beginning, using Peano or Hilbert curve guarantees that you
  will eventually "mark" the whole 2D space.

  Peano curve fills a plane size 3^Level, Hilbert curve fills size 2^Level.

  Angle is an initial orientation, determining in which direction
  the curve will be drawn. If you want to fill the space up and to the left
  from the initial point (according to conventions that
  Angle = 0 is right, Angle = 1 is up, and so on (CCW)), then use:

  @unorderedList(
    @itemSpacing Compact
    @item(InitialOrient = false and Angle = 0 for PeanoCurve.)
    @item(InitialOrient = true and Angle = 0 for HilbertCurve.)
  )

  @groupBegin
}
procedure PeanoCurve(InitialOrient: boolean; Angle: TSFCAngle; InitialLevel: Cardinal;
  Step: TSFCStepFunction; StepData: Pointer);
procedure HilbertCurve(InitialOrient: boolean; Angle: TSFCAngle; InitialLevel: Cardinal;
  Step: TSFCStepFunction; StepData: Pointer);
{ @groupEnd }

type
  { Base abstract space-filling curve class.

    Generates consecutive points by NextPixel, until EndOfPixels = @true,
    filling the 2D space in [0..SizeX-1, 0..SizeY-1].
    To process (exactly once) each point of your 2D space, you can use
    something like

    @longCode(# while not SFCurve.EndOfPixels do DoSomethingOnPixel(SFCurve.NextPixel); #)

    We try to make NextPixel and EndOfPixels work instantly fast,
    preferably making some preprocessing at construction time. }
  TSpaceFillingCurve = class
  private
    FSizeX, FSizeY, FPixelsCount: Cardinal;
  public
    { Size of the 2D space filled by space-filling curve.
      It's OK even if they are 0 (then EndOfPixels = @true instantly).
      @groupBegin }
    property SizeX: Cardinal read FSizeX;
    property SizeY: Cardinal read FSizeY;
    { @groupEnd }

    { Shortcut for SizeX * SizeY. }
    property PixelsCount: Cardinal read FPixelsCount;

    constructor Create(ASizeX, ASizeY: Cardinal); virtual;

    function EndOfPixels: boolean; virtual; abstract;

    { Get next point. Do not ever call this when EndOfPixels = @true. }
    function NextPixel: TVector2Cardinal; virtual; abstract;

    { Skip next SkipCount curve points. Just like you would call
      NextPixel SkipCount times, ignoring the result. Although may
      be implemented much faster, so don't worry about calling with
      large SkipCount values.

      Do not ever try to skip beyond the end of points.
      That is, do not use @code(SkipCount > PixelsCount - PixelsDone).
      For example, do not use SkipCount > 0 if currently EndOfPixels - @true. }
    procedure SkipPixels(SkipCount: Cardinal); virtual; abstract;

    { Start generating points back from the beginning. }
    procedure Reset; virtual; abstract;

    { How many curve points were generated. Number of generated
      points (by NextPixel or skipped by SkipPixels),
      since the last @link(Reset) or constructor. }
    function PixelsDone: Cardinal; virtual; abstract;

    { Nice curve name, like 'swapscan', 'hilbert' or 'peano'. }
    class function SFCName: string; virtual; abstract;
  end;

  TSpaceFillingCurveClass = class of TSpaceFillingCurve;

  { Abstract space-filling curve class, helping implementing
    curves that precalculate points at construction.
    In descendants, you only need to override GeneratePoints. }
  TPrecalcCurve = class(TSpaceFillingCurve)
  private
    Pixels: PVector2CardinalArray;
    NextPixelNum: Cardinal;
  protected
    { Generate next PixelsCount points. You should generate next
      PixelsCount points to the Pixels table
      (it's guaranteed that PixelsCount > 0). }
    procedure GeneratePixels(APixels: PVector2CardinalArray); virtual; abstract;
  public
    constructor Create(ASizeX, ASizeY: Cardinal); override;
    destructor Destroy; override;
    function EndOfPixels: boolean; override;
    function NextPixel: TVector2Cardinal; override;
    procedure SkipPixels(SkipCount: Cardinal); override;
    procedure Reset; override;
    function PixelsDone: Cardinal; override;
  end;

  { Simple space-filling curve that goes row by row, swapping direction.
    This is a trivial space-filling curve, at least it goes by neighboring
    points. We go from the bottom to the top, in even rows we go the right,
    in odd rows we go to the left. }
  TSwapScanCurve = class(TPrecalcCurve)
  protected
    procedure GeneratePixels(APixels: PVector2CardinalArray); override;
  public
    class function SFCName: string; override;
  end;

  { Space-filling Hilbert curve.
    See e.g. [http://en.wikipedia.org/wiki/Space-filling_curve].
    It's cut off to fill nicely the rectangular
    [0..SizeX-1, 0..SizeY-1] space. }
  THilbertCurve = class(TPrecalcCurve)
  protected
    procedure GeneratePixels(APixels: PVector2CardinalArray); override;
  public
    class function SFCName: string; override;
  end;

  { Space-filling Peano curve.
    See e.g. [http://en.wikipedia.org/wiki/Space-filling_curve].
    It's cut off to fill nicely the rectangular
    [0..SizeX-1, 0..SizeY-1] space. }
  TPeanoCurve = class(TPrecalcCurve)
  protected
    procedure GeneratePixels(APixels: PVector2CardinalArray); override;
  public
    class function SFCName: string; override;
  end;

const
  { Available space-filling curves (non-abstract TSpaceFillingCurve descendants). }
  AvailableSFCurveClasses: array[0..2]of TSpaceFillingCurveClass=
  (TSwapScanCurve, THilbertCurve, TPeanoCurve);

type
  EInvalidSFCurveClassName = class(Exception);

{ For curve name (matching some TSpaceFillingCurve.SFCName),
  return appropriate class. Not case-sensitive.
  @raises EInvalidSFCurveClassName For unknown curve names. }
function StrToSFCurveClass(const s: string): TSpaceFillingCurveClass;

{ All non-abstract space-filling curve names.
  Separated by commas, in apostrophes. }
function AllSFCurveClassesNames: string;

implementation

uses Math,
  CastleUtils;

const
  { AngleTurn[Angle, Orient] = (definicja)
      if Orient then
       result := ChangeIntCycle(Angle, 1, 3) else
       result := ChangeIntCycle(Angle, -1, 3);
    Uzywanie tablicy juz przeliczonych wartosci da nam tutaj maly zysk czasowy. }
  AngleTurn: array[TSFCAngle, boolean]of TSFCAngle =
  (
    {Angle = 0} (3, 1),
    {Angle = 1} (0, 2),
    {Angle = 2} (1, 3),
    {Angle = 3} (2, 0)
  );

procedure PeanoCurve(InitialOrient: boolean; Angle: TSFCAngle; InitialLevel: Cardinal;
  Step: TSFCStepFunction; StepData: Pointer);
{ na podstawie "Graphic Gems II", gem I.8.
  Zmienna Angle jest globalna z punktu widzenia kolejnych rekurencyjnych
  wywolan Peano(), one wszystkie modyfikuja po prostu zadeklarowany powyzej
  parametr Angle. }

  procedure Peano(Orient: boolean; Level: Cardinal);
  begin
   if Level = 0 then Exit;
   Dec(Level);

   Peano(Orient, Level);
   Step(Angle, StepData);
   Peano(not Orient, Level);
   Step(Angle, StepData);
   Peano(Orient, Level);
   Angle := AngleTurn[Angle, not Orient];
   Step(Angle, StepData);
   Angle := AngleTurn[Angle, not Orient];
   Peano(not Orient, Level);
   Step(Angle, StepData);
   Peano(Orient, Level);
   Step(Angle, StepData);
   Peano(not Orient, Level);
   Angle := AngleTurn[Angle, Orient];
   Step(Angle, StepData);
   Angle := AngleTurn[Angle, Orient];
   Peano(Orient, Level);
   Step(Angle, StepData);
   Peano(not Orient, Level);
   Step(Angle, StepData);
   Peano(Orient, Level);
  end;

begin
 Peano(InitialOrient, InitialLevel);
end;

procedure HilbertCurve(InitialOrient: boolean; Angle: TSFCAngle; InitialLevel: Cardinal;
  Step: TSFCStepFunction; StepData: Pointer);
{ na podstawie "Graphic Gems II", gem I.8 }

  procedure Hilbert(Orient: boolean; Level: Cardinal);
  begin
   if Level = 0 then Exit;
   Dec(Level);

   Angle := AngleTurn[Angle, Orient];
   Hilbert(not Orient, Level);
   Step(Angle, StepData);
   Angle := AngleTurn[Angle, not Orient];
   Hilbert(Orient, Level);
   Step(Angle, StepData);
   Hilbert(Orient, Level);
   Angle := AngleTurn[Angle, not Orient];
   Step(Angle, StepData);
   Hilbert(not Orient, Level);
   Angle := AngleTurn[Angle, Orient];
  end;

begin
 Hilbert(InitialOrient, InitialLevel);
end;

{ TSpaceFillingCurve ------------------------------------------------------------ }

constructor TSpaceFillingCurve.Create(ASizeX, ASizeY: Cardinal);
begin
 inherited Create;
 FSizeX := ASizeX;
 FSizeY := ASizeY;
 FPixelsCount := SizeX*SizeY;
end;

{ TPrecalcCurve ------------------------------------------------------------ }

constructor TPrecalcCurve.Create(ASizeX, ASizeY: Cardinal);
begin
 inherited;
 if PixelsCount <> 0 then
 begin
  Pixels := GetMem(SizeOf(TVector2Cardinal) * PixelsCount);
  GeneratePixels(Pixels);
 end;
 Reset;
end;

destructor TPrecalcCurve.Destroy;
begin
 FreeMemNiling(Pointer(Pixels));
end;

function TPrecalcCurve.EndOfPixels: boolean;
begin
 result := NextPixelNum >= PixelsCount;
end;

function TPrecalcCurve.NextPixel: TVector2Cardinal;
begin
 Assert(not EndOfPixels);
 result := Pixels^[NextPixelNum];
 Inc(NextPixelNum);
end;

procedure TPrecalcCurve.SkipPixels(SkipCount: Cardinal);
begin
 Assert(SkipCount <= PixelsCount-PixelsDone);
 NextPixelNum := NextPixelNum + SkipCount;
end;

procedure TPrecalcCurve.Reset;
begin
 NextPixelNum := 0;
end;

function TPrecalcCurve.PixelsDone: Cardinal;
begin
 result := NextPixelNum;
end;

{ TSwapScanCurve ------------------------------------------------------------ }

procedure TSwapScanCurve.GeneratePixels(APixels: PVector2CardinalArray);
var
  NextPixelToWriteNum: Cardinal;

  procedure AppendPixel(x, y: Cardinal);
  begin
    APixels^[NextPixelToWriteNum].X := x;
    APixels^[NextPixelToWriteNum].Y := y;
    Inc(NextPixelToWriteNum);
  end;

var
  x, y: Cardinal;
begin
  NextPixelToWriteNum := 0;

  for y := 0 to SizeY-1 do
  begin
    if Odd(y) then
      for x := SizeX-1 downto 0 do AppendPixel(x, y)
    else
      for x := 0 to SizeX-1 do AppendPixel(x, y);
  end;
end;

class function TSwapScanCurve.SFCName: string;
begin
 result := 'swapscan';
end;

{ pomoce do THilbertCurve i TPeanoCurve ---------------------------------------- }

type
  TStepData = record
    LastX, LastY, NextPixelToWriteNum, SizeX, SizeY: Cardinal;
    Pixels: PVector2CardinalArray;
  end;
  PStepData=^TStepData;

procedure InitStepData(out StepData: TStepData; Pixels: PVector2CardinalArray;
  const SizeX, SizeY: Cardinal);
begin
 FillChar(Pixels^[0], SizeOf(Pixels^[0]), 0);
 StepData.Pixels := Pixels;
 StepData.NextPixelToWriteNum := 1;
 StepData.LastX := 0;
 StepData.LastY := 0;
 StepData.SizeX := SizeX;
 StepData.SizeY := SizeY;
end;

procedure HilbertPeanoStep(Angle: TSFCAngle; RawData: Pointer);
{ callback dla TSFCStepFunction dla Hilbert lub PeanoCurve.
  Zapisze Pixels^[NextPixelToWriteNum], uaktualniajac tez
  LastX, LastY, NextPixelToWriteNum. Bedzie obcinal zapisywane do Pixels
  elementy do 0..SizeX-1, 0..SizeY-1. }
var Data: PStepData absolute RawData;
begin
 with Data^ do
 begin
  case Angle of
   0: Inc(LastX);
   1: Inc(LastY);
   2: Dec(LastX);
   3: Dec(LastY);
  end;

  if Between(LastX, 0, SizeX-1) and Between(LastY, 0, SizeY-1) then
  begin
   Pixels^[NextPixelToWriteNum].X := LastX;
   Pixels^[NextPixelToWriteNum].Y := LastY;
   Inc(NextPixelToWriteNum);
  end;
 end;
end;

{ THilbertCurve ------------------------------------------------------------ }

procedure THilbertCurve.GeneratePixels(APixels: PVector2CardinalArray);
var StepData: TStepData;
    Level: Cardinal;
begin
 Level := Smallest2Exponent(Max(SizeX, SizeY));
 Assert((Level = 0) or (NatNatPower(2, Level-1) < Max(SizeX, SizeY)));

 InitStepData(StepData, APixels, SizeX, SizeY);
 HilbertCurve(true, 0, Level,
   {$ifdef FPC} @ {$endif} HilbertPeanoStep, @StepData);
end;

class function THilbertCurve.SFCName: string;
begin
 result := 'hilbert';
end;

{ TPeanoCurve ------------------------------------------------------------ }

procedure TPeanoCurve.GeneratePixels(APixels: PVector2CardinalArray);
var StepData: TStepData;
    MaxSize, Power3Level, Level: Cardinal;
begin
 { calculate Level, pomagajac sobie MaxSize i Power3Level }
 MaxSize := Max(SizeX, SizeY);
 Level := 0;
 Power3Level := 1; { = zawsze 3^Level }
 while Power3Level < MaxSize do
 begin
   Inc(Level);
   Power3Level := Power3Level * 3
 end;
 Assert((Level = 0) or (NatNatPower(3, Level-1) < Max(SizeX, SizeY)));

 InitStepData(StepData, APixels, SizeX, SizeY);
 PeanoCurve(false, 0, Level,
   {$ifdef FPC} @ {$endif} HilbertPeanoStep, @StepData);
end;

class function TPeanoCurve.SFCName: string;
begin
 result := 'peano';
end;

{ operacje na SFCName -------------------------------------------------------- }

function StrToSFCurveClass(const s: string): TSpaceFillingCurveClass;
var i: Integer;
begin
 for i := 0 to High(AvailableSFCurveClasses) do
  if AnsiSameText(s, AvailableSFCurveClasses[i].SFCName) then
   Exit(AvailableSFCurveClasses[i]);
 raise EInvalidSFCurveClassName.Create('Invalid space filling curve name : "'+
   s+'", allowed names are '+AllSFCurveClassesNames+'.');
end;

function AllSFCurveClassesNames: string;
var
  i: Integer;
begin
  result := '"'+AvailableSFCurveClasses[0].SFCName+'"';
  for i := 1 to High(AvailableSFCurveClasses) do
    Result := Result + ', "'+AvailableSFCurveClasses[i].SFCName+'"';
end;

end.
