{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple demo of operating on image in the frequency domain.
  Uses ImagesFftw unit (that uses FFTW library) to perform DFT and IDFT
  on the image.

  As $1 pass image file name.

  Practically, this is the solution for exercise 2 on
  http://www.ii.uni.wroc.pl/~anl/dyd/PO/ }
program fft_tests;

uses SysUtils, Classes, CastleUtils, CastleImages, ImagesFftw, CastleWindow,
  CastleTimeUtils, CastleStringUtils, Fftw_s,
  Math, CastleParameters, CastleColors, CastleControls;

type
  TWindowImage = class(TCastleWindowCustom)
  strict private
    Background: TCastleSimpleBackground;
    ImageControl: TCastleImageControl;
    function GetImage: TRGBImage;
    procedure SetImage(const Value: TRGBImage);
  public
    property Image: TRGBImage read GetImage write SetImage;
    constructor Create(AOwner: TComponent); override;
    procedure ImageChanged;
  end;

constructor TWindowImage.Create(AOwner: TComponent);
begin
  inherited;

  Background := TCastleSimpleBackground.Create(Self);
  Controls.InsertFront(Background);

  ImageControl := TCastleImageControl.Create(Self);
  Controls.InsertFront(ImageControl);
end;

function TWindowImage.GetImage: TRGBImage;
begin
  { as the image can only be set by SetImage, we know it is of TRGBImage class }
  Result := ImageControl.Image as TRGBImage;
end;

procedure TWindowImage.SetImage(const Value: TRGBImage);
begin
  ImageControl.Image := Value;
end;

procedure TWindowImage.ImageChanged;
begin
  ImageControl.ImageChanged;
end;

type
  TOperation = (opNone, opRe, opIm, opModulus, opNormalize, opConjugate,
    opScaleRe, opMulMNSqr,
    opZeroOutsideSquare, opZeroInsideSquare,
    opZeroOutsideCircle, opZeroInsideCircle,
    { opDarkGaussianInsideCircle, }
    opMulMinus1, opShift);

var
  Source, Freq, Output: TWindowImage;
  Fft: TImageFftw;
  Operation: TOperation;
  Alpha: Single = 0.5;
  Beta: Single = 10.0;

{ Remake Freq.Image and Output.Image, using Fft }
procedure MakeFft;
var
  W, H: Integer;

  { Is X, Y inside square with waves of lowest frequency. }
  function InsideSquare(const X, Y: Integer): boolean;
  begin
    Result :=
      ( (X <=     Beta) and ( (Y <= Beta) or (Y >= H - Beta) ) ) or
      ( (X >= W - Beta) and ( (Y <= Beta) or (Y >= H - Beta) ) );
  end;

  { Is X, Y inside circle with waves of lowest frequency. }
  function InsideCircle(const X, Y: Integer): boolean;
  begin
    Result :=
      (Sqr(    X) + Sqr(    Y) < Sqr(Beta)) or
      (Sqr(W - X) + Sqr(    Y) < Sqr(Beta)) or
      (Sqr(W - X) + Sqr(H - Y) < Sqr(Beta)) or
      (Sqr(    X) + Sqr(H - Y) < Sqr(Beta));
  end;

{ Works, but not interesting, Gaussian is too spread out:

  procedure DarkGaussianInsideCircle(const X, Y: Integer; var Value: Complex_Single);
  var
    StdDev: Single;
  begin
    StdDev := RadiusToStdDev(Beta);
    if Sqr(    X) + Sqr(    Y) < Sqr(Beta) then Value *= 1- Gaussian2D(    X,     Y, StdDev) else
    if Sqr(W - X) + Sqr(    Y) < Sqr(Beta) then Value *= 1- Gaussian2D(W - X,     Y, StdDev) else
    if Sqr(W - X) + Sqr(H - Y) < Sqr(Beta) then Value *= 1- Gaussian2D(W - X, H - Y, StdDev) else
    if Sqr(    X) + Sqr(H - Y) < Sqr(Beta) then Value *= 1- Gaussian2D(    X, H - Y, StdDev);
  end;
}

var
  X, Y, Color: Integer;
  Ptr: Pcomplex_single;
  Tmp, Phi, R: Single;
begin
  Fft.Image := Source.Image;

  ProcessTimerBegin;
  Fft.DFT;
  Writeln('Making DFT: ', ProcessTimerEnd:1:2, ' secs');

  W := Freq.Image.Width;
  H := Freq.Image.Height;

  { Now perform the Operation on image in frequency domain.
    Lameness warning: yes, it's ultra lame to perform "case Operation..."
    check in every loop, but this allowed me to write this very shortly. }
  for Color := 0 to 2 do
  begin
    Ptr := Fft.ImageF[Color];
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        case Operation of
          opNone: ;
          opRe: Ptr^.Im := 0;
          opIm: Ptr^.Re := 0;
          opModulus: begin Ptr^.Re := CMod(Ptr^); Ptr^.Im := 0; end;
          opNormalize: Ptr^ := CNormalized(Ptr^);
          opConjugate: Ptr^.Im := -Ptr^.Im;
          opScaleRe: Ptr^.Re *= Alpha;
          opMulMNSqr:
            begin
              Tmp := (Sqr(X {- W  div 2}) +
                      Sqr(Y {- H div 2})) / 100000.0;
              Ptr^.Re *= Tmp;
              Ptr^.Im *= Tmp;
            end;
          opZeroOutsideSquare: if not InsideSquare(X, Y) then Ptr^ := CZero;
          opZeroInsideSquare : if     InsideSquare(X, Y) then Ptr^ := CZero;
          opZeroOutsideCircle: if not InsideCircle(X, Y) then Ptr^ := CZero;
          opZeroInsideCircle : if     InsideCircle(X, Y) then Ptr^ := CZero;
          { opDarkGaussianInsideCircle: DarkGaussianInsideCircle(X, Y, Ptr^); }
          opMulMinus1:
            if Odd(X + Y) then
            begin
              Ptr^.Re := -Ptr^.Re;
              Ptr^.Im := -Ptr^.Im;
            end;
          opShift:
            begin
              R := Sqrt(Sqr(Ptr^.Re) + Sqr(Ptr^.Im));
              Phi := ArcTan2(Ptr^.Im, Ptr^.Re);

              Phi += Alpha * (X + Y);

              Ptr^.Re := R * Cos(Phi);
              Ptr^.Im := R * Sin(Phi);
            end;
        end;
        Inc(Ptr);
      end;
  end;

  Fft.ImageFModulusAsRGB(Freq.Image, 0.01);
  Freq.ImageChanged;

  Fft.Image := Output.Image;

  ProcessTimerBegin;
  Fft.IDFT;
  Writeln('Making IDFT: ', ProcessTimerEnd:1:2, ' secs');

  Output.ImageChanged;
end;

procedure MenuClick(Container: TUIContainer; Item: TMenuItem);
begin
  case Item.IntData of
    10: Application.Quit;
    100..100 + Ord(High(TOperation)):
      begin
        Operation := TOperation(Item.IntData - 100);
        MakeFft;
      end;
    200: begin Alpha -= 0.1; MakeFft; Writeln('Alpha = ', Alpha:1:10); end;
    201: begin Alpha += 0.1; MakeFft; Writeln('Alpha = ', Alpha:1:10); end;
    205: begin  Beta -= 0.5; MakeFft; Writeln('Beta = ' , Beta:1:10);  end;
    206: begin  Beta += 0.5; MakeFft; Writeln('Beta = ', Beta:1:10);   end;
  end;
end;

function CreateMainMenu: TMenu;

  procedure AddOperations(const Base: Integer; M: TMenu);
  const
    OperationNames: array [TOperation] of string =
    ('No operation',
     'Only real',
     'Only imaginary',
     'Set to modulus',
     'Normalize',
     'Conjugate',
     'Re *= Alpha',
     'Multiply by m^2+n^2',
     'Zero outside the square (size = 2 * Beta)',
     'Zero inside the square (size = 2 * Beta)',
     'Zero outside the circle (radius = Beta)',
     'Zero inside the circle (radius = Beta)',
     { 'Dark Gaussian inside the circle (radius = Beta)', }
     'Multiply by (-1)^(m+n)',
     'Phase += Alpha (shift image)');
  var
    RadioGroup: TMenuItemRadioGroup;
    Radio: TMenuItemRadio;
    Op: TOperation;
  begin
    RadioGroup := nil;

    for Op := Low(Op) to High(Op) do
    begin
      Radio := TMenuItemRadio.Create(OperationNames[Op], Base + Ord(Op),
        Operation = Op, true);
      if RadioGroup <> nil then
        Radio.Group := RadioGroup else
        RadioGroup := Radio.Group;
      M.Append(Radio);
    end;
  end;

var
  M: TMenu;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_File');
    AddOperations(100, M);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Alpha -= 0.1' , 200, CtrlA));
    M.Append(TMenuItem.Create('Alpha += 0.1' , 201, CtrlD));
    M.Append(TMenuItem.Create('Beta -= 0.5' , 205, CtrlS));
    M.Append(TMenuItem.Create('Beta += 0.5' , 206, CtrlW));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Quit', 10, CharEscape));
    Result.Append(M);
end;

begin
  Parameters.CheckHigh(1);
  try
    Source := TWindowImage.Create(nil);
    Source.Image := LoadImage(Parameters[1], [TRGBImage]) as TRGBImage;
    Source.Width  := Source.Image.Width ;
    Source.Height := Source.Image.Height;
    Source.Caption := 'Source image';
    Source.OnResize := @Resize2d;
    Source.Left := 10;
    Source.Top := 10;

    Freq := TWindowImage.Create(nil);
    Freq.Image := TRGBImage.Create(Source.Image.Width, Source.Image.Height);
    Freq.Width  := Freq.Image.Width ;
    Freq.Height := Freq.Image.Height;
    Freq.Caption := 'Frequency domain - modulus';
    Freq.OnResize := @Resize2d;
    Freq.Left := 10;
    Freq.Top := Source.Top + Source.Height + 10;

    Output := TWindowImage.Create(nil);
    Output.Image := TRGBImage.Create(Source.Image.Width, Source.Image.Height);
    Output.Width  := Output.Image.Width ;
    Output.Height := Output.Image.Height;
    Output.Caption := 'Output image';
    Output.OnResize := @Resize2d;
    Output.Left := Source.Left + Source.Width + 10;
    Output.Top := Source.Top + Source.Height + 10;

    Output.MainMenu := CreateMainMenu;
    Output.OnMenuClick := @MenuClick;

    ProcessTimerBegin;
    Fft := TImageFftw.Create(Source.Image);
    Writeln('Creating FFTW plans for image with ', Fft.Size, ' pixels: ', ProcessTimerEnd:1:2, ' secs');
    MakeFft;

    Source.Open;
    Freq.Open;
    Output.Open;
    Application.Run;
  finally
    FreeAndNil(Fft);

    FreeAndNil(Output);
    FreeAndNil(Freq);
    FreeAndNil(Source);
  end;
end.
