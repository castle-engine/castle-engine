{
  Copyright 2009-2010 Michalis Kamburelis.

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

uses SysUtils, KambiUtils, Images, ImagesFftw, GLWindow, GL, GLImages,
  KambiTimeUtils, KambiStringUtils, Fftw_s, KambiGLUtils, Math,
  KambiParameters;

type
  TGLWindowImage = class(TGLWindowDemo)
  public
    Image: TRGBImage;
    ImageDL: TGLuint;
    destructor Destroy; override;
    procedure UpdateDL;
    procedure EventDraw; override;
    procedure EventOpen; override;
  end;

destructor TGLWindowImage.Destroy;
begin
  FreeAndNil(Image);
  inherited;
end;

procedure TGLWindowImage.EventDraw;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT);
  glCallList(ImageDL);
end;

procedure TGLWindowImage.EventOpen;
begin
  inherited;
  UpdateDL;
end;

procedure TGLWindowImage.UpdateDL;
begin
  MakeCurrent;
  glFreeDisplayList(ImageDL);
  ImageDL := ImageDrawToDisplayList(Image);
  PostRedisplay;
end;

type
  TOperation = (opNone, opRe, opIm, opModulus, opNormalize, opConjugate,
    opScaleRe, opMulMNSqr, opZeroOutsideSquare, opZeroInsideSquare,
    opMulMinus1, opShift);

var
  Source, Freq, Output: TGLWindowImage;
  Fft: TImageFftw;
  Operation: TOperation;
  Alpha: Single = 0.5;

{ Remake Freq.Image and Output.Image, using Fft }
procedure MakeFft;

  { Is X, Y inside square (with size SquareSize with waves of lowest frequency). }
  function InsideSquare(const X, Y: Integer): boolean;
  const
    SquareSize = 50;
    SquareSize2 = SquareSize div 2;
  begin
    Result :=
      ( (X <=                    SquareSize2) and ( (Y <= SquareSize2) or (Y >= Freq.Image.Height - SquareSize2) ) ) or
      ( (X >= Freq.Image.Width - SquareSize2) and ( (Y <= SquareSize2) or (Y >= Freq.Image.Height - SquareSize2) ) );
  end;

var
  X, Y, Color: Integer;
  Ptr: Pcomplex_single;
  Tmp, Phi, R: Single;
begin
  Fft.Image := Source.Image;

  ProcessTimerBegin;
  Fft.DFT;
  Writeln('Making DFT: ', ProcessTimerEnd:1:2, ' secs');

  { Now perform the Operation on image in frequency domain.
    Lameness warning: yes, it's ultra lame to perform "case Operation..."
    check in every loop, but this allowed me to write this very shortly. }
  for Color := 0 to 2 do
  begin
    Ptr := Fft.ImageF[Color];
    for Y := 0 to Freq.Image.Height - 1 do
      for X := 0 to Freq.Image.Width - 1 do
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
              Tmp := (Sqr(X {- Freq.Image.Width  div 2}) +
                      Sqr(Y {- Freq.Image.Height div 2})) / 100000.0;
              Ptr^.Re *= Tmp;
              Ptr^.Im *= Tmp;
            end;
          opZeroOutsideSquare: if not InsideSquare(X, Y) then Ptr^ := CZero;
          opZeroInsideSquare : if     InsideSquare(X, Y) then Ptr^ := CZero;
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
  if not Freq.Closed then Freq.UpdateDL;

  Fft.Image := Output.Image;

  ProcessTimerBegin;
  Fft.IDFT;
  Writeln('Making IDFT: ', ProcessTimerEnd:1:2, ' secs');

  if not Output.Closed then Output.UpdateDL;
end;

procedure MenuCommand(glwin: TGLWindow; Item: TMenuItem);
begin
  case Item.IntData of
    10: Application.Quit;
    100..100 + Ord(High(TOperation)):
      begin
        Operation := TOperation(Item.IntData - 100);
        MakeFft;
      end;
    200:
      begin
        Alpha -= 0.1;
        MakeFft;
        Writeln('Alpha = ', Alpha:1:10);
      end;
    201:
      begin
        Alpha += 0.1;
        MakeFft;
        Writeln('Alpha = ', Alpha:1:10);
      end;
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
     'Zero outside the square',
     'Zero inside the square',
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
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Quit', 10, CharEscape));
    Result.Append(M);
end;

begin
  Parameters.CheckHigh(1);
  try
    Source := TGLWindowImage.Create(nil);
    Source.Image := LoadImage(Parameters[1], [TRGBImage], []) as TRGBImage;
    Source.Width  := Source.Image.Width ;
    Source.Height := Source.Image.Height;
    Source.Caption := 'Source image';
    Source.OnResize := @Resize2d;
    Source.Left := 10;
    Source.Top := 10;
    Source.Close_CharKey := #0;

    Freq := TGLWindowImage.Create(nil);
    Freq.Image := TRGBImage.Create(Source.Image.Width, Source.Image.Height);
    Freq.Width  := Freq.Image.Width ;
    Freq.Height := Freq.Image.Height;
    Freq.Caption := 'Frequency domain - modulus';
    Freq.OnResize := @Resize2d;
    Freq.Left := 10;
    Freq.Top := Source.Top + Source.Height + 10;
    Freq.Close_CharKey := #0;

    Output := TGLWindowImage.Create(nil);
    Output.Image := TRGBImage.Create(Source.Image.Width, Source.Image.Height);
    Output.Width  := Output.Image.Width ;
    Output.Height := Output.Image.Height;
    Output.Caption := 'Output image';
    Output.OnResize := @Resize2d;
    Output.Left := Source.Left + Source.Width + 10;
    Output.Top := Source.Top + Source.Height + 10;
    Output.Close_CharKey := #0;

    Output.MainMenu := CreateMainMenu;
    Output.OnMenuCommand := @MenuCommand;

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
