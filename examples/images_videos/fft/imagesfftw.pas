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

{ Doing Fourier transforms (DFT and inverse) on images of TRGBImage class.
  Uses FFTW library through fftw_s unit provided with FPC.

  Note that this works only with FPC >= 2.4.0 (needs fixed
  fftw_getmem/freemem, see
  http://bugs.freepascal.org/view.php?id=13463 ). }
unit ImagesFftw;

interface

uses Fftw_s, CastleImages;

type
  { Image expressed as three arrays of complex values.
    This is how FFTW views an image. }
  TImageComplex = array [0..2] of Pcomplex_single;

  TImageFftw = class
  private
    FImage: TRGBImage;
    FImageF, FImageComplex: TImageComplex;
    FSize: Cardinal;
    PlanDFT, PlanIDFT: array [0..2] of fftw_plan_single;

    function GetImageF(Color: Integer): Pcomplex_single;

    { Convert real values of image encoded in Complex
      into normal RGB image in Img. Apply scaling Scale by the way. }
    procedure ComplexToRGB(Complex: TImageComplex;
      Img: TRGBImage; const Scale: Single);
  public
    constructor Create(AImage: TRGBImage);
    destructor Destroy; override;

    { Image in the spatial domain.

      You can freely modify and change it whenever you like,
      the only requirement is that it's size (width and height)
      must stay the same. And it must always be assigned (non-nil). }
    property Image: TRGBImage read FImage write FImage;

    property Size: Cardinal read FSize;

    { Image in the frequency domain. Contents filled by the DFT call.

      Actually we have three images, for red, green and blue components.

      You can modify it between DFT and IDFT (that's actually the very
      purpose of this class, otherwise there's no point in doing
      DFT followed by IDFT just to get the same image...). }
    property ImageF[Color: Integer]: Pcomplex_single read GetImageF;

    { Fill TRGBImage showing the ImageF modulus contents.
      This actually shows the image in frequency domain, so is generally
      useful only for testing (you usually do not want to look
      at image in frequency domain, unless you understand what
      the frequency domain represents.) }
    procedure ImageFModulusAsRGB(Img: TRGBImage; const Scale: Single);

    { Perform DFT: from Image contents, make ImageF. }
    procedure DFT;

    { Perform IDFT: from ImageF, fill Image contents.

      At this point, we also take care of scaling, to make the result
      actually match the input: we divide each component by
      the number of Image pixels. }
    procedure IDFT;
  end;

{ Basic functions to operate on complex numbers.
  FFTW library unfortunately uses different complex type than
  UComplex standard unit (no surprise, they cannot depend on each other...),
  so we have to write our own routines. }

{ }
const
  CZero: complex_single = (re: 0; im: 0);

function CNormalized(const Z: complex_single): complex_single;
function CMod(const Z: complex_single): Single;
function CNorm(const Z: complex_single): Single;

operator* (const Z: Complex_Single; const X: Single): Complex_Single;

implementation

uses CastleVectors, CastleUtils;

{ Avoid fftw_execute_dft, fake it by simply copying.
  This is to avoid mysterious fftw bugs. }
{ $define FAKE_FFT_EXECUTE}

procedure fftw_execute_dft(plan:fftw_plan_single; i, o:Pcomplex_single);
  cdecl; external fftwlib name 'fftwf_execute_dft';

constructor TImageFftw.Create(AImage: TRGBImage);
var
  Color: Integer;
begin
  inherited Create;

  FImage := AImage;
  FSize := FImage.Width * FImage.Height;

  for Color := 0 to 2 do
  begin
    fftw_getmem(FImageComplex[Color], Size * SizeOf(complex_single));
    fftw_getmem(FImageF      [Color], Size * SizeOf(complex_single));

    { Make FFTW plans, since pointers FImageComplex and FImageF
      (for this Color index) are now constant. }

    PlanDFT[Color] := fftw_plan_dft_2d(Image.Height, Image.Width,
      FImageComplex[Color], FImageF[Color],
      fftw_forward, [fftw_estimate]);
    PlanIDFT[Color] := fftw_plan_dft_2d(Image.Height, Image.Width,
      FImageF[Color], FImageComplex[Color],
      fftw_backward, [fftw_estimate]);
  end;
end;

destructor TImageFftw.Destroy;
var
  Color: Integer;
begin
  for Color := 0 to 2 do
  begin
    fftw_destroy_plan(PlanDFT[Color]);
    fftw_destroy_plan(PlanIDFT[Color]);

    fftw_freemem(FImageComplex[Color]);
    fftw_freemem(FImageF      [Color]);
  end;
  inherited;
end;

function TImageFftw.GetImageF(Color: Integer): Pcomplex_single;
begin
  Result := FImageF[Color];
end;

procedure TImageFftw.DFT;
var
  Ptr: PVector3Byte;
  ImgComplexPtr: TImageComplex;
  Color, I: Integer;
begin
  { Copy Image to ImageComplex }

  Ptr := Image.RGBPixels;
  ImgComplexPtr := FImageComplex;

  for I := 0 to Size - 1 do
  begin
    for Color := 0 to 2 do
    begin
      ImgComplexPtr[Color]^.Re := Ptr^[Color];
      ImgComplexPtr[Color]^.Im := 0;
      Inc(ImgComplexPtr[Color]);
    end;

    Inc(Ptr);
  end;

  { Execute plans to convert ImageComplex to ImageF }
  {$ifdef FAKE_FFT_EXECUTE}
  for Color := 0 to 2 do
    Move(FImageComplex[Color]^, FImageF[Color]^, SizeOf(complex_single) * Size);
  {$else}
  for Color := 0 to 2 do
    { TODO: why does simple fftw_execute fail with SIGSEGV?
      The pointers in FImageComplex and FImageF should be constant?
    fftw_execute(PlanDFT[Color]); }
    fftw_execute_dft(PlanDFT[Color], FImageComplex[Color], FImageF[Color]);
  {$endif}
end;

procedure TImageFftw.ComplexToRGB(Complex: TImageComplex;
  Img: TRGBImage; const Scale: Single);
var
  Ptr: PVector3Byte;
  Color, I: Integer;
begin
  Ptr := Img.RGBPixels;

  for I := 0 to Size - 1 do
  begin
    for Color := 0 to 2 do
    begin
      Ptr^[Color] := Clamped(Round(Complex[Color]^.Re
        {$ifndef FAKE_FFT_EXECUTE} * Scale {$endif}),
        Low(Byte), High(Byte));
      Inc(Complex[Color]);
    end;

    Inc(Ptr);
  end;
end;

procedure TImageFftw.ImageFModulusAsRGB(Img: TRGBImage; const Scale: Single);
var
  Ptr: PVector3Byte;
  Color, ComplexIndex, X, Y: Integer;
  Complex: TImageComplex;
begin
  Ptr := Img.RGBPixels;
  Complex := FImageF;

  for Y := 0 to Img.Height - 1 do
    for X := 0 to Img.Width - 1 do
    begin
      { We shift complex image by (width/2,height/2) here.
        This is how FFT results on 2D images are usually shown, since
        the interesting stuff happens this way in the middle. }
      ComplexIndex := (X + Img.Width  div 2) mod Img.Width +
         Img.Width * ((Y + Img.Height div 2) mod Img.Height);

      for Color := 0 to 2 do
        Ptr^[Color] := Clamped(Round(CMod(Complex[Color][ComplexIndex])
          * Scale), Low(Byte), High(Byte));
      Inc(Ptr);
    end;
end;

procedure TImageFftw.IDFT;
var
  Color: Integer;
begin
  {$ifdef FAKE_FFT_EXECUTE}
  for Color := 0 to 2 do
    Move(FImageF[Color]^, FImageComplex[Color]^, SizeOf(complex_single) * Size);
  {$else}
  { Execute plans to convert ImageF to ImageComplex }
  for Color := 0 to 2 do
    fftw_execute(PlanIDFT[Color]);
  {$endif}

  { Copy ImageComplex to Image, also normalizing (dividing by Size)
    by the way. }
  ComplexToRGB(FImageComplex, Image, 1 / Size);
end;

{ Complex functions ---------------------------------------------------------- }

function CNormalized(const Z: complex_single): complex_single;
var
  M: Single;
begin
  M := CMod(Z);
  Result.Re := Z.Re / M;
  Result.Im := Z.Im / M;
end;

function CMod(const Z: complex_single): Single;
begin
  Result := Sqrt(Sqr(Z.Re) + Sqr(Z.Im));
end;

function CNorm(const Z: complex_single): Single;
begin
  Result := Sqr(Z.Re) + Sqr(Z.Im);
end;

operator* (const Z: Complex_Single; const X: Single): Complex_Single;
begin
  Result.Re := Z.Re * X;
  Result.Im := Z.Im * X;
end;

end.
