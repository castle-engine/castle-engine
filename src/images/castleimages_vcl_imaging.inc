{%MainUnit castleimages.pas}
{
  Copyright 2021-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handle images format using Delphi's Vcl.Imaging:
  https://docwiki.embarcadero.com/Libraries/Sydney/en/Vcl.Imaging }

{$ifndef FPC}
{$ifndef USE_VAMPYRE_IMAGING}

function LoadPNG_PngImage(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
var
  PngImage: TPngImage;
  ImageClass: TCastleImageClass;
  W, H: Cardinal;

  procedure CopyRGB;
  var
    X, Y: Integer;
    PngScanline: PVector3Byte;
    OutImage: PVector3Byte;
  begin
    for Y := 0 to H - 1 do
    begin
      PngScanline := PVector3Byte(PngImage.Scanline[H - 1 - Y]);
      OutImage := PVector3Byte(TCastleImage(Result).RowPtr(Y));
      Move(PngScanline^, OutImage^, W * 3);
      // convert BGR -> RGB
      for X := 0 to W - 1 do
      begin
        SwapValues(OutImage^.X, OutImage^.Z);
        Inc(OutImage);
      end;
    end;
  end;

  procedure CopyRGBAlpha;
  var
    X, Y: Integer;
    PngScanline: PVector3Byte;
    PngAlphaScanline: PByte;
    OutImage: PVector4Byte;
  begin
    for Y := 0 to H - 1 do
    begin
      PngScanline := PVector3Byte(PngImage.Scanline[H - 1 - Y]);
      PngAlphaScanline := PByte(PngImage.AlphaScanline[H - 1 - Y]);
      OutImage := PVector4Byte(TCastleImage(Result).RowPtr(Y));
      for X := 0 to W - 1 do
      begin
        // convert BGR -> RGB by the way
        OutImage^.X := PngScanline^[2];
        OutImage^.Y := PngScanline^[1];
        OutImage^.Z := PngScanline^[0];
        OutImage^.W := PngAlphaScanline^;
        Inc(PngScanline);
        Inc(PngAlphaScanline);
        Inc(OutImage);
      end;
    end;
  end;

begin
  PngImage := TPngImage.Create;
  try
    PngImage.LoadFromStream(Stream);

    if PngImage.Header.BitDepth <> 8 then
      raise EImageLoadError.Create('Loading PNG using PngImage: supports only 8-bit PNG images');

    case PngImage.Header.ColorType of
      PNG_COLOR_TYPE_GRAY      : ImageClass := TGrayscaleImage;
      PNG_COLOR_TYPE_RGB       : ImageClass := TRGBImage;
      PNG_COLOR_TYPE_RGB_ALPHA : ImageClass := TRGBAlphaImage;
      PNG_COLOR_TYPE_GRAY_ALPHA: ImageClass := TGrayscaleAlphaImage;
      else
        // possible in case of PNG_COLOR_TYPE_PALETTE
        raise EImageLoadError.Create('Loading PNG using PngImage: does not support PNG images with palette');
    end;

    // TODO: what happens in case of transparency by tRNS

    W := PngImage.Header.Width;
    H := PngImage.Header.Height;
    Result := ImageClass.Create(W, H);
    try
      case PngImage.Header.ColorType of
        PNG_COLOR_TYPE_RGB: CopyRgb;
        PNG_COLOR_TYPE_RGB_ALPHA: CopyRGBAlpha;
        // TODO: PNG_COLOR_TYPE_GRAY
        // TODO: PNG_COLOR_TYPE_GRAY_ALPHA
        else raise EImageLoadError.CreateFmt('Loading PNG using PngImage: Color type %d not implemented', [
          PngImage.Header.ColorType
        ]);
      end;
    except FreeAndNil(Result); raise end;
  finally FreeAndNil(PngImage) end;
end;

{$endif}
{$endif}

