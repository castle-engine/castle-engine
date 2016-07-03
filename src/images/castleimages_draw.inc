{
  Copyright 2001-2015 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implementations of DrawFrom, DrawTo and DrawFromCore methods. }

{ TCastleImage --------------------------------------------------------------- }

const
  SErrBlendModeNotImplemented = 'Blend mode not implemented (drawing from image class %s to %s)';

procedure TCastleImage.DrawFromCore(Source: TCastleImage;
  X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
  const Mode: TDrawMode);
var
  Line: Integer;
  Ptr, SourcePtr: Pointer;
  RowWidth, SourceRowWidth, SourceCopyRowWidth: Cardinal;
begin
  if Source.ClassType <> ClassType then
    raise EImageDrawError.CreateFmt('Blend mode for this source/destination combination may be not implemented. Cannot draw pixels from image class %s to %s',
      [Source.ClassName, ClassName]);

  if Mode <> dmBlend then
    raise EImageDrawError.CreateFmt('Blend mode for this source/destination combination may be not implemented. Base TCastleImage.DrawFromCore cannot draw pixels with Mode <> dmBlend, override DrawFromCore to implement this correctly, from image class %s to %s',
      [Source.ClassName, ClassName]);

  Ptr := PixelPtr(X, Y);
  RowWidth := Width * PixelSize;

  SourcePtr := Source.PixelPtr(SourceX, SourceY);
  SourceRowWidth := Source.Width * Source.PixelSize;
  SourceCopyRowWidth := SourceWidth * Source.PixelSize;

  for Line := 0 to Integer(SourceHeight) - 1 do
  begin
    Move(SourcePtr^, Ptr^, SourceCopyRowWidth);
    PtrUInt(Ptr) := PtrUInt(Ptr) + RowWidth;
    PtrUInt(SourcePtr) := PtrUInt(SourcePtr) + SourceRowWidth;
  end;
end;

procedure TCastleImage.DrawFrom(Source: TCastleImage;
  X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
  const Mode: TDrawMode);
begin
  if X < 0 then
  begin
    SourceX += -X;
    SourceWidth -= -X;
    X := 0;
  end;

  if Y < 0 then
  begin
    SourceY += -Y;
    SourceHeight -= -Y;
    Y := 0;
  end;

  if SourceX < 0 then
  begin
    X += -SourceX;
    SourceWidth -= -SourceX;
    SourceX := 0;
  end;

  if SourceY < 0 then
  begin
    Y += -SourceY;
    SourceHeight -= -SourceY;
    SourceY := 0;
  end;

  SourceWidth  := Min(SourceWidth , Width  - X, Source.Width );
  SourceHeight := Min(SourceHeight, Height - Y, Source.Height);

  if (SourceWidth > 0) and
     (SourceHeight > 0) and
     (SourceX < Source.Width) and
     (SourceY < Source.Height) then
    DrawFromCore(Source, X, Y, SourceX, SourceY, SourceWidth, SourceHeight, Mode);
end;

procedure TCastleImage.DrawFrom(Source: TCastleImage; const X, Y: Integer;
  const Mode: TDrawMode);
begin
  DrawFrom(Source, X, Y, 0, 0, Source.Width, Source.Height, Mode);
end;

procedure TCastleImage.DrawTo(Destination: TCastleImage; const X, Y: Integer;
  const Mode: TDrawMode);
begin
  Destination.DrawFrom(Self, X, Y, Mode);
end;

{ Helper routines ------------------------------------------------------------ }

function BlendBytes(const Dest, Source, Opacity: Byte): Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif}
var
  W: Word;
begin
  W :=
    Word(Dest  ) * (255 - Opacity) div 255 +
    Word(Source) * Opacity         div 255;
  if W > 255 then W := 255;
  Result := W;
end;

function AddBytes(const Dest, Source, Opacity: Byte): Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif}
var
  W: Word;
begin
  W := Dest + Word(Source) * Opacity div 255;
  if W > 255 then W := 255;
  Result := W;
end;

function AddBytesPremultiplied(const Dest, Source: Byte): Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif}
var
  W: Word;
begin
  W := Dest + Source;
  if W > 255 then W := 255;
  Result := W;
end;

function MultiplyBytes(const Dest, Source: Byte): Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif}
var
  W: Word;
begin
  W := Dest * Word(Source) div 255;
  if W > 255 then W := 255;
  Result := W;
end;

function MultiplyBytesOpaque(const Dest, Source, Opacity: Byte): Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif}
var
  W: Word;
begin
  W := Dest * (255 - Opacity) div 255 + Dest * Word(Source) div 255 * Opacity div 255;
  if W > 255 then W := 255;
  Result := W;
end;


{ Drawing when TRGBAlphaImage is a destination ------------------------------- }

procedure TRGBAlphaImage.DrawFromCore(Source: TCastleImage;
  X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
  const Mode: TDrawMode);

  procedure SourceRGB(Source: TRGBImage);
  var
    PSource: PVector3Byte;
    PDest: PVector4Byte;
    DestX, DestY: Integer;
  begin
    case Mode of
      dmBlend:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PVector3Byte(PDest)^ := PSource^;
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytes(PDest^[0],PSource^[0]);
              PDest^[1] := MultiplyBytes(PDest^[1],PSource^[1]);
              PDest^[2] := MultiplyBytes(PDest^[2],PSource^[2]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := AddBytesPremultiplied(PDest^[0], PSource^[0]);
            PDest^[1] := AddBytesPremultiplied(PDest^[1], PSource^[1]);
            PDest^[2] := AddBytesPremultiplied(PDest^[2], PSource^[2]);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceRGBAlpha(Source: TRGBAlphaImage);
  var
    PSource: PVector4Byte;
    PDest: PVector4Byte;
    DestX, DestY: Integer;

    //variables for smart-blending mode
    alpha1,alpha2,alpha1d,alphaSum:single;
  begin
    case Mode of
      dmBlend:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := BlendBytes(PDest^[0], PSource^[0], PSource^[3]);
            PDest^[1] := BlendBytes(PDest^[1], PSource^[1], PSource^[3]);
            PDest^[2] := BlendBytes(PDest^[2], PSource^[2], PSource^[3]);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmBlendSmart:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            //get alpha in 0..1 range
            alpha1 := PDest^[3]/255;
            alpha2 := PSource^[3]/255;
            //calculate alpha-sums according to https://en.wikipedia.org/wiki/Alpha_compositing
            alpha1d := alpha1*(1-alpha2);
            alphasum := alpha1+(1-alpha1)*alpha2;
            // if alpha sum >0 then do the pixel colors and alpha summation according to the formula
            if alphaSum > 0 then
            begin
              PDest^[0] := Round((PDest^[0]*alpha1d + PSource^[0]*alpha2)/alphaSum);
              PDest^[1] := Round((PDest^[1]*alpha1d + PSource^[1]*alpha2)/alphaSum);
              PDest^[2] := Round((PDest^[2]*alpha1d + PSource^[2]*alpha2)/alphaSum);
              PDest^[3] := Round(255*alphaSum);
            end;
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytes(PDest^[0],PSource^[0]);
              PDest^[1] := MultiplyBytes(PDest^[1],PSource^[1]);
              PDest^[2] := MultiplyBytes(PDest^[2],PSource^[2]);
              PDest^[3] := MultiplyBytes(PDest^[3],PSource^[3]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        if Source.PremultipliedAlpha then
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytesPremultiplied(PDest^[0], PSource^[0]);
              PDest^[1] := AddBytesPremultiplied(PDest^[1], PSource^[1]);
              PDest^[2] := AddBytesPremultiplied(PDest^[2], PSource^[2]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end else
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytes(PDest^[0], PSource^[0], PSource^[3]);
              PDest^[1] := AddBytes(PDest^[1], PSource^[1], PSource^[3]);
              PDest^[2] := AddBytes(PDest^[2], PSource^[2], PSource^[3]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceGrayscaleAlpha(Source: TGrayscaleAlphaImage);
  var
    PSource: PVector2Byte;
    PDest: PVector4Byte;
    DestX, DestY: Integer;

    //variables for smart-blending mode
    alpha1,alpha2,alpha1d,alphaSum:single;
  begin
    case Mode of
      dmBlend:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := BlendBytes(PDest^[0], PSource^[0], PSource^[1]);
            PDest^[1] := BlendBytes(PDest^[1], PSource^[0], PSource^[1]);
            PDest^[2] := BlendBytes(PDest^[2], PSource^[0], PSource^[1]);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmBlendSmart:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            //get alpha in 0..1 range
            alpha1 := PDest^[3]/255;
            alpha2 := PSource^[1]/255;
            //calculate alpha-sums according to https://en.wikipedia.org/wiki/Alpha_compositing
            alpha1d := alpha1*(1-alpha2);
            alphasum := alpha1+(1-alpha1)*alpha2;
            // if alpha sum >0 then do the pixel colors and alpha summation according to the formula
            if alphaSum > 0 then
            begin
              PDest^[0] := Round((PDest^[0]*alpha1d + PSource^[0]*alpha2)/alphaSum);
              PDest^[1] := Round((PDest^[1]*alpha1d + PSource^[0]*alpha2)/alphaSum);
              PDest^[2] := Round((PDest^[2]*alpha1d + PSource^[0]*alpha2)/alphaSum);
              PDest^[3] := Round(255*alphaSum);
            end;
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytes(PDest^[0],PSource^[0]);
              PDest^[1] := MultiplyBytes(PDest^[1],PSource^[0]);
              PDest^[2] := MultiplyBytes(PDest^[2],PSource^[0]);
              PDest^[3] := MultiplyBytes(PDest^[3],PSource^[1]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        {if Source.PremultipliedAlpha then
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytesPremultiplied(PDest^[0], PSource^[0]);
              PDest^[1] := AddBytesPremultiplied(PDest^[1], PSource^[0]);
              PDest^[2] := AddBytesPremultiplied(PDest^[2], PSource^[0]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end else}
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytes(PDest^[0], PSource^[0], PSource^[1]);
              PDest^[1] := AddBytes(PDest^[1], PSource^[0], PSource^[1]);
              PDest^[2] := AddBytes(PDest^[2], PSource^[0], PSource^[1]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceGrayscale(Source: TGrayscaleImage);
  var
    PSource: PByte;
    PDest: PVector4Byte;
    DestX, DestY: Integer;
  begin
    case Mode of
      dmBlend:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := PSource^;
            PDest^[1] := PSource^;
            PDest^[2] := PSource^;
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytes(PDest^[0],PSource^);
              PDest^[1] := MultiplyBytes(PDest^[1],PSource^);
              PDest^[2] := MultiplyBytes(PDest^[2],PSource^);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := AddBytesPremultiplied(PDest^[0], PSource^);
            PDest^[1] := AddBytesPremultiplied(PDest^[1], PSource^);
            PDest^[2] := AddBytesPremultiplied(PDest^[2], PSource^);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

begin
  if Source is TRGBImage then
    SourceRGB(TRGBImage(Source)) else
  if Source is TRGBAlphaImage then
    SourceRGBAlpha(TRGBAlphaImage(Source)) else
  if Source is TGrayscaleAlphaImage then
    SourceGrayscaleAlpha(TGrayscaleAlphaImage(Source)) else
  if Source is TGrayscaleImage then
    SourceGrayscale(TGrayscaleImage(Source)) else
    inherited;
end;

{ Drawing when TRGBImage is a destination ------------------------------------ }

procedure TRGBImage.DrawFromCore(Source: TCastleImage;
  X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
  const Mode: TDrawMode);

  procedure SourceRGB(Source: TRGBImage);
  var
    PSource: PVector3Byte;
    PDest: PVector3Byte;
    DestX, DestY: Integer;
  begin
    case Mode of
      { dmBlend is handled by inherited }
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytes(PDest^[0],PSource^[0]);
              PDest^[1] := MultiplyBytes(PDest^[1],PSource^[1]);
              PDest^[2] := MultiplyBytes(PDest^[2],PSource^[2]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := AddBytesPremultiplied(PDest^[0], PSource^[0]);
            PDest^[1] := AddBytesPremultiplied(PDest^[1], PSource^[1]);
            PDest^[2] := AddBytesPremultiplied(PDest^[2], PSource^[2]);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceRGBAlpha(Source: TRGBAlphaImage);
  var
    PSource: PVector4Byte;
    PDest: PVector3Byte;
    DestX, DestY: Integer;
  begin
    case Mode of
      dmBlend,dmBlendSmart:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := BlendBytes(PDest^[0], PSource^[0], PSource^[3]);
            PDest^[1] := BlendBytes(PDest^[1], PSource^[1], PSource^[3]);
            PDest^[2] := BlendBytes(PDest^[2], PSource^[2], PSource^[3]);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytesOpaque(PDest^[0],PSource^[0],PSource^[3]);
              PDest^[1] := MultiplyBytesOpaque(PDest^[1],PSource^[1],PSource^[3]);
              PDest^[2] := MultiplyBytesOpaque(PDest^[2],PSource^[2],PSource^[3]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        if Source.PremultipliedAlpha then
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytesPremultiplied(PDest^[0], PSource^[0]);
              PDest^[1] := AddBytesPremultiplied(PDest^[1], PSource^[1]);
              PDest^[2] := AddBytesPremultiplied(PDest^[2], PSource^[2]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end else
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytes(PDest^[0], PSource^[0], PSource^[3]);
              PDest^[1] := AddBytes(PDest^[1], PSource^[1], PSource^[3]);
              PDest^[2] := AddBytes(PDest^[2], PSource^[2], PSource^[3]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceGrayscaleAlpha(Source: TGrayscaleAlphaImage);
  var
    PSource: PVector2Byte;
    PDest: PVector3Byte;
    DestX, DestY: Integer;
  begin
    case Mode of
      dmBlend,dmBlendSmart:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := BlendBytes(PDest^[0], PSource^[0], PSource^[1]);
            PDest^[1] := BlendBytes(PDest^[1], PSource^[0], PSource^[1]);
            PDest^[2] := BlendBytes(PDest^[2], PSource^[0], PSource^[1]);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytesOpaque(PDest^[0],PSource^[0],PSource^[1]);
              PDest^[1] := MultiplyBytesOpaque(PDest^[1],PSource^[0],PSource^[1]);
              PDest^[2] := MultiplyBytesOpaque(PDest^[2],PSource^[0],PSource^[1]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        {if Source.PremultipliedAlpha then
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytesPremultiplied(PDest^[0], PSource^[0]);
              PDest^[1] := AddBytesPremultiplied(PDest^[1], PSource^[0]);
              PDest^[2] := AddBytesPremultiplied(PDest^[2], PSource^[0]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end else}
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytes(PDest^[0], PSource^[0], PSource^[1]);
              PDest^[1] := AddBytes(PDest^[1], PSource^[0], PSource^[1]);
              PDest^[2] := AddBytes(PDest^[2], PSource^[0], PSource^[1]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceGrayscale(Source: TGrayscaleImage);
  var
    PSource: PByte;
    PDest: PVector3Byte;
    DestX, DestY: Integer;
  begin
    case Mode of
      dmBlend:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := PSource^;
            PDest^[1] := PSource^;
            PDest^[2] := PSource^;
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytes(PDest^[0],PSource^);
              PDest^[1] := MultiplyBytes(PDest^[1],PSource^);
              PDest^[2] := MultiplyBytes(PDest^[2],PSource^);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := AddBytesPremultiplied(PDest^[0], PSource^);
            PDest^[1] := AddBytesPremultiplied(PDest^[1], PSource^);
            PDest^[2] := AddBytesPremultiplied(PDest^[2], PSource^);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

begin
  if (Source is TRGBImage) and ((Mode = dmAdd) or (Mode = dmMultiply)) then
    SourceRGB(TRGBImage(Source)) else
  if Source is TRGBAlphaImage then
    SourceRGBAlpha(TRGBAlphaImage(Source)) else
  if Source is TGrayscaleAlphaImage then
    SourceGrayscaleAlpha(TGrayscaleAlphaImage(Source)) else
  if Source is TGrayscaleImage then
    SourceGrayscale(TGrayscaleImage(Source)) else
    inherited;
end;

{ Drawing when TGrayscaleImage is a destination ------------------------------ }

procedure TGrayscaleImage.DrawFromCore(Source: TCastleImage;
  X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
  const Mode: TDrawMode);

  procedure SourceRGBAlpha(Source: TRGBAlphaImage);
  var
    PSource: PVector4Byte;
    PDest: PByte;
    DestX, DestY: Integer;
  begin
    case Mode of
      dmBlend,dmBlendSmart:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^ := BlendBytes(PDest^, GrayscaleValue(PSource^), PSource^[3]);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^ := MultiplyBytesOpaque(PDest^,GrayscaleValue(PSource^),PSource^[3]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        if Source.PremultipliedAlpha then
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^ := AddBytesPremultiplied(PDest^, GrayscaleValue(PSource^));
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end else
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^ := AddBytes(PDest^, GrayscaleValue(PSource^), PSource^[3]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceGrayscaleAlpha(Source: TGrayscaleAlphaImage);
  var
    PSource: PVector2Byte;
    PDest: PByte;
    DestX, DestY: Integer;
  begin
    case Mode of
      dmBlend,dmBlendSmart:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^ := BlendBytes(PDest^, PSource^[0], PSource^[1]);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^ := MultiplyBytesOpaque(PDest^,PSource^[0],PSource^[1]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        {if Source.PremultipliedAlpha then
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^ := AddBytesPremultiplied(PDest^, PSource^[0]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end else}
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^ := AddBytes(PDest^, PSource^[0], PSource^[1]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceGrayscale(Source: TGrayscaleImage);
  var
    PSource: PByte;
    PDest: PByte;
    DestX, DestY: Integer;
  begin
    case Mode of
      { dmBlend handled by inherited }
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^ := MultiplyBytes(PDest^,PSource^);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        {if Source.PremultipliedAlpha then
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^ := AddBytesPremultiplied(PDest^, PSource^[0]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end else}
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^ := AddBytesPremultiplied(PDest^, PSource^);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceRGB(Source: TRGBImage);
  var
    PSource: PVector3Byte;
    PDest: PByte;
    DestX, DestY: Integer;
  begin
    case Mode of
      dmBlend:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^ := GrayscaleValue(PSource^);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^ := MultiplyBytes(PDest^,GrayscaleValue(PSource^));
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^ := AddBytesPremultiplied(PDest^, GrayscaleValue(PSource^));
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

begin
  if Source is TRGBAlphaImage then
    SourceRGBAlpha(TRGBAlphaImage(Source)) else
  if (Source is TGrayscaleImage) and ((Mode = dmAdd) or (Mode = dmMultiply)) then
    SourceGrayscale(TGrayscaleImage(Source)) else
  if Source is TGrayscaleAlphaImage then
    SourceGrayscaleAlpha(TGrayscaleAlphaImage(Source)) else
  if Source is TRGBImage then
    SourceRGB(TRGBImage(Source)) else
    inherited;
end;

{ Drawing when TRGBAlphaImage is a destination ------------------------------- }

procedure TGrayscaleAlphaImage.DrawFromCore(Source: TCastleImage;
  X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
  const Mode: TDrawMode);

  procedure SourceRGB(Source: TRGBImage);
  var
    PSource: PVector3Byte;
    PDest: PVector2Byte;
    DestX, DestY: Integer;
  begin
    case Mode of
      dmBlend:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := GrayscaleValue(PSource^);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytes(PDest^[0],GrayscaleValue(PSource^));
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := AddBytesPremultiplied(PDest^[0], GrayscaleValue(PSource^));
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceRGBAlpha(Source: TRGBAlphaImage);
  var
    PSource: PVector4Byte;
    PDest: PVector2Byte;
    DestX, DestY: Integer;

    //variables for smart-blending mode
    alpha1,alpha2,alpha1d,alphaSum:single;
  begin
    case Mode of
      dmBlend:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := BlendBytes(PDest^[0], GrayscaleValue(PSource^), PSource^[3]);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmBlendSmart:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            //get alpha in 0..1 range
            alpha1 := PDest^[1]/255;
            alpha2 := PSource^[3]/255;
            //calculate alpha-sums according to https://en.wikipedia.org/wiki/Alpha_compositing
            alpha1d := alpha1*(1-alpha2);
            alphasum := alpha1+(1-alpha1)*alpha2;
            // if alpha sum >0 then do the pixel colors and alpha summation according to the formula
            if alphaSum > 0 then
            begin
              PDest^[0] := Round((PDest^[0]*alpha1d + GrayscaleValue(PSource^)*alpha2)/alphaSum);
              PDest^[1] := Round(255*alphaSum);
            end;
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytes(PDest^[0],GrayscaleValue(PSource^));
              PDest^[1] := MultiplyBytes(PDest^[1],PSource^[3]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        if Source.PremultipliedAlpha then
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytesPremultiplied(PDest^[0], GrayscaleValue(PSource^));
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end else
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytes(PDest^[0], GrayscaleValue(PSource^), PSource^[3]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceGrayscaleAlpha(Source: TGrayscaleAlphaImage);
  var
    PSource: PVector2Byte;
    PDest: PVector2Byte;
    DestX, DestY: Integer;

    //variables for smart-blending mode
    alpha1,alpha2,alpha1d,alphaSum:single;
  begin
    case Mode of
      dmBlend:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := BlendBytes(PDest^[0], PSource^[0], PSource^[1]);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmBlendSmart:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            //get alpha in 0..1 range
            alpha1 := PDest^[1]/255;
            alpha2 := PSource^[1]/255;
            //calculate alpha-sums according to https://en.wikipedia.org/wiki/Alpha_compositing
            alpha1d := alpha1*(1-alpha2);
            alphasum := alpha1+(1-alpha1)*alpha2;
            // if alpha sum >0 then do the pixel colors and alpha summation according to the formula
            if alphaSum > 0 then
            begin
              PDest^[0] := Round((PDest^[0]*alpha1d + PSource^[0]*alpha2)/alphaSum);
              PDest^[1] := Round(255*alphaSum);
            end;
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytes(PDest^[0],PSource^[0]);
              PDest^[1] := MultiplyBytes(PDest^[1],PSource^[1]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        {if Source.PremultipliedAlpha then
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytesPremultiplied(PDest^[0], PSource^[0]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end else}
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := AddBytes(PDest^[0], PSource^[0], PSource^[1]);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

  procedure SourceGrayscale(Source: TGrayscaleImage);
  var
    PSource: PByte;
    PDest: PVector2Byte;
    DestX, DestY: Integer;
  begin
    case Mode of
      dmBlend:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := PSource^;
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      dmMultiply:
        begin
          for DestY := Y to Y + SourceHeight - 1 do
          begin
            PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
            PDest := PixelPtr(X, DestY);
            for DestX := X to X + SourceWidth - 1 do
            begin
              PDest^[0] := MultiplyBytes(PDest^[0],PSource^);
              Inc(PSource);
              Inc(PDest);
            end;
          end;
        end;
      dmAdd:
        for DestY := Y to Y + SourceHeight - 1 do
        begin
          PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
          PDest := PixelPtr(X, DestY);
          for DestX := X to X + SourceWidth - 1 do
          begin
            PDest^[0] := AddBytesPremultiplied(PDest^[0], PSource^);
            Inc(PSource);
            Inc(PDest);
          end;
        end;
      else raise EImageDrawError.CreateFmt(SErrBlendModeNotImplemented, [Source.ClassName, ClassName]);
    end;
  end;

begin
  if Source is TRGBImage then
    SourceRGB(TRGBImage(Source)) else
  if Source is TRGBAlphaImage then
    SourceRGBAlpha(TRGBAlphaImage(Source)) else
  if Source is TGrayscaleAlphaImage then
    SourceGrayscaleAlpha(TGrayscaleAlphaImage(Source)) else
  if Source is TGrayscaleImage then
    SourceGrayscale(TGrayscaleImage(Source)) else
    inherited;
end;
