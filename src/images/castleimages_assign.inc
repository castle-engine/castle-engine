{%MainUnit castleimages.pas}
{
  Copyright 2001-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various Assign method implementations for TEncodedImage descendants. }

{ TCastleImage --------------------------------------------------------------- }

procedure TCastleImage.Assign(const Source: TCastleImage);
begin
  if Source.ClassType = ClassType then
  begin
    SetSize(Source);
    // if Source.RawPixels = nil, then we're already freed by SetSize above
    if Source.RawPixels <> nil then
      Move(Source.RawPixels^, RawPixels^, Size);
    URL := Source.URL;
  end else
    raise EImageAssignmentError.CreateFmt('Cannot copy image contents from %s to %s',
      [Source.ClassName, ClassName]);

  { TODO:
    Maybe one day, this should just call
      DrawFrom(Source, 0, 0, Source.Width, Source.Height, dmOverwrite);
    instead of raising EImageAssignmentError.
    TCastleImage.Assign will not even need to be virtual then.

    However, for now, DrawFrom and friends do not handle the Z coordinate
    (they only draw from/to Z = 0).

    It is possible we will rather deprecate/simplify DrawFrom in the future,
    and do drawing only on GPU?
  }
end;

{ TRGBImage ------------------------------------------------------------------ }

procedure TRGBImage.Assign(const Source: TCastleImage);
var
  TargetRgb: PVector3Byte;

  procedure FromGrayscale;
  var
    SourceG: PByte;
    I: Cardinal;
  begin
    SourceG := TGrayscaleImage(Source).Pixels;
    if TGrayscaleImage(Source).TreatAsAlpha then
    begin
      for I := 1 to Width * Height * Depth do
      begin
        TargetRgb^ := TGrayscaleImage(Source).ColorWhenTreatedAsAlpha;
        Inc(SourceG);
        Inc(TargetRgb);
      end;
    end else
    begin
      for I := 1 to Width * Height * Depth do
      begin
        TargetRgb^.Data[0] := SourceG^;
        TargetRgb^.Data[1] := SourceG^;
        TargetRgb^.Data[2] := SourceG^;
        Inc(SourceG);
        Inc(TargetRgb);
      end;
    end;
  end;

  procedure FromGrayscaleAlpha;
  var
    SourceGa: PVector2Byte;
    I: Cardinal;
  begin
    SourceGa := TGrayscaleAlphaImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetRgb^[0] := SourceGa^[0];
      TargetRgb^[1] := SourceGa^[0];
      TargetRgb^[2] := SourceGa^[0];
      Inc(TargetRgb);
      Inc(SourceGa);
    end;
  end;

  procedure FromRGBAlpha;
  var
    SourceRgba: PVector4Byte;
    I: Cardinal;
  begin
    SourceRgba := TRGBAlphaImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      Move(SourceRgba^, TargetRgb^, SizeOf(TVector3Byte));
      Inc(TargetRgb);
      Inc(SourceRgba);
    end;
  end;

  procedure FromRGBFloat;
  var
    SourceFloatRgb: PVector3;
    I: Cardinal;
  begin
    SourceFloatRgb := TRGBFloatImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetRgb^ := Vector3Byte(SourceFloatRgb^);
      Inc(TargetRgb);
      Inc(SourceFloatRgb);
    end;
  end;

begin
  if Source is TGrayscaleImage then
  begin
    SetSize(Source);
    TargetRgb := Pixels; // must be done after SetSize
    FromGrayscale;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TGrayscaleAlphaImage then
  begin
    SetSize(Source);
    TargetRgb := Pixels; // must be done after SetSize
    FromGrayscaleAlpha;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TRGBAlphaImage then
  begin
    SetSize(Source);
    TargetRgb := Pixels; // must be done after SetSize
    FromRGBAlpha;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TRGBFloatImage then
  begin
    SetSize(Source);
    TargetRgb := Pixels; // must be done after SetSize
    FromRGBFloat;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

    inherited;
end;

{ TRGBAlphaImage ------------------------------------------------------------- }

procedure TRGBAlphaImage.Assign(const Source: TCastleImage);
var
  TargetRgba: PVector4Byte;

  procedure FromRGBFloat;
  var
    SourceFloatRgb: PVector3;
    I: Cardinal;
  begin
    SourceFloatRgb := TRGBFloatImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      PVector3Byte(TargetRgba)^ := Vector3Byte(SourceFloatRgb^);
      TargetRgba^[3] := High(Byte);
      Inc(TargetRgba);
      Inc(SourceFloatRgb);
    end;
  end;

  procedure FromRGB;
  var
    SourceRgb: PVector3Byte;
    I: Cardinal;
  begin
    SourceRgb := TRGBImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      Move(SourceRgb^, TargetRgba^, SizeOf(TVector3Byte));
      TargetRgba^[3] := High(Byte);
      Inc(TargetRgba);
      Inc(SourceRgb);
    end;
  end;

  procedure FromGrayscaleAlpha;
  var
    SourceGa: PVector2Byte;
    I: Cardinal;
  begin
    SourceGa := TGrayscaleAlphaImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetRgba^[0] := SourceGa^[0];
      TargetRgba^[1] := SourceGa^[0];
      TargetRgba^[2] := SourceGa^[0];
      TargetRgba^[3] := SourceGa^[1];
      Inc(TargetRgba);
      Inc(SourceGa);
    end;
  end;

  procedure FromGrayscale;
  var
    SourceG: PByte;
    I: Cardinal;
  begin
    SourceG := TGrayscaleImage(Source).Pixels;
    if TGrayscaleImage(Source).TreatAsAlpha then
    begin
      for I := 1 to Width * Height * Depth do
      begin
        PVector3Byte(TargetRgba)^ := TGrayscaleImage(Source).ColorWhenTreatedAsAlpha;
        TargetRgba^.Data[3] := SourceG^;
        Inc(SourceG);
        Inc(TargetRgba);
      end;
    end else
    begin
      for I := 1 to Width * Height * Depth do
      begin
        TargetRgba^.Data[0] := SourceG^;
        TargetRgba^.Data[1] := SourceG^;
        TargetRgba^.Data[2] := SourceG^;
        TargetRgba^.Data[3] := High(Byte);
        Inc(SourceG);
        Inc(TargetRgba);
      end;
    end;
  end;

begin
  if Source is TGrayscaleImage then
  begin
    SetSize(Source);
    TargetRgba := Pixels; // must be done after SetSize
    FromGrayscale;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TGrayscaleAlphaImage then
  begin
    SetSize(Source);
    TargetRgba := Pixels; // must be done after SetSize
    FromGrayscaleAlpha;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TRGBImage then
  begin
    SetSize(Source);
    TargetRgba := Pixels; // must be done after SetSize
    FromRGB;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TRGBFloatImage then
  begin
    SetSize(Source);
    TargetRgba := Pixels; // must be done after SetSize
    FromRGBFloat;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

    inherited;
end;

{ TGrayscaleImage ------------------------------------------------------------ }

procedure TGrayscaleImage.Assign(const Source: TCastleImage);
var
  TargetG: PByte;

  procedure FromGrayscaleAlpha;
  var
    SourceGa: PVector2Byte;
    I: Cardinal;
  begin
    SourceGa := TGrayscaleAlphaImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetG^ := SourceGa^.Data[0];
      Inc(TargetG);
      Inc(SourceGa);
    end;
  end;

  procedure FromRGBAlpha;
  var
    SourceRgba: PVector4Byte;
    I: Cardinal;
  begin
    SourceRgba := TRGBAlphaImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetG^ := GrayscaleValue(PVector3Byte(SourceRgba)^);
      Inc(TargetG);
      Inc(SourceRgba);
    end;
  end;

  procedure FromRGB;
  var
    SourceRgb: PVector3Byte;
    I: Cardinal;
  begin
    SourceRgb := TRGBImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetG^ := GrayscaleValue(SourceRgb^);
      Inc(TargetG);
      Inc(SourceRgb);
    end;
  end;

  procedure FromRGBFloat;
  var
    SourceFloatRgb: PVector3;
    I: Cardinal;
  begin
    SourceFloatRgb := TRGBFloatImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetG^ := GrayscaleValue(Vector3Byte(SourceFloatRgb^));
      Inc(TargetG);
      Inc(SourceFloatRgb);
    end;
  end;

begin
  if Source is TGrayscaleAlphaImage then
  begin
    SetSize(Source);
    TargetG := Pixels; // must be done after SetSize
    FromGrayscaleAlpha;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TRGBAlphaImage then
  begin
    SetSize(Source);
    TargetG := Pixels; // must be done after SetSize
    FromRGBAlpha;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TRGBImage then
  begin
    SetSize(Source);
    TargetG := Pixels; // must be done after SetSize
    FromRGB;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TRGBFloatImage then
  begin
    SetSize(Source);
    TargetG := Pixels; // must be done after SetSize
    FromRGBFloat;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

    inherited;
end;

{ TGrayscaleAlphaImage ------------------------------------------------------- }

procedure TGrayscaleAlphaImage.Assign(const Source: TCastleImage);
var
  TargetGa: PVector2Byte;

  procedure FromRGBAlpha;
  var
    SourceRgba: PVector4Byte;
    I: Cardinal;
  begin
    SourceRgba := TRGBAlphaImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetGa^.Data[0] := GrayscaleValue(PVector3Byte(SourceRgba)^);
      TargetGa^.Data[1] := SourceRgba^.Data[3];
      Inc(TargetGa);
      Inc(SourceRgba);
    end;
  end;

  procedure FromRGB;
  var
    SourceRgb: PVector3Byte;
    I: Cardinal;
  begin
    SourceRgb := TRGBImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetGa^.Data[0] := GrayscaleValue(SourceRgb^);
      TargetGa^.Data[1] := High(Byte);
      Inc(TargetGa);
      Inc(SourceRgb);
    end;
  end;

  procedure FromRGBFloat;
  var
    SourceFloatRgb: PVector3;
    I: Cardinal;
  begin
    SourceFloatRgb := TRGBFloatImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetGa^.Data[0] := GrayscaleValue(Vector3Byte(SourceFloatRgb^));
      TargetGa^.Data[1] := High(Byte);
      Inc(TargetGa);
      Inc(SourceFloatRgb);
    end;
  end;

  procedure FromGrayscale;
  var
    SourceG: PByte;
    I: Cardinal;
  begin
    SourceG := TGrayscaleImage(Source).Pixels;
    if TGrayscaleImage(Source).TreatAsAlpha then
    begin
      for I := 1 to Width * Height * Depth do
      begin
        TargetGa^.Data[0] := TGrayscaleImage(Source).GrayscaleColorWhenTreatedAsAlpha;
        TargetGa^.Data[1] := SourceG^;
        Inc(SourceG);
        Inc(TargetGa);
      end;
    end else
    begin
      for I := 1 to Width * Height * Depth do
      begin
        TargetGa^.Data[0] := SourceG^;
        TargetGa^.Data[1] := High(Byte);
        Inc(SourceG);
        Inc(TargetGa);
      end;
    end;
  end;

begin
  if Source is TRGBAlphaImage then
  begin
    SetSize(Source);
    TargetGa := Pixels; // must be done after SetSize
    FromRGBAlpha;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TRGBImage then
  begin
    SetSize(Source);
    TargetGa := Pixels; // must be done after SetSize
    FromRGB;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TRGBFloatImage then
  begin
    SetSize(Source);
    TargetGa := Pixels; // must be done after SetSize
    FromRGBFloat;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TGrayscaleImage then
  begin
    SetSize(Source);
    TargetGa := Pixels; // must be done after SetSize
    FromGrayscale;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

    inherited;
end;

{ TRGBFloatImage ------------------------------------------------------- }

procedure TRGBFloatImage.Assign(const Source: TCastleImage);
var
  TargetFloatRgb: PVector3;

  procedure FromRGBAlpha;
  var
    SourceRgba: PVector4Byte;
    I: Cardinal;
  begin
    SourceRgba := TRGBAlphaImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetFloatRgb^ := Vector3(PVector3Byte(SourceRgba)^);
      Inc(TargetFloatRgb);
      Inc(SourceRgba);
    end;
  end;

  procedure FromRGB;
  var
    SourceRgb: PVector3Byte;
    I: Cardinal;
  begin
    SourceRgb := TRGBImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetFloatRgb^ := Vector3(SourceRgb^);
      Inc(TargetFloatRgb);
      Inc(SourceRgb);
    end;
  end;

  procedure FromGrayscaleAlpha;
  var
    SourceGa: PVector2Byte;
    I: Cardinal;
  begin
    SourceGa := TGrayscaleAlphaImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      TargetFloatRgb^ := Vector3(
        SourceGa^.Data[0] / High(Byte),
        SourceGa^.Data[0] / High(Byte),
        SourceGa^.Data[0] / High(Byte)
      );
      Inc(TargetFloatRgb);
      Inc(SourceGa);
    end;
  end;

  procedure FromGrayscale;
  var
    SourceG: PByte;
    I: Cardinal;
  begin
    SourceG := TGrayscaleImage(Source).Pixels;
    if TGrayscaleImage(Source).TreatAsAlpha then
    begin
      for I := 1 to Width * Height * Depth do
      begin
        TargetFloatRgb^ := Vector3(
          TGrayscaleImage(Source).GrayscaleColorWhenTreatedAsAlpha / High(Byte),
          TGrayscaleImage(Source).GrayscaleColorWhenTreatedAsAlpha / High(Byte),
          TGrayscaleImage(Source).GrayscaleColorWhenTreatedAsAlpha / High(Byte)
        );
        Inc(TargetFloatRgb);
        Inc(SourceG);
      end;
    end else
    begin
      for I := 1 to Width * Height * Depth do
      begin
        TargetFloatRgb^ := Vector3(
          SourceG^ / High(Byte),
          SourceG^ / High(Byte),
          SourceG^ / High(Byte)
        );
        Inc(TargetFloatRgb);
        Inc(SourceG);
      end;
    end;
  end;

begin
  if Source is TRGBAlphaImage then
  begin
    SetSize(Source);
    TargetFloatRgb := Pixels; // must be done after SetSize
    FromRGBAlpha;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TRGBImage then
  begin
    SetSize(Source);
    TargetFloatRgb := Pixels; // must be done after SetSize
    FromRGB;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TGrayscaleAlphaImage then
  begin
    SetSize(Source);
    TargetFloatRgb := Pixels; // must be done after SetSize
    FromGrayscaleAlpha;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

  if Source is TGrayscaleImage then
  begin
    SetSize(Source);
    TargetFloatRgb := Pixels; // must be done after SetSize
    FromGrayscale;
    URL := Source.URL + '[Assigned to ' + ClassName + ']';
  end else

    inherited;
end;
