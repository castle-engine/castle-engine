{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    XPM writer implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPWriteXPM;

interface

uses FPImage, classes, sysutils;

type

  TFPWriterXPM = class (TFPCustomImageWriter)
    private
      FPalChars : string;
      FColorFormat : string;
      FColorShift : word;
      FColorSize : byte;
      procedure SetColorSize (AValue : byte);
      function ColorToHex (const c:TFPColor) : string;
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); override;
    public
      constructor Create; override;
      property PalChars : string read FPalChars write FPalChars;
      property ColorCharSize : byte read FColorSize write SetColorSize;
      // number of characters to use for 1 colorcomponent
  end;


implementation

const
  DefPalChars = '.,-*abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#;:=+%$()[]';

constructor TFPWriterXPM.create;
begin
  inherited create;
  PalChars := DefPalChars;
  FColorSize := 4;
end;

procedure TFPWriterXPM.SetColorSize (AValue : byte);
begin
  if AValue > 3 then
    FColorSize := 4
  else if AValue = 0 then
    FColorSize := 1
  else
    FColorSize := AValue;
end;

function TFPWriterXPM.ColorToHex (const c:TFPColor) : string;
var r,g,b : word;
begin
  with c do
    begin
    r := red shr FColorShift;
    g := green shr FColorShift;
    b := blue shr FColorShift;
    end;
  result := format(FColorFormat,[r,g,b]);
end;

procedure TFPWriterXPM.InternalWrite (Str:TStream; Img:TFPCustomImage);
var p, l : TStringList;
    c, len, r, t : integer;
    TmpPalette, Palette: TFPPalette;
  procedure BuildPaletteStrings;
  var r,c,e : integer;
    procedure MakeCodes (const head:string; charplace:integer);
    var r : integer;
    begin
      r := 1;
      dec (charplace);
      while (r <= e) and (c >= 0) do
        begin
        if Charplace > 0 then
          MakeCodes (head+PalChars[r],charplace)
        else begin
          p.Add (head+PalChars[r]);
          dec(c);
        end;
        inc (r);
        end;
    end;
  begin
    // Calculate length of codes
    len := 1;
    e := length(PalChars);
    r := e;
    c := Palette.count;
    while (r <= c) do
      begin
      inc (len);
      r := r * e;
      end;
    MakeCodes ('',len);
  end;
  procedure InitConsts;
  var fmt : string;
  begin
    fmt := inttostr(FColorSize);
    fmt := '%'+fmt+'.'+fmt+'x';
    FColorFormat := fmt+fmt+fmt;
    case FColorSize of
      1 : FColorShift := 12;
      2 : FColorShift := 8;
      3 : FColorShift := 4;
      else FColorShift := 0;
    end;
  end;
var s : string;
begin
  l := TStringList.Create;
  p := TStringList.Create;
  TmpPalette := nil;
  try
    l.Add ('/* XPM */');
    l.Add ('static char *graphic[] = {');
    Palette := img.palette;
    if not Assigned(Palette) then begin
      TmpPalette := TFPPalette.Create(0);
      TmpPalette.Build(Img);
      Palette := TmpPalette;
    end;
    c := Palette.count;
    BuildPaletteStrings;
    l.add (format('"%d %d %d %d",',[img.width,img.height,c,len]));
    InitConsts;
    for r := 0 to c-1 do
      begin
      if Palette[r] <> colTransparent then
        l.Add (format('"%s c #%s",',[p[r],ColorToHex(Palette.color[r])]))
      else
        l.Add (format('"%s c None",',[p[r]]));
      end;
    for r := 0 to img.Height-1 do
      begin
      s := '';
      for t := 0 to img.Width-1 do
        if Assigned(TmpPalette) then
          s := s + p[TmpPalette.IndexOf(img.Colors[t,r])]
        else
          s := s + p[img.pixels[t,r]];
      s := '"'+s+'"';
      if r < img.Height-1 then
        s := s + ',';
      l.Add (s);
      end;
    l.Add ('};');
  finally
    TmpPalette.Free;
    l.SaveToStream (Str);
    p.Free;
    l.Free;
  end;
end;

initialization
  ImageHandlers.RegisterImageWriter ('XPM Format', 'xpm', TFPWriterXPM);
end.
