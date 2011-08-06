{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Make any SVG graphic grayscale.

  Changes every color within SVG file to grayscale, optionally also scaling
  the stroke width to reflect color intensity. The rest of the SVG content
  stays untouched.

  Call like
    svg_grayscale INPUT-FILE-NAME OUTPUT-FILE-NAME [WIDTH-SCALE]

  It's Ok for OUTPUT-FILE-NAME to be the same as INPUT-FILE-NAME.

  WIDTH-SCALE is a float, says how much of stroke width is under the
  influence of color intensity (color intensity = just grayscale value of
  this color). New width is calculated as
    MapRange(ColorIntensity, 0, 1, OldWidth * (1 - WIDTH-SCALE), OldWidth * (1 + WIDTH-SCALE))
  E.g.
    WIDTH-SCALE = 0 says "don't modify width at all"
    WIDTH-SCALE = 1 says "new width is between 0 and 2*old width,
      depending on color intensity, black is thinnner and white is thicker"
  Negative WIDTH-SCALE also works Ok (since MapRange handles it Ok),
  and it changes the direction of intensity influence. For example
    WIDTH-SCALE = -1 says "new width is between 0 and 2*old width,
      depending on color intensity, but now black is thicker and
      white is thinnner"
  0.1 is the default value, if not specified.

  Note: color conversion to grayscale is human-eye-based (that is,
  1.0 green has more intensity than 1.0 blue and such). }

program svg_grayscale;

{$I kambiconf.inc}

uses SysUtils, DOM, XMLRead, NoIndentXMLWrite, KambiStringUtils, VectorMath,
  StrUtils, KambiXMLUtils, Classes, KambiUtils, KambiParameters;

var
  WidthScale: Float = 0.1;

{ Sets Col and returns @true if S is a simple color specification.

  Otherwise returns @false. This can happen for various reasons,
  even for valid SVG files: fill may have some special values like
  "none", "url(#gradient_name)". }
function HexToCol(const S: string; out Col: TVector3Byte): boolean;
begin
  Result := false;
  if SCharIs(S, 1, '#') then
  begin
    if Length(S) = 4 then
    begin
      Col[0] := StrHexToInt(S[2] + S[2]);
      Col[1] := StrHexToInt(S[3] + S[3]);
      Col[2] := StrHexToInt(S[4] + S[4]);
      Result := true;
    end else
    if Length(S) = 7 then
    begin
      Col[0] := StrHexToInt(Copy(S, 2, 2));
      Col[1] := StrHexToInt(Copy(S, 4, 2));
      Col[2] := StrHexToInt(Copy(S, 6, 2));
      Result := true;
    end;
  end;
end;

function ColToHex(const Col: TVector3Byte): string;
begin
  Result := '#' + IntToStr16(Col[0], 2) +
                  IntToStr16(Col[1], 2) +
                  IntToStr16(Col[2], 2);
end;

{ SVG width specification may look like "10px". This splits such width
  into float part and string describing units. }
procedure SplitWidth(const S: string; out Width: Float; out WidthUnits: string);
var
  I: Integer;
begin
  I := Length(S);
  while (I >= 1) and (S[I] in ['a'..'z', 'A'..'Z']) do Dec(I);
  Width := StrToFloat(Copy(S, 1, I));
  WidthUnits := SEnding(S, I + 1);
end;

function JoinWidth(const Width: Float; const WidthUnits: string): string;
begin
  Result := FloatToStr(Width) + WidthUnits;
end;

procedure ProcDocument(const Input, Output: string);
var
  Doc: TXMLDocument;

  procedure ProcessElement(E: TDOMElement);

    { WidthKey may be '' if there's no width attribute expected. }
    function ProcStrokeAttr(const S, ColorKey, WidthKey: string): string;
    var
      Width: Float;
      Color: TVector3Byte;
      Strs: TStringList;
      WidthUnits: string;
    begin
      Result := '';

      Strs := TStringList.Create;
      try
        Strs.Delimiter := ';';
        Strs.NameValueSeparator := ':';
        Strs.DelimitedText := S;

        if (Strs.Values[ColorKey] <> '') and
           HexToCol(Strs.Values[ColorKey], Color) then
        begin
          GrayscaleTo1st(Color);
          Strs.Values[ColorKey] := ColToHex(Color);

          if (WidthKey <> '') and (Strs.Values[WidthKey] <> '') then
          begin
            SplitWidth(Strs.Values[WidthKey], Width, WidthUnits);
            Width := MapRange(Color[0]/255, 0, 1,
              Width * (1 - WidthScale),
              Width * (1 + WidthScale));
            Strs.Values[WidthKey] := JoinWidth(Width, WidthUnits);
          end;
        end;

        Result := Strs.DelimitedText;
      finally FreeAndNil(Strs) end;
    end;

  var
    I: TXMLElementIterator;
  begin
    if E.AttribStrings['style'] <> '' then
    begin
      if (E.TagName = 'rect') or
         (E.TagName = 'path') then
      begin
        E.AttribStrings['style'] := ProcStrokeAttr(E.AttribStrings['style'], 'stroke', 'stroke-width');
        E.AttribStrings['style'] := ProcStrokeAttr(E.AttribStrings['style'], 'fill', '');
      end else
      if E.TagName = 'stop' then
        E.AttribStrings['style'] := ProcStrokeAttr(E.AttribStrings['style'], 'stop-color', '');
    end;

    I := TXMLElementIterator.Create(E);
    try
      while I.GetNext do
        ProcessElement(I.Current);
    finally FreeAndNil(I); end;
  end;

begin
  try
    { ReadXMLFile always sets TXMLDocument param (possibly to nil),
      even in case of exception. So place it inside try..finally. }
    ReadXMLFile(Doc, Input);

    ProcessElement(Doc.DocumentElement);
    WriteXMLFile(Doc, Output);
  finally
    FreeAndNil(Doc);
  end;
end;

begin
  Parameters.CheckHighAtLeast(2);
  Parameters.CheckHighAtMost(3);
  if Parameters.High = 3 then
    WidthScale := StrToFloat(Parameters[3]);

  ProcDocument(Parameters[1], Parameters[2]);
end.
