{
  Copyright 2004-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Converting fonts (TTextureFontData) to Pascal source code. }
unit CastleFontToPascal;

interface

uses CastleTextureFontData, Classes;

{ Generate Pascal unit that defines given Font. }
procedure FontToPascal(const Font: TTextureFontData;
  const UnitName, FontFunctionName: string; Stream: TStream); overload;
procedure FontToPascal(const Font: TTextureFontData;
  const UnitName, FontFunctionName: string; const OutUrl: String); overload;

implementation

uses SysUtils, CastleUtils, CastleStringUtils, CastleClassUtils, CastleDownload,
  CastleUnicode, CastleUriUtils;

{ Return string literal with given value in Pascal.
  By default this includes surrounding apostrophes. }
function StringToPascal(const S: String;
  const SurroundApostrophes: Boolean = true): String;
const
  Apos = ''''; // using this constant makes the code below more readable
begin
  Result := StringReplace(S, Apos, Apos + Apos, [rfReplaceAll]);
  if SurroundApostrophes then
    Result := Apos + Result + Apos;
end;

function BoolToPascal(const B: Boolean): String;
begin
  Result := Iff(B, 'true', 'false');
end;

procedure FontToPascal(const Font: TTextureFontData;
  const UnitName, FontFunctionName: string; Stream: TStream);
var
  C: TUnicodeChar;
  G: TTextureFontData.TGlyph;
  ImageInterface, ImageImplementation, ImageInitialization, ImageFinalization: string;
  LoadedGlyphs: TUnicodeCharList;
begin
  WriteStr(Stream,
    '{ -*- buffer-read-only: t -*- }' +NL+
    NL+
    '{ Unit automatically generated by ' + ApplicationName + ',' +NL+
    '  to embed font data in Pascal source code.' +NL+
    '  @exclude (Exclude this unit from PasDoc documentation.)' +NL+
    NL+
    '  Source font:' +NL+
    '    Family Name : ' + Font.FamilyName +NL+
    '    Style Name  : ' + Font.StyleName +NL+
    '    Bold        : ' + BoolToPascal(Font.Bold) +NL+
    '    Italic      : ' + BoolToPascal(Font.Italic) +NL+
    NL+
    '  Data generated with options:' +NL+
    '    Size        : ' + IntToStr(Font.Size) +NL+
    '    AntiAliased : ' + BoolToPascal(Font.AntiAliased) +NL+
    '}' +NL+
    'unit ' + UnitName + ';' +NL+
    NL+
    'interface'+NL+
    NL+
    'uses CastleTextureFontData;' +NL+
    NL+
    'function ' + FontFunctionName + ': TTextureFontData;' +NL+
    NL+
    'implementation' +NL+
    NL+
    'uses SysUtils, CastleImages, CastleInternalDataCompression;' + NL+
    NL
  );

  ImageInterface := '';
  ImageImplementation := '';
  ImageInitialization := '';
  ImageFinalization := '';

  Font.Image.SaveToPascalCode('FontImage',
    ImageInterface, ImageImplementation, ImageInitialization, ImageFinalization);

  WriteStr(Stream,
    // ImageInterface + // no need for this (and it would require additional "forward")
    '{ -----------------------------------------------------------------------' + NL +
    '  Embedded font image (TCastleImage instance). }' + NL +
    NL +
    ImageImplementation +
    NL +
    '{ -----------------------------------------------------------------------' + NL +
    '  Embedded font (TTextureFontData instance). }' + NL +
    NL +
    'function Create' + FontFunctionName + ': TTextureFontData;' +NL+
    'var' +NL+
    '  Glyphs: TTextureFontData.TGlyphDictionary;' +NL+
    '  G: TTextureFontData.TGlyph;' +NL+
    '  FontInformation: TTextureFontDataInformation;' +NL+
    'begin' +NL+
    '  FontImage.TreatAsAlpha := true;' +NL+
    '  FontImage.Url := ''embedded-font:/' +
      // use UrlEncode, in case font family has spaces which should be %20 in URL
      StringToPascal(UrlEncode(Font.FamilyName), false) + ''';' +NL+
    NL+
    '  Glyphs := TTextureFontData.TGlyphDictionary.Create;' +NL+
    NL);

  LoadedGlyphs := Font.LoadedGlyphs;
  try
    for C in LoadedGlyphs do
    begin
      G := Font.Glyph(C, false);
      if G <> nil then
      begin
        WriteStr(Stream,
          '  G := TTextureFontData.TGlyph.Create;' +NL+
          '  G.X := ' + IntToStr(G.X) + ';' +NL+
          '  G.Y := ' + IntToStr(G.Y) + ';' +NL+
          '  G.AdvanceX := ' + IntToStr(G.AdvanceX) + ';' +NL+
          '  G.AdvanceY := ' + IntToStr(G.AdvanceY) + ';' +NL+
          '  G.Width := ' + IntToStr(G.Width) + ';' +NL+
          '  G.Height := ' + IntToStr(G.Height) + ';' +NL+
          '  G.ImageX := ' + IntToStr(G.ImageX) + ';' +NL+
          '  G.ImageY := ' + IntToStr(G.ImageY) + ';' +NL+
          '  Glyphs[' + IntToStr(Ord(C)) + '] := G;' +NL+
          NL);
      end;
    end;
  finally FreeAndNil(LoadedGlyphs) end;

  WriteStr(Stream,
    '  FontInformation := TTextureFontDataInformation.Create;' +NL+
    '  try' +NL+
    '    FontInformation.Size := ' + IntToStr(Font.Size) + ';' +NL+
    '    FontInformation.AntiAliased := ' + BoolToPascal(Font.AntiAliased) + ';' +NL+
    '    FontInformation.FamilyName := ' + StringToPascal(Font.FamilyName) + ';' +NL+
    '    FontInformation.StyleName := ' + StringToPascal(Font.StyleName) + ';' +NL+
    '    FontInformation.Bold := ' + BoolToPascal(Font.Bold) + ';' +NL+
    '    FontInformation.Italic := ' + BoolToPascal(Font.Italic) + ';' +NL+
    NL+
    '    Result := TTextureFontData.CreateFromData(Glyphs, FontImage, FontInformation);' +NL+
    '  finally' +NL+
    '    FreeAndNil(FontInformation);' +NL+
    '  end;' +NL+
    'end;' +NL+
    NL+
    'var' +NL+
    '  FFont: TTextureFontData;' +NL+
    '' +NL+
    'function ' + FontFunctionName + ': TTextureFontData;' +NL+
    'begin' +NL+
    '  // create FFont on-demand' +NL+
    '  if FFont = nil then FFont := Create' + FontFunctionName + ';' +NL+
    '  Result := FFont;' +NL+
    'end;' +NL+
    NL+
    'initialization' +NL+
    ImageInitialization +
    'finalization' +NL+
    '  FreeAndNil(FFont);' +NL+
    { This frees the image, but we don't want it --
      if the image is created, it is owned by TTextureFontData instance. }
    // ImageFinalization +
    'end.' + NL);
end;

procedure FontToPascal(const Font: TTextureFontData;
  const UnitName, FontFunctionName: string; const OutUrl: String); overload;
var
  Stream: TStream;
begin
  Stream := UrlSaveStream(OutUrl);
  try
    FontToPascal(Font, UnitName, FontFunctionName, Stream);
  finally Stream.Free end;
end;

end.
