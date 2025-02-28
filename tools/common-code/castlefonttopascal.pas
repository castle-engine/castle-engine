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

{ @noAutoLinkHere }
procedure FontToPascal(const Font: TTextureFontData;
  const UnitName, PrecedingComment, FontFunctionName: string; Stream: TStream);
  overload;

{ @noAutoLinkHere }
procedure FontToPascal(const Font: TTextureFontData;
  const UnitName, PrecedingComment, FontFunctionName: string;
  const OutUrl: String); overload;

implementation

uses SysUtils, CastleUtils, CastleStringUtils, CastleClassUtils, CastleDownload,
  CastleUnicode;

procedure FontToPascal(const Font: TTextureFontData;
  const UnitName, PrecedingComment, FontFunctionName: string; Stream: TStream);
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
    PrecedingComment+
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
    'begin' +NL+
    '  FontImage.TreatAsAlpha := true;' +NL+
    '  FontImage.Url := ''embedded-font:/' + UnitName + ''';' +NL+
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
    '  Result := TTextureFontData.CreateFromData(Glyphs, FontImage, ' +
      IntToStr(Font.Size) + ', ' +
      LowerCase(BoolToStr(Font.AntiAliased, true)) + ');' +NL+
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
  const UnitName, PrecedingComment, FontFunctionName: string;
  const OutUrl: String); overload;
var
  Stream: TStream;
begin
  Stream := UrlSaveStream(OutUrl);
  try
    FontToPascal(Font, UnitName, PrecedingComment, FontFunctionName, Stream);
  finally Stream.Free end;
end;

end.
