{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for fonts. }
unit ToolFonts;

interface

uses ToolProject;

{ Place units called
  CastleAutoGenetatedAllFonts,
  CastleAutoGenetatedFont1,
  CastleAutoGenetatedFont2 etc. inside the subdirectory FontUnitsOutputPath.

  Each CastleAutoGenetatedFontX unit contains the code to define
  one font found in project's data.
  The CastleAutoGenetatedAllFonts registers them using
  RegisterEmbeddedFont, so that later when TCastleFont.Load is used,
  the embedded font is loaded from memory, instead of loading using FreeType.

  Effectively, you can just use the CastleAutoGenetatedAllFonts unit
  in your application to have all fonts loaded from memory,
  and not need FreeType at run-time. }
procedure GenerateEmbeddedFonts(const Project: TCastleProject;
  const FontUnitsOutputPath: string);

implementation

uses SysUtils, Generics.Collections,
  CastleFindFiles, CastleFileFilters, CastleTextureFontData, CastleUtils,
  CastleStringUtils, CastleFontToPascal, CastleUriUtils, CastleFilesUtils;

{ TGenerateEmbeddedFontsHelper ----------------------------------------------- }

type
  TFontUnit = class
    FontUnitName: String;
    FontFunctionName: String;
    FontRelativeUrl: String;
  end;

  TFontUnitList = {ifdef FPC}specialize{endif} TObjectList<TFontUnit>;

  { Helper for GenerateEmbeddedFonts that processes each font file in data. }
  TGenerateEmbeddedFontsHelper = class
  strict private
    FontFilters: TFileFilterList;
  public
    Units: TFontUnitList;
    FontUnitsOutputPath, DataPath: String;
    constructor Create;
    destructor Destroy; override;
    procedure ProcessFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

constructor TGenerateEmbeddedFontsHelper.Create;
begin
  inherited;
  FontFilters := TFileFilterList.Create(true);
  FontFilters.AddFiltersFromString(LoadFont_FileFilters);
  Units := TFontUnitList.Create(true);
end;

destructor TGenerateEmbeddedFontsHelper.Destroy;
begin
  FreeAndNil(FontFilters);
  FreeAndNil(Units);
  inherited;
end;

procedure TGenerateEmbeddedFontsHelper.ProcessFile(const FileInfo: TFileInfo;
  var StopSearch: Boolean);
const
  { TODO: web: we have to assume some things - size, anti-aliasing, characters
    when generating fonts.
    If this going to be really long-term solution, we should define this in a file,
    so that generator takes font file + config how to generate it.
    Size 50, not 20, to make platformer UI like "credits" look good. }
  FontOptimalSize = 50;
  FontAntiAliasing = true;
var
  FontUnit: TFontUnit;
  Font: TTextureFontData;
  FontUnitName, FontFunctionName, FontRelativeUrl, FontUnitFileName: String;
begin
  if FontFilters.Matches(FileInfo.Url) then
  begin
    Font := TTextureFontData.Create(FileInfo.Url, FontOptimalSize, FontAntiAliasing);
    try
      // calculate various information about the font

      FontUnitName := 'CastleAutoGenetatedFont' + IntToStr(Units.Count);
      FontFunctionName := 'TextureFont' + IntToStr(Units.Count);

      FontUnitFileName := CombinePaths(FontUnitsOutputPath, LowerCase(FontUnitName) + '.pas');

      FontRelativeUrl := ExtractRelativePath(InclPathDelim(DataPath), FileInfo.AbsoluteName);
      // simple way to turn relative path into a relative URL
      FontRelativeUrl := SReplaceChars(FontRelativeUrl, PathDelim, '/');
      FontRelativeUrl := 'castle-data:/' + FontRelativeUrl;

      // generate font file
      FontToPascal(Font, FontUnitName, FontFunctionName, FontUnitFileName);

      // extend Units list with new font information
      FontUnit := TFontUnit.Create;
      FontUnit.FontUnitName := FontUnitName;
      FontUnit.FontFunctionName := FontFunctionName;
      FontUnit.FontRelativeUrl := FontRelativeUrl;
      Units.Add(FontUnit);

      Writeln(Format('Generated embedded font in %s from font %s', [
        FontUnitName,
        FontRelativeUrl
      ]));
    finally FreeAndNil(Font) end;
  end;
end;

{ GenerateEmbeddedFonts ------------------------------------------------------ }

procedure GenerateEmbeddedFonts(const Project: TCastleProject;
  const FontUnitsOutputPath: string);
var
  Helper: TGenerateEmbeddedFontsHelper;
  AllFontsUnit: String;
  FontUnit: TFontUnit;
begin
  Helper := TGenerateEmbeddedFontsHelper.Create;
  try
    Helper.FontUnitsOutputPath := FontUnitsOutputPath;
    Helper.DataPath := Project.DataPath;
    FindFiles(Project.DataPath, '*', false,
      {$ifdef FPC}@{$endif} Helper.ProcessFile, [ffRecursive]);

    AllFontsUnit :=
      '{ Auto-generated embedded fonts, because web target cannot load fonts at runtime yet. }' + NL +
      'unit CastleAutoGenetatedAllFonts;' + NL +
      'interface' + NL +
      'implementation' + NL +
      'uses CastleTextureFontData' + NL;

    for FontUnit in Helper.Units do
    begin
      AllFontsUnit := AllFontsUnit + ', ' + FontUnit.FontUnitName + NL;
    end;

    AllFontsUnit := AllFontsUnit + ';' + NL +
      'initialization' + NL;

    for FontUnit in Helper.Units do
    begin
      AllFontsUnit := AllFontsUnit +
        '  RegisterEmbeddedFont(' + FontUnit.FontFunctionName + ', ''' +
          FontUnit.FontRelativeUrl + ''');' + NL;
    end;

    AllFontsUnit := AllFontsUnit + 'end.' + NL;
    StringToFile(
      CombinePaths(FontUnitsOutputPath, 'castleautogenetatedallfonts.pas'),
      AllFontsUnit);
  finally FreeAndNil(Helper) end;
end;

end.
