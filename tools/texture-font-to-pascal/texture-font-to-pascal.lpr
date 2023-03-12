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

{ Convert font files (.ttf, .otf and other formats handled by FreeType) to Pascal units,
  to embed fonts inside source code. }

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses Classes, SysUtils,
  CastleFont2Pascal, CastleUtils, CastleClassUtils, CastleLog,
  CastleParameters, CastleTextureFontData, CastleStringUtils,
  CastleURIUtils, CastleUnicode,
  CastleImages, CastleApplicationProperties, CastleLocalizationGetText;

var
  Size: Integer = 10;
  AntiAliasing: boolean = true;
  ParamUnitName, ParamFunctionName: string;
  OnlySampleText: boolean = false;
  DebugFontImage: boolean = false;
  Characters: TUnicodeCharList;

const
  Options: array [0..11] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: #0; Long: 'size'; Argument: oaRequired),
    (Short: #0; Long: 'no-anti-alias'; Argument: oaNone),
    (Short: #0; Long: 'sample-text'; Argument: oaRequired),
    (Short: #0; Long: 'sample-code'; Argument: oaRequired),
    (Short: #0; Long: 'sample-get-text-mo'; Argument: oaRequired),
    (Short: #0; Long: 'function-name'; Argument: oaRequired),
    (Short: #0; Long: 'unit-name'; Argument: oaRequired),
    (Short: #0; Long: 'debug-log'; Argument: oaNone),
    (Short: #0; Long: 'debug-font-image'; Argument: oaNone),
    (Short: #0; Long: 'only-sample-text'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         Writeln(
           'texture-font-to-pascal: convert font file (like TTF or OTF)' +NL+
           'to a Pascal source file, based on types' +NL+
           'in Castle Game Engine CastleTextureFontData unit.' +NL+
           NL+
           'Usage:' +NL+
           '  texture-font-to-pascal [options...] MyFontFile.ttf' +NL+
           NL+
           'Available options:' +NL+
           OptionDescription('-h / --help',
             'Print this help message and exit.') + NL +
           OptionDescription('--size=FONT-SIZE', '') + NL +
           OptionDescription('--no-anti-alias', '') + NL +
           OptionDescription('--sample-text=TEXT',
             'Load (if existing in the font file) all the listed characters. You can use this parameter multiple times.') + NL +
           OptionDescription('--sample-code=TEXT',
             'Load (if existing in the font file) the listed character code. You can use this parameter multiple times.') + NL +
           OptionDescription('--sample-get-text-mo=URL',
             'Load (if existing in the font file) all the character codes present in translated strings in URL. URL must point to a GetText MO file, it can be a regular filename as well. You can use this parameter multiple times.') + NL +
           OptionDescription('--only-sample-text',
             'Load only characters from --sample-text and --sample-code, do not add standard ASCII chars. By default we add standard ASCII chars, regardless of --sample-text and --sample-code.') + NL +
           OptionDescription('--function-name=PASCAL-FUNCTION-NAME',
             'Set function name to access the font. By default we automatically calculate it based on font name and size.') + NL +
           OptionDescription('--unit-name=PASCAL-UNIT-NAME',
             'Set generated unit name. This also determines the output filename. By default we automatically calculate it based on function name (which in turn is automatically calculated based on font name and size).') + NL +
           OptionDescription('--debug-log',
             'See the log, showing e.g. the font image size.') + NL +
           OptionDescription('--debug-font-image',
             'Write to disk font images as png.') + NL +
           NL+
           ApplicationProperties.Description);
         Halt;
       end;
    1: begin
         // include ApplicationName in version, good for help2man
         Writeln(ApplicationName + ' ' + ApplicationProperties.Version);
         Halt;
       end;
    2: Size := StrToInt(Argument);
    3: AntiAliasing := false;
    4: Characters.Add(Argument);
    5: Characters.Add(StrToInt(Argument));
    6: AddTranslatedCharacters(Argument, Characters);
    7: ParamFunctionName := Argument;
    8: ParamUnitName := Argument;
    9: InitializeLog;
    10: DebugFontImage := true;
    11: OnlySampleText := true;
    else raise EInternalError.Create('OptionProc');
  end;
end;

var
  Font: TTextureFontData;
  PrecedingComment, UnitName, FontFunctionName, OutURL, FontURL, FontName: string;
begin
  ApplicationProperties.ApplicationName := 'texture-font-to-pascal';
  ApplicationProperties.Version := '1.0';
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Characters := TUnicodeCharList.Create;
  try
    Parameters.Parse(Options, @OptionProc, nil);
    Parameters.CheckHigh(1);
    FontURL := Parameters[1];
    if not OnlySampleText then
      Characters.Add(SimpleAsciiCharacters);
    if Characters.Count = 0 then
      raise EInvalidParams.Create('No font characters requested to be loaded');

    FontName := DeleteURIExt(ExtractURIName(FontURL));
    if ParamFunctionName <> '' then
      FontFunctionName := ParamFunctionName
    else
      FontFunctionName := 'TextureFont_' +
        SDeleteChars(FontName, AllChars - ['a'..'z', 'A'..'Z', '0'..'9']) +
        '_' + IntToStr(Size);

    if ParamUnitName <> '' then
      UnitName := ParamUnitName
    else
      UnitName := 'Castle' + FontFunctionName;

    PrecedingComment := Format(
      '  Source font:' +NL+
      '    Name         : %s' +NL+
      '    Size         : %d' +NL+
      '    AntiAliasing : %s' +nl,
      [ FontName, Size, BoolToStr(AntiAliasing, true) ]);

    Font := TTextureFontData.Create(FontURL, Size, AntiAliasing, Characters);
    try
      OutURL := LowerCase(UnitName) + '.pas';
      Font2Pascal(Font, UnitName, PrecedingComment, FontFunctionName, OutURL);
      Writeln('texture-font-to-pascal: "' + OutURL + '" generated, texture size ',
        Font.Image.Width, ' x ',
        Font.Image.Height);
      if DebugFontImage then
      begin
        OutURL := LowerCase(UnitName) + '.png';
        SaveImage(Font.Image, OutURL);
        Writeln('texture-font-to-pascal: font image "' + OutURL + '" written');
      end;
    finally FreeAndNil(Font) end;
  finally FreeAndNil(Characters) end;
end.
