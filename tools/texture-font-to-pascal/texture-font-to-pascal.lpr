{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert ttf fonts to Pascal units, to embed fonts inside source code. }

{$apptype CONSOLE}

uses Classes, SysUtils,
  CastleFont2Pascal, CastleUtils, CastleClassUtils, CastleLog,
  CastleParameters, CastleTextureFontData, CastleStringUtils,
  CastleURIUtils, CastleProgress, CastleProgressConsole, CastleUnicode,
  CastleImages, CastleApplicationProperties;

var
  Size: Integer = 10;
  AntiAliasing: boolean = true;
  SampleText, ParamUnitName: string;
  OnlySampleText: boolean = false;
  DebugFontImage: boolean = false;

const
  Options: array [0..7] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: #0; Long: 'size'; Argument: oaRequired),
    (Short: #0; Long: 'no-anti-alias'; Argument: oaNone),
    (Short: #0; Long: 'sample-text'; Argument: oaRequired),
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
           'texture-font-to-pascal: convert ttf font' +NL+
           'to a Pascal source file, based on types' +NL+
           'in Castle Game Engine CastleTextureFontData unit.' +NL+
           NL+
           'Call like this:' +NL+
           '  texture-font-to-pascal [options...] MyFontFile.ttf' +NL+
           NL+
           'Available options:' +NL+
           '  -h / --help           Print this help message and exit' +NL+
           '  --size FONT-SIZE' +NL+
           '  --no-anti-alias' +NL+
           '  --sample-text TEXT    Load (if existing) all characters' +NL+
           '                        listed here. We also always add ASCII chars,' +NL+
           '                        unless --only-sample-text given.' + NL+
           '  --only-sample-text    Load only characters from --sample-text,' +NL+
           '                        do not add standard ASCII chars.' +NL+
           '  --unit-name UnitName  Set UnitName, by default we automatically' +NL+
           '                        calculate it based on font name and size.' +NL+
           '  --debug-log           See the log, showing e.g. the font image size.' +NL+
           '  --debug-font-image    Write to disk font images as png.' +NL+
           NL+
           SCastleEngineProgramHelpSuffix('texture-font-to-pascal', '1.0.0', true));
         Halt;
       end;
    1: Size := StrToInt(Argument);
    2: AntiAliasing := false;
    3: SampleText := Argument;
    4: ParamUnitName := Argument;
    5: InitializeLog;
    6: DebugFontImage := true;
    7: OnlySampleText := true;
    else raise EInternalError.Create('OptionProc');
  end;
end;

var
  Font: TTextureFontData;
  PrecedingComment, UnitName, FontConstantName, OutURL, FontURL, FontName: string;
  Characters: TUnicodeCharList;
begin
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHigh(1);
  FontURL := Parameters[1];
  if OnlySampleText and (SampleText = '') then
    raise EInvalidParams.Create('Parameter --only-sample-text specified, but --sample-text not given (or has empty argument). No characters would be loaded.');

  Progress.UserInterface := ProgressConsoleInterface;

  FontName := DeleteURIExt(ExtractURIName(FontURL));
  FontConstantName := 'TextureFont_' +
    SDeleteChars(FontName, AllChars - ['a'..'z', 'A'..'Z', '0'..'9']) +
    '_' + IntToStr(Size);

  if ParamUnitName <> '' then
    UnitName := ParamUnitName else
    UnitName := 'Castle' + FontConstantName;
  PrecedingComment := Format(
    '  Source font:' +NL+
    '    Name         : %s' +NL+
    '    Size         : %d' +NL+
    '    AntiAliasing : %s' +nl,
    [ FontName, Size, BoolToStr(AntiAliasing, true) ]);

  Characters := TUnicodeCharList.Create;
  try
    if not OnlySampleText then
      Characters.Add(SimpleAsciiCharacters);
    Characters.Add(SampleText);
    Font := TTextureFontData.Create(FontURL, Size, AntiAliasing, Characters);
    try
      OutURL := LowerCase(UnitName) + '.pas';
      Font2Pascal(Font, UnitName, PrecedingComment, FontConstantName, OutURL);
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
