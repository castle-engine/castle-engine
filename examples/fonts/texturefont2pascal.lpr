{
  Copyright 2004-2014 Michalis Kamburelis.

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
  CastleFont2Pascal, CastleUtils, CastleClassUtils, CastleWarnings,
  CastleParameters, CastleTextureFontData, CastleStringUtils,
  CastleURIUtils, CastleProgress, CastleProgressConsole, CastleUnicode;

var
  Size: Integer = 10;
  AntiAliasing: boolean = true;
  SampleText, ParamUnitName: string;

const
  Options: array [0..4] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: #0; Long: 'size'; Argument: oaRequired),
    (Short: #0; Long: 'no-anti-alias'; Argument: oaNone),
    (Short: #0; Long: 'sample-text'; Argument: oaRequired),
    (Short: #0; Long: 'unit-name'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         Writeln(
           'texturefont2pascal: convert ttf font' +nl+
           'to a Pascal source file, based on types' +nl+
           'in Castle Game Engine CastleTextureFontData unit.' +nl+
           nl+
           'Call like this:' +nl+
           '  texturefont2pascal [options...] MyFontFile.ttf' +nl+
           nl+
           'Available options:' +nl+
           '  -h / --help           Print this help message and exit' +nl+
           '  --size FONT-SIZE' +nl+
           '  --no-anti-alias' +nl+
           '  --sample-text TEXT    Load (if existing) all characters' +nl+
           '                        listed here, in addition to ASCII chars.' +nl+
           '  --unit-name UnitName  Set UnitName, by default we automatically' +nl+
           '                        calculate it based on font name and size.' +nl+
           nl+
           SCastleEngineProgramHelpSuffix('texturefont2pascal', '1.0.0', true));
         ProgramBreak;
       end;
    1: Size := StrToInt(Argument);
    2: AntiAliasing := false;
    3: SampleText := Argument;
    4: ParamUnitName := Argument;
    else raise EInternalError.Create('OptionProc');
  end;
end;

var
  Font: TTextureFontData;
  PrecedingComment, UnitName, FontConstantName, OutURL, FontURL, FontName: string;
  Characters: TUnicodeCharList;
begin
  OnWarning := @OnWarningWrite;

  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHigh(1);
  FontURL := Parameters[1];

  Progress.UserInterface := ProgressConsoleInterface;

  FontName := DeleteURIExt(ExtractURIName(FontURL));
  FontConstantName := 'TextureFont_' +
    SDeleteChars(FontName, AllChars - ['a'..'z', 'A'..'Z', '0'..'9']) +
    '_' + IntToStr(Size);

  if ParamUnitName <> '' then
    UnitName := ParamUnitName else
    UnitName := 'Castle' + FontConstantName;
  PrecedingComment := Format(
    '  Source font:' +nl+
    '    Name         : %s' +nl+
    '    Size         : %d' +nl+
    '    AntiAliasing : %s' +nl,
    [ FontName, Size, BoolToStr[AntiAliasing] ]);

  Characters := TUnicodeCharList.Create;
  try
    Characters.Add(SimpleAsciiCharacters);
    Characters.Add(SampleText);
    Font := TTextureFontData.Create(FontURL, Size, AntiAliasing, Characters);
    try
      OutURL := LowerCase(UnitName) + '.pas';
      Font2Pascal(Font, UnitName, PrecedingComment, FontConstantName, OutURL);
      Writeln('texturefont2pascal: "' + OutURL + '" generated');
    finally FreeAndNil(Font) end;
  finally FreeAndNil(Characters) end;
end.