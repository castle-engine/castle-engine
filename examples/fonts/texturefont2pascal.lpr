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

uses Classes, SysUtils, CastleFont2Pascal, CastleUtils, CastleClassUtils,
  CastleParameters, CastleTextureFontData, CastleStringUtils,
  CastleURIUtils, CastleProgress, CastleProgressConsole;

var
  Size: Integer = 10;
  AntiAliasing: boolean = true;

const
  Options: array [0..2] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: #0; Long: 'size'; Argument: oaRequired),
    (Short: #0; Long: 'no-anti-alias'; Argument: oaNone)
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
           nl+
           SCastleEngineProgramHelpSuffix('texturefont2pascal', '1.0.0', true));
         ProgramBreak;
       end;
    1: Size := StrToInt(Argument);
    2: AntiAliasing := false;
    else raise EInternalError.Create('OptionProc');
  end;
end;

var
  Font: TTextureFontData;
  PrecedingComment, UnitName, FontConstantName, OutURL, FontURL, FontName: string;
begin
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHigh(1);
  FontURL := Parameters[1];

  Progress.UserInterface := ProgressConsoleInterface;

  FontName := DeleteURIExt(ExtractURIName(FontURL));
  FontConstantName := 'TextureFont_' +
    SDeleteChars(FontName, AllChars - ['a'..'z', 'A'..'Z', '0'..'9']) +
    '_' + IntToStr(Size);

  UnitName := 'Castle' + FontConstantName;
  PrecedingComment := Format(
    '  Source font:' +nl+
    '    Name         : %s' +nl+
    '    Size         : %d' +nl+
    '    AntiAliasing : %s' +nl,
    [ FontName, Size, BoolToStr[AntiAliasing] ]);

  Font := TTextureFontData.Create(FontURL, Size, AntiAliasing);
  try
    OutURL := LowerCase(UnitName) + '.pas';
    Font2Pascal(Font, UnitName, PrecedingComment, FontConstantName, OutURL);
    Writeln('texturefont2pascal: "' + OutURL + '" generated');
  finally FreeAndNil(Font) end;
end.