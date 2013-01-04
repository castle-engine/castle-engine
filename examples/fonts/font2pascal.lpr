{
  Copyright 2004-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert fonts to Pascal units, to embed fonts inside source code.

  TODO: This program is Windows-only for now, as it uses WinFontConvert
  to read font information, which in turn uses GetGlpyhOutline WinAPI.
  Michalis plans (since a long time...) to switch to using freetype
  (as I don't even use Windows since a long time), but so far there was no time
  to do it. The plan is to implement unit like CastleFonts that reads a font file
  (like ttf) and for a given character returns PBFNTChar structure (for bitmap
  font) and PTTFChar (for geometric font, composed on lines and curves).
  It's possible to also change the definition of our PBFNTChar/PTTFChar types
  at that point, maybe integrate them more with freetype structures,
  basically: do anything to make the job easier.
  Contributions are most welcome, if you're familiar with freetype
  this may even be easy!
}

{$apptype CONSOLE}

uses Windows, SysUtils, WindowsFonts, CastleFont2Pascal, CastleUtils,
  CastleClassUtils, CastleParameters, TTFontsTypes, BmpFontsTypes, WinFontConvert,
  Classes, CastleStringUtils, CastleTimeUtils;

const
  DefaultFontHeight: Integer = -20;

var
  WinFont: TWindowsFont;
  AsTtf: boolean = true;
  WasParam_Dir: boolean = false;
  Dir: string;

const
  Options: array [0..8] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'n'; Long: 'font-name'; Argument: oaRequired),
    (Short: 'b'; Long: 'font-bold'; Argument: oaRequired),
    (Short: 'w'; Long: 'font-weight'; Argument: oaRequired),
    (Short: 'i'; Long: 'font-italic'; Argument: oaRequired),
    (Short: #0; Long: 'grab-to'; Argument: oaRequired),
    (Short: #0; Long: 'font-charset'; Argument: oaRequired),
    (Short: #0; Long: 'font-height'; Argument: oaRequired),
    (Short: #0; Long: 'dir'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         Writeln(
           'font2pascal: converts fonts installed' +nl+
           'on your system (only TrueType, *. ttf) to Pascal source' +nl+
           'files, based on types in Castle Game Engine TTFontsTypes or BmpFontsTypes' +nl+
           'units.' +nl+
           nl+
           'Available options:' +nl+
           '  -h / --help           Print this help message and exit' +nl+
           '  --grab-to ttf|bfnt    Set whether to generate font as trutype' +nl+
           '                        (TTFontsTypes) or bitmap font (BmpFontsTypes)' +nl+
           '                        Default is "as truetype".' +nl+
           '  --dir DIR             This tells font2pascal to write output to file' +nl+
           '                        in directory DIR with a filename deduced from' +nl+
           '                        generated unit name (e.g. unit TTF_CourierNew_Unit' +nl+
           '                        will be output to ttf_couriernew_unit.pas).' +nl+
           '                        Some one-line comment (like' +nl+
           '                        "font2pascal: ttf_couriernew_unit.pas generated")' +nl+
           '                        will be then printed on StdOut.' +nl+
           '                        Default is to write output to StdOut.' +nl+
           nl+
           'Options setting font properties:' +nl+
           '(exact meaning of these font attributes are determined by' +nl+
           'Windows.CreateFont function, see docs of this function for details)' +nl+
           '  -n / --font-name FONT-NAME' +nl+
           '                        Set font name' +nl+
           ' --font-height FONT-HEIGHT' +nl+
           '                        Set font height. Default is ' +IntToStr(DefaultFontHeight)+ '.' +nl+
           '  -w / --font-weight FONT-WEIGHT' +nl+
           '                        Set font weight (0..1000, 400 is normal,' +nl+
           '                        700 is bold, default is 400)' +nl+
           '  -b / --font-bold false|true|0|1' +nl+
           '                        Set font boldness.' +nl+
           '                        -b=false is shortcut for -w=400' +nl+
           '                        -b=true is shortcut for -w=700' +nl+
           '  -i / --font-italic false|true|0|1' +nl+
           '                        Set font italic state. Default is false.' +nl+
           ' --font-charset CHARSET' +nl+
           '                        Set font charset. Available values are' +nl+
           '                        Windows.XXX_CHARSET consts names,' +nl+
           '                        e.g. ANSI_CHARSET, EASTEUROPE_CHARSET etc.' +nl+
           '                        Default is DEFAULT_CHARSET.' +nl+
           nl+
           SCastleEngineProgramHelpSuffix('font2pascal', '1.0.0', true));
         ProgramBreak;
       end;
    1: WinFont.FaceName := Argument;
    2: if StrToBool(Argument) then
         WinFont.Weight := FW_BOLD else
         WinFont.Weight := FW_REGULAR;
    3: WinFont.Weight := StrToInt(Argument);
    4: WinFont.Italic := StrToBool(Argument);
    5: if Argument = 'ttf'  then AsTtf := true else
       if Argument = 'bfnt' then AsTtf := false else
         raise EInvalidParams.CreateFmt('Wrong argument, should be "ttf" or "bfnt" '+
           'but is "%s"', [Argument]);
    6: WinFont.CharSet := WinCharSetFromName(Argument);
    7: WinFont.Height := StrToInt(Argument);
    8: begin
         WasParam_Dir := true;
         Dir := Argument;
       end;
    else raise EInternalError.Create('OptionProc');
  end;
end;

var
  WinFontHandle: HFont;
  TTFont: TTrueTypeFont;
  BmpFont: TBmpFont;
  S, PrecedingComment, UnitName, FontConstantName, OutFileName: string;
  Stream: TStream;
begin
  WinFont := TWindowsFont.Create(DefaultFontHeight);
  try
    Parameters.Parse(Options, @OptionProc, nil);
    Parameters.CheckHigh(0);

    WinFontHandle := WinFont.GetHandle;
    try
      if AsTtf then s := 'TTF_' else s := 'BFNT_';
      s := s + SDeleteChars(WinFont.FaceName, AllChars - ['a'..'z', 'A'..'Z', '0'..'9']);
      if WinFont.Weight >= FW_BOLD then s := s + '_Bold';
      if WinFont.Italic then s := s + '_Italic';
      if WinFont.Height <> DefaultFontHeight then
        s := s + '_' + StringReplace(IntToStr(WinFont.Height), '-', 'm', [rfReplaceAll]);

      FontConstantName := s;
      UnitName := s + '_Unit';
      PrecedingComment := Format(
        '  Generated by font2pascal on %s' +nl+
        '  Source font:' +nl+
        '    Face name : %s' +nl+
        '    Height    : %d' +nl+
        '    CharSet   : %s' +nl+
        '    Weight    : %d' +nl+
        '    Italic    : %s' +nl,
        [ DateTimeToAtStr(Now),
          WinFont.FaceName, WinFont.Height,
          CharSetsNames[WinFont.CharSet],
          WinFont.Weight, BoolToStr[WinFont.Italic] ]);

      { evaluate Stream }
      if WasParam_Dir then
      begin
        OutFileName := InclPathDelim(Dir) + LowerCase(UnitName) + '.pas';
        Stream := TFileStream.Create(OutFileName, fmCreate);
      end else
        Stream := StdOutStream;

      try
        if AsTtf then
        begin
          Font2TrueTypeFont(WinFontHandle, TTFont);
          try
            Font2Pascal(TTFont, UnitName, PrecedingComment, FontConstantName, Stream);
          finally FreeMemNilingAllChars(TTFont) end;
        end else
        begin
          Font2BitmapFont(WinFontHandle, BmpFont);
          try
            Font2Pascal(BmpFont, UnitName, PrecedingComment, FontConstantName, Stream);
          finally FreeMemNilingAllChars(BmpFont) end;
        end;

        if WasParam_Dir then
          Writeln('font2pascal: "' +OutFileName +'" generated');
      finally
        if WasParam_Dir then Stream.Free;
      end;
    finally DeleteObject(WinFontHandle) end;
  finally WinFont.Free end;
end.