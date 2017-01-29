{
  Copyright 2001-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Command-line utility for various string and filename operations.
  Very useful in scripts, when simple Unix utils (like dirname or basename)
  are not enough (and when using sed seems just an awful ugly hackery).

  For a list of possible operations, run "stringoper help".

  In case of error, exit code is 1 and error message is on stderr.
  If all Ok, exit code is 0 and result is on stdout. }
program stringoper;

{$apptype CONSOLE}

uses SysUtils, CastleUtils, Classes, CastleStringUtils, CastleFilesUtils,
  CastleParameters;

{ help ------------------------------------------------------------------  }

const
  HelpString =
    'stringoper: Command-line utility for various string and filename operations.' +nl+
    'Very useful in scripts (bash etc.). By Kambi.' +nl+
    nl+
    'Allowed command-line parameters:' +nl+
    '  help                      StdOut <<ta pomoc' +nl+
    '  ChangeFileExt fname ext   StdOut <<ChangeFileExt(fname, ext)' +nl+
    '    Change the last filename (fname) extension to ext. ext should contain' +nl+
    '    the leading dot (if you want to see it in the result).' +nl+
    '  DeleteFileExt fname       StdOut <<fname without last extension' +nl+
    '  ExtractFileExt fname      StdOut <<last extension of fname (with leading dot)' +nl+
    '  ExtractFileName fname     StdOut <<sama nazwa pliku + rozszerzenie' +nl+
    '    czyli wycina nazwe dysku+katalogu' +nl+
    '  ExtractFilePath fname     StdOut <<drive+katalog pliku' +nl+
    '    wycina drive i filename+extension. Wynik zawiera (back)slash.' +nl+
    '  ExtractFileDir fname      StdOut <<drive+katalog pliku' +nl+
    '    Jak FilePath ale bez (back)slasha.' +nl+
    '    Wynik (jezeli niepusty) nadaje sie do zrobienia cd $wynik itp.' +nl+
    '  InclPathDelim dirname     StdOut <<dirname z (back)slashem na koncu' +nl+
    '  ExclPathDelim dirname     StdOut <<dirname bez (back)slasha na koncu' +nl+
    '    dirname jesli go tam nie ma' +nl+
    '  SubString str start len StdOut <<wycina ze stringa STR LEN znakow, ' +nl+
    '    zaczynajac od pozycji START (pozycje sa liczone od 1). ' +nl+
    '    Jesli str jest za krotki to wynik jest obcinany (nie jest ' +nl+
    '    sygnalizowany blad ani nic).' +nl+
    '    Mozesz nie podac LEN - bedzie to oznaczalo jakby LEN bylo nieskonczone.'+nl+
    '  ExpandFileName FILENAME'+nl+
    '  UpperCase STR - print STR upper case. Conversion respects current locale.'+nl+
    '  LowerCase STR - print STR lower case. Conversion respects current locale.'+nl+
    '  ReplaceAll STR OLD-PATTERN NEW-PATTERN '+nl+
    '    Replace is STR all occurrences of OLD-PATTERN (not-case-sens)' +nl+
    '    with NEW-PATTERN.' +nl+
    '  Trim STR - trim whitespaces at beginning and ending of STR' +nl+
    '  -- (druga grupa polecen - polecenia na ktore odp = true lub false' +nl+
    '     co sygnalizowane jest kodem wyjscia 0 (true) lub 1 (false), ' +nl+
    '     tak jak program ''test'' pod UNIXami)' +nl+
    '  IsPrefix prefix s         czy prefix jest prefixem s ? (not-case-sens)' +nl+
    '  IsSuffix suffix s         czy suffix jest sufiksem s ? (not-case-sens)' +nl+
    nl+
    'Notes:'+nl+
    '- Kropka jest uwazana za czesc rozszerzenia i zwracana razem z nim.' +nl+
    '- (Back)slash jest uwazany za czesc katalogu i zwracany razem z nim.' +nl+
    '- Gdy filename nie zawiera rozszerzenia / katalogu zwracany jest pusty' +nl+
    '  string zamiast pojedynczej kropki lub (back)slasha.' +nl+
    '- Gdy filename mial kilka rozszerzen wszystkie rozszerzenia poza ostatnim' +nl+
    '  uznawane sa za czesc normalnej nazwy pliku.' +nl+
    '- Under non-Windows, "drive letter" is always an empty string.' +nl+
    '- Command names are case-insensitive.' +nl+
    nl+
    'Examples :'+nl+
    nl+
    ' (unix)  /usr/share/doc/fpc/  README         .txt' +nl+
    '         |-----------------|  |----|         |--|' +nl+
    '         path                 onlyfname      ext' +nl+
    '                              filename= onlyfname   +  ext' +nl+
    nl+
    ' (win)   c:      \programy\fpc\  fpc         .exe' +nl+
    '         ||      |------------|  |--|        |--|' +nl+
    '         drive   path            onlyfname   ext' +nl+
    '                       filename= onlyfname + ext';

{ Commands type and consts and funcs --------------------------------------- }

type
  TCommandResult = (crString, crBoolean);
  TCommand = record
    { must be in lowercase (according even to current locale, i.e. to "Ansi"
      string functions) ! }
    Param: string;
    { -1 if no paramater is required }
    ParCountRequired: Integer;
    CommandResult: TCommandResult;
  end;

const
  CommandsCount = 16;
  Commands: array [0..CommandsCount-1]of TCommand =
  (
    (Param:'changefileext'; ParCountRequired: 2; CommandResult: crString),
    (Param:'deletefileext'; ParCountRequired: 1; CommandResult: crString),
    (Param:'extractfileext'; ParCountRequired: 1; CommandResult: crString),
    (Param:'extractfilename'; ParCountRequired: 1; CommandResult: crString),
    (Param:'extractfilepath'; ParCountRequired: 1; CommandResult: crString),
    (Param:'extractfiledir'; ParCountRequired: 1; CommandResult: crString),
    (Param:'inclpathdelim'; ParCountRequired: 1; CommandResult: crString),
    (Param:'exclpathdelim'; ParCountRequired: 1; CommandResult: crString),
    (Param:'isprefix'; ParCountRequired: 2; CommandResult: crBoolean),
    (Param:'issuffix'; ParCountRequired: 2; CommandResult: crBoolean),
    (Param:'substring'; ParCountRequired:-1; CommandResult: crString),
    (Param:'expandfilename'; ParCountRequired: 1; CommandResult: crString),
    (Param:'uppercase'; ParCountRequired: 1; CommandResult: crString),
    (Param:'lowercase'; ParCountRequired: 1; CommandResult: crString),
    (Param:'replaceall'; ParCountRequired:3; CommandResult: crString),
    (Param:'trim'; ParCountRequired: 1; CommandResult: crString)
  );

{ zwraca liczbe z przedzialu 0..CommandsCount-1 jesli Param zgadza sie
  (case-insensitive) z Commands[result].Param. Jesli nie zgadza sie z zadnym,
  to zwraca -1. }
function ParamToCommand(const Param: string): Integer;
var ParamLower: string;
begin
 ParamLower := AnsiLowerCase(Param);
 for result := 0 to CommandsCount-1 do
  if Commands[result].Param = ParamLower then Exit;
 result := -1;
end;

{ main -------------------------------------------------------------------- }

var CommandNum: Integer;
    ResultStr: string;
    ResultBool: boolean;
begin
 //BonusErrorMessg := 'Run with parameter "help" for, well, help about using program.';

 Parameters.CheckHighAtLeast(1);
 if AnsiLowerCase(Parameters[1]) = 'help' then
  begin Writeln(HelpString); Exit end;

 { calculate CommandNum. Check Commands[CommandNum].ParCountRequired. }
 CommandNum := ParamToCommand(Parameters[1]);
 if CommandNum = -1 then
  raise EInvalidParams.Create('invalid first parameter "'+Parameters[1]+'"');
 Parameters.Delete(1);
 if Commands[CommandNum].ParCountRequired <> -1 then
  Parameters.CheckHigh(Commands[CommandNum].ParCountRequired);

 { calculate Result*, executing CommandNum.

   Each command may read parameters from Parameters[1]..Parameters[Parameters.High]
   (and should check if there are no missing/excessive args;
   easiest way to do this was to define ParCountRequired <> -1,
   but sometimes more complex rules need checking.)

   Command must set one of the Result* variables, suitable for
   it's type in CommandResult: ctString to ResultStr, ctBoolean to ResultBool.
   In case of problems, raise any exception. }
 case CommandNum of
  0 : ResultStr := ChangeFileExt(Parameters[1], Parameters[2]);
  1 : ResultStr := DeleteFileExt(Parameters[1]);
  2 : ResultStr := ExtractFileExt(Parameters[1]);
  3 : ResultStr := ExtractFileName(Parameters[1]);
  4 : ResultStr := ExtractFilePath(Parameters[1]);
  5 : ResultStr := ExtractFileDir(Parameters[1]);
  6 : ResultStr := InclPathDelim(Parameters[1]);
  7 : ResultStr := ExclPathDelim(Parameters[1]);
  8 : ResultBool := IsPrefix(Parameters[1], Parameters[2]);
  9 : ResultBool := IsSuffix(Parameters[1], Parameters[2]);
  10: case Parameters.High of
       2: ResultStr := SEnding(Parameters[1], StrToInt(Parameters[2]));
       3: ResultStr := Copy(Parameters[1], StrToInt(Parameters[2]), StrToInt(Parameters[3]));
       else raise EInvalidParams.Create('invalid params count for SubString command');
      end;
  11: ResultStr := ExpandFileName(Parameters[1]);
  12: ResultStr := AnsiUpperCase(Parameters[1]);
  13: ResultStr := AnsiLowerCase(Parameters[1]);
  14: ResultStr := StringReplace(Parameters[1], Parameters[2], Parameters[3],
        [rfReplaceAll, rfIgnoreCase]);
  15: ResultStr := Trim(Parameters[1]);
  else raise EInternalError.Create('CommandNum not impl');
 end;

 { return Result* in appropriate way }
 case Commands[CommandNum].CommandResult of
  crString: Write(ResultStr);
  crBoolean: if ResultBool then Halt(0) else Halt(1);
  else raise EInternalError.Create('CommandResult not impl');
 end;
end.
