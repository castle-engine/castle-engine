{
  Copyright 2002-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Some wrappers around a Readln call (useful when you suspect
  that user is running program in interactive mode (i.e. in console,
  not from some script that redirects some file on our stdin)).

  And some wrappers around Write[ln] that can bahave nicely when used
  from Windows GUI programs (without a console). }

{$ifdef read_interface}

{ Reading ------------------------------------------------------------ }

function ReadlnInt(const Prompt: string): integer;
function ReadlnStr(const Prompt: string): string;

{ Prompt nie powinien zawierac koncowego ' [y/n] ' czy cos w tym rodzaju.
  Dla przykladowego
    ReadlnChoice('Really do this ?', defTrue)
  bedzie wyswietlal 'Really do this ? [Y/n]' i czekal na odpowiedz przez
  Readln(string). Jesli podany string bedzie rowny 'y' lub 'n' (case ignored)
  to zwroci odpowiednio true lub false. Jesli Default <> defNone to string ''
  tez jest akceptowany (i oznacza true lub false, odpowiednio, dla
  Default = defTrue i defFalse).

  Przyrostek ' [y/n] ' lub ' [Y/n] ' lub ' [y/N] ' odzwierciedla stan Default. }
type TYesNoDefault=(defFalse, defNone, defTrue);
function ReadlnYesNo(Prompt: string; Default: TYesNoDefault): boolean;

{ Prompt powinien zawierac koncowy znak, np. '?' lub ':' ale nie powinien zawierac
  Choices lub zapisu ze Choice 0 jest default. Przykladowe wywolanie to
    ReadlnChoice('Do you want to rm -Rf / ?', ['no', 'yes, really'], true, true)
  w rezultacie bedzie wypisywal
    'Do you want to rm -Rf / ? (no/yes, really) [no]'
  i robil Readln(string) az podany przez usera string bedzie rowny
  jednemu sposrod Choices (przy czym bedzie uzywal zmiennej IgnoreCase).
  Jezeli Choice0Default to jezeli user wpisze '' to bedzie to potraktowane
  tak samo jakby wpisal Choices[0].

  TODO: untested}
function ReadlnChoice(Prompt: string; const Choices: array of string;
  Choice0Default, IgnoreCase: boolean): Integer;

{ Writing ---------------------------------------------------------------- }

{ XxxWrite: if we are under Windows and not IsConsole then
  uses XxxBox (from windows/kambiutils_os_specific.inc).
  Else (IsConsole or we are not under Windows):
  - ErrorWrite and WarningWrite do Writeln(ErrOutput, S)
  - InfoWrite does Writeln(S)

  Notes for Windows: even when IsConsole = false, we can have
  GetStdHandle(STD_ERROR_HANDLE) and/or GetStdHandle(STD_OUTPUT_HANDLE)
  available. User can always run GUI program and redirect our
  stdin/out/err and then GUI app will have some of stdin, stdout, stderr
  available. In other words, GUI app *may* have stdin/out/err,
  but it doesn't have to. Depends on how the user run the app.
  But XxxWrite functions will always for GUI app (that have
  IsConsole = false) use XxxBox functions, never Writeln,
  even when stdin/out/err will be available.
  This ensures that program always behaves in the same way. }
procedure ErrorWrite(const s: string); overload;
procedure WarningWrite(const s: string); overload;
procedure InfoWrite(const s: string); overload;

procedure ErrorWrite(const s: string; const args: array of const); overload;
procedure WarningWrite(const s: string; const args: array of const); overload;
procedure InfoWrite(const s: string; const args: array of const); overload;

{ Jezeli nie jest Windows lub IsConsole to po prostu wszystkie elememnty Messages
  zostana po kolei wypisane na stdout przez Writeln (a wiec kazdy Messages[]
  bedzie dodatkowo zakonczony newline'm).

  Wpp. sprawa jest ciekawsza: dla kazdego i-tego elementu tablicy Messages
  (i jest zero-based) zostanie wyswietlone InfoBox z i trescia = Messages[i]
  i caption = Format(TitleFormat, [i+1, Messages.Count]) gdzie
  Messages.Count = tradycyjnie High(Messages)+1.

  W ten sposob mamy przynajmniej polowiczne rozwiazanie dla sytuacji w ktorej
  musimy wypisac duzo tekstu przy pomocy InfoWrite i pod Windowsem ten tekst
  jest za duzy zeby go upchnac w jednym InfoBox. }
procedure InfoWriteParts(const TitleFormat: string; const Messages: array of string);

{$endif read_interface}

{$ifdef read_implementation}

{ Reading -------------------------------------------------------- }

function ReadlnInt(const Prompt: string): integer;
{$WARNINGS OFF}
var s: string;
begin
 repeat
  Write(Prompt);
  Readln(s);
  try
   result := StrToInt(s);
   break;
  except on EConvertError do ; end;
 until false;
end;
{$WARNINGS ON}

function ReadlnStr(const Prompt: string): string;
begin
 Write(Prompt); Readln(result);
end;

function ReadlnYesNo(Prompt: string; Default: TYesNoDefault): boolean;
const
  PromptSuffix: array[TYesNoDefault]of string=  (' [y/N] ', ' [y/n] ', ' [Y/n] ');
var s: string;
begin
 Prompt := Prompt +PromptSuffix[Default];
 repeat
  s := ReadlnStr(Prompt);
  if (s = 'y') or (s = 'Y') or ((s = '') and (Default = defTrue)) then
   begin result := true; Exit end else
  if (s = 'n') or (s = 'N') or ((s = '') and (Default = defFalse)) then
   begin result := false; Exit end;
 until false;
end;

function ReadlnChoice(Prompt: string; const Choices: array of string;
  Choice0Default, IgnoreCase: boolean): Integer;
var ChoiceStr: string;
    i: Integer;
begin
 Assert(High(Choices) >= 0, 'ReadlnChoice requires at least one choice');

 Prompt := Prompt +' (' +Choices[0];
 for i := 1 to High(Choices) do Prompt := Prompt +'/' +Choices[i];
 Prompt := Prompt +') ';

 if Choice0Default then
  Prompt := Prompt +'[' +Choices[0] +'] ';

 repeat
  ChoiceStr := ReadlnStr(Prompt);
  if Choice0Default and (ChoiceStr = '') then
   result := 0 else
   result := ArrayPosText(ChoiceStr, Choices, IgnoreCase);
 until result <> -1;
end;

{ Writing ---------------------------------------------------------------- }

procedure ErrorWrite(const s: string);
begin
 {$ifdef MSWINDOWS} if not IsConsole then ErrorBox(s) else {$endif}
 Writeln(ErrOutput, s);
end;

procedure WarningWrite(const s: string);
begin
 {$ifdef MSWINDOWS} if not IsConsole then WarningBox(s) else {$endif}
 Writeln(ErrOutput, s);
end;

procedure InfoWrite(const s: string);
begin
 {$ifdef MSWINDOWS} if not IsConsole then InfoBox(s) else {$endif}
 Writeln(s);
end;

procedure ErrorWrite(const s: string; const args: array of const);
begin ErrorWrite(format(s, args)) end;
procedure WarningWrite(const s: string; const args: array of const);
begin WarningWrite(format(s, args)) end;
procedure InfoWrite(const s: string; const args: array of const);
begin InfoWrite(format(s, args)) end;

procedure InfoWriteParts(const TitleFormat: string; const Messages: array of string);
var i: Integer;
begin
 {$ifdef MSWINDOWS}
 if not IsConsole then
 begin
  for i := 0 to High(Messages) do
   InfoBox(Messages[i], Format(TitleFormat, [i+1, High(Messages)+1]));
 end else
 {$endif}
 begin
  for i := 0 to High(Messages) do Writeln(Messages[i]);
 end;
end;

{$endif read_implementation}