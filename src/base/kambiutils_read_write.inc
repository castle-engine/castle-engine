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

{ Prompt a user to input integer value. }
function ReadlnInt(const Prompt: string): integer;
{ Prompt a user to input a string. }
function ReadlnStr(const Prompt: string): string;

type
  TYesNoDefault=(defFalse, defNone, defTrue);

{ Prompt a user to answer a yes/no question.
  We write prompt on standard output, and read from standard input.

  Prompt nie powinien zawierac koncowego ' [y/n] ' czy cos w tym rodzaju.
  Dla przykladowego
    ReadlnChoice('Really do this ?', defTrue)
  bedzie wyswietlal 'Really do this ? [Y/n]' i czekal na odpowiedz przez
  Readln(string). Jesli podany string bedzie rowny 'y' lub 'n' (case ignored)
  to zwroci odpowiednio true lub false. Jesli Default <> defNone to string ''
  tez jest akceptowany (i oznacza true lub false, odpowiednio, dla
  Default = defTrue i defFalse).

  Przyrostek ' [y/n] ' lub ' [Y/n] ' lub ' [y/N] ' odzwierciedla stan Default. }
function ReadlnYesNo(Prompt: string; Default: TYesNoDefault): boolean;

{ Prompt a user to input a string, and wait until one of the specified answers
  is given. We write prompt on standard output, and read from standard input.

  Prompt powinien zawierac koncowy znak, np. '?' lub ':' ale nie powinien zawierac
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

{ Write routines that use a dialog box or console to best present the message.

  If we are under Windows and not IsConsole then we use
  dialog boxes through ErrorBox, WarningBox, InfoBox.
  Otherwise (not under Windows, or a console) we output the message
  (to standard output for InfoWrite, or ErrOutput
  for ErrorWrite and WarningWrite).

  Notes for Windows: even when IsConsole = false, we can have
  GetStdHandle(STD_ERROR_HANDLE) and/or GetStdHandle(STD_OUTPUT_HANDLE)
  available. User can always run GUI program and redirect our
  stdin/out/err and then GUI app will have some of stdin, stdout, stderr
  available. In other words, GUI app *may* have stdin/out/err,
  but it doesn't have to. Depends on how the user run the app.
  But XxxWrite functions will always for GUI app (that have
  IsConsole = false) use XxxBox functions, never Writeln,
  even when stdin/out/err will be available.
  This ensures that program always behaves in the same way.

  @groupBegin }
procedure ErrorWrite(const s: string); overload;
procedure WarningWrite(const s: string); overload;
procedure InfoWrite(const s: string); overload;

procedure ErrorWrite(const s: string; const args: array of const); overload;
procedure WarningWrite(const s: string; const args: array of const); overload;
procedure InfoWrite(const s: string; const args: array of const); overload;
{ @groupEnd }

{ Output messages, using console or dialog box.

  If we're not on Windows or IsConsole, then we simply output Messages
  using Writeln.

  If we're on Windows and not IsConsole, then every Messages is displayed
  in a separate dialog box. Dialog box uses our InfoBox routine,
  with Messages[I] being message content and title being
  @code(Format(TitleFormat, [I + 1, Messages.Count])).

  This is good for outputting a lot of information. }
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