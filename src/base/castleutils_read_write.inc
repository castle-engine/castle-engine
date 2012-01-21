{
  Copyright 2002-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Some wrappers around Write[ln] that can bahave nicely when used
  from Windows GUI programs (without a console). }

{$ifdef read_interface}

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