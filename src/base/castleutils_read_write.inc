{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{ Write using a dialog box or console.

  If we are a Windows GUI program (not IsConsole) then we use
  native Windows dialog boxes.
  Otherwise (a console is available, which is always true on non-Windows)
  we output the message using simple Writeln (to standard output for
  InfoWrite, or ErrOutput for ErrorWrite and WarningWrite).

  @groupBegin }
procedure ErrorWrite(const S: String); overload;
procedure WarningWrite(const S: String); overload;
procedure InfoWrite(const S: String); overload;

procedure ErrorWrite(const S: String; const args: array of const); overload;
procedure WarningWrite(const S: String; const args: array of const); overload;
procedure InfoWrite(const S: String; const args: array of const); overload;
{ @groupEnd }

{ Output messages, using console or dialog box.

  If we're not on Windows or IsConsole, then we simply output Messages
  using Writeln.

  If we're on Windows and not IsConsole, then every Messages is displayed
  in a separate dialog box. Dialog box uses our InfoBox routine,
  with Messages[I] being message content and title being
  @code(Format(TitleFormat, [I + 1, Messages.Count])).

  This is good for outputting a lot of information.

  @deprecated Deprecated. This just looks ugly in GUI version.
  It's better to present long information using only a console (just use Writeln),
  or only a full-featured GUI (like Lazarus LCL or our CastleUIControls). }
procedure InfoWriteParts(const TitleFormat: String; const Messages: array of String); deprecated;

{$endif read_interface}

{$ifdef read_implementation}

{ Notes for Windows: even when IsConsole = false, we can have
  GetStdHandle(STD_ERROR_HANDLE) and/or GetStdHandle(STD_OUTPUT_HANDLE)
  available. User can always run GUI program and redirect our
  stdin/out/err and then GUI app will have some of stdin, stdout, stderr
  available. In other words, GUI app *may* have stdin/out/err,
  but it doesn't have to. Depends on how the user run the app.
  But XxxWrite functions will always for GUI app (that have
  IsConsole = false) use XxxBox functions, never Writeln,
  even when stdin/out/err will be available.
  This ensures that program always behaves in the same way.
}

{$warnings off}
{ These use some deprecated procedures on Windows.
  It's Ok, they will be just removed from unit public interface
  and undeprecated then in the future. }

procedure ErrorWrite(const S: String);
begin
  {$ifdef MSWINDOWS} if not IsConsole then ErrorBox(S) else {$endif}
  Writeln(ErrOutput, S);
end;

procedure WarningWrite(const S: String);
begin
  {$ifdef MSWINDOWS} if not IsConsole then WarningBox(S) else {$endif}
  Writeln(ErrOutput, S);
end;

procedure InfoWrite(const S: String);
begin
  {$ifdef MSWINDOWS} if not IsConsole then InfoBox(S) else {$endif}
  Writeln(S);
end;

procedure ErrorWrite(const S: String; const args: array of const);
begin ErrorWrite(format(S, args)) end;
procedure WarningWrite(const S: String; const args: array of const);
begin WarningWrite(format(S, args)) end;
procedure InfoWrite(const S: String; const args: array of const);
begin InfoWrite(format(S, args)) end;

procedure InfoWriteParts(const TitleFormat: String; const Messages: array of String);
var I: Integer;
begin
 {$ifdef MSWINDOWS}
 if not IsConsole then
 begin
  for I := 0 to High(Messages) do
   InfoBox(Messages[I], Format(TitleFormat, [I+1, High(Messages)+1]));
 end else
 {$endif}
 begin
  for I := 0 to High(Messages) do Writeln(Messages[I]);
 end;
end;

{$warnings on}

{$endif read_implementation}
