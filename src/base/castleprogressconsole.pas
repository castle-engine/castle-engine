{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Progress bar displayed on console (actually, on StdErr).)

  You can assign ProgressConsoleInterface to Progress.UserInterface,
  like

  @longCode(#  Progress.UserInterface := ProgressConsoleInterface;#)

  and then all progress bars will be displayed on console.

  This displays a title surrounded by [] characters and the progress
  is indicated by displaying dots. This way we visualize progress
  incrementing from 0 to 100%, and at the same time we use only
  normal streaming I/O on StdErr. Totally no console/terminal specific
  operations, no special codes, no Crt / Curses unit's used etc.
  So this unit doesn't create any terminal compatibility problems,
  doesn't mess standard input/output/error streams etc.

  The only restriction is that you should not output anything
  (on stdout and stderr) to not mess the displayed progress bar.
  Of course, the worse that will happen is that the progress bar
  will stop looking good for user, nothing more.

  If you really want a progress bar that uses your terminal capabilities
  see ProgressVideo unit, that displays progress on terminal by FPC's
  Video unit.
}
unit CastleProgressConsole;

{$I castleconf.inc}

interface

uses CastleProgress;

type
  { }
  TProgressConsoleInterface = class(TProgressUserInterface)
  private
    { will grow from 0 to ConsoleWidth }
    KropeczkiWritten: Integer;
  public
    procedure Init(Progress: TProgress); override;
    procedure Update(Progress: TProgress); override;
    procedure Fini(Progress: TProgress); override;
  end;

var
  { Assign this to Progress.UserInterface to use console progress bar.
    This instance is created in initialization, freed in finalization. }
  ProgressConsoleInterface :TProgressConsoleInterface;

implementation

uses
  SysUtils, CastleUtils;

const
  { This is the width of the console that we can write without automatically
    moving to the next line.

    Of course, this shouldn't be a constant,
    but actually there is no way here to do anything better.
    I can't use here any console/video/terminal functions,
    because this unit is supposed to work only with bare StdErr. }
  ConsoleWidth = 60;

procedure Write(const s: string);
begin System.Write(ErrOutput, s); end;

procedure Writeln(const s: string);
begin System.Writeln(ErrOutput, s); end;

{ realizacja procedur progressa  ------------------------------------------------}

procedure TProgressConsoleInterface.Init(Progress: TProgress);
var LeftSpace, RightSpace: integer;
begin
 if Length(Progress.Title) > ConsoleWidth-2 then
 begin
  Writeln(Progress.Title);
  Writeln('[' +StringOfChar(' ', ConsoleWidth-2) +']');
 end else
 begin
  { poniewaz Length(Progress.Title) > ConsoleWidth-2 to
    2 <= ConsoleWidth-Length(Progress.Title) wiec
    0 <= (ConsoleWidth-Length(Progress.Title)) div 2 - 1. }
  LeftSpace:=(ConsoleWidth-Length(Progress.Title)) div 2 - 1;
  RightSpace := ConsoleWidth - LeftSpace - Length(Progress.Title) - 2;
  Writeln('[' +StringOfChar(' ', LeftSpace) +Progress.Title +
    StringOfChar(' ', RightSpace) +']');
 end;

 KropeczkiWritten := 0;
end;

procedure TProgressConsoleInterface.Update(Progress: TProgress);
var KropeczkiNow: integer;
begin
 KropeczkiNow := Progress.Position*ConsoleWidth div Progress.Max;
 if KropeczkiNow > KropeczkiWritten then
 begin
  Write(StringOfChar('.', KropeczkiNow-KropeczkiWritten));
  KropeczkiWritten := KropeczkiNow;
 end;
end;

procedure TProgressConsoleInterface.Fini(Progress: TProgress);
begin
 Writeln('');
end;

initialization
 ProgressConsoleInterface := TProgressConsoleInterface.Create;
finalization
 FreeAndNil(ProgressConsoleInterface);
end.
