{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Unit zarzadzajacy progressem na konsoli (na StdErr, dokladniej).)

  To jest prosty progress ktory wyswietla tytulik otoczony nawiasami []
  i potem w linijce ponizej biegna kropeczki. Specjalnie zostal tak
  napisany zeby nie wymagac zadnego modulu crt, ncurses, WinConsole
  itp. Wystarcza mu proste Write na StdErr ! Dzieki temu mozemy
  nawet zostac przekierowani do pliku i nic nie zauwazymy !

  Po ladniejszy progress, ktory wyglada lepiej i jest naprawde
  konsolowy (a nie tylko na StdErr) patrz ProgressVideo.

  Niczego nie wypisuj na StdErr w czasie gdy ten program dziala.

  Rejestruje odpowiednie funkcje modulu ProgressUnit w RegisterProgressConsole.
}

unit ProgressConsole;

{$I kambiconf.inc}

interface

uses ProgressUnit;

type
  TProgressConsoleInterface = class(TProgressUserInterface)
  private
    KropeczkiWritten: Integer; {bedzie sie zmieniac od 0 do ConsoleWidth}
  public
    procedure Init(Progress: TProgress); override;
    procedure Update(Progress: TProgress); override;
    procedure Fini(Progress: TProgress); override;
  end;

var
  { created in initialization, freed in finalization }
  ProgressConsoleInterface :TProgressConsoleInterface;

implementation

uses
  SysUtils, KambiUtils;

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
