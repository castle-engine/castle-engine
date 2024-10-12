{
  Copyright 2008-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Form to track and display warnings. }
unit ConsoleF;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CastleLog;

type
  { Track and display warnings
    (send to Castle Game Engine log by WritelnWarning). }
  TConsole = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    WarningsCount: Integer;
    procedure WarningToConsole(const Category, S: string);
  end;

var
  Console: TConsole;

implementation

uses MainF,
  CastleApplicationProperties;

{$R *.lfm}

{ TConsole }

procedure TConsole.WarningToConsole(const Category, S: string);
begin
  Memo1.Lines.Append(Category + ': ' + S);
  Inc(WarningsCount);
end;

procedure TConsole.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TConsole.FormCreate(Sender: TObject);
begin
  ApplicationProperties.OnWarning.Add(@WarningToConsole);
end;

procedure TConsole.FormDestroy(Sender: TObject);
begin
  ApplicationProperties.OnWarning.Remove(@WarningToConsole);
end;

end.

