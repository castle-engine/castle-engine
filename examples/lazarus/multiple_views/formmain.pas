{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main menu. }
unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CastleControl;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonMainMenu: TButton;
    ButtonPlay: TButton;
    CastleControl1: TCastleControl;
    Panel1: TPanel;
    procedure ButtonMainMenuClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

uses GameViewMainMenu, GameViewPlay;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ButtonMainMenuClick(Sender: TObject);
begin
  CastleControl1.Container.View := ViewMainMenu;
end;

procedure TMainForm.ButtonPlayClick(Sender: TObject);
begin
  CastleControl1.Container.View := ViewPlay;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ViewMainMenu := TViewMainMenu.Create(Application);
  ViewPlay := TViewPlay.Create(Application);

  CastleControl1.Container.View := ViewMainMenu;
end;

end.

