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

{ Main LCL form. }
unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CastleControl;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonTest2D: TButton;
    ButtonTest3D: TButton;
    CastleControl1: TCastleControl;
    Panel1: TPanel;
    procedure ButtonTest2DClick(Sender: TObject);
    procedure ButtonTest3DClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

uses GameViewTest2D, GameViewTest3D;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ButtonTest2DClick(Sender: TObject);
begin
  CastleControl1.Container.View := ViewTest2D;
end;

procedure TMainForm.ButtonTest3DClick(Sender: TObject);
begin
  CastleControl1.Container.View := ViewTest3D;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ViewTest2D := TViewTest2D.Create(Application);
  ViewTest3D := TViewTest3D.Create(Application);

  CastleControl1.Container.View := ViewTest2D;
end;

end.

