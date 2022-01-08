{
  Copyright 2014-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main form. }
unit MainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CastleControl, CastleControls, CastleOnScreenMenu,
  GameViewMain;

type
  TForm1 = class(TForm)
    CastleControl: TCastleControlBase;
    procedure FormCreate(Sender: TObject);
  private
    ViewMain: TViewMain;
    procedure MenuClick(Sender: TObject);
    procedure MainButtonClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses CastleSceneCore, CastleUtils, CastleImages, CastleVectors,
  CastleControlsImages, CastleUIControls, CastleComponentSerialize;

{ TForm1 --------------------------------------------------------------------- }

procedure TForm1.FormCreate(Sender: TObject);

  { Customize the look of all controls by adjusting Theme. }
  procedure AdjustTheme;
  begin
    { Change the tooltip image to have rounded corners,
      using a predefined TooltipRounded image from CastleControlsImages
      unit. You could also use load and use your own image,
      e.g. by LoadImage. }
    Theme.ImagesPersistent[tiTooltip].Image := TooltipRounded;
    Theme.ImagesPersistent[tiTooltip].ProtectedSides.AllSides := 9;
  end;

begin
  { This corresponds to the Application.OnInitialize contents of
    a TCastleWindow-based application.
    We do
    - some one-time initialization,
    - create TCastleView instances,
    - set one TCastleView instance as current using "CastleControl.View := ..." }

  AdjustTheme;

  ViewMain := TViewMain.Create(Self);
  ViewMain.OnMenuClick := @MenuClick;
  ViewMain.OnMainButtonClick := @MainButtonClick;

  CastleControl.Container.View := ViewMain;
end;

procedure TForm1.MenuClick(Sender: TObject);
begin
  ShowMessage(Format('Clicked menu item %s.', [(Sender as TCastleOnScreenMenuItem).Caption]));
end;

procedure TForm1.MainButtonClick(Sender: TObject);
begin
  ShowMessage('Main button clicked.');
end;

initialization
  {$I mainf.lrs}
end.
