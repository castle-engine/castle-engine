{
  Copyright 2014-2022 Michalis Kamburelis.

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
  CastleControl, CastleControls;

type
  TForm1 = class(TForm)
    Browser: TCastleControl;
    procedure FormCreate(Sender: TObject);
    procedure MainButtonClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses CastleSceneCore, CastleUtils, CastleImages, CastleVectors,
  CastleUIControls, CastleComponentSerialize;

{ TForm1 --------------------------------------------------------------------- }

procedure TForm1.FormCreate(Sender: TObject);

  { Customize the look of all controls by adjusting Theme. }
  procedure AdjustTheme;
  begin
    { Change the tooltip image to have rounded corners,
      using a predefined TooltipRounded image from CastleInternalControlsImages
      unit. You could also use load and use your own image,
      e.g. by LoadImage. }
    Theme.ImagesPersistent[tiTooltip].Url := 'castle-data:/TooltipRounded.png';
    Theme.ImagesPersistent[tiTooltip].ProtectedSides.AllSides := 9;
  end;

  { Initialize user interface designed by CGE editor.
    It is already loaded by Browser.DesignUrl.
    This is the most advised way to design UI. }
  procedure InitializeDesignedUserInterface;
  var
    MainButton: TCastleButton;
  begin
    MainButton := Browser.Container.DesignedComponent('MainButton') as TCastleButton;
    MainButton.OnClick := @MainButtonClick;
  end;

  { Add additional user interface elements created manually,
    by code, just to show that we can. }
  procedure AddUserInterfaceFromCode;
  var
    Button: TCastleButton;
  begin
    Button := TCastleButton.Create(Self);
    Button.Caption := 'Button added from code';
    Button.Anchor(hpRight, -10);
    Button.Anchor(vpBottom, 120);
    Browser.Controls.InsertFront(Button);
  end;

begin
  AdjustTheme;
  InitializeDesignedUserInterface;
  AddUserInterfaceFromCode;
end;

procedure TForm1.MainButtonClick(Sender: TObject);
begin
  ShowMessage('Main button clicked.');
end;

end.

