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
  CastleControl, CastleControls, CastleOnScreenMenu;

type
  TForm1 = class(TForm)
    Browser: TCastleControlBase;
    procedure FormCreate(Sender: TObject);
    procedure OnScreenMenu1Click(Sender: TObject);
    procedure MainButtonClick(Sender: TObject);
  private
    OnScreenMenu1: TCastleOnScreenMenu;
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

  { Add user interface designed by CGE editor.
    This is the most advised way to design UI. }
  procedure AddUserInterfaceDesigned;
  var
    UiRoot: TCastleUserInterface;
    UiOwner: TComponent;
    MainButton: TCastleButton;
  begin
    { UiOwner will own (free) the UI loaded by UserInterfaceLoad,
      and will also be useful to find UI items by name,
      like "UiOwner.FindRequiredComponent". }
    UiOwner := TComponent.Create(Self);

    UiRoot := UserInterfaceLoad('castle-data:/main.castle-user-interface', UiOwner);
    Browser.Controls.InsertFront(UiRoot);

    MainButton := UiOwner.FindRequiredComponent('MainButton') as TCastleButton;
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

    OnScreenMenu1 := TCastleOnScreenMenu.Create(Self);
    OnScreenMenu1.Bottom := 140;
    OnScreenMenu1.Left := 400;
    OnScreenMenu1.Add('one', @OnScreenMenu1Click);
    OnScreenMenu1.Add('two', @OnScreenMenu1Click);
    OnScreenMenu1.Add('three', @OnScreenMenu1Click);
    Browser.Controls.InsertFront(OnScreenMenu1);
  end;

begin
  AdjustTheme;
  AddUserInterfaceDesigned;
  AddUserInterfaceFromCode;
end;

procedure TForm1.OnScreenMenu1Click(Sender: TObject);
begin
  ShowMessage(Format('Clicked menu item %d.', [OnScreenMenu1.CurrentItem]));
end;

procedure TForm1.MainButtonClick(Sender: TObject);
begin
  ShowMessage('Button clicked !');
end;

initialization
  {$I mainf.lrs}
end.
