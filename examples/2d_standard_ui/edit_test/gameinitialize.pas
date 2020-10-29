{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initialize the game window and states. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleWindow, CastleControls, CastleUtils, CastleColors, CastleUIControls,
  CastleApplicationProperties;

var
  Window: TCastleWindowBase;
  Edit1, Edit2, EditNumbers: TCastleEdit;
  ButtonCopyText: TCastleButton;

type
  TEventHandler = class
    procedure ButtonCopyTextClick(Sender: TObject);
  end;

procedure TEventHandler.ButtonCopyTextClick(Sender: TObject);
begin
  Edit2.Text := Edit1.Text;
end;

procedure ApplicationInitialize;
var
  Group: TCastleVerticalGroup;
  Label1, Label2, Label3: TCastleLabel;
  Background: TCastleRectangleControl;
  Spacer: TCastleUserInterface;
begin
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  Background := TCastleRectangleControl.Create(Application);
  Background.Color := White;
  Background.FullSize := true;
  Window.Controls.InsertFront(Background);

  Group := TCastleVerticalGroup.Create(Application);
  Group.Anchor(vpTop);
  Group.Anchor(hpLeft);
  Group.Alignment := hpLeft;
  Group.Spacing := 10;
  Group.Padding := 10;
  Window.Controls.InsertFront(Group);

  Label1 := TCastleLabel.Create(Application);
  Label1.Caption := 'Type something in the box below';
  Label1.Color := Black;
  Group.InsertFront(Label1);

  Edit1 := TCastleEdit.Create(Application);
  Edit1.Text := 'Start text:';
  Group.InsertFront(Edit1);

  ButtonCopyText := TCastleButton.Create(Application);
  ButtonCopyText.AutoSizeWidth := false;
  ButtonCopyText.Caption := 'Copy text from one edit box to another';
  ButtonCopyText.OnClick := @TEventHandler(nil).ButtonCopyTextClick;
  Group.InsertFront(ButtonCopyText);

  Spacer := TCastleUserInterface.Create(Application);
  Spacer.Width := 1;
  Spacer.Height := 20;
  Group.InsertFront(Spacer);

  Label2 := TCastleLabel.Create(Application);
  Label2.Caption := 'Another edit box, with larger font:';
  Label2.Color := Black;
  Group.InsertFront(Label2);

  Edit2 := TCastleEdit.Create(Application);
  Edit2.FontSize := 40;
  // Edit2.CaptureAllInput := true;
  Edit2.PaddingVertical := 10;
  Group.InsertFront(Edit2);

  Spacer := TCastleUserInterface.Create(Application);
  Spacer.Width := 1;
  Spacer.Height := 20;
  Group.InsertFront(Spacer);

  Label3 := TCastleLabel.Create(Application);
  Label3.Caption := 'An edit box that only allows to input 9 digits:';
  Label3.Color := Black;
  Group.InsertFront(Label3);

  EditNumbers := TCastleEdit.Create(Application);
  EditNumbers.AllowedChars := ['0'..'9'];
  EditNumbers.MaxLength := 9;
  Group.InsertFront(EditNumbers);
end;

procedure WindowResize(Container: TUIContainer);
const
  Margin = 10;
begin
  Edit1.Width := Container.UnscaledWidth - 2 * Margin;
  Edit2.Width := Container.UnscaledWidth - 2 * Margin;
  EditNumbers.Width := Container.UnscaledWidth - 2 * Margin;
  ButtonCopyText.Width := Container.UnscaledWidth - 2 * Margin;
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'edit_test';

  Window := TCastleWindowBase.Create(Application);
  Window.OnResize := @WindowResize;
  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
end.
