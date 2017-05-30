{
  Copyright 2016-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initialize the game window and states. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindowCustom;

implementation

uses SysUtils, Classes,
  CastleControls, CastleUtils, CastleColors, CastleUIControls;

const
  Margin = 10;

var
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
  Label1, Label2, Label3: TCastleLabel;
  SimpleBackground: TCastleSimpleBackground;
  Y: Integer;
begin
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  SimpleBackground := TCastleSimpleBackground.Create(Application);
  SimpleBackground.Color := White;
  Window.Controls.InsertFront(SimpleBackground);

  Y := -Margin;

  Label1 := TCastleLabel.Create(Application);
  Label1.Anchor(vpTop, Y);
  Label1.Anchor(hpLeft, Margin);
  Label1.Caption := 'Type something in the box below';
  Label1.Color := Black;
  Window.Controls.InsertFront(Label1);
  Y -= Label1.CalculatedHeight + Margin;

  Edit1 := TCastleEdit.Create(Application);
  Edit1.Anchor(vpTop, Y);
  Edit1.Anchor(hpLeft, Margin);
  Edit1.Text := 'Start text:';
  Window.Controls.InsertFront(Edit1);
  Y -= Edit1.CalculatedHeight + Margin;

  ButtonCopyText := TCastleButton.Create(Application);
  ButtonCopyText.Anchor(vpTop, Y);
  ButtonCopyText.Anchor(hpLeft, Margin);
  ButtonCopyText.AutoSizeWidth := false;
  ButtonCopyText.Caption := 'Copy text from one edit box to another';
  ButtonCopyText.OnClick := @TEventHandler(nil).ButtonCopyTextClick;
  Window.Controls.InsertFront(ButtonCopyText);
  Y -= ButtonCopyText.CalculatedHeight + Margin + 20 { extra margin };

  Label2 := TCastleLabel.Create(Application);
  Label2.Anchor(vpTop, Y);
  Label2.Anchor(hpLeft, Margin);
  Label2.Caption := 'Another edit box, with larger font:';
  Label2.Color := Black;
  Window.Controls.InsertFront(Label2);
  Y -= Label2.CalculatedHeight + Margin;

  Edit2 := TCastleEdit.Create(Application);
  Edit2.Anchor(vpTop, Y);
  Edit2.Anchor(hpLeft, Margin);
  Edit2.FontSize := 40;
  // Edit2.CaptureAllInput := true;
  Edit2.PaddingVertical := 10;
  Window.Controls.InsertFront(Edit2);
  Y -= Edit2.CalculatedHeight + Margin + 20 { extra margin };

  Label3 := TCastleLabel.Create(Application);
  Label3.Anchor(vpTop, Y);
  Label3.Anchor(hpLeft, Margin);
  Label3.Caption := 'An edit box that only allows to input 9 digits:';
  Label3.Color := Black;
  Window.Controls.InsertFront(Label3);
  Y -= Label3.CalculatedHeight + Margin;

  EditNumbers := TCastleEdit.Create(Application);
  EditNumbers.Anchor(vpTop, Y);
  EditNumbers.Anchor(hpLeft, Margin);
  EditNumbers.AllowedChars := ['0'..'9'];
  EditNumbers.MaxLength := 9;
  Window.Controls.InsertFront(EditNumbers);
  Y -= EditNumbers.CalculatedHeight + Margin;
end;

procedure WindowResize(Container: TUIContainer);
begin
  Edit1.Width := Container.UnscaledWidth - 2 * Margin;
  Edit2.Width := Container.UnscaledWidth - 2 * Margin;
  EditNumbers.Width := Container.UnscaledWidth - 2 * Margin;
  ButtonCopyText.Width := Container.UnscaledWidth - 2 * Margin;
end;

function MyGetApplicationName: string;
begin
  Result := 'edit_test';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  Window := TCastleWindowCustom.Create(Application);
  Window.OnResize := @WindowResize;
  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
end.
