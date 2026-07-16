{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ @seealso TViewEnd }
unit GameViewEnd;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  GameInvaders;

type
  { View displaying you won / you lost, and allowing to restart the game. }
  TViewEnd = class(TCastleView)
  private
    procedure ClickRestartEasy(Sender: TObject);
    procedure ClickRestartHard(Sender: TObject);
  public
    Won: Boolean; //< Set before view starts.
    Message: String; //< Set before view starts.
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewEnd: TViewEnd;

implementation

uses SysUtils,
  CastleColors, CastleWindow,
  GameViewMain;

{ TViewEnd ----------------------------------------------------------------- }

constructor TViewEnd.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: web: Loading castle-data:/ not supported yet on web
  // DesignUrl := 'castle-data:/gameviewend.castle-user-interface';
end;

procedure TViewEnd.Start;
var
  VerticalGroup: TCastleVerticalGroup;
  LabelInfo: TCastleLabel;
  ButtonRestartHard, ButtonRestartEasy: TCastleButton;
begin
  inherited;

  VerticalGroup := TCastleVerticalGroup.Create(FreeAtStop);
  VerticalGroup.Spacing := 10;
  VerticalGroup.Alignment := hpMiddle;
  VerticalGroup.Anchor(hpMiddle);
  VerticalGroup.Anchor(vpMiddle);
  InsertFront(VerticalGroup);

  LabelInfo := TCastleLabel.Create(FreeAtStop);
  LabelInfo.FontSize := 15;
  if Won then
    LabelInfo.Color := Yellow
  else
    LabelInfo.Color := Red;
  LabelInfo.Caption := Message;
  LabelInfo.Alignment := hpMiddle;
  VerticalGroup.InsertFront(LabelInfo);

  ButtonRestartHard := TCastleButton.Create(FreeAtStop);
  ButtonRestartHard.CustomBackground := true;
  ButtonRestartHard.CustomColorNormal := HexToColor('A11200');
  ButtonRestartHard.CustomColorFocused := HexToColor('CC1700');
  ButtonRestartHard.CustomColorPressed := HexToColor('6B0C00');
  ButtonRestartHard.Caption := 'Restart (Hard Mode)';
  ButtonRestartHard.OnClick := {$ifdef FPC}@{$endif} ClickRestartHard;
  VerticalGroup.InsertFront(ButtonRestartHard);

  ButtonRestartEasy := TCastleButton.Create(FreeAtStop);
  ButtonRestartEasy.CustomBackground := true;
  ButtonRestartEasy.CustomColorNormal := HexToColor('A11200');
  ButtonRestartEasy.CustomColorFocused := HexToColor('CC1700');
  ButtonRestartEasy.CustomColorPressed := HexToColor('6B0C00');
  ButtonRestartEasy.Caption := 'Restart (Easy Mode)';
  ButtonRestartEasy.OnClick := {$ifdef FPC}@{$endif} ClickRestartEasy;
  VerticalGroup.InsertFront(ButtonRestartEasy);
end;

procedure TViewEnd.ClickRestartEasy(Sender: TObject);
begin
  ViewMain.Easy := true;
  Container.View := ViewMain;
end;

procedure TViewEnd.ClickRestartHard(Sender: TObject);
begin
  ViewMain.Easy := false;
  Container.View := ViewMain;
end;

function TViewEnd.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyEnter) then
  begin
    // don't change difficulty level, just restart the game
    Container.View := ViewMain;
    Exit(true);
  end;

  if Event.IsKey(keyEscape) then
  begin
    Application.Terminate;
    Exit(true);
  end;
end;

end.
