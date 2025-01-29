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
{ @seealso TViewInitial }
unit GameViewInitial;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  GameInvaders;

type
  { Initial view to choose difficulty and show instructions. }
  TViewInitial = class(TCastleView)
  private
    procedure ClickStartEasy(Sender: TObject);
    procedure ClickStartHard(Sender: TObject);
  public
    Won: Boolean; //< Set before view starts.
    Message: String; //< Set before view starts.
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewInitial: TViewInitial;

implementation

uses SysUtils,
  CastleColors, CastleUtils,
  GameViewMain;

{ TViewInitial ----------------------------------------------------------------- }

constructor TViewInitial.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: web: Loading castle-data:/ not supported yet on web
  // DesignUrl := 'castle-data:/gameViewInitial.castle-user-interface';
end;

procedure TViewInitial.Start;
var
  VerticalGroup: TCastleVerticalGroup;
  LabelInfo: TCastleLabel;
  ButtonStartHard, ButtonStartEasy: TCastleButton;
  Spacer: TCastleUserInterface;
begin
  inherited;

  VerticalGroup := TCastleVerticalGroup.Create(FreeAtStop);
  VerticalGroup.Spacing := 10;
  VerticalGroup.Alignment := hpMiddle;
  VerticalGroup.Anchor(hpMiddle);
  VerticalGroup.Anchor(vpMiddle);
  InsertFront(VerticalGroup);

  LabelInfo := TCastleLabel.Create(FreeAtStop);
  LabelInfo.FontSize := 20;
  LabelInfo.Color := White;
  LabelInfo.Caption := 'Invaders!' + NL +
    NL +
    'Keys:' + NL +
    '- Move: A / D' + NL +
    '- Shoot: W';
  LabelInfo.Alignment := hpMiddle;
  VerticalGroup.InsertFront(LabelInfo);

  Spacer := TCastleUserInterface.Create(FreeAtStop);
  Spacer.Height := 20;
  VerticalGroup.InsertFront(Spacer);

  ButtonStartHard := TCastleButton.Create(FreeAtStop);
  ButtonStartHard.CustomBackground := true;
  ButtonStartHard.CustomColorNormal := HexToColor('A11200');
  ButtonStartHard.CustomColorFocused := HexToColor('CC1700');
  ButtonStartHard.CustomColorPressed := HexToColor('6B0C00');
  ButtonStartHard.Caption := 'Start (Hard Mode)';
  ButtonStartHard.OnClick := {$ifdef FPC}@{$endif} ClickStartHard;
  VerticalGroup.InsertFront(ButtonStartHard);

  ButtonStartEasy := TCastleButton.Create(FreeAtStop);
  ButtonStartEasy.CustomBackground := true;
  ButtonStartEasy.CustomColorNormal := HexToColor('A11200');
  ButtonStartEasy.CustomColorFocused := HexToColor('CC1700');
  ButtonStartEasy.CustomColorPressed := HexToColor('6B0C00');
  ButtonStartEasy.Caption := 'Start (Easy Mode)';
  ButtonStartEasy.OnClick := {$ifdef FPC}@{$endif} ClickStartEasy;
  VerticalGroup.InsertFront(ButtonStartEasy);
end;

procedure TViewInitial.ClickStartEasy(Sender: TObject);
begin
  ViewMain.Easy := true;
  Container.View := ViewMain;
end;

procedure TViewInitial.ClickStartHard(Sender: TObject);
begin
  ViewMain.Easy := false;
  Container.View := ViewMain;
end;

end.
