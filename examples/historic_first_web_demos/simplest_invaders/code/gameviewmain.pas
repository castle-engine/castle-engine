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
{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  GameInvaders;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start.
      TODO: web: For now they are created manually in Start, not loaded. }
    LabelFps: TCastleLabel;
  private
    Invaders: TInvadersGame;
  public
    Easy: Boolean; //< Set before view starts.
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleColors, CastleUtils,
  GameViewEnd;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: web: Loading castle-data:/ not supported yet on web
  // DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  LabelInfo: TCastleLabel;
begin
  inherited;
  Invaders := TInvadersGame.Create(FreeAtStop, Easy);
  Invaders.FullSize := true;
  InsertFront(Invaders);

  LabelFps := TCastleLabel.Create(FreeAtStop);
  LabelFps.FontSize := 15;
  LabelFps.Anchor(hpRight, -5);
  LabelFps.Anchor(vpTop, -5);
  LabelFps.Color := Green;
  InsertFront(LabelFps);

  LabelInfo := TCastleLabel.Create(FreeAtStop);
  LabelInfo.FontSize := 15;
  LabelInfo.Caption := 'Mode: '  + Iff(Easy, 'Easy', 'Hard');
  LabelInfo.Color := Gray;
  LabelInfo.Anchor(hpLeft, 5);
  LabelInfo.Anchor(vpTop, -5);
  InsertFront(LabelInfo);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if not Invaders.PlayerAlive then
  begin
    ViewEnd.Won := false;
    ViewEnd.Message := 'You lost!' + NL + '(Hit by enemy rocket)';
    Container.View := ViewEnd;
  end else
  if Invaders.EnemiesGotToPlayer then
  begin
    ViewEnd.Won := false;
    ViewEnd.Message := 'You lost!' + NL +
      '(Enemy got too close to your ship.' + NL +
      'Shoot the closest enemies first!)';
    Container.View := ViewEnd;
  end else
  if not Invaders.SomeEnemyAlive then
  begin
    ViewEnd.Won := true;
    ViewEnd.Message := 'You won!' + NL + '(All the enemies destroyed)';
    Container.View := ViewEnd;
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys
end;

end.
