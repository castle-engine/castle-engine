{
  Copyright 2021-2024 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ View pause. }
unit GameViewPause;

interface

uses Classes,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewPause = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonMenu: TCastleButton;
    ButtonResume: TCastleButton;
  private
    procedure ClickResume(Sender: TObject);
    procedure ClickMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewPause: TViewPause;

implementation

uses CastleSoundEngine,
  GameSound, GameViewMenu;

constructor TViewPause.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewpause.castle-user-interface';
end;

procedure TViewPause.ClickResume(Sender: TObject);
begin
  Container.PopView(Self);
end;

procedure TViewPause.ClickMenu(Sender: TObject);
begin
  Container.View := ViewMenu;
end;

procedure TViewPause.Start;
begin
  inherited;
  ButtonResume.OnClick := {$ifdef FPC}@{$endif}ClickResume;
  ButtonMenu.OnClick := {$ifdef FPC}@{$endif}ClickMenu;

  { Play menu music }
  SoundEngine.LoopingChannel[0].Sound := NamedSound('MenuMusic');
end;

function TViewPause.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  // allow to exit pause with the same key as entering pause: Escape
  if Event.IsKey(keyEscape) then
  begin
    ClickResume(nil);
    Result := true;
  end;
end;

end.
