{
  Copyright 2021-2021 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Credits screen }
unit GameViewCredits;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls;

type
  { Credits view }
  TViewCredits = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonCGE, ButtonGraphics, ButtonMusic, ButtonSources, ButtonMenu: TCastleButton;
  private
    procedure ClickCGE(Sender: TObject);
    procedure ClickGraphics(Sender: TObject);
    procedure ClickMusic(Sender: TObject);
    procedure ClickSources(Sender: TObject);
    procedure ClickMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewCredits: TViewCredits;

implementation

uses CastleApplicationProperties, CastleWindow, CastleOpenDocument,
  GameViewMenu;

{ TViewMenu ----------------------------------------------------------------- }

constructor TViewCredits.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewcredits.castle-user-interface';
end;

procedure TViewCredits.Start;
begin
  inherited;
  ButtonMenu.OnClick := {$ifdef FPC}@{$endif}ClickMenu;
  ButtonCGE.OnClick := {$ifdef FPC}@{$endif}ClickCGE;
  ButtonGraphics.OnClick := {$ifdef FPC}@{$endif}ClickGraphics;
  ButtonMusic.OnClick := {$ifdef FPC}@{$endif}ClickMusic;
  ButtonSources.OnClick := {$ifdef FPC}@{$endif}ClickSources;
end;

procedure TViewCredits.ClickMenu(Sender: TObject);
begin
  Container.View := ViewMenu;
end;

procedure TViewCredits.ClickCGE(Sender: TObject);
begin
  OpenURL('https://castle-engine.io');
end;

procedure TViewCredits.ClickGraphics(Sender: TObject);
begin
  OpenURL('https://www.kenney.nl');
end;

procedure TViewCredits.ClickMusic(Sender: TObject);
begin
  OpenURL('https://www.akimaze.com');
end;

procedure TViewCredits.ClickSources(Sender: TObject);
begin
  OpenURL('https://github.com/castle-engine/castle-engine/tree/master/examples/platformer');
end;

end.
