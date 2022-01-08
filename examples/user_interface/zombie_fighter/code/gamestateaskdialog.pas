{
  Copyright 2016-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game state that asks a question using a dialog. }
unit GameStateAskDialog;

interface

uses Classes,
  CastleControls, CastleUIState, CastleUIControls;

type
  TStateAskDialog = class(TUIState)
  strict private
    type
      { Example of designing a custom TCastleUserInterface descendant,
        which acts as reusable UI control.

        Note: In this example, this is quite pointless.
        You could (and should) just put the dialog inside
        state_ask_dialog.castle-user-interface, and you don't need nested
        class TZombieDialog! This is just a demo that you *could* design a reusable
        UI component like this.

        Other approach to have reusable UI is to use TCastleDesign to load one
        design inside another. See examples/advanced_editor/ for demo. }
      TZombieDialog = class(TCastleUserInterface)
      strict private
        ImageEnemy: TCastleImageControl;
        ButtonRun, ButtonFight: TCastleButton;
        procedure ClickRun(Sender: TObject);
        procedure ClickFight(Sender: TObject);
      public
        constructor Create(AOwner: TComponent; const Male: boolean); reintroduce;
      end;
    var
      Dialog: TZombieDialog;
  public
    { Whether to show male image. Set before doing @link(Start). }
    Male: boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  StateAskDialog: TStateAskDialog;

implementation

uses CastleUtils, CastleVectors, CastleComponentSerialize;

{ TStateAskDialog.TZombieDialog ---------------------------------------------- }

constructor TStateAskDialog.TZombieDialog.Create(AOwner: TComponent; const Male: boolean);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
begin
  inherited Create(AOwner);

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner := TComponent.Create(Self);

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/ask_dialog.castle-user-interface', UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  ImageEnemy := UiOwner.FindRequiredComponent('ImageEnemy') as TCastleImageControl;
  ButtonRun := UiOwner.FindRequiredComponent('ButtonRun') as TCastleButton;
  ButtonFight := UiOwner.FindRequiredComponent('ButtonFight') as TCastleButton;

  if Male then
    ImageEnemy.URL := 'castle-data:/enemy_images/Male-Zombie-300px.png'
  else
    ImageEnemy.URL := 'castle-data:/enemy_images/Female-Zombie-300px.png';

  ButtonRun.OnClick := {$ifdef FPC}@{$endif}ClickRun;
  ButtonFight.OnClick := {$ifdef FPC}@{$endif}ClickFight;

  { Set own size to be equal to designed dialog in ask_dialog.castle-user-interface,
    that has explicit Width and Height set in editor. }
  AutoSizeToChildren := true;
end;

procedure TStateAskDialog.TZombieDialog.ClickRun(Sender: TObject);
begin
  { As this is just a demo, there's no actual "running",
    we just return to StatePlay. }
  Container.PopView(StateAskDialog);
end;

procedure TStateAskDialog.TZombieDialog.ClickFight(Sender: TObject);
begin
  { As this is just a demo, there's no actual "fighting",
    we just return to StatePlay. }
  Container.PopView(StateAskDialog);
end;

{ TStateAskDialog ------------------------------------------------------------ }

constructor TStateAskDialog.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateaskdialog.castle-user-interface';
end;

procedure TStateAskDialog.Start;
begin
  inherited;

  { Do not allow clicks to pass to StatePlay underneath.
    We are transparent (show the StatePlay underneath),
    but we don't want to allow user to interact with it (e.g. by causing
    another StateAskDialog by clicking, or by pressing on
    StatePlay.ButtonBack). }
  InterceptInput := true;

  Dialog := TZombieDialog.Create(FreeAtStop, Male);
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpMiddle);
  InsertFront(Dialog);
end;

end.
