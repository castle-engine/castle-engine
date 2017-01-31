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

{ Game state that asks a question using a dialog. }
unit GameStateAskDialog;

interface

uses Classes, CastleControls, CastleUIState;

type
  TStateAskDialog = class(TUIState)
  strict private
    type
      TZombieDialog = class(TCastleRectangleControl)
      strict private
        InsideRect: TCastleRectangleControl;
        Image: TCastleImageControl;
        LabelStats: TCastleLabel;
        ButtonRun, ButtonFight: TCastleButton;
        procedure RunClick(Sender: TObject);
        procedure FightClick(Sender: TObject);
      public
        constructor Create(AOwner: TComponent; const Male: boolean); reintroduce;
      end;
    var
    TransparentBackground: TCastleRectangleControl;
    Dialog: TZombieDialog;
  public
    { Whether to show male image. Set before doing @link(Start). }
    Male: boolean;
    procedure Start; override;
  end;

var
  StateAskDialog: TStateAskDialog;

implementation

uses CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils,
  CastleUtils, CastleVectors;

{ TStateAskDialog.TZombieDialog ---------------------------------------------- }

constructor TStateAskDialog.TZombieDialog.Create(AOwner: TComponent; const Male: boolean);
begin
  inherited Create(AOwner);

  Width := 400;
  Height := 500;
  Color := HexToColor('5f3939'); // equivalent: Vector4Single(95/255, 57/255, 57/255, 1.0);

  InsideRect := TCastleRectangleControl.Create(Self);
  InsideRect.Width := CalculatedWidth - 10;
  InsideRect.Height := CalculatedHeight - 10;
  InsideRect.Color := Silver;
  InsideRect.Anchor(hpMiddle);
  InsideRect.Anchor(vpMiddle);
  InsertFront(InsideRect);

  Image := TCastleImageControl.Create(Self);
  if Male then
    Image.URL := ApplicationData('Male-Zombie-300px.png')
  else
    Image.URL := ApplicationData('Female-Zombie-300px.png');
  Image.Anchor(hpMiddle);
  Image.Anchor(vpTop, -10);
  InsideRect.InsertFront(Image);

  LabelStats := TCastleLabel.Create(Self);
  LabelStats.Color := Black;
  LabelStats.Html := true;
  { anything, just to show off the HTML :) }
  LabelStats.Caption := 'Statistics:' + NL +
    'Life: <font color="#ff0000">12%</font>' + NL +
    'Stamina: <font color="#ffff00">34%</font>' + NL +
    'Mana: <font color="#0000ff">56%</font>';
  LabelStats.Anchor(hpMiddle);
  LabelStats.Anchor(vpBottom, 100);
  InsideRect.InsertFront(LabelStats);

  ButtonRun := TCastleButton.Create(Self);
  ButtonRun.Caption := 'Run';
  ButtonRun.Anchor(hpLeft, 10);
  ButtonRun.Anchor(vpBottom, 10);
  ButtonRun.PaddingHorizontal := 40;
  ButtonRun.OnClick := @RunClick;
  InsideRect.InsertFront(ButtonRun);

  ButtonFight := TCastleButton.Create(Self);
  ButtonFight.Caption := 'Fight';
  ButtonFight.Anchor(hpRight, -10);
  ButtonFight.Anchor(vpBottom, 10);
  ButtonFight.PaddingHorizontal := 40;
  ButtonFight.OnClick := @FightClick;
  InsideRect.InsertFront(ButtonFight);
end;

procedure TStateAskDialog.TZombieDialog.RunClick(Sender: TObject);
begin
  { As this is just a demo, there's no actual "running",
    we just return to StatePlay. }
  TUIState.Pop(StateAskDialog);
end;

procedure TStateAskDialog.TZombieDialog.FightClick(Sender: TObject);
begin
  { As this is just a demo, there's no actual "fighting",
    we just return to StatePlay. }
  TUIState.Pop(StateAskDialog);
end;

{ TStateAskDialog ------------------------------------------------------------ }

procedure TStateAskDialog.Start;
begin
  inherited;

  { Do not allow clicks to pass to StatePlay underneath.
    We are transparent (show the StatePlay underneath),
    but we don't want to allow user to interact with it (e.g. by causing
    another StateAskDialog by clicking, or by pressing on
    StatePlay.ButtonBack). }
  InterceptInput := true;

  TransparentBackground := TCastleRectangleControl.Create(FreeAtStop);
  TransparentBackground.Color := Vector4Single(0.1, 0.1, 0.1, 0.5);
  TransparentBackground.FullSize := true;
  InsertFront(TransparentBackground);

  Dialog := TZombieDialog.Create(FreeAtStop, Male);
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpMiddle);
  InsertFront(Dialog);
end;

end.
