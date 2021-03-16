{
  Copyright 2016-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from state_main.castle-user-interface. }
    LabelMain: TCastleLabel;
    ButtonHtml, ButtonWrap, ButtonAlignLeft, ButtonAlignMiddle, ButtonAlignRight: TCastleButton;
    ScrollView: TCastleScrollView;

    { The critical thing to make TCastleScrollView work correctly is to set right
      the ScrollView.ScrollArea size.
      ScrollView.ScrollArea should enclose the children vertically,
      but the width of ScrollView.ScrollArea should match the available parent width.
      This is what this routine does.

      It is possible it will be possible easier (without even writing code)
      in the future. We have WidthFraction, we have AutoSizeToChildren,
      but we would need AutoSizeToChildrenHeight (that doesn't cause recursive loop
      in case parent<->child each depend on other size).

      This routine also updates LabelMain.MaxWidth. }
    procedure UpdateSize;

    procedure ClickHtml(Sender: TObject);
    procedure ClickWrap(Sender: TObject);
    procedure ClickAlignLeft(Sender: TObject);
    procedure ClickAlignMiddle(Sender: TObject);
    procedure ClickAlignRight(Sender: TObject);
  public
    procedure Start; override;
    procedure Resize; override;
  end;

var
  StateMain: TStateMain;

implementation

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  LabelMain := UiOwner.FindRequiredComponent('LabelMain') as TCastleLabel;
  ButtonHtml := UiOwner.FindRequiredComponent('ButtonHtml') as TCastleButton;
  ButtonWrap := UiOwner.FindRequiredComponent('ButtonWrap') as TCastleButton;
  ButtonAlignLeft := UiOwner.FindRequiredComponent('ButtonAlignLeft') as TCastleButton;
  ButtonAlignMiddle := UiOwner.FindRequiredComponent('ButtonAlignMiddle') as TCastleButton;
  ButtonAlignRight := UiOwner.FindRequiredComponent('ButtonAlignRight') as TCastleButton;
  ScrollView := UiOwner.FindRequiredComponent('ScrollView') as TCastleScrollView;

  { Assign buttons OnClick handlers }
  ButtonHtml.OnClick := @ClickHtml;
  ButtonWrap.OnClick := @ClickWrap;
  ButtonAlignRight.OnClick := @ClickAlignRight;
  ButtonAlignMiddle.OnClick := @ClickAlignMiddle;
  ButtonAlignLeft.OnClick := @ClickAlignLeft;
end;

procedure TStateMain.Resize;
begin
  inherited;
  UpdateSize;
end;

procedure TStateMain.UpdateSize;
var
  WidthInsideScrollArea: Single;
begin
  WidthInsideScrollArea := ScrollView.EffectiveWidthForChildren
    - ScrollView.EffectiveScrollBarWidth;

  // do this first, as it updates LabelMain.EffectiveHeight
  if ButtonWrap.Pressed then
    LabelMain.MaxWidth := WidthInsideScrollArea
  else
    LabelMain.MaxWidth := 0;

  ScrollView.ScrollArea.Width := WidthInsideScrollArea;
  ScrollView.ScrollArea.Height := LabelMain.EffectiveHeight;
end;

procedure TStateMain.ClickHtml(Sender: TObject);
begin
  ButtonHtml.Pressed := not ButtonHtml.Pressed;
  LabelMain.Html := ButtonHtml.Pressed;
  UpdateSize;
end;

procedure TStateMain.ClickWrap(Sender: TObject);
begin
  ButtonWrap.Pressed := not ButtonWrap.Pressed;
  UpdateSize;
end;

procedure TStateMain.ClickAlignLeft(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := true;
  ButtonAlignMiddle.Pressed := false;
  ButtonAlignRight.Pressed := false;
  LabelMain.Alignment := hpLeft;
  LabelMain.Anchor(hpLeft);
end;

procedure TStateMain.ClickAlignMiddle(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := false;
  ButtonAlignMiddle.Pressed := true;
  ButtonAlignRight.Pressed := false;
  LabelMain.Alignment := hpMiddle;
  LabelMain.Anchor(hpMiddle);
end;

procedure TStateMain.ClickAlignRight(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := false;
  ButtonAlignMiddle.Pressed := false;
  ButtonAlignRight.Pressed := true;
  LabelMain.Alignment := hpRight;
  LabelMain.Anchor(hpRight);
end;

end.
