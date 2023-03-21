{
  Copyright 2016-2023 Michalis Kamburelis.

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
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelMain: TCastleLabel;
    ButtonHtml, ButtonWrap, ButtonAlignLeft, ButtonAlignMiddle, ButtonAlignRight: TCastleButton;
    ScrollView: TCastleScrollView;
  private
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
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Resize; override;
  end;

var
  ViewMain: TViewMain;

implementation

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { Assign buttons' OnClick handlers }
  ButtonHtml.OnClick := {$ifdef FPC}@{$endif} ClickHtml;
  ButtonWrap.OnClick := {$ifdef FPC}@{$endif} ClickWrap;
  ButtonAlignRight.OnClick := {$ifdef FPC}@{$endif} ClickAlignRight;
  ButtonAlignMiddle.OnClick := {$ifdef FPC}@{$endif} ClickAlignMiddle;
  ButtonAlignLeft.OnClick := {$ifdef FPC}@{$endif} ClickAlignLeft;
end;

procedure TViewMain.Resize;
begin
  inherited;
  UpdateSize;
end;

procedure TViewMain.UpdateSize;
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

procedure TViewMain.ClickHtml(Sender: TObject);
begin
  ButtonHtml.Pressed := not ButtonHtml.Pressed;
  LabelMain.Html := ButtonHtml.Pressed;
  UpdateSize;
end;

procedure TViewMain.ClickWrap(Sender: TObject);
begin
  ButtonWrap.Pressed := not ButtonWrap.Pressed;
  UpdateSize;
end;

procedure TViewMain.ClickAlignLeft(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := true;
  ButtonAlignMiddle.Pressed := false;
  ButtonAlignRight.Pressed := false;
  LabelMain.Alignment := hpLeft;
  LabelMain.Anchor(hpLeft);
end;

procedure TViewMain.ClickAlignMiddle(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := false;
  ButtonAlignMiddle.Pressed := true;
  ButtonAlignRight.Pressed := false;
  LabelMain.Alignment := hpMiddle;
  LabelMain.Anchor(hpMiddle);
end;

procedure TViewMain.ClickAlignRight(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := false;
  ButtonAlignMiddle.Pressed := false;
  ButtonAlignRight.Pressed := true;
  LabelMain.Alignment := hpRight;
  LabelMain.Anchor(hpRight);
end;

end.
