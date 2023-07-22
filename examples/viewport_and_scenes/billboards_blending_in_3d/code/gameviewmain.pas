{
  Copyright 2023-2023 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleViewport,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ViewportRegular, ViewportTop: TCastleViewport;
    CameraTop: TCastleCamera;
    ButtonVerticalBillboards, ButtonFreeBillboards: TCastleButton;
    CheckboxBatching: TCastleCheckbox;
    LabelViewportRegularStats, LabelViewpotTopStats: TCastleLabel;
  private
    procedure ClickVerticalBillboards(Sender: TObject);
    procedure ClickFreeBillboards(Sender: TObject);
    procedure ChangeBatching(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleBehaviors, CastleRenderOptions, CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { Share items between 2 viewports,
    see https://castle-engine.io/multiple_viewports_to_display_one_world }
  ViewportTop.Items := ViewportRegular.Items;
  ViewportTop.Camera := CameraTop;

  ButtonVerticalBillboards.OnClick := {$ifdef FPC}@{$endif} ClickVerticalBillboards;
  ButtonFreeBillboards.OnClick := {$ifdef FPC}@{$endif} ClickFreeBillboards;
  CheckboxBatching.OnChange := {$ifdef FPC}@{$endif} ChangeBatching;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  { A small trick to make TCastleViewport.Statistics.ToString multiline,
    that assumes that it uses ',' to separate important pieces. }
  function MakeViewportStatsMultiline(const S: String): String;
  begin
    Result := StringReplace(S, ', ', ',' + NL, [rfReplaceAll]);
  end;

begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  LabelViewportRegularStats.Caption := MakeViewportStatsMultiline(
    ViewportRegular.Statistics.ToString);
  LabelViewpotTopStats.Caption := MakeViewportStatsMultiline(
    ViewportTop.Statistics.ToString);
end;

procedure TViewMain.ClickVerticalBillboards(Sender: TObject);
var
  BillboardList: TCastleBehaviorList;
  Billboard: TCastleBillboard;
  B: TCastleBehavior;
begin
  BillboardList := ViewportRegular.Items.FindAllBehaviors(TCastleBillboard);
  try
    for B in BillboardList do
    begin
      Billboard := B as TCastleBillboard;
      Billboard.AxisOfRotation := Vector3(0, 1, 0);
    end;
  finally FreeAndNil(BillboardList) end;
  ViewportRegular.BlendingSort := sort3DVerticalBillboards;
  ViewportTop.BlendingSort := sort3DVerticalBillboards;
  ButtonVerticalBillboards.Pressed := true;
  ButtonFreeBillboards.Pressed := false;
end;

procedure TViewMain.ClickFreeBillboards(Sender: TObject);
var
  BillboardList: TCastleBehaviorList;
  Billboard: TCastleBillboard;
  B: TCastleBehavior;
begin
  BillboardList := ViewportRegular.Items.FindAllBehaviors(TCastleBillboard);
  try
    for B in BillboardList do
    begin
      Billboard := B as TCastleBillboard;
      Billboard.AxisOfRotation := TVector3.Zero
    end;
  finally FreeAndNil(BillboardList) end;
  ViewportRegular.BlendingSort := sort3D;
  ViewportTop.BlendingSort := sort3D;
  ButtonVerticalBillboards.Pressed := false;
  ButtonFreeBillboards.Pressed := true;
end;

procedure TViewMain.ChangeBatching(Sender: TObject);
begin
  ViewportRegular.DynamicBatching := CheckboxBatching.Checked;
  ViewportTop.DynamicBatching := CheckboxBatching.Checked;
end;

end.
