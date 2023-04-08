{
  Copyright 2023-2023 Michalis Kamburelis, Andrzej Kilijanski

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Edit PhysicsProperties.LayerCollisions. }
unit FormLayerCollisionsPropertyEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, CastleTransform;

type
  TLayerCollisionsPropertyEditorForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    LayerNamesButton: TButton;
    CheckboxesPanel: TPanel;
    RevertButton: TButton;
    VerticalNamesPanel: TPanel;
    HorizontalNamesPanel: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormResize(Sender: TObject);
    procedure LayerNamesButtonClick(Sender: TObject);
    procedure RevertButtonClick(Sender: TObject);
  strict private
    {
       Checkboxes matrix looking like that:
       19,0 18,0 17,0 ...
       19,1 18,1 17,1 ...
       19,2 18,2 17,1 ...
       ...
    }
    Checkboxes: array [TPhysicsLayer, TPhysicsLayer] of TCheckBox;
    { Horizontal panels for horizontal checkboxes arrangement }
    CheckboxesPanels: array [TPhysicsLayer] of TPanel;
    { Horizontal names labels from 0 to 19 }
    HorizontalNames: array [TPhysicsLayer] of TLabel;
    { Pointer to physics properties layer collisions object }
    FLayerCollisions: TCastleLayerCollisions;

    { Method to create panels checkboxes when window is created }
    procedure CreateCheckboxes;
    { Creates horizontal names on the left side of the window }
    procedure CreateHorizontalNames;
    { Creates vertical names on the top of the window }
    procedure CreateVerticalNames;

    { Helper to get name of the layer assuming the parent is TPhysicsProperties }
    function GetLayerName(PhysicsLayer: TPhysicsLayer): String;
    { Helper to get description of the layer assuming the parent is TPhysicsProperties }
    function GetLayerDescription(PhysicsLayer: TPhysicsLayer): String;

    { Updates vertical coordinate of names TLabel's }
    procedure UpdateHorizontalNamesTop;
    { Vertical names are drawn on canvas in repaint event  }
    procedure RepaintVerticalNames(Sender: TObject);
    { Updates horzontal layers names - used after closing layers names dialog }
    procedure UpdateHorizontalNames;
    { Updates layers names in checkboxes }
    procedure UpdateCheckboxHint(Checkbox: TCheckBox; X, Y: TPhysicsLayer);
    { Updates layers names - used after closing layers names dialog }
    procedure UpdateCheckboxesHints;
    { Load layers config to grid }
    procedure Load;
    { Save layers config to physics properties object }
    procedure Save;
  public
    { Initialize the gui and load data from physics properties object }
    procedure Init(const LayerCollisions: TCastleLayerCollisions);
  end;

implementation

{$R *.lfm}

uses FormPhysicsLayerNamesPropertyEditor;

{ TLayerCollisionsPropertyEditorForm ----------------------------------------- }

procedure TLayerCollisionsPropertyEditorForm.FormResize(Sender: TObject);
begin
  UpdateHorizontalNamesTop;
end;

procedure TLayerCollisionsPropertyEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    Save;
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.LayerNamesButtonClick(
  Sender: TObject);
var
  LayerNamesForm: TPhysicsLayerNamesPropertyEditorForm;
  LayerNames: TCastleLayerNames;
begin
  if not (FLayerCollisions.Owner is TPhysicsProperties) then
    Exit;

  LayerNames := TPhysicsProperties(FLayerCollisions.Owner).LayerNames;

  LayerNamesForm := TPhysicsLayerNamesPropertyEditorForm.Create(nil);
  try
    LayerNamesForm.Init(LayerNames);
    if LayerNamesForm.ShowModal = mrOK then
    begin
      // update layers names and
      RepaintVerticalNames(Self);
      UpdateHorizontalNames;
      UpdateCheckboxesHints;
    end;
  finally
    FreeAndNil(LayerNamesForm);
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.RevertButtonClick(Sender: TObject);
begin
  Load;
end;

procedure TLayerCollisionsPropertyEditorForm.CreateCheckboxes;
var
  Panel: TPanel;
  PreviousPanel: TPanel;
  I, J : TPhysicsLayer;

  function AddCheckbox(X, Y: TPhysicsLayer; Panel: TPanel): TCheckBox;
  var
    C: TCheckBox;
  begin
    C := TCheckBox.Create(Panel);
    C.Parent := Panel;
    C.AnchorSide[akTop].Side := asrCenter;
    C.AnchorSide[akTop].Control := Panel;
    C.Caption := '';
    UpdateCheckboxHint(C, X, Y);
    C.ShowHint := true;
    C.ParentShowHint := false;
    C.AutoSize := true;

    if X = High(TPhysicsLayer) then
    begin
      C.AnchorSide[akLeft].Side := asrLeft;
      C.AnchorSide[akLeft].Control := Panel
    end else
    begin
      C.AnchorSide[akLeft].Side := asrRight;
      C.AnchorSide[akLeft].Control := CheckBoxes[X + 1, Y];
    end;
    Result := C;
  end;

begin
  PreviousPanel := nil;
  CheckboxesPanel.AutoSize := true;
  CheckboxesPanel.Caption := '';
  CheckboxesPanel.BevelOuter := bvNone;

  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
  begin
    Panel := TPanel.Create(CheckboxesPanel);
    Panel.Parent := CheckboxesPanel;
    Panel.Caption := '';
    Panel.BevelOuter := bvNone;

    if PreviousPanel = nil then
    begin
      Panel.AnchorSide[akTop].Side  := asrTop;
      Panel.AnchorSide[akTop].Control := CheckboxesPanel
    end else
    begin
      Panel.AnchorSide[akTop].Side  := asrBottom;
      Panel.AnchorSide[akTop].Control := PreviousPanel;
    end;

    for J := High(TPhysicsLayer) downto I do
      Checkboxes[J, I] := AddCheckbox(J, I, Panel);

    Panel.AutoSize := true;
    CheckboxesPanels[I] := Panel;
    PreviousPanel := Panel;
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.CreateHorizontalNames;
var
  ALabel: TLabel;
  I: TPhysicsLayer;
begin
  HorizontalNamesPanel.BevelOuter := bvNone;
  HorizontalNamesPanel.Caption := '';

  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
  begin
    ALabel := TLabel.Create(HorizontalNamesPanel);
    ALabel.Parent := HorizontalNamesPanel;
    ALabel.Caption := IntToStr(I) + ': ' + GetLayerName(I);
    ALabel.Hint := GetLayerDescription(I);
    ALabel.ShowHint := true;
    ALabel.AutoSize := true;

    ALabel.Anchors := [akTop, akRight];
    ALabel.AnchorSide[akTop].Side  := asrTop;
    ALabel.AnchorSide[akTop].Control := nil;
    ALabel.AnchorSide[akRight].Side := asrRight;
    ALabel.AnchorSide[akRight].Control := HorizontalNamesPanel;
    ALabel.BorderSpacing.Right := 2;

    HorizontalNames[I] := ALabel;
  end;
  HorizontalNamesPanel.AutoSize := true;
end;

procedure TLayerCollisionsPropertyEditorForm.CreateVerticalNames;
begin
  VerticalNamesPanel.BevelOuter := bvNone;
  VerticalNamesPanel.Caption := '';
  VerticalNamesPanel.OnPaint := @RepaintVerticalNames;
  RepaintVerticalNames(VerticalNamesPanel);
end;

function TLayerCollisionsPropertyEditorForm.GetLayerName(
  PhysicsLayer: TPhysicsLayer): String;
begin
  Result := '';

  if FLayerCollisions = nil then
    Exit;

  if FLayerCollisions.Owner is TPhysicsProperties then
    Result := TPhysicsProperties(FLayerCollisions.Owner).LayerNames.Names[PhysicsLayer];
end;

function TLayerCollisionsPropertyEditorForm.GetLayerDescription(
  PhysicsLayer: TPhysicsLayer): String;
begin
  Result := '';

  if FLayerCollisions = nil then
    Exit;

  if FLayerCollisions.Owner is TPhysicsProperties then
    Result := TPhysicsProperties(FLayerCollisions.Owner).LayerNames.Descriptions[PhysicsLayer];
end;

procedure TLayerCollisionsPropertyEditorForm.UpdateHorizontalNamesTop;
var
  Margin: Integer;
  I: TPhysicsLayer;
begin
  Margin := (CheckboxesPanels[0].Height - HorizontalNames[0].Height) div 2;
  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
    HorizontalNames[I].Top :=  CheckboxesPanels[I].Top + Margin;
end;

procedure TLayerCollisionsPropertyEditorForm.RepaintVerticalNames(Sender: TObject);
var
  I: TPhysicsLayer;
  X, Y: Integer;
  CheckboxWidth: Integer;
  MaxWidth: Integer;
  Margin: Integer;
  VName: String;
  VNameWidth: Integer;
begin
  VerticalNamesPanel.Canvas.Font.Orientation := 900;
  VerticalNamesPanel.Canvas.Font.Color := clWindowText;
  VerticalNamesPanel.Color := clWindow;

  MaxWidth := 20;
  X := 0;
  CheckboxWidth := Checkboxes[High(TPhysicsLayer), Low(TPhysicsLayer)].Width;
  Y := VerticalNamesPanel.Height;
  Margin := (CheckboxWidth - VerticalNamesPanel.Canvas.TextExtent('I').Height) div 2;

  for I := High(TPhysicsLayer) downto Low(TPhysicsLayer) do
  begin
    VName := IntToStr(I) + ': ' + GetLayerName(I);
    VNameWidth := VerticalNamesPanel.Canvas.TextExtent(VName).Width;
    if VNameWidth > MaxWidth then
       MaxWidth := VNameWidth;
    VerticalNamesPanel.Canvas.TextOut(X + Margin, Y, VName);
    X := X + CheckboxWidth;
  end;

  if MaxWidth <> VerticalNamesPanel.Constraints.MinHeight then
  begin
    VerticalNamesPanel.Constraints.MinHeight := MaxWidth;
    VerticalNamesPanel.Constraints.MaxHeight := MaxWidth;
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.UpdateHorizontalNames;
var
  I: TPhysicsLayer;
begin
  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
  begin
    HorizontalNames[I].Caption := IntToStr(I) + ': ' + GetLayerName(I);
    HorizontalNames[I].Hint := GetLayerDescription(I);
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.UpdateCheckboxHint(
  Checkbox: TCheckBox; X, Y: TPhysicsLayer);
var
  LayerXName, LayerYName: String;
begin
  LayerXName := GetLayerName(X);
  LayerYName := GetLayerName(Y);

  if (LayerXName = '') and (LayerYName = '') then
    Checkbox.Hint := '[' + IntToStr(Y) + ',' + IntToStr(X) + ']'
  else
  begin
    if LayerXName = '' then
      LayerXName := IntToStr(X);
    if LayerYName = '' then
      LayerYName := IntToStr(Y);
    Checkbox.Hint := LayerYName + ' x ' + LayerXName +
      ' [' + IntToStr(Y) + ',' + IntToStr(X) + ']'
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.UpdateCheckboxesHints;
var
  I, J : TPhysicsLayer;
begin
  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
  begin
    for J := High(TPhysicsLayer) downto I do
      UpdateCheckboxHint(Checkboxes[J, I], J, I);
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.Load;
var
  I, J: TPhysicsLayer;
begin
  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
  begin
    for J := High(TPhysicsLayer) downto I do
      Checkboxes[J, I].Checked := FLayerCollisions.Collides[I, J];
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.Save;
var
  I, J: TPhysicsLayer;
begin
  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
  begin
    for J := High(TPhysicsLayer) downto I do
    begin
      FLayerCollisions.Collides[I, J] := Checkboxes[J, I].Checked;
      // 2nd assignment should not be necessary, as Collides setter already sets both directions
      //FLayerCollisions.Collides[J, I] := Checkboxes[J, I].Checked;
      Assert(FLayerCollisions.Collides[J, I] = Checkboxes[J, I].Checked);
    end;
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.Init(const LayerCollisions: TCastleLayerCollisions);
begin
  FLayerCollisions := LayerCollisions;

  CreateCheckboxes;
  CreateHorizontalNames;
  CreateVerticalNames;
  UpdateHorizontalNamesTop;

  Load;
end;

end.

