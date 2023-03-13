unit formlayercollisionspropertyeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CastleTransform;

type
  TLayerCollisionsPropertyEditorForm = class(TForm)
    LayersNamesButton: TButton;
    RevertButton: TButton;
    CancelButton: TButton;
    OkButton: TButton;
    CheckboxesPanel: TPanel;
    VerticalNamesPanel: TPanel;
    HorizontalNamesPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LayersNamesButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
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
    CheckboxesPanels: array [TPhysicsLayer] of TPanel;
    { Horizontal names labels from 0  to 19 }
    HorizontalNames: array [TPhysicsLayer] of TLabel;

    FLayerCollisions: TCastleLayerCollisions;

    procedure CreateCheckboxes;
    procedure CreateHorizontalNames;
    procedure CreateVerticalNames;

    function GetLayerName(PhysicsLayer: TPhysicsLayer): String;

    procedure UpdateHorizontalNamesTop;
    procedure RepaintVerticalNames(Sender: TObject);
    procedure UpdateHorizontalNames;
    procedure UpdateCheckboxHint(Checkbox: TCheckBox; X, Y: TPhysicsLayer);
    procedure UpdateCheckboxesHints;
    procedure Load;
    procedure Save;
  public
    procedure Init(const LayerCollisions: TCastleLayerCollisions);
  end;

//var
//  LayerCollisionsPropertyEditorForm: TLayerCollisionsPropertyEditorForm;

implementation

{$R *.lfm}

uses FormPhysicsLayersNamesPropertyEditor;

{ TLayerCollisionsPropertyEditorForm ----------------------------------------- }

procedure TLayerCollisionsPropertyEditorForm.FormCreate(Sender: TObject);
begin
end;


procedure TLayerCollisionsPropertyEditorForm.FormResize(Sender: TObject);
begin
  UpdateHorizontalNamesTop;
end;

procedure TLayerCollisionsPropertyEditorForm.LayersNamesButtonClick(
  Sender: TObject);
var
  LayersNamesForm: TPhysicsLayersNamesPropertyEditorForm;
  LayerNames: TCastleLayersNames;
begin
  if not (FLayerCollisions.Owner is TPhysicsProperties) then
    Exit;

  LayerNames := TPhysicsProperties(FLayerCollisions.Owner).LayerNames;

  LayersNamesForm := TPhysicsLayersNamesPropertyEditorForm.Create(nil);
  try
    LayersNamesForm.Init(LayerNames);
    if LayersNamesForm.ShowModal = mrOK then
    begin
      // update layers names and
      RepaintVerticalNames(Self);
      UpdateHorizontalNames;
      UpdateCheckboxesHints;
    end;
  finally
    FreeAndNil(LayersNamesForm);
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.OkButtonClick(Sender: TObject);
begin
  Save;
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
    ALabel.AutoSize := true;

    ALabel.Anchors := [akTop, akRight];
    ALabel.AnchorSide[akTop].Side  := asrTop;
    ALabel.AnchorSide[akTop].Control := nil;
    ALabel.AnchorSide[akRight].Side := asrRight;
    ALabel.AnchorSide[akRight].Control := HorizontalNamesPanel;

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
    Checkbox.Hint := '[' + IntToStr(X) + ',' + IntToStr(Y) + ']'
  else
  begin
    if LayerXName = '' then
      LayerXName := IntToStr(X);
    if LayerYName = '' then
      LayerYName := IntToStr(Y);
    Checkbox.Hint := LayerXName + ' x ' + LayerYName +
      ' [' + IntToStr(X) + ',' + IntToStr(Y) + ']'
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
      FLayerCollisions.Collides[J, I] := Checkboxes[J, I].Checked;
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

