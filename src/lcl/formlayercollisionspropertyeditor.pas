unit formlayercollisionspropertyeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CastleTransform;

type
  TLayerCollisionsPropertyEditorForm = class(TForm)
    CheckboxesPanel: TPanel;
    HorizontalNamesPanel: TPanel;
    Label1: TLabel;
    StaticText1: TStaticText;
    VerticalNamesPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
    { Vertical names labels from 0  to 19 }
    VerticalNames: array [TPhysicsLayer] of TLabel;
    { Horizontal names labels from 19 to 0 }
    HorizontalNames: array [TPhysicsLayer] of TLabel;

    procedure CreateCheckboxes;
    procedure CreateVerticalNames;
    procedure CreateHorizontalNames;

    procedure UpdateVerticalNamesTop;
    procedure RepaintHorizontalNames(Sender: TObject);
  public

  end;

//var
//  LayerCollisionsPropertyEditorForm: TLayerCollisionsPropertyEditorForm;

implementation

{$R *.lfm}

{ TLayerCollisionsPropertyEditorForm ----------------------------------------- }

procedure TLayerCollisionsPropertyEditorForm.FormCreate(Sender: TObject);
begin
  CreateCheckboxes;
  CreateVerticalNames;
  CreateHorizontalNames;
  UpdateVerticalNamesTop;
end;


procedure TLayerCollisionsPropertyEditorForm.FormResize(Sender: TObject);
begin
  UpdateVerticalNamesTop;
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
    C.Hint := '[' + IntToStr(X) + ',' + IntToStr(Y) + ']';
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
    begin
      Checkboxes[J, I] := AddCheckbox(J, I, Panel);

    end;
    Panel.AutoSize := true;
    CheckboxesPanels[I] := Panel;
    PreviousPanel := Panel;
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.CreateVerticalNames;
var
  ALabel: TLabel;
  I: TPhysicsLayer;
begin
  VerticalNamesPanel.BevelOuter := bvNone;
  VerticalNamesPanel.Caption := '';

  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
  begin
    ALabel := TLabel.Create(VerticalNamesPanel);
    ALabel.Parent := VerticalNamesPanel;
    ALabel.Caption := IntToStr(I) + ': ';
    ALabel.AutoSize := true;

    ALabel.Anchors := [akTop, akRight];
    ALabel.AnchorSide[akTop].Side  := asrTop;
    ALabel.AnchorSide[akTop].Control := nil;
    ALabel.AnchorSide[akRight].Side := asrRight;
    ALabel.AnchorSide[akRight].Control := VerticalNamesPanel;

    VerticalNames[I] := ALabel;
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.CreateHorizontalNames;
begin
  HorizontalNamesPanel.BevelOuter := bvNone;
  HorizontalNamesPanel.Caption := '';
  HorizontalNamesPanel.OnPaint := @RepaintHorizontalNames;
  RepaintHorizontalNames(HorizontalNamesPanel);
end;

procedure TLayerCollisionsPropertyEditorForm.UpdateVerticalNamesTop;
var
  Margin: Integer;
  I: TPhysicsLayer;
begin
  Margin := (CheckboxesPanels[0].Height - VerticalNames[0].Height) div 2;
  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
    VerticalNames[I].Top :=  CheckboxesPanels[I].Top + Margin;
end;

procedure TLayerCollisionsPropertyEditorForm.RepaintHorizontalNames(Sender: TObject);
var
  I: TPhysicsLayer;
  X, Y: Integer;
  CheckboxWidth: Integer;
  MaxWidth: Integer;
  HName: String;
  HNameWidth: Integer;
begin
  HorizontalNamesPanel.Canvas.Font.Orientation := 900;
  HorizontalNamesPanel.Canvas.Font.Color := clWindowText;
  HorizontalNamesPanel.Color := clWindow;

  MaxWidth := 100;
  X := 0;
  CheckboxWidth := Checkboxes[High(TPhysicsLayer), Low(TPhysicsLayer)].Width;
  Y := HorizontalNamesPanel.Height;
  for I := High(TPhysicsLayer) downto Low(TPhysicsLayer) do
  begin
    HName := IntToStr(I) + ': ';
    HNameWidth := HorizontalNamesPanel.Canvas.TextExtent(HName).Width;
    if HNameWidth > MaxWidth then
       MaxWidth := HNameWidth;
    HorizontalNamesPanel.Canvas.TextOut(X, Y, HName);
    X := X + CheckboxWidth;
  end;

  if MaxWidth <> HorizontalNamesPanel.Constraints.MinHeight then
    HorizontalNamesPanel.Constraints.MinHeight := MaxWidth;
end;

end.

