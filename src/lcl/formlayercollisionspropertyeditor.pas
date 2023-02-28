unit formlayercollisionspropertyeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CastleTransform;

type
  TLayerCollisionsPropertyEditorForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckboxesPanel: TPanel;
    GroupBox1: TGroupBox;
    HorizontalNamesPanel: TPanel;
    Label1: TLabel;
    VerticalNamesPanel: TPanel;
    procedure FormCreate(Sender: TObject);
  strict private
    {
       Checkboxes matrix looking like that:
       19,0 18,0 17,0 ...
       19,1 18,1 17,1 ...
       19,2 18,2 17,1 ...
       ...
    }
    Checkboxes: array [TPhysicsLayer, TPhysicsLayer] of TCheckBox;
    { Vertical names labels from 0  to 19 }
    VerticalNames: array [TPhysicsLayer] of TLabel;
    { Horizontal names labels from 19 to 0 }
    HorizontalNames: array [TPhysicsLayer] of TLabel;

    procedure CreateCheckboxes;
    procedure CreateVerticalNames;
    procedure CreateHorizontalNames;
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

    ALabel.Anchors := [akTop, akRight];
    ALabel.AnchorSide[akTop].Side  := asrCenter;
    ALabel.AnchorSide[akTop].Control := Checkboxes[High(TPhysicsLayer), I];
    ALabel.AnchorSide[akRight].Side := asrRight;
    ALabel.AnchorSide[akRight].Control := VerticalNamesPanel;

    ALabel.Caption := IntToStr(I);
    ALabel.AutoSize := true;

    VerticalNames[I] := ALabel;
  end;
end;

procedure TLayerCollisionsPropertyEditorForm.CreateHorizontalNames;
begin
  HorizontalNamesPanel.BevelOuter := bvNone;
  HorizontalNamesPanel.Caption := '';
end;

end.

