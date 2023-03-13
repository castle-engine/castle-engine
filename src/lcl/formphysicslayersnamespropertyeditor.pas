unit FormPhysicsLayersNamesPropertyEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls,
  StdCtrls, ValEdit, CastleTransform;

type
  TPhysicsLayersNamesPropertyEditorForm = class(TForm)
    Button1: TButton;
    ButtonOK: TButton;
    NamesAndDescStringGrid: TStringGrid;
    procedure ButtonOKClick(Sender: TObject);
    procedure NamesAndDescStringGridResize(Sender: TObject);
  strict private
     FLayersNames: TCastleLayersNames;

    procedure Load;
    procedure Save;
    procedure RecalculateColumnsWidth;
  public
    procedure Init(const LayersNames: TCastleLayersNames);
  end;

implementation

{$R *.lfm}

uses Math;

{ TPhysicsLayersNamesPropertyEditorForm -------------------------------------- }

procedure TPhysicsLayersNamesPropertyEditorForm.ButtonOKClick(Sender: TObject);
begin
  Save;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.NamesAndDescStringGridResize(
  Sender: TObject);
begin
  RecalculateColumnsWidth;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.Load;
var
  I: Integer;
begin
  NamesAndDescStringGrid.RowCount := High(TPhysicsLayer) + 2;

  for I := Low(TPhysicsLayer) + 1 to High(TPhysicsLayer) + 1 do
  begin
    NamesAndDescStringGrid.Cols[0][I] := IntToStr(I - 1);
    NamesAndDescStringGrid.Cols[1][I] := FLayersNames.Names[TPhysicsLayer(I-1)];
    NamesAndDescStringGrid.Cols[2][I] := FLayersNames.Descriptions[TPhysicsLayer(I-1)];
  end;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.Save;
var
  I: Integer;
begin
  for I := Low(TPhysicsLayer) + 1 to High(TPhysicsLayer) + 1 do
  begin
    FLayersNames.Names[TPhysicsLayer(I-1)] := NamesAndDescStringGrid.Cols[1][I];
    FLayersNames.Descriptions[TPhysicsLayer(I-1)] := NamesAndDescStringGrid.Cols[2][I];
  end;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.RecalculateColumnsWidth;
const
  MinColWidth = 30;
  NumberColPercent: Single = 0.07;
  NameColPercent: Single = 0.20;
  DescriptionColPercent: Single = 0.73;
var
  GridClientWidth: Integer;
  LinesSize: Integer;
begin
  LinesSize := (NamesAndDescStringGrid.ColCount + 1) *
    NamesAndDescStringGrid.GridLineWidth;
  NamesAndDescStringGrid.Constraints.MinWidth := MinColWidth * 3 + LinesSize;
  GridClientWidth := NamesAndDescStringGrid.ClientWidth;
  NamesAndDescStringGrid.ColWidths[0] := Max(MinColWidth,
    Round(GridClientWidth * NumberColPercent));
  NamesAndDescStringGrid.ColWidths[1] := Max(MinColWidth,
    Round(GridClientWidth * NameColPercent));
  NamesAndDescStringGrid.ColWidths[2] := Max(MinColWidth,
    Round(GridClientWidth * DescriptionColPercent));
end;

procedure TPhysicsLayersNamesPropertyEditorForm.Init(
  const LayersNames: TCastleLayersNames);
begin
  FLayersNames := LayersNames;

  Load;
  RecalculateColumnsWidth;
end;

end.

