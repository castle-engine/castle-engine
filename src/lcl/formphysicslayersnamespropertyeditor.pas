unit FormPhysicsLayersNamesPropertyEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls,
  StdCtrls, ValEdit, CastleTransform, Types;

type
  TPhysicsLayersNamesPropertyEditorForm = class(TForm)
    CancelButton: TButton;
    OkButton: TButton;
    NamesAndDescStringGrid: TStringGrid;
    procedure OkButtonClick(Sender: TObject);
    procedure NamesAndDescStringGridPrepareCanvas(Sender: TObject; ACol,
      ARow: Integer; AState: TGridDrawState);
    procedure NamesAndDescStringGridResize(Sender: TObject);
    procedure NamesAndDescStringGridSelectEditor(Sender: TObject; ACol,
      ARow: Integer; var Editor: TWinControl);
  strict private
    { Pointer to layers names in physics properties }
    FLayersNames: TCastleLayersNames;
    { Pointer to custom editor }
    FEditor: TMemo;
    { Column edited by custom editor, used by EditorKeyDown() }
    FEditorCol: Integer;
    { Row edited by custom editor, used by EditorKeyDown() }
    FEditorRow: Integer;
    { Load layers names to grid }
    procedure Load;
    { Save layers names from grid to physics properties }
    procedure Save;
    { Recalculate columns widths to fit grid control }
    procedure RecalculateColumnsWidth;
    { Recalculate row height to fit text }
    procedure RecalculateRowWidth(const RowIndex: Integer);
    { Recalculate all rows height }
    procedure RecalculateRowsWidth;

    { OnKeyDown event for custom editor }
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    { Initialize window }
    procedure Init(const LayersNames: TCastleLayersNames);
  end;

implementation

{$R *.lfm}

uses Math, LCLIntf, LCLType;

const
  NumberColIndex = 0;
  NameColIndex = 1;
  DescriptionColIndex = 2;

{ TPhysicsLayersNamesPropertyEditorForm -------------------------------------- }

procedure TPhysicsLayersNamesPropertyEditorForm.OkButtonClick(Sender: TObject);
begin
  Save;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.NamesAndDescStringGridPrepareCanvas
  (Sender: TObject; ACol, ARow: Integer; AState: TGridDrawState);
var
  ATextStyle: TTextStyle;
begin
  { based on https://forum.lazarus.freepascal.org/index.php?topic=25662.0 }
  if ACol = DescriptionColIndex then
  begin
    ATextStyle := TStringGrid(Sender).Canvas.TextStyle;
    ATextStyle.SingleLine := False;
    ATextStyle.Wordbreak := True;
    TStringGrid(Sender).Canvas.TextStyle := ATextStyle;
  end;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.NamesAndDescStringGridResize(
  Sender: TObject);
begin
  RecalculateColumnsWidth;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.NamesAndDescStringGridSelectEditor
  (Sender: TObject; ACol, ARow: Integer; var Editor: TWinControl);
var
  EditorRect: TRect;
begin
  if ACol = DescriptionColIndex then
  begin
    if FEditor = nil then
      FEditor := TMemo.Create(NamesAndDescStringGrid);
    Editor := FEditor;
    EditorRect := NamesAndDescStringGrid.CellRect(ACol, ARow);
    EditorRect.Right := EditorRect.Right - 1;
    EditorRect.Bottom := EditorRect.Bottom - 1;
    FEditor.BoundsRect := EditorRect;
    FEditorCol := ACol;
    FEditorRow := ARow;
    FEditor.WantReturns := false;
    FEditor.Text := NamesAndDescStringGrid.Cols[ACol][ARow];
    FEditor.SelLength := 0;
    FEditor.SelStart := Length(FEditor.Text);
    FEditor.BorderStyle := bsNone;
    FEditor.WordWrap := true;
    FEditor.OnKeyDown := @EditorKeyDown;
    FEditor.Show;
  end;
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
  NamesAndDescStringGrid.ColWidths[NumberColIndex] := Max(MinColWidth,
    Round(GridClientWidth * NumberColPercent));
  NamesAndDescStringGrid.ColWidths[NameColIndex] := Max(MinColWidth,
    Round(GridClientWidth * NameColPercent));
  NamesAndDescStringGrid.ColWidths[DescriptionColIndex] := Max(MinColWidth,
    Round(GridClientWidth * DescriptionColPercent));

  RecalculateRowsWidth;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.RecalculateRowWidth(const RowIndex: Integer);
var
  ARect: TRect;
begin
  ARect.Top := 0;
  ARect.Left := 0;
  ARect.Right := NamesAndDescStringGrid.ColWidths[DescriptionColIndex];
  ARect.Bottom := 0;

  DrawText(NamesAndDescStringGrid.Canvas.Handle,
    PChar(NamesAndDescStringGrid.Cols[DescriptionColIndex][RowIndex]),
    Length(NamesAndDescStringGrid.Cols[DescriptionColIndex][RowIndex]),
    ARect,  DT_CALCRECT or DT_WORDBREAK);

  NamesAndDescStringGrid.RowHeights[RowIndex] := Max(
    NamesAndDescStringGrid.DefaultRowHeight, ARect.Bottom);
end;

procedure TPhysicsLayersNamesPropertyEditorForm.RecalculateRowsWidth;
var
  I: Integer;
begin
  for I := 1 to NamesAndDescStringGrid.RowCount -1 do
    RecalculateRowWidth(I);
end;

procedure TPhysicsLayersNamesPropertyEditorForm.EditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = 13) and (Shift = []) then
  begin
    NamesAndDescStringGrid.Cols[FEditorCol][FEditorRow] := FEditor.Text;
    FEditor.Hide;
    NamesAndDescStringGrid.SetFocus;
  end else
  if (Key = 27) and (Shift = []) then
  begin
    FEditor.Hide;
    NamesAndDescStringGrid.SetFocus;
  end;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.Init(
  const LayersNames: TCastleLayersNames);
begin
  FLayersNames := LayersNames;

  Load;
  RecalculateColumnsWidth;
end;

end.

