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
    procedure NamesAndDescStringGridPrepareCanvas(Sender: TObject; ACol,
      ARow: Integer; AState: TGridDrawState);
    procedure NamesAndDescStringGridResize(Sender: TObject);
  strict private
     FLayersNames: TCastleLayersNames;

    procedure Load;
    procedure Save;
    procedure RecalculateColumnsWidth;
    procedure RecalculateRowWidth(const RowIndex: Integer);
    procedure RecalculateRowsWidth;
  public
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

procedure TPhysicsLayersNamesPropertyEditorForm.ButtonOKClick(Sender: TObject);
begin
  Save;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.NamesAndDescStringGridPrepareCanvas
  (Sender: TObject; ACol, ARow: Integer; AState: TGridDrawState);
var
  ATextStyle: TTextStyle;
begin
  // based on https://forum.lazarus.freepascal.org/index.php?topic=25662.0
  if ACol = 2 then
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

  Writeln('Desc Width: ' + IntToStr(NamesAndDescStringGrid.ColWidths[DescriptionColIndex]));

  RecalculateRowsWidth;
end;

procedure TPhysicsLayersNamesPropertyEditorForm.RecalculateRowWidth(const RowIndex: Integer);
var
  ARect: TRect;
begin
  ARect.Top := 0;
  ARect.Left := 0;
  ARect.Right := NamesAndDescStringGrid.RowHeights[RowIndex];
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

procedure TPhysicsLayersNamesPropertyEditorForm.Init(
  const LayersNames: TCastleLayersNames);
begin
  FLayersNames := LayersNames;

  Load;
  RecalculateColumnsWidth;
end;

end.

