unit FormPhysicsLayersNamesPropertyEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls,
  StdCtrls, ValEdit, CastleTransform;

type
  TPhysicsLayersNamesPropertyEditorForm = class(TForm)
    ButtonOK: TButton;
    NamesAndDescStringGrid: TStringGrid;
    procedure ButtonOKClick(Sender: TObject);
  strict private
     FLayersNames: TCastleLayersNames;

    procedure Load;
    procedure Save;
  public
    procedure Init(const LayersNames: TCastleLayersNames);
  end;

implementation

{$R *.lfm}

{ TPhysicsLayersNamesPropertyEditorForm -------------------------------------- }

procedure TPhysicsLayersNamesPropertyEditorForm.ButtonOKClick(Sender: TObject);
begin
  Save;
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

procedure TPhysicsLayersNamesPropertyEditorForm.Init(
  const LayersNames: TCastleLayersNames);
begin
  FLayersNames := LayersNames;

  Load;
end;

end.

