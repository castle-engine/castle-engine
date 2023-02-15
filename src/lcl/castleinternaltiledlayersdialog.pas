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

{ Dialog for selecting Tiled layers to show, used by CastlePropEdits. }
unit CastleInternalTiledLayersDialog;

{$I castleconf.inc}

interface

uses
  Generics.Collections, Contnrs,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls, CastleTiledMap;

type
  TTiledLayersDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckLayers: TScrollBox;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure SelectAllButtonClick(Sender: TObject);
  private
    Checkboxes: TObjectList;
  public
    AvailableLayers: TCastleTiledMapData.TLayerList;
    Layers: TCastleTiledMap.TLayers;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSelectionUi;
  end;

implementation

uses Math,
  CastleLclUtils;

{$R *.lfm}

{ TTiledLayersDialog ---------------------------------------------------- }

constructor TTiledLayersDialog.Create(AOwner: TComponent);
begin
  inherited;
  Checkboxes := TObjectList.Create(false);
end;

destructor TTiledLayersDialog.Destroy;
begin
  FreeAndNil(Checkboxes);
  inherited;
end;

procedure TTiledLayersDialog.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  I: Integer;
begin
  if ModalResult = mrOK then
  begin
    { calculate Layers from Checkboxes }
    Layers := [];
    for I := 0 to Min(Checkboxes.Count - 1, High(TCastleTiledMap.TLayerIndex)) do
    begin
      if TCheckBox(Checkboxes[I]).Checked then
        Include(Layers, I);
    end;
    { Set to true remaining layers, so that future added layers are visible.
      This also means that "all layers" state is reliably expressed as TCastleTiledMap.AllLayers,
      and is not saved to JSON as it is the default value,
      and is displayed as simple "[all]" by TTiledLayersPropertyEditor.OrdValueToVisualValue. }
    for I := Checkboxes.Count to High(TCastleTiledMap.TLayerIndex) do
      Include(Layers, I);
  end;
end;

procedure TTiledLayersDialog.SelectAllButtonClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Checkboxes.Count - 1 do
    TCheckBox(Checkboxes[I]).Checked := true;
end;

procedure TTiledLayersDialog.UpdateSelectionUi;
var
  S: String;
  Checkbox: TCheckBox;
  I: Integer;
begin
  Checkboxes.Clear;

  if AvailableLayers <> nil then // AvailableLayers may be nil when no map loaded
  begin
    for I := 0 to AvailableLayers.Count - 1 do
    begin
      S := Format('%d: %s', [
        I,
        AvailableLayers[I].Name
      ]);
      if not AvailableLayers[I].Visible then
        S := S + ' (always hidden; toggle layer visibility in Tiled to show it)';

      if I > High(TCastleTiledMap.TLayerIndex) then
        S := S + Format(' (index > %d, always visible)', [High(TCastleTiledMap.TLayerIndex)]);

      Checkbox := TCheckBox.Create(Self);
      Checkbox.Caption := S;
      Checkbox.Checked :=
        (I > High(TCastleTiledMap.TLayerIndex)) or
        (I in Layers);
      Checkbox.Parent := CheckLayers;
      Checkboxes.Add(Checkbox);
    end;
  end;
end;

end.
