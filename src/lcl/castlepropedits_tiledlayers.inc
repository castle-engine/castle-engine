{%MainUnit castlepropedits.pas}
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

{ TCastleTiledMap.Layers property editor. }

{ TLayersPropertyEditor ------------------------------------------ }

type
  { Property editor to select layers in TCastleTiledMap. }
  TTiledLayersPropertyEditor = class(TSetPropertyEditor)
  strict private
    procedure SetAllValues(const NewValue: TCastleTiledMap.TLayers);
  public
    { Open dialog with Tiled layer names to select. }
    procedure Edit; override;
    { Use TOneLayerPropertyEditor to show nice names for layers when expanded in object inspector. }
    procedure GetProperties(Proc: TGetPropEditProc); override;
    { Makes value in object inspector look nice. }
    function OrdValueToVisualValue(OrdValue: Integer): String; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { Editor for each layer (row) in TTiledLayersPropertyEditor. }
  TOneLayerPropertyEditor = class(TSetElementPropertyEditor)
  protected
    FElementToShow: Integer;
  public
    constructor Create(Parent: TPropertyEditor; AElement: Integer); reintroduce;
    function GetName: ShortString; override;
  end;

{ TTiledLayersPropertyEditor ------------------------------------------------- }

procedure TTiledLayersPropertyEditor.Edit;
var
  D: TTiledLayersDialog;
  Map: TCastleTiledMap;
begin
  D := TTiledLayersDialog.Create(Application);
  try
    Map := GetComponent(0) as TCastleTiledMap;
    D.Caption := 'Edit ' + Map.Name + '.Layers';
    if Map.Data <> nil then
      D.AvailableLayers := Map.Data.Layers;
    D.Layers := Map.Layers;
    D.UpdateSelectionUi;
    if D.ShowModal = mrOK then
      SetAllValues(D.Layers);
  finally FreeAndNil(D) end;
end;

procedure TTiledLayersPropertyEditor.SetAllValues(const NewValue: TCastleTiledMap.TLayers);
var
  I: Integer;
  Map: TCastleTiledMap;
begin
  for I := 0 to PropCount - 1 do
  begin
    Map := GetComponent(I) as TCastleTiledMap;
    Map.Layers := NewValue;
  end;
  Modified;
end;

procedure TTiledLayersPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
begin
  { Make TOneLayerPropertyEditor actually used. }
  with GetTypeData(GetTypeData(GetPropType)^.CompType)^ do
    for I := MinValue to MaxValue do
      Proc(TOneLayerPropertyEditor.Create(Self, I));
end;

function TTiledLayersPropertyEditor.OrdValueToVisualValue(OrdValue: Integer): String;
var
  Layers: TCastleTiledMap.TLayers;
  LayerIndex: TCastleTiledMap.TLayerIndex;
begin
  Layers := TCastleTiledMap.TLayers(OrdValue);
  if Layers = TCastleTiledMap.AllLayers then
    Result := '[all]'
  else
  begin
    Result := '';
    for LayerIndex := Low(LayerIndex) to High(LayerIndex) do
      if LayerIndex in Layers then
        Result := SAppendPart(Result, ',', IntToStr(LayerIndex));
    Result := '[' + Result + ']';
  end;
end;

function TTiledLayersPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := (inherited GetAttributes) + [paDialog];
end;

{ TOneLayerPropertyEditor ---------------------------------------------------- }

constructor TOneLayerPropertyEditor.Create(Parent: TPropertyEditor;
  AElement: Integer);
begin
  inherited Create(Parent, AElement);
  FElementToShow := AElement;
end;

function TOneLayerPropertyEditor.GetName: ShortString;
begin
  { Show single-digit numbers like 1 with a leading space, ' 1',
    so that the layers are nicely sorted 1, 2, ... 9, 10, 11, ... in the object
    inspector. }
  Result := Format('%2d',[FElementToShow]);
end;
