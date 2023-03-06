{%MainUnit castlepropedits.pas}
{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijanski

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastle property editor. }

{ TLayerCollisionsPropertyEditor ------------------------------------------ }

type

  { Property editor for layer collisions }
  TLayerCollisionsPropertyEditor = class(TSubPropertiesEditor)
  strict private
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  public
  end;


function TLayerCollisionsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paRevertable];
end;

procedure TLayerCollisionsPropertyEditor.Edit;
var
  LayerCollisionsForm: TLayerCollisionsPropertyEditorForm;
  LayerCollisions: TCastleLayerCollisions;
begin
  LayerCollisions := GetObjectValue as TCastleLayerCollisions;
  if LayerCollisions = nil then
    Exit;

  LayerCollisionsForm := TLayerCollisionsPropertyEditorForm.Create(nil);
  try
    LayerCollisionsForm.Init(LayerCollisions);
    if LayerCollisionsForm.ShowModal = mrOK then
      Modified;
  finally
    FreeAndNil(LayerCollisionsForm);
  end;
end;


