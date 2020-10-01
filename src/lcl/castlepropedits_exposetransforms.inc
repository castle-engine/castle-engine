{%MainUnit castlepropedits.pas}
{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastleSceneCore.ExposeTransforms property editor. }

{ TExposeTransformsPropertyEditor ------------------------------------------ }

type
  { Property editor to select transforms in TCastleSceneCore. }
  TExposeTransformsPropertyEditor = class(TStringsPropertyEditor)
  strict private
    PossibleTransforms: TStringList; // valid only during EnumTransformNode
    procedure EnumTransformNode(Node: TX3DNode);
  public
    procedure Edit; override;
  end;

procedure TExposeTransformsPropertyEditor.EnumTransformNode(Node: TX3DNode);
begin
  if Node.X3DName <> '' then
    PossibleTransforms.Add(Node.X3DName);
end;

procedure TExposeTransformsPropertyEditor.Edit;
var
  Scene: TCastleSceneCore;
begin
  PossibleTransforms := TStringList.Create;
  try
    PossibleTransforms.Sorted := true; // to honor Duplicates
    PossibleTransforms.Duplicates := dupIgnore;
    Scene := GetComponent(0) as TCastleSceneCore;
    if Scene.RootNode <> nil then
      Scene.RootNode.EnumerateNodes(TTransformNode, @EnumTransformNode, false);

    ShowMessage(Format('Found %d possible transformation (bone) names in file: ' + NL + NL + '%s', [
      PossibleTransforms.Count,
      PossibleTransforms.Text
    ]));
  finally FreeAndNil(PossibleTransforms) end;

  inherited;
end;


