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
    DialogSelection: TExposeTransformSelection; // valid only during EnumTransformNode
    procedure EnumTransformNode(Node: TX3DNode);
  public
    procedure Edit; override;
  end;

procedure TExposeTransformsPropertyEditor.EnumTransformNode(Node: TX3DNode);
var
  TransformName: String;
  Item: TExposeTransformSelectionItem;
begin
  if Node.X3DName <> '' then
  begin
    TransformName := Node.X3DName;
    if DialogSelection.FindName(TransformName) = nil then
    begin
      Item := TExposeTransformSelectionItem.Create;
      Item.Name := TransformName;
      Item.ExistsInScene := true;
      Item.Selected := false; // may be changed to true later
      DialogSelection.Add(Item);
    end;
  end;
end;

procedure TExposeTransformsPropertyEditor.Edit;
var
  D: TExposeTransformsDialog;
  Scene: TCastleSceneCore;
  ValueStrings, SelectionList: TStrings;
  S: String;
  SelItem: TExposeTransformSelectionItem;
begin
  D := TExposeTransformsDialog.Create(Application);
  try
    Scene := GetComponent(0) as TCastleSceneCore;
    D.Caption := 'Edit ' + Scene.Name + '.ExposeTransforms';

    DialogSelection := D.Selection;
    DialogSelection.Clear;

    // add to D.Selection all possible transforms from the scene
    if Scene.RootNode <> nil then
      Scene.RootNode.EnumerateNodes(TTransformNode, @EnumTransformNode, false);

    // add/update in D.Selection all currently selected transforms
    ValueStrings := TStrings(GetObjectValue);
    for S in ValueStrings do
      if S <> '' then // ignore empty strings, just like TCastleSceneCore.ExposeTransformsChange does
      begin
        SelItem := D.Selection.FindName(S);
        if SelItem = nil then
        begin
          SelItem := TExposeTransformSelectionItem.Create;
          SelItem.Name := S;
          SelItem.ExistsInScene := false;
          DialogSelection.Add(SelItem);
        end;
        SelItem.Selected := true
      end;

    D.UpdateSelectionUi;
    if D.ShowModal = mrOK then
    begin
      SelectionList := DialogSelection.ToList;
      try
        SetPtrValue(SelectionList);
      finally FreeAndNil(SelectionList) end;
    end;
    Modified;
  finally FreeAndNil(D) end;
end;


