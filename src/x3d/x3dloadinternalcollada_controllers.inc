{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  TColladaController = class
    Name: string;
    Source: string;
    BoundShapeMatrix: TMatrix4Single;
    BoundShapeMatrixIdentity: boolean;
  end;

  { List of Collada controllers. Read from <library_controllers>, used by
    <instance_controller>. }
  TColladaControllerList = class(specialize TFPGObjectList<TColladaController>)
  public
    { Find a TColladaController with given Name, @nil if not found. }
    function Find(const Name: string): TColladaController;
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TColladaControllerList.Find(const Name: string): TColladaController;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(Items[I]);

  Result := nil;
end;

{$endif read_implementation}

{$ifdef read_implementation_nested}
var
  Controllers: TColladaControllerList;

{ Read <instance_controller>, adding resulting X3D node into
  ParentGroup. }
procedure ReadInstanceController(ParentGroup: TAbstractX3DGroupingNode;
  InstantiatingElement: TDOMElement);
var
  ControllerId: string;
  Controller: TColladaController;
  Group: TAbstractX3DGroupingNode;
  Geometry: TColladaGeometry;
begin
  if InstantiatingElement.AttributeString('url', ControllerId) and
     SCharIs(ControllerId, 1, '#') then
  begin
    Delete(ControllerId, 1, 1);
    Controller := Controllers.Find(ControllerId);
    if Controller = nil then
    begin
      WritelnWarning('Collada', Format('<node> instantiates non-existing ' +
        '<controller> element "%s"', [ControllerId]));
    end else
    begin
      Geometry := Geometries.Find(Controller.Source);
      if Geometry = nil then
      begin
        WritelnWarning('Collada', Format('<controller> references non-existing ' +
          '<geometry> element "%s"', [Controller.Source]));
      end else
      begin
        if Controller.BoundShapeMatrixIdentity then
        begin
          Group := TGroupNode.Create('', BaseUrl);
        end else
        begin
          Group := TMatrixTransformNode.Create('', BaseUrl);
          TMatrixTransformNode(Group).FdMatrix.Value := Controller.BoundShapeMatrix;
        end;
        ParentGroup.FdChildren.Add(Group);
        AddGeometryInstance(Group, Geometry, InstantiatingElement);
      end;
    end;
  end else
    WritelnWarning('Collada', Format('Element <%s> missing url attribute (that has to start with #)',
      [InstantiatingElement.TagName]));
end;

{ Read <controller> from Collada 1.4.x }
procedure ReadController(ControllerElement: TDOMElement);
var
  Controller: TColladaController;
  Skin, BindShapeMatrix: TDOMElement;
begin
  Controller := TColladaController.Create;
  Controllers.Add(Controller);
  Controller.BoundShapeMatrixIdentity := true;

  if not ControllerElement.AttributeString('id', Controller.Name) then
    Controller.Name := '';

  Skin := ControllerElement.ChildElement('skin', false);
  if Skin <> nil then
  begin
    if Skin.AttributeString('source', Controller.Source) then
    begin
      { this should be true, controller.source is URL }
      if SCharIs(Controller.Source, 1, '#') then
        Delete(Controller.Source, 1, 1);
    end;

    BindShapeMatrix := Skin.ChildElement('bind_shape_matrix', false);
    if BindShapeMatrix <> nil then
    begin
      Controller.BoundShapeMatrixIdentity := false;
      Controller.BoundShapeMatrix := ReadMatrix(BindShapeMatrix);
    end;
  end;
end;

{ Read <library_controllers> from Collada 1.4.x }
procedure ReadLibraryControllers(LibraryElement: TDOMElement);
var
  I: TXMLElementIterator;
begin
  I := LibraryElement.ChildrenIterator('controller');
  try
    while I.GetNext do
      ReadController(I.Current);
      { other I.Current.TagName not supported for now }
  finally FreeAndNil(I) end;
end;

{$endif read_implementation_nested}
