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

{$ifdef read_interface_nested}

procedure ReadNodeElement(ParentGroup: TAbstractX3DGroupingNode;
  NodeElement: TDOMElement); forward;

{$endif read_interface_nested}

{$ifdef read_implementation_nested}

{ Read <node> element, add it to ParentGroup. }
procedure ReadNodeElement(ParentGroup: TAbstractX3DGroupingNode;
  NodeElement: TDOMElement);
var
  { This is either TTransformNode or TMatrixTransformNode. }
  NodeTransform: TAbstractX3DGroupingNode;

  { Create new Transform node, place it as a child of current Transform node
    and switch current Transform node to the new one.

    The idea is that each
    Collada transformation creates new nested X3D Transform node
    (since Collada transformations may represent any transformation,
    not necessarily representable by a single X3D Transform node).

    Returns NodeTransform, typecasted to TTransformNode, for your comfort. }
  function NestedTransform: TTransformNode;
  var
    NewNodeTransform: TTransformNode;
  begin
    NewNodeTransform := TTransformNode.Create('', BaseUrl);
    NodeTransform.FdChildren.Add(NewNodeTransform);

    NodeTransform := NewNodeTransform;
    Result := NewNodeTransform;
  end;

  function NestedMatrixTransform: TMatrixTransformNode;
  var
    NewNodeTransform: TMatrixTransformNode;
  begin
    NewNodeTransform := TMatrixTransformNode.Create('', BaseUrl);
    NodeTransform.FdChildren.Add(NewNodeTransform);

    NodeTransform := NewNodeTransform;
    Result := NewNodeTransform;
  end;

var
  I: TXMLElementIterator;
  NodeId: string;
  V3: TVector3Single;
  V4: TVector4Single;
begin
  if not NodeElement.AttributeString('id', NodeId) then
    NodeId := '';

  NodeTransform := TTransformNode.Create(NodeId, BaseUrl);
  { Add NodeTransform to ParentGroup (which may be LibraryNodes) first.
    This means that if instance_node will create a cycle, we will recreate
    this cycle in VRML/X3D graph, without hanging the importer code. }
  ParentGroup.FdChildren.Add(NodeTransform);

  { First iterate to gather all transformations.

    For Collada 1.4, this shouldn't be needed (spec says that
    transforms must be before all instantiations).

    But Collada 1.3.1 specification doesn't say anything about the order.
    And e.g. Blender Collada 1.3 exporter in fact generates files
    with <instantiate> first, then <matrix>, and yes: it expects matrix
    should affect instantiated object. So the bottom line is that for
    Collada 1.3, I must first gather all transforms, then do instantiations. }

  I := NodeElement.ChildrenIterator;
  try
    while I.GetNext do
    begin
      if I.Current.TagName = 'matrix' then
      begin
        NestedMatrixTransform.FdMatrix.Value := ReadMatrix(I.Current);
      end else
      if I.Current.TagName = 'rotate' then
      begin
        V4 := Vector4SingleFromStr(I.Current.TextData);
        if V4[3] <> 0.0 then
        begin
          NestedTransform.FdRotation.ValueDeg := V4;
        end;
      end else
      if I.Current.TagName = 'scale' then
      begin
        V3 := Vector3SingleFromStr(I.Current.TextData);
        if not VectorsPerfectlyEqual(V3, Vector3Single(1, 1, 1)) then
        begin
          NestedTransform.FdScale.Value := V3;
        end;
      end else
      if I.Current.TagName = 'lookat' then
      begin
        NestedMatrixTransform.FdMatrix.Value := ReadLookAt(I.Current);
      end else
      if I.Current.TagName = 'skew' then
      begin
        { TODO }
      end else
      if I.Current.TagName = 'translate' then
      begin
        V3 := Vector3SingleFromStr(I.Current.TextData);
        if not VectorsPerfectlyEqual(V3, ZeroVector3Single) then
        begin
          NestedTransform.FdTranslation.Value := V3;
        end;
      end;
    end;
  finally FreeAndNil(I); end;

  { Now iterate to read instantiations and recursive nodes. }

  I := NodeElement.ChildrenIterator;
  try
    while I.GetNext do
    begin
      if (I.Current.TagName = 'instance') or
         (I.Current.TagName = 'instance_geometry') then
        ReadInstanceGeometry(NodeTransform, I.Current) else
      if I.Current.TagName = 'instance_controller' then
        ReadInstanceController(NodeTransform, I.Current) else
      if I.Current.TagName = 'instance_camera' then
        ReadInstance(NodeTransform, I.Current, Cameras) else
      if I.Current.TagName = 'instance_light' then
        ReadInstance(NodeTransform, I.Current, Lights) else
      if I.Current.TagName = 'instance_node' then
        ReadInstanceNode(NodeTransform, I.Current) else
      if I.Current.TagName = 'node' then
        ReadNodeElement(NodeTransform, I.Current);
    end;
  finally FreeAndNil(I) end;
end;

{ Read <node> and <instance_node> sequence within given SceneElement,
  adding nodes to Group.
  This is used to handle <visual_scene> for Collada 1.4.x
  and <scene> for Collada 1.3.x. }
procedure ReadNodesSequence(Group: TAbstractX3DGroupingNode;
  SceneElement: TDOMElement);
var
  I: TXMLElementIterator;
begin
  I := SceneElement.ChildrenIterator;
  try
    while I.GetNext do
      if I.Current.TagName = 'node' then
        ReadNodeElement(Group, I.Current)

        // TODO: untested, search collada tests for instance_node
      //   ; else
      // if I.Current.TagName = 'instance_node' then
      //   ReadInstanceNode(Group, I.Current);
  finally FreeAndNil(I) end;
end;

{$endif read_implementation_nested}
