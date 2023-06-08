{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Reading of Collada <library_nodes> and <instance_node>. }

{$ifdef read_interface}

type
  TNodeAvailableInLibrary = record
    Element: TDOMElement;
    Handled: boolean;
  end;
  PNodeAvailableInLibrary = ^TNodeAvailableInLibrary;
  TNodeAvailableInLibraryList = {$ifdef FPC}specialize{$endif} TStructList<TNodeAvailableInLibrary>;

{$endif read_interface}

{$ifdef read_implementation_nested}

var
  { Used when reading library_nodes, to handle dependencies between nodes.
    Nodes are specified on library_nodes in a wild order,
    not necessarily good for resolving them.
    So ReadInstanceNode may lookup the node in LibraryNodesSwitch,
    or it may be forced to read the node earlier using
    LibraryNodesAvailable. }
  LibraryNodesAvailable: TNodeAvailableInLibraryList;
  LibraryNodesSwitch: TSwitchNode;

{ Read <instance_node>, adding resulting X3D node into ParentGroup. }
procedure ReadInstanceNode(ParentGroup: TAbstractGroupingNode;
  InstantiatingElement: TDOMElement);
var
  Id: string;
  Node: TAbstractChildNode;
  J: Integer;
begin
  if InstantiatingElement.AttributeString('url', Id) and
     SCharIs(Id, 1, '#') then
  begin
    Delete(Id, 1, 1);
    Node := LibraryNodesSwitch.FdChildren.FindName(Id) as TAbstractChildNode;

    { try to load required node on-demand from LibraryNodesAvailable }
    if Node = nil then
    begin
      if LibraryNodesAvailable <> nil then
      begin
        for J := 0 to LibraryNodesAvailable.Count - 1 do
          if (not LibraryNodesAvailable.L[J].Handled) and
             (LibraryNodesAvailable.L[J].Element.AttributeStringDef('id', '') = Id) then
          begin
            LibraryNodesAvailable.L[J].Handled := true;
            ReadNodeElement(LibraryNodesSwitch, LibraryNodesAvailable.L[J].Element);
            Node := LibraryNodesSwitch.FdChildren.FindName(Id) as TAbstractChildNode;
            break;
          end;
      end;
    end;

    if Node = nil then
      WritelnWarning('Collada', Format('<node> instantiates non-existing element "%s"',
        [Id])) else
      ParentGroup.AddChildren(Node);
  end else
    WritelnWarning('Collada', Format('Element <%s> missing url attribute (that has to start with #)',
      [InstantiatingElement.TagName]));
end;

{ Read <library_nodes>. All obtained X3D nodes are added
  to the LibraryNodes list. }
procedure ReadLibraryNodes(LibraryNodesElement: TDOMElement);
var
  I: TXMLElementIterator;
  NodeAvail: PNodeAvailableInLibrary;
  J: Integer;
begin
  if LibraryNodesSwitch = nil then
  begin
    { Library of nodes is simply a X3D Switch, with each
      node inside as one choice. This way we export to X3D all
      nodes from Collada, even those not chosen as current scene.
      That's good --- it's always nice to keep some data when
      converting. }

    LibraryNodesSwitch := TSwitchNode.Create('collada_library_nodes', BaseUrl);
    ResultModel.AddChildren(LibraryNodesSwitch);
  end;

  LibraryNodesAvailable := TNodeAvailableInLibraryList.Create;
  try
    { calculate LibraryNodesAvailable contents }
    I := LibraryNodesElement.ChildrenIterator('node');
    try
      while I.GetNext do
      begin
        NodeAvail := PNodeAvailableInLibrary(LibraryNodesAvailable.Add);
        NodeAvail^.Handled := false;
        NodeAvail^.Element := I.Current;
      end;
    finally FreeAndNil(I) end;

    { iterate over LibraryNodesAvailable, resolving nodes.
      Note that some nodes may cause resolving of other nodes faster,
      due to ReadInstanceNode requiring some node faster. }
    for J := 0 to LibraryNodesAvailable.Count - 1 do
      if not LibraryNodesAvailable.L[J].Handled then
      begin
        LibraryNodesAvailable.L[J].Handled := true;
        ReadNodeElement(LibraryNodesSwitch, LibraryNodesAvailable.L[J].Element);
      end;
  finally FreeAndNil(LibraryNodesAvailable) end;
end;

{$endif read_implementation_nested}
