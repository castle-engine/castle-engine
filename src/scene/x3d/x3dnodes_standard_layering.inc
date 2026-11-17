{%MainUnit castleinternalnodesunsupported.pas}
{
  Copyright 2008-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TAbstractViewportNode = class;

  { }
  TAbstractLayerNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dlayernode.inc}
  end;

  TAbstractViewportNode = class(TAbstractGroupingNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dviewportnode.inc}
  end;

  TLayerNode = class(TAbstractLayerNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  {$I auto_generated_node_helpers/x3dnodes_layer.inc}
  end;

  TLayerSetNode = class(TAbstractNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  {$I auto_generated_node_helpers/x3dnodes_layerset.inc}
  end;

  TViewportNode = class(TAbstractGroupingNode)
  {$I auto_generated_node_helpers/x3dnodes_viewport.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$I auto_generated_node_helpers/x3dnodes_x3dlayernode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dviewportnode.inc}
{$I auto_generated_node_helpers/x3dnodes_layer.inc}
{$I auto_generated_node_helpers/x3dnodes_layerset.inc}
{$I auto_generated_node_helpers/x3dnodes_viewport.inc}

{ TODO: Below is a super-simple implementation of Layer and LayerSet.
  See https://castle-engine.io/x3d_implementation_layering.php
  for explanation and TODOs. }

{ TLayerSet ------------------------------------------------------------------ }

function TLayerNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited DirectEnumerateActive(Func);

  if Visible then
  begin
    Result := FdChildren.Enumerate(Func);
    if Result <> nil then Exit; // this line is pointless now, but useful in case more calls to Func will be made below
  end;
end;

{ TLayerSetNode -------------------------------------------------------------- }

function TLayerSetNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
var
  I, Index: Integer;
  LayerNode: TX3DNode;
begin
  Result := inherited DirectEnumerateActive(Func);
  if Result <> nil then Exit;

  for I := 0 to FdOrder.Count - 1 do
  begin
    Index := FdOrder.Items[I];
    // Index is 1-based index for FdLayers
    if Between(Index, 1, FdLayers.Count) then
    begin
      LayerNode := FdLayers[Index - 1];
      Result := Func(Self, LayerNode);
      if Result <> nil then Exit;
    end;
  end;
end;

procedure RegisterLayeringNodes;
begin
  NodesManager.RegisterNodeClasses([
    TLayerNode,
    TLayerSetNode,
    TViewportNode
  ]);
end;

{$endif read_implementation}
