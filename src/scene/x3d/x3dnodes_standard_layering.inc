{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
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
    {$I auto_generated_node_helpers/x3dnodes_layer.inc}
  end;

  TLayerSetNode = class(TAbstractNode)
    {$I auto_generated_node_helpers/x3dnodes_layerset.inc}
  end;

  TViewportNode = class(TAbstractGroupingNode)
    {$I auto_generated_node_helpers/x3dnodes_viewport.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure RegisterLayeringNodes;
begin
  NodesManager.RegisterNodeClasses([
    TLayerNode,
    TLayerSetNode,
    TViewportNode
  ]);
end;

{$endif read_implementation}
