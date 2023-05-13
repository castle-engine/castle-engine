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
  { }
  TAbstractLayoutNode = class(TAbstractChildNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dlayoutnode.inc}
  end;

  TLayoutNode = class(TAbstractLayoutNode)
    {$I auto_generated_node_helpers/x3dnodes_layout.inc}
  end;

  TLayoutGroupNode = class(TAbstractGroupingNode)
    {$I auto_generated_node_helpers/x3dnodes_layoutgroup.inc}
  end;

  TLayoutLayerNode = class(TAbstractLayerNode)
    {$I auto_generated_node_helpers/x3dnodes_layoutlayer.inc}
  end;

  TScreenFontStyleNode = class(TAbstractFontStyleNode)
    {$I auto_generated_node_helpers/x3dnodes_screenfontstyle.inc}
  end;

  TScreenGroupNode = class(TAbstractGroupingNode)
    {$I auto_generated_node_helpers/x3dnodes_screengroup.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure RegisterLayoutNodes;
begin
  NodesManager.RegisterNodeClasses([
    TLayoutNode,
    TLayoutGroupNode,
    TLayoutLayerNode,
    TScreenFontStyleNode,
    TScreenGroupNode
  ]);
end;

{$endif read_implementation}
