{%MainUnit x3dnodes.pas}
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

{$ifdef read_interface}

  { Abstract node for all base nodes that specify texture projection.
    @bold(This standard X3D node is not implemented yet.)
    Instead of this node, you can use ProjectedTextureCoordinate in CGE. }
  TAbstractTextureProjectorNode = class(TAbstractLightNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dtextureprojectornode.inc}
  end;

  { Texture projection following perspective projection volume.
    @bold(This standard X3D node is not implemented yet.)
    Instead of this node, you can use ProjectedTextureCoordinate in CGE. }
  TTextureProjectorNode = class(TAbstractTextureProjectorNode)
  {$I auto_generated_node_helpers/x3dnodes_textureprojector.inc}
  end;

  { Texture projection following parallel (orthographic) projection volume.
    @bold(This standard X3D node is not implemented yet.)
    Instead of this node, you can use ProjectedTextureCoordinate in CGE. }
  TTextureProjectorParallelNode = class(TAbstractTextureProjectorNode)
  {$I auto_generated_node_helpers/x3dnodes_textureprojectorparallel.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$I auto_generated_node_helpers/x3dnodes_textureprojector.inc}
{$I auto_generated_node_helpers/x3dnodes_textureprojectorparallel.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dtextureprojectornode.inc}

procedure RegisterTextureProjectorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TAbstractTextureProjectorNode,
    TTextureProjectorNode,
    TTextureProjectorParallelNode
  ]);
end;

{$endif read_implementation}
