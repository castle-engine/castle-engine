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
  TAbstractFollowerNode = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dfollowernode.inc}
  end;

  TAbstractDamperNode = class(TAbstractFollowerNode)
  {$I auto_generated_node_helpers/x3dnodes_x3ddampernode.inc}
  end;

  TAbstractChaserNode = class(TAbstractFollowerNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dchasernode.inc}
  end;

  TColorDamperNode = class(TAbstractDamperNode)
  {$I auto_generated_node_helpers/x3dnodes_colordamper.inc}
  end;

  TCoordinateDamperNode = class(TAbstractDamperNode)
  {$I auto_generated_node_helpers/x3dnodes_coordinatedamper.inc}
  end;

  TOrientationChaserNode = class(TAbstractChaserNode)
  {$I auto_generated_node_helpers/x3dnodes_orientationchaser.inc}
  end;

  TOrientationDamperNode = class(TAbstractDamperNode)
  {$I auto_generated_node_helpers/x3dnodes_orientationdamper.inc}
  end;

  TPositionChaserNode = class(TAbstractChaserNode)
  {$I auto_generated_node_helpers/x3dnodes_positionchaser.inc}
  end;

  TPositionChaser2DNode = class(TAbstractChaserNode)
  {$I auto_generated_node_helpers/x3dnodes_positionchaser2d.inc}
  end;

  TPositionDamperNode = class(TAbstractDamperNode)
  {$I auto_generated_node_helpers/x3dnodes_positiondamper.inc}
  end;

  TPositionDamper2DNode = class(TAbstractDamperNode)
  {$I auto_generated_node_helpers/x3dnodes_positiondamper2d.inc}
  end;

  TScalarChaserNode = class(TAbstractChaserNode)
  {$I auto_generated_node_helpers/x3dnodes_scalarchaser.inc}
  end;

  TTexCoordDamper2DNode = class(TAbstractDamperNode)
  {$I auto_generated_node_helpers/x3dnodes_texcoorddamper2d.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure RegisterFollowersNodes;
begin
  NodesManager.RegisterNodeClasses([
    TColorDamperNode,
    TCoordinateDamperNode,
    TOrientationChaserNode,
    TOrientationDamperNode,
    TPositionChaserNode,
    TPositionChaser2DNode,
    TPositionDamperNode,
    TPositionDamper2DNode,
    TScalarChaserNode,
    TTexCoordDamper2DNode
  ]);
end;

{$endif read_implementation}
