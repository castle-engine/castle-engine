{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TPickSensorIntersectionType = (
    piGeometry,
    piBounds
  );

  TPickSensorRange = (
    prAny,
    prClosest,
    prAll,
    prAllSorted
  );

  TAbstractPickSensorNode = class(TAbstractSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dpicksensornode.inc}
  end;

  TLinePickSensorNode = class(TAbstractPickSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_linepicksensor.inc}
  end;

  TPickableGroupNode = class(TAbstractGroupingNode)
  {$I auto_generated_node_helpers/x3dnodes_pickablegroup.inc}
  end;

  TPointPickSensorNode = class(TAbstractPickSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_pointpicksensor.inc}
  end;

  TPrimitivePickSensorNode = class(TAbstractPickSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_primitivepicksensor.inc}
  end;

  TVolumePickSensorNode = class(TAbstractPickSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_volumepicksensor.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  PickSensorIntersectionTypeNames: array [TPickSensorIntersectionType] of String = (
    'GEOMETRY',
    'BOUNDS'
  );

  PickSensorRangeNames: array [TPickSensorRange] of String = (
    'ANY',
    'CLOSEST',
    'ALL',
    'ALL_SORTED'
  );

procedure RegisterPickingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TLinePickSensorNode,
    TPickableGroupNode,
    TPointPickSensorNode,
    TPrimitivePickSensorNode,
    TVolumePickSensorNode
  ]);
end;

{$endif read_implementation}
