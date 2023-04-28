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

{$ifdef read_interface}
  { Base type for all the environmental sensor nodes. }
  TAbstractEnvironmentalSensorNode = class(TAbstractSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_x3denvironmentalsensornode.inc}
  end;

  { Generate events when the viewer enters, exits, and/or moves within
    a region of 3D space. }
  TProximitySensorNode = class(TAbstractEnvironmentalSensorNode)
  public
    function TransformationChange: TNodeTransformationChange; override;
  {$I auto_generated_node_helpers/x3dnodes_proximitysensor.inc}
  end;

  { Generates events when the target object
    enters, exits, and moves within a region of 3D space.
    @bold(Not implemented yet.) }
  TTransformSensorNode = class(TAbstractEnvironmentalSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_transformsensor.inc}
  end;

  { Detects visibility changes of a rectangular box as the user navigates the world. }
  TVisibilitySensorNode = class(TAbstractEnvironmentalSensorNode)
  strict private
    FIsActive: boolean;
  public
    function TransformationChange: TNodeTransformationChange; override;
    function Box: TBox3D;

    property IsActive: boolean read FIsActive;
    procedure SetIsActive(const Value: boolean; const Time: TX3DTime);

  {$I auto_generated_node_helpers/x3dnodes_visibilitysensor.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TProximitySensorNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcProximitySensor;
end;

function TVisibilitySensorNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcVisibilitySensor;
end;

function TVisibilitySensorNode.Box: TBox3D;
begin
  if (FdSize.Value[0] < 0) or
     (FdSize.Value[1] < 0) or
     (FdSize.Value[2] < 0) then
  begin
    WritelnWarning('X3D', 'All VisibilitySensor.size components must be >= 0');
    Exit(TBox3D.Empty);
  end;

  { In case of VisibilitySensor, specification doesn't say that size = 0
    indicates empty box (like in case of ProximitySensor).
    And this is sensible: flat (0 depth or such) VisibilitySensor
    makes sense. So for a valid X3D, this box is never TBox3D.Empty. }

  Result := Box3DAroundPoint(FdCenter.Value, FdSize.Value);
end;

procedure TVisibilitySensorNode.SetIsActive(const Value: boolean; const Time: TX3DTime);
begin
  if FIsActive <> Value then
  begin
    FIsActive := Value;
    EventIsActive.Send(Value, Time);
    if Value then
      EventEnterTime.Send(Time.Seconds, Time) else
      EventExitTime.Send(Time.Seconds, Time);
  end;
end;

procedure RegisterEnvironmentalSensorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TProximitySensorNode,
    TTransformSensorNode,
    TVisibilitySensorNode
  ]);
end;

{$endif read_implementation}
