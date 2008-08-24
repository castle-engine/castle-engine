{
  Copyright 2007-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef read_interface}
  TNodeX3DPointingDeviceSensorNode = class(TNodeX3DSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    { Event: SFBool, out } { }
    private FEventisOver: TVRMLEvent;
    public property EventisOver: TVRMLEvent read FEventisOver;
  end;

  TNodeX3DDragSensorNode = class(TNodeX3DPointingDeviceSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdautoOffset: TSFBool;
    public property FdautoOffset: TSFBool read FFdautoOffset;

    { Event: SFVec3f, out } { }
    private FEventtrackPoint_changed: TVRMLEvent;
    public property EventtrackPoint_changed: TVRMLEvent read FEventtrackPoint_changed;
  end;

  TNodeX3DTouchSensorNode = class(TNodeX3DPointingDeviceSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: SFTime, out } { }
    private FEventtouchTime: TVRMLEvent;
    public property EventtouchTime: TVRMLEvent read FEventtouchTime;
  end;

  TNodeCylinderSensor = class(TNodeX3DDragSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdaxisRotation: TSFRotation;
    public property FdaxisRotation: TSFRotation read FFdaxisRotation;

    private FFddiskAngle: TSFFloat;
    public property FddiskAngle: TSFFloat read FFddiskAngle;

    private FFdmaxAngle: TSFFloat;
    public property FdmaxAngle: TSFFloat read FFdmaxAngle;

    private FFdminAngle: TSFFloat;
    public property FdminAngle: TSFFloat read FFdminAngle;

    private FFdoffset: TSFFloat;
    public property Fdoffset: TSFFloat read FFdoffset;

    { Event: SFRotation, out } { }
    private FEventrotation_changed: TVRMLEvent;
    public property Eventrotation_changed: TVRMLEvent read FEventrotation_changed;
  end;

  TNodePlaneSensor = class(TNodeX3DDragSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdaxisRotation: TSFRotation;
    public property FdaxisRotation: TSFRotation read FFdaxisRotation;

    private FFdmaxPosition: TSFVec2f;
    public property FdmaxPosition: TSFVec2f read FFdmaxPosition;

    private FFdminPosition: TSFVec2f;
    public property FdminPosition: TSFVec2f read FFdminPosition;

    private FFdoffset: TSFVec3f;
    public property Fdoffset: TSFVec3f read FFdoffset;

    { Event: SFVec3f, out } { }
    private FEventtranslation_changed: TVRMLEvent;
    public property Eventtranslation_changed: TVRMLEvent read FEventtranslation_changed;
  end;

  TNodeSphereSensor = class(TNodeX3DDragSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdoffset: TSFRotation;
    public property Fdoffset: TSFRotation read FFdoffset;

    { Event: SFRotation, out } { }
    private FEventrotation_changed: TVRMLEvent;
    public property Eventrotation_changed: TVRMLEvent read FEventrotation_changed;
  end;

  TNodeTouchSensor = class(TNodeX3DTouchSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, out } { }
    private FEventhitNormal_changed: TVRMLEvent;
    public property EventhitNormal_changed: TVRMLEvent read FEventhitNormal_changed;

    { Event: SFVec3f, out } { }
    private FEventhitPoint_changed: TVRMLEvent;
    public property EventhitPoint_changed: TVRMLEvent read FEventhitPoint_changed;

    { Event: SFVec2f, out } { }
    private FEventhitTexCoord_changed: TVRMLEvent;
    public property EventhitTexCoord_changed: TVRMLEvent read FEventhitTexCoord_changed;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DPointingDeviceSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFddescription);

  FEventisOver := TVRMLEvent.Create(Self, 'isOver', TSFBool, false);
  Events.Add(FEventisOver);
end;

constructor TNodeX3DDragSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdautoOffset := TSFBool.Create(Self, 'autoOffset', true);
  Fields.Add(FFdautoOffset);

  FEventtrackPoint_changed := TVRMLEvent.Create(Self, 'trackPoint_changed', TSFVec3f, false);
  Events.Add(FEventtrackPoint_changed);
end;

constructor TNodeX3DTouchSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventtouchTime := TVRMLEvent.Create(Self, 'touchTime', TSFTime, false);
  Events.Add(FEventtouchTime);
end;

constructor TNodeCylinderSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdaxisRotation := TSFRotation.Create(Self, 'axisRotation', Vector3Single(0, 1, 0), 0);
  Fields.Add(FFdaxisRotation);

  FFddiskAngle := TSFFloat.Create(Self, 'diskAngle', Pi/12);
  Fields.Add(FFddiskAngle);
  { X3D specification comment: [0,Pi/2] }

  FFdmaxAngle := TSFFloat.Create(Self, 'maxAngle', -1);
  Fields.Add(FFdmaxAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdminAngle := TSFFloat.Create(Self, 'minAngle', 0);
  Fields.Add(FFdminAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdoffset := TSFFloat.Create(Self, 'offset', 0);
  Fields.Add(FFdoffset);
  { X3D specification comment: (-Inf,Inf) }

  FEventrotation_changed := TVRMLEvent.Create(Self, 'rotation_changed', TSFRotation, false);
  Events.Add(FEventrotation_changed);

  DefaultContainerField := 'children';
end;

class function TNodeCylinderSensor.ClassNodeTypeName: string;
begin
  Result := 'CylinderSensor';
end;

function TNodeCylinderSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeCylinderSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePlaneSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdaxisRotation := TSFRotation.Create(Self, 'axisRotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdaxisRotation);

  FFdmaxPosition := TSFVec2f.Create(Self, 'maxPosition', Vector2Single(-1, -1));
  Fields.Add(FFdmaxPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdminPosition := TSFVec2f.Create(Self, 'minPosition', Vector2Single(0, 0));
  Fields.Add(FFdminPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdoffset := TSFVec3f.Create(Self, 'offset', Vector3Single(0, 0, 0));
  Fields.Add(FFdoffset);
  { X3D specification comment: (-Inf,Inf) }

  FEventtranslation_changed := TVRMLEvent.Create(Self, 'translation_changed', TSFVec3f, false);
  Events.Add(FEventtranslation_changed);

  DefaultContainerField := 'children';
end;

class function TNodePlaneSensor.ClassNodeTypeName: string;
begin
  Result := 'PlaneSensor';
end;

function TNodePlaneSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodePlaneSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSphereSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdoffset := TSFRotation.Create(Self, 'offset', Vector3Single(0, 1, 0), 0);
  Fields.Add(FFdoffset);
  { X3D specification comment: [-1,1],(-Inf,Inf) }

  FEventrotation_changed := TVRMLEvent.Create(Self, 'rotation_changed', TSFRotation, false);
  Events.Add(FEventrotation_changed);

  DefaultContainerField := 'children';
end;

class function TNodeSphereSensor.ClassNodeTypeName: string;
begin
  Result := 'SphereSensor';
end;

function TNodeSphereSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeSphereSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTouchSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventhitNormal_changed := TVRMLEvent.Create(Self, 'hitNormal_changed', TSFVec3f, false);
  Events.Add(FEventhitNormal_changed);

  FEventhitPoint_changed := TVRMLEvent.Create(Self, 'hitPoint_changed', TSFVec3f, false);
  Events.Add(FEventhitPoint_changed);

  FEventhitTexCoord_changed := TVRMLEvent.Create(Self, 'hitTexCoord_changed', TSFVec2f, false);
  Events.Add(FEventhitTexCoord_changed);

  DefaultContainerField := 'children';
end;

class function TNodeTouchSensor.ClassNodeTypeName: string;
begin
  Result := 'TouchSensor';
end;

function TNodeTouchSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeTouchSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterPointingDeviceSensorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeCylinderSensor,
    TNodePlaneSensor,
    TNodeSphereSensor,
    TNodeTouchSensor
  ]);
end;

{$endif read_implementation}
