{
  Copyright 2008 Michalis Kamburelis.

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
  TNodeX3DEnvironmentalSensorNode = class(TNodeX3DSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;

    { Event: SFTime, out } { }
    private FEvententerTime: TVRMLEvent;
    public property EvententerTime: TVRMLEvent read FEvententerTime;

    { Event: SFTime, out } { }
    private FEventexitTime: TVRMLEvent;
    public property EventexitTime: TVRMLEvent read FEventexitTime;
  end;

  TNodeProximitySensor = class(TNodeX3DEnvironmentalSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, out } { }
    private FEventcenterOfRotation_changed: TVRMLEvent;
    public property EventcenterOfRotation_changed: TVRMLEvent read FEventcenterOfRotation_changed;

    { Event: SFRotation, out } { }
    private FEventorientation_changed: TVRMLEvent;
    public property Eventorientation_changed: TVRMLEvent read FEventorientation_changed;

    { Event: SFVec3f, out } { }
    private FEventposition_changed: TVRMLEvent;
    public property Eventposition_changed: TVRMLEvent read FEventposition_changed;
  end;

  TNodeTransformSensor = class(TNodeX3DEnvironmentalSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtargetObject: TSFNode;
    public property FdtargetObject: TSFNode read FFdtargetObject;

    { Event: SFRotation, out } { }
    private FEventorientation_changed: TVRMLEvent;
    public property Eventorientation_changed: TVRMLEvent read FEventorientation_changed;

    { Event: SFVec3f, out } { }
    private FEventposition_changed: TVRMLEvent;
    public property Eventposition_changed: TVRMLEvent read FEventposition_changed;
  end;

  TNodeVisibilitySensor = class(TNodeX3DEnvironmentalSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DEnvironmentalSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdsize := TSFVec3f.Create(Self, 'size', Vector3Single(0, 0, 0));
  Fields.Add(FFdsize);
  { X3D specification comment: (-Inf,Inf) }

  FEvententerTime := TVRMLEvent.Create(Self, 'enterTime', TSFTime, false);
  Events.Add(FEvententerTime);

  FEventexitTime := TVRMLEvent.Create(Self, 'exitTime', TSFTime, false);
  Events.Add(FEventexitTime);

  DefaultContainerField := 'children';
end;

constructor TNodeProximitySensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventcenterOfRotation_changed := TVRMLEvent.Create(Self, 'centerOfRotation_changed', TSFVec3f, false);
  Events.Add(FEventcenterOfRotation_changed);

  FEventorientation_changed := TVRMLEvent.Create(Self, 'orientation_changed', TSFRotation, false);
  Events.Add(FEventorientation_changed);

  FEventposition_changed := TVRMLEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventposition_changed);

  DefaultContainerField := 'children';
end;

class function TNodeProximitySensor.ClassNodeTypeName: string;
begin
  Result := 'ProximitySensor';
end;

function TNodeProximitySensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeProximitySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTransformSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtargetObject := TSFNode.Create(Self, 'targetObject', [TNodeX3DGroupingNode, TNodeX3DShapeNode]);
  Fields.Add(FFdtargetObject);

  FEventorientation_changed := TVRMLEvent.Create(Self, 'orientation_changed', TSFRotation, false);
  Events.Add(FEventorientation_changed);

  FEventposition_changed := TVRMLEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventposition_changed);

  DefaultContainerField := 'children';
end;

class function TNodeTransformSensor.ClassNodeTypeName: string;
begin
  Result := 'TransformSensor';
end;

function TNodeTransformSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTransformSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeVisibilitySensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeVisibilitySensor.ClassNodeTypeName: string;
begin
  Result := 'VisibilitySensor';
end;

function TNodeVisibilitySensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeVisibilitySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterEnvironmentalSensorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeProximitySensor,
    TNodeTransformSensor,
    TNodeVisibilitySensor
  ]);
end;

{$endif read_implementation}
