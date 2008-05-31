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
  TNodeX3DViewpointNode = class(TNodeX3DBindableNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdcenterOfRotation: TSFVec3f;
    public property FdcenterOfRotation: TSFVec3f read FFdcenterOfRotation;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdjump: TSFBool;
    public property Fdjump: TSFBool read FFdjump;

    private FFdorientation: TSFRotation;
    public property Fdorientation: TSFRotation read FFdorientation;

    private FFdretainUserOffsets: TSFBool;
    public property FdretainUserOffsets: TSFBool read FFdretainUserOffsets;
  end;

  TNodeBillboard = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdaxisOfRotation: TSFVec3f;
    public property FdaxisOfRotation: TSFVec3f read FFdaxisOfRotation;
  end;

  TNodeCollision = class(TNodeX3DGroupingNode, INodeX3DSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    { Event: SFTime, out } { }
    private FEventcollideTime: TVRMLEvent;
    public property EventcollideTime: TVRMLEvent read FEventcollideTime;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    private FFdproxy: TSFNode;
    public property Fdproxy: TSFNode read FFdproxy;
  end;

  TNodeLOD = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFInt32, out } { }
    private FEventlevel_changed: TVRMLEvent;
    public property Eventlevel_changed: TVRMLEvent read FEventlevel_changed;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdforceTransitions: TSFBool;
    public property FdforceTransitions: TSFBool read FFdforceTransitions;

    private FFdrange: TMFFloat;
    public property Fdrange: TMFFloat read FFdrange;
  end;

  TNodeNavigationInfo = class(TNodeX3DBindableNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdavatarSize: TMFFloat;
    public property FdavatarSize: TMFFloat read FFdavatarSize;

    private FFdheadlight: TSFBool;
    public property Fdheadlight: TSFBool read FFdheadlight;

    private FFdspeed: TSFFloat;
    public property Fdspeed: TSFFloat read FFdspeed;

    private FFdtransitionTime: TSFTime;
    public property FdtransitionTime: TSFTime read FFdtransitionTime;

    private FFdtransitionType: TMFString;
    public property FdtransitionType: TMFString read FFdtransitionType;

    private FFdtype: TMFString;
    public property Fdtype: TMFString read FFdtype;

    private FFdvisibilityLimit: TSFFloat;
    public property FdvisibilityLimit: TSFFloat read FFdvisibilityLimit;

    { Event: SFBool, out } { }
    private FEventtransitionComplete: TVRMLEvent;
    public property EventtransitionComplete: TVRMLEvent read FEventtransitionComplete;
  end;

  TNodeOrthoViewpoint = class(TNodeX3DViewpointNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdfieldOfView: TMFFloat;
    public property FdfieldOfView: TMFFloat read FFdfieldOfView;

    private FFdposition: TSFVec3f;
    public property Fdposition: TSFVec3f read FFdposition;
  end;

  TNodeViewpoint = class(TNodeX3DViewpointNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdfieldOfView: TSFFloat;
    public property FdfieldOfView: TSFFloat read FFdfieldOfView;

    private FFdposition: TSFVec3f;
    public property Fdposition: TSFVec3f read FFdposition;
  end;

  TNodeViewpointGroup = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    private FFddisplayed: TSFBool;
    public property Fddisplayed: TSFBool read FFddisplayed;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DViewpointNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenterOfRotation := TSFVec3f.Create('centerOfRotation', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenterOfRotation);
  { X3D specification comment: (-Inf,Inf) }

  FFddescription := TSFString.Create('description', '');
  Fields.Add(FFddescription);

  FFdjump := TSFBool.Create('jump', true);
  Fields.Add(FFdjump);

  FFdorientation := TSFRotation.Create('orientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdorientation);
  { X3D specification comment: [-1,1],(-Inf,Inf) }

  FFdretainUserOffsets := TSFBool.Create('retainUserOffsets', false);
  Fields.Add(FFdretainUserOffsets);

  DefaultContainerField := 'children';
end;

constructor TNodeBillboard.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdaxisOfRotation := TSFVec3f.Create('axisOfRotation', Vector3Single(0, 1, 0));
  Fields.Add(FFdaxisOfRotation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeBillboard.ClassNodeTypeName: string;
begin
  Result := 'Billboard';
end;

function TNodeBillboard.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeBillboard.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeCollision.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdenabled := TSFBool.Create('enabled', );
  Fields.Add(FFdenabled);

  FEventcollideTime := TVRMLEvent.Create('collideTime', TSFTime, false);
  Events.Add(FEventcollideTime);

  FEventisActive := TVRMLEvent.Create('isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FFdproxy := TSFNode.Create(Self, 'proxy', [X3DChildNode]);
  FFdproxy.Exposed := false;
  Fields.Add(FFdproxy);

  DefaultContainerField := 'children';
end;

class function TNodeCollision.ClassNodeTypeName: string;
begin
  Result := 'Collision';
end;

function TNodeCollision.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeCollision.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeLOD.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventlevel_changed := TVRMLEvent.Create('level_changed', TSFInt32, false);
  Events.Add(FEventlevel_changed);

  FFdcenter := TSFVec3f.Create('center', Vector3Single(0, 0, 0));
  FFdcenter.Exposed := false;
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdforceTransitions := TSFBool.Create('forceTransitions', false);
  FFdforceTransitions.Exposed := false;
  Fields.Add(FFdforceTransitions);

  FFdrange := TMFFloat.Create('range', []);
  FFdrange.Exposed := false;
  Fields.Add(FFdrange);
  { X3D specification comment: [0,Inf) or -1 }

  DefaultContainerField := 'children';
end;

class function TNodeLOD.ClassNodeTypeName: string;
begin
  Result := 'LOD';
end;

function TNodeLOD.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLOD.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNavigationInfo.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdavatarSize := TMFFloat.Create('avatarSize', [0.25 1.6 0.75]);
  Fields.Add(FFdavatarSize);
  { X3D specification comment: [0,Inf) }

  FFdheadlight := TSFBool.Create('headlight', true);
  Fields.Add(FFdheadlight);

  FFdspeed := TSFFloat.Create('speed', 1.0);
  Fields.Add(FFdspeed);
  { X3D specification comment: [0,Inf) }

  FFdtransitionTime := TSFTime.Create('transitionTime', 1.0);
  Fields.Add(FFdtransitionTime);
  { X3D specification comment: [0, Inf) }

  FFdtransitionType := TMFString.Create('transitionType', ['LINEAR']);
  Fields.Add(FFdtransitionType);
  { X3D specification comment: ["TELEPORT","LINEAR","ANIMATE",...] }

  FFdtype := TMFString.Create('type', ['EXAMINE' 'ANY']);
  Fields.Add(FFdtype);
  { X3D specification comment: ["ANY","WALK","EXAMINE","FLY","LOOKAT","NONE",...] }

  FFdvisibilityLimit := TSFFloat.Create('visibilityLimit', 0.0);
  Fields.Add(FFdvisibilityLimit);
  { X3D specification comment: [0,Inf) }

  FEventtransitionComplete := TVRMLEvent.Create('transitionComplete', TSFBool, false);
  Events.Add(FEventtransitionComplete);

  DefaultContainerField := 'children';
end;

class function TNodeNavigationInfo.ClassNodeTypeName: string;
begin
  Result := 'NavigationInfo';
end;

function TNodeNavigationInfo.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNavigationInfo.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeOrthoViewpoint.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfieldOfView := TMFFloat.Create('fieldOfView', -1,);
  Fields.Add(FFdfieldOfView);
  { X3D specification comment: -1, 1, 1  (-Inf,Inf) }

  FFdposition := TSFVec3f.Create('position', Vector3Single(0, 0, 10));
  Fields.Add(FFdposition);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeOrthoViewpoint.ClassNodeTypeName: string;
begin
  Result := 'OrthoViewpoint';
end;

function TNodeOrthoViewpoint.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeOrthoViewpoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeViewpoint.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfieldOfView := TSFFloat.Create('fieldOfView', Pi/4);
  Fields.Add(FFdfieldOfView);
  { X3D specification comment: (0,Pi) }

  FFdposition := TSFVec3f.Create('position', Vector3Single(0, 0, 10));
  Fields.Add(FFdposition);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeViewpoint.ClassNodeTypeName: string;
begin
  Result := 'Viewpoint';
end;

function TNodeViewpoint.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeViewpoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeViewpointGroup.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create('center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdchildren := TMFNode.Create(Self, 'children', [X3DViewpointNode | ViewpointGroup]);
  Fields.Add(FFdchildren);

  FFddisplayed := TSFBool.Create('displayed', true);
  Fields.Add(FFddisplayed);

  FFdsize := TSFVec3f.Create('size', Vector3Single(0, 0, 0));
  Fields.Add(FFdsize);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeViewpointGroup.ClassNodeTypeName: string;
begin
  Result := 'ViewpointGroup';
end;

function TNodeViewpointGroup.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeViewpointGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterNavigationNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeBillboard,
    TNodeCollision,
    TNodeLOD,
    TNodeNavigationInfo,
    TNodeOrthoViewpoint,
    TNodeViewpoint,
    TNodeViewpointGroup
  ]);
end;

{$endif read_implementation}
