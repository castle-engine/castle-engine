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
  TNodeX3DInterpolatorNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;
  end;

  TNodeColorInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFColor;
    public property FdkeyValue: TMFColor read FFdkeyValue;

    { Event: SFColor, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeCoordinateInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    { Event: MFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeCoordinateInterpolator2D = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec2f;
    public property FdkeyValue: TMFVec2f read FFdkeyValue;

    { Event: MFVec2f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeEaseInEaseOut = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdeaseInEaseOut: TMFVec2f;
    public property FdeaseInEaseOut: TMFVec2f read FFdeaseInEaseOut;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;

    { Event: SFFloat, out } { }
    private FEventmodifiedFraction_changed: TVRMLEvent;
    public property EventmodifiedFraction_changed: TVRMLEvent read FEventmodifiedFraction_changed;
  end;

  TNodeNormalInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    { Event: MFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeOrientationInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFRotation;
    public property FdkeyValue: TMFRotation read FFdkeyValue;

    { Event: SFRotation, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodePositionInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodePositionInterpolator2D = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec2f;
    public property FdkeyValue: TMFVec2f read FFdkeyValue;

    { Event: SFVec2f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeScalarInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFFloat;
    public property FdkeyValue: TMFFloat read FFdkeyValue;

    { Event: SFFloat, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeSplinePositionInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdclosed: TSFBool;
    public property Fdclosed: TSFBool read FFdclosed;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    private FFdkeyVelocity: TMFVec3f;
    public property FdkeyVelocity: TMFVec3f read FFdkeyVelocity;

    private FFdnormalizeVelocity: TSFBool;
    public property FdnormalizeVelocity: TSFBool read FFdnormalizeVelocity;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeSplinePositionInterpolator2D = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdclosed: TSFBool;
    public property Fdclosed: TSFBool read FFdclosed;

    private FFdkeyValue: TMFVec2f;
    public property FdkeyValue: TMFVec2f read FFdkeyValue;

    private FFdkeyVelocity: TMFVec2f;
    public property FdkeyVelocity: TMFVec2f read FFdkeyVelocity;

    private FFdnormalizeVelocity: TSFBool;
    public property FdnormalizeVelocity: TSFBool read FFdnormalizeVelocity;

    { Event: SFVec2f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeSplineScalarInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdclosed: TSFBool;
    public property Fdclosed: TSFBool read FFdclosed;

    private FFdkeyValue: TMFFloat;
    public property FdkeyValue: TMFFloat read FFdkeyValue;

    private FFdkeyVelocity: TMFFloat;
    public property FdkeyVelocity: TMFFloat read FFdkeyVelocity;

    private FFdnormalizeVelocity: TSFBool;
    public property FdnormalizeVelocity: TSFBool read FFdnormalizeVelocity;

    { Event: SFFloat, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeSquadOrientationInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFRotation;
    public property FdkeyValue: TMFRotation read FFdkeyValue;

    private FFdnormalizeVelocity: TSFBool;
    public property FdnormalizeVelocity: TSFBool read FFdnormalizeVelocity;

    { Event: SFRotation, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DInterpolatorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

constructor TNodeColorInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFColor.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: [0,1] }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFColor, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeColorInterpolator.ClassNodeTypeName: string;
begin
  Result := 'ColorInterpolator';
end;

function TNodeColorInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeColorInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeCoordinateInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TMFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeCoordinateInterpolator.ClassNodeTypeName: string;
begin
  Result := 'CoordinateInterpolator';
end;

function TNodeCoordinateInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeCoordinateInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeCoordinateInterpolator2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFVec2f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TMFVec2f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeCoordinateInterpolator2D.ClassNodeTypeName: string;
begin
  Result := 'CoordinateInterpolator2D';
end;

function TNodeCoordinateInterpolator2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeCoordinateInterpolator2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeEaseInEaseOut.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdeaseInEaseOut := TMFVec2f.Create('easeInEaseOut', []);
  Fields.Add(FFdeaseInEaseOut);
  { X3D specification comment: (-Inf,Inf) }

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);
  { X3D specification comment: (-Inf,Inf) }

  FEventmodifiedFraction_changed := TVRMLEvent.Create('modifiedFraction_changed', TSFFloat, false);
  Events.Add(FEventmodifiedFraction_changed);

  DefaultContainerField := 'children';
end;

class function TNodeEaseInEaseOut.ClassNodeTypeName: string;
begin
  Result := 'EaseInEaseOut';
end;

function TNodeEaseInEaseOut.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeEaseInEaseOut.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNormalInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TMFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeNormalInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NormalInterpolator';
end;

function TNodeNormalInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeNormalInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeOrientationInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFRotation.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFRotation, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeOrientationInterpolator.ClassNodeTypeName: string;
begin
  Result := 'OrientationInterpolator';
end;

function TNodeOrientationInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeOrientationInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePositionInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodePositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'PositionInterpolator';
end;

function TNodePositionInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodePositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePositionInterpolator2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFVec2f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec2f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodePositionInterpolator2D.ClassNodeTypeName: string;
begin
  Result := 'PositionInterpolator2D';
end;

function TNodePositionInterpolator2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodePositionInterpolator2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeScalarInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFFloat.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFFloat, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeScalarInterpolator.ClassNodeTypeName: string;
begin
  Result := 'ScalarInterpolator';
end;

function TNodeScalarInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeScalarInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSplinePositionInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdclosed := TSFBool.Create('closed', false);
  Fields.Add(FFdclosed);

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdkeyVelocity := TMFVec3f.Create('keyVelocity', []);
  Fields.Add(FFdkeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalizeVelocity := TSFBool.Create('normalizeVelocity', false);
  Fields.Add(FFdnormalizeVelocity);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeSplinePositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'SplinePositionInterpolator';
end;

function TNodeSplinePositionInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSplinePositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSplinePositionInterpolator2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdclosed := TSFBool.Create('closed', false);
  Fields.Add(FFdclosed);

  FFdkeyValue := TMFVec2f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdkeyVelocity := TMFVec2f.Create('keyVelocity', []);
  Fields.Add(FFdkeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalizeVelocity := TSFBool.Create('normalizeVelocity', false);
  Fields.Add(FFdnormalizeVelocity);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec2f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeSplinePositionInterpolator2D.ClassNodeTypeName: string;
begin
  Result := 'SplinePositionInterpolator2D';
end;

function TNodeSplinePositionInterpolator2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSplinePositionInterpolator2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSplineScalarInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdclosed := TSFBool.Create('closed', false);
  Fields.Add(FFdclosed);

  FFdkeyValue := TMFFloat.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdkeyVelocity := TMFFloat.Create('keyVelocity', []);
  Fields.Add(FFdkeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalizeVelocity := TSFBool.Create('normalizeVelocity', false);
  Fields.Add(FFdnormalizeVelocity);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFFloat, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeSplineScalarInterpolator.ClassNodeTypeName: string;
begin
  Result := 'SplineScalarInterpolator';
end;

function TNodeSplineScalarInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSplineScalarInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSquadOrientationInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFRotation.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalizeVelocity := TSFBool.Create('normalizeVelocity', false);
  Fields.Add(FFdnormalizeVelocity);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFRotation, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeSquadOrientationInterpolator.ClassNodeTypeName: string;
begin
  Result := 'SquadOrientationInterpolator';
end;

function TNodeSquadOrientationInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSquadOrientationInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterInterpolationNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeColorInterpolator,
    TNodeCoordinateInterpolator,
    TNodeCoordinateInterpolator2D,
    TNodeEaseInEaseOut,
    TNodeNormalInterpolator,
    TNodeOrientationInterpolator,
    TNodePositionInterpolator,
    TNodePositionInterpolator2D,
    TNodeScalarInterpolator,
    TNodeSplinePositionInterpolator,
    TNodeSplinePositionInterpolator2D,
    TNodeSplineScalarInterpolator,
    TNodeSquadOrientationInterpolator
  ]);
end;

{$endif read_implementation}
