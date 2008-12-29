{
  Copyright 2002-2008 Michalis Kamburelis.

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
  { X3DLightNode. }
  TNodeX3DLightNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdOn: TSFBool;
    public property FdOn: TSFBool read FFdOn;

    private FFdIntensity: TSFFloat;
    public property FdIntensity: TSFFloat read FFdIntensity;

    private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    private FFdglobal: TSFBool;
    public property Fdglobal: TSFBool read FFdglobal;

    private FFdKambiShadows: TSFBool;
    public property FdKambiShadows: TSFBool read FFdKambiShadows;

    private FFdKambiShadowsMain: TSFBool;
    public property FdKambiShadowsMain: TSFBool read FFdKambiShadowsMain;

    private FFdshowProxyGeometry: TSFBool;
    { showProxyGeometry field is an Avalon extension, see
      [http://instant-reality.com/documentation/nodetype/Light/]. }
    public property FdshowProxyGeometry: TSFBool read FFdshowProxyGeometry;
  end;

  { This is the base class for all VRML / X3D light nodes.

    This is for all VRML versions --- in particular, VRML 1.0 nodes
    also inherit from this, to reuse this and other light classes.
    The fact that is descends from TNodeX3DLightNode is no problem for older
    than X3D versions, the interpretation of all fields is the same
    (and older VRML versions only benefit from additional fields).
    Well, actually, VRML 1.0 interprets them a little differently
    ("ambientIntensity" < 0 has special meaning,
    and "global" is ignored --- VRML 1.0 has own scoping rules by traversing). }
  TVRMLLightNode = class(TNodeX3DLightNode)
  protected
    procedure MiddleTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
  public
    { Create TActiveLight record describing this light node under given
      State. }
    function CreateActiveLight(State: TVRMLGraphTraverseState): TActiveLight;

    { Update TActiveLight record when lighting State changes.
      Assumes that ActiveLight.LightNode = Self.

      This will set ActiveLight.Transform properties, and recalculate
      all ActiveLight.TransfXxx properties. }
    procedure UpdateActiveLightState(
      var ActiveLight: TActiveLight;
      State: TVRMLGraphTraverseState);

    { Update TActiveLight record when lighting location/direction (and other
      properties precalculated on TActiveLight) change.
      Assumes that ActiveLight.LightNode = Self. }
    procedure UpdateActiveLight(
      var ActiveLight: TActiveLight); virtual;
  end;

  TObjectsListItem_1 = TVRMLLightNode;
  {$I objectslist_1.inc}
  TVRMLLightNodesList = class(TObjectsList_1);

  TVRMLDirectionalLightNode = class(TVRMLLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    procedure UpdateActiveLight(var ActiveLight: TActiveLight); override;
  end;

  TVRMLPositionalLightNode = class(TVRMLLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdLocation: TSFVec3f;
    public property FdLocation: TSFVec3f read FFdLocation;

    private FFdAttenuation: TSFVec3f;
    public property FdAttenuation: TSFVec3f read FFdAttenuation;

    { Attenuation obliczaja attenuation (tzn. wzorek 1/max( attenuation[0] + ...)
      zgodnie ze specyfik. VRMLa 97. Poniewaz obliczenie parametru DistanceToLight
      moze czasem wiazac sie ze spora strata czasu mozesz sprawdzic najpierw
      wartosc DistanceNeededForAttenuation --- jezeli jest false, to wartosc
      parametru DistanceToLight dla Attenutaion() nie ma znaczenia (mozesz
      podac cokolwiek).

      PAMIETAJ --- DistanceToLight powinien byc w lights coordinate system.
      TODO: raytracer nie realizuje teraz tego "PAMIETAJ" powyzej
      TODO: nie wiem czy OpenGL realizuje to "PAMIETAJ" powyzej
      w swoich swiatlach, check w OpenGL spec }
    function DistanceNeededForAttenuation: boolean;
    function Attenuation(const DistanceToLight: Single): Single; overload;
    function Attenuation(const DistanceToLight: Double): Double; overload;

    procedure UpdateActiveLight(var ActiveLight: TActiveLight); override;
  end;

  TVRMLPointLightNode = class(TVRMLPositionalLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
  end;

  TNodeDirectionalLight_2 = class(TVRMLDirectionalLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;
  end;

  TNodePointLight_2 = class(TVRMLPointLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    procedure UpdateActiveLight(var ActiveLight: TActiveLight); override;
  end;

  TNodeSpotLight_2 = class(TVRMLPositionalLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdbeamWidth: TSFFloat;
    public property FdbeamWidth: TSFFloat read FFdbeamWidth;

    private FFdcutOffAngle: TSFFloat;
    public property FdcutOffAngle: TSFFloat read FFdcutOffAngle;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    procedure UpdateActiveLight(var ActiveLight: TActiveLight); override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DLightNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdglobal := TSFBool.Create(Self, 'global', false);
  Fields.Add(FFdglobal);

  FFdon := TSFBool.Create(Self, 'on', true);
  Fields.Add(FFdon);

  FFdintensity := TSFFloat.Create(Self, 'intensity', 1);
  Fields.Add(FFdintensity);

  FFdcolor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
  Fields.Add(FFdcolor);

  FFdambientIntensity := TSFFloat.Create(Self, 'ambientIntensity', 0);
  Fields.Add(FFdambientIntensity);

  FFdkambiShadows := TSFBool.Create(Self, 'kambiShadows', false);
  Fields.Add(FFdkambiShadows);

  FFdkambiShadowsMain := TSFBool.Create(Self, 'kambiShadowsMain', false);
  Fields.Add(FFdkambiShadowsMain);

  FFdshowProxyGeometry := TSFBool.Create(Self, 'showProxyGeometry', false);
  Fields.Add(FFdshowProxyGeometry);

  DefaultContainerField := 'children';
end;

function TVRMLLightNode.CreateActiveLight(
  State: TVRMLGraphTraverseState): TActiveLight;
begin
  Result.LightNode := Self;
  UpdateActiveLightState(Result, State);
end;

procedure TVRMLLightNode.UpdateActiveLightState(
  var ActiveLight: TActiveLight;
  State: TVRMLGraphTraverseState);
begin
  ActiveLight.Transform := State.Transform;
  ActiveLight.AverageScaleTransform := State.AverageScaleTransform;
  UpdateActiveLight(ActiveLight);
end;

procedure TVRMLLightNode.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  { Nothing to do in this class. }
  Assert(ActiveLight.LightNode = Self);
end;

procedure TVRMLLightNode.MiddleTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.VRML1ActiveLights.AppendItem(CreateActiveLight(StateStack.Top));
end;

constructor TVRMLDirectionalLightNode.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, -1));
  Fields.Add(FFddirection);
  FFddirection.ProcessedInActiveLight := true;
end;

class function TVRMLDirectionalLightNode.ClassNodeTypeName: string;
begin
  Result := 'DirectionalLight';
end;

procedure TVRMLDirectionalLightNode.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  inherited;
  ActiveLight.TransfNormDirection :=
    Normalized( MatrixMultDirection(ActiveLight.Transform, FdDirection.Value) );
end;

constructor TVRMLPositionalLightNode.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdlocation := TSFVec3f.Create(Self, 'location', Vector3Single(0, 0, 0));
  Fields.Add(FFdlocation);
  { X3D specification comment: (-Inf,Inf) }
  FFdlocation.ProcessedInActiveLight := true;

  FFdattenuation := TSFVec3f.Create(Self, 'attenuation', Vector3Single(1, 0, 0));
  Fields.Add(FFdattenuation);
  { X3D specification comment: [0,Inf) }
end;

function TVRMLPositionalLightNode.DistanceNeededForAttenuation: boolean;
begin
  Result := (FdAttenuation.Value[1] > 0) or (FdAttenuation.Value[2] > 0);
end;

procedure TVRMLPositionalLightNode.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  inherited;

  ActiveLight.TransfLocation := MatrixMultPoint(ActiveLight.Transform,
    FdLocation.Value);
end;

{$define ATTENUATION_IMPLEMENTATION:=
begin
 (* moglibysmy tu nie badac czy DistanceNeededForAttenuation i zawsze
    robic wersje pelna (bo przeciez
      FdAttenuation.Value[1] * DistanceToLight +
      FdAttenuation.Value[2] * Sqr(DistanceToLight)
    i tak bedzie = 0 gdy FdAttenuation.Value[1] = FdAttenuation.Value[2] = 0.
    Ale wydaje mi sie ze tak jest szybciej - testowanie kosztuje nas
    troszke czasu ale mozemy sobie w ten sposob ocalic 2 x mnozenie i dodawanie. *)

 (* we check whether attenuation = (0, 0, 0). VRML 97 spec says that specifying
    (0, 0, 0) should be equal to specifying (1, 0, 0). (well, we avoid
    division by zero possibility this way so it's quite sensible, even
    if it wastes some time) *)
 if (FdAttenuation.Value[0] = 0) and
    (FdAttenuation.Value[1] = 0) and
    (FdAttenuation.Value[2] = 0) then result := 1;

 if DistanceNeededForAttenuation then
  result := 1/ KambiUtils.max(FdAttenuation.Value[0] +
                   FdAttenuation.Value[1] * DistanceToLight +
                   FdAttenuation.Value[2] * Sqr(DistanceToLight), Single(1.0)) else
  result := 1/ KambiUtils.max(FdAttenuation.Value[0], Single(1.0));
end;}

function TVRMLPositionalLightNode.Attenuation(const DistanceToLight: Single): Single;
ATTENUATION_IMPLEMENTATION

function TVRMLPositionalLightNode.Attenuation(const DistanceToLight: Double): Double;
ATTENUATION_IMPLEMENTATION

constructor TVRMLPointLightNode.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  { no new fields - this is just TVRMLPositionalLightNode }
end;

class function TVRMLPointLightNode.ClassNodeTypeName: string;
begin
  Result := 'PointLight';
end;

constructor TNodeDirectionalLight_2.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FdGlobal.Value := false;

  DefaultContainerField := 'children';
end;

function TNodeDirectionalLight_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeDirectionalLight_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeDirectionalLight_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

constructor TNodePointLight_2.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdradius := TSFFloat.Create(Self, 'radius', 100);
  Fields.Add(FFdradius);
  { X3D specification comment: [0,Inf) }
  FFdradius.ProcessedInActiveLight := true;

  FdGlobal.Value := true;

  DefaultContainerField := 'children';
end;

function TNodePointLight_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodePointLight_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodePointLight_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodePointLight_2.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  inherited;

  { TODO: For non-uniform scale, this will simply use average scale.
    This is not fully correct, VRML spec doesn't clarify this
    but I guess that the intention was that the non-uniform scale will
    make radius non-uniform, i.e. light volume will not be a regular sphere
    but some 3d ellipsoid. Unfortunately this would require quite more
    work, UpdateVRML2ActiveLights would then have to check for collision
    between
      sphere transformed by matrix Transform
    and
      bounding box
    which I don't know how to do *easily*... }
  ActiveLight.TransfRadius := FdRadius.Value * ActiveLight.AverageScaleTransform;
end;

constructor TNodeSpotLight_2.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdbeamWidth := TSFFloat.Create(Self, 'beamWidth', Pi / 2);
  Fields.Add(FFdbeamWidth);
  { X3D specification comment: (0,Pi/2] }

  FFdcutOffAngle := TSFFloat.Create(Self, 'cutOffAngle', Pi / 4);
  Fields.Add(FFdcutOffAngle);
  { X3D specification comment: (0,Pi/2] }

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, -1));
  Fields.Add(FFddirection);
  { X3D specification comment: (-Inf,Inf) }
  FFddirection.ProcessedInActiveLight := true;

  FFdradius := TSFFloat.Create(Self, 'radius', 100);
  Fields.Add(FFdradius);
  { X3D specification comment: [0,Inf) }
  FFdradius.ProcessedInActiveLight := true;

  FdGlobal.Value := true;

  DefaultContainerField := 'children';
end;

class function TNodeSpotLight_2.ClassNodeTypeName: string;
begin
  Result := 'SpotLight';
end;

function TNodeSpotLight_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeSpotLight_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeSpotLight_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodeSpotLight_2.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  inherited;

  ActiveLight.TransfNormDirection :=
    Normalized( MatrixMultDirection(ActiveLight.Transform, FdDirection.Value) );

  { TODO: For non-uniform scale, this is too easy,
    see TNodePointLight_2.CreateActiveLight for more comments. }
  ActiveLight.TransfRadius := FdRadius.Value * ActiveLight.AverageScaleTransform;
end;

procedure RegisterLightingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeDirectionalLight_2,
    TNodePointLight_2,
    TNodeSpotLight_2
  ]);
end;

{$endif read_implementation}
