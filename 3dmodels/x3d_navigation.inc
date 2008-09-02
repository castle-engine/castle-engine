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
  TVRMLCameraKind = (ckOrthographic, ckPerspective);

  { A common class for both VRML 1.0 camera nodes and VRML >= 2.0 viewpoints
    nodes. }
  TVRMLViewpointNode = class(TNodeX3DBindableNode)
  private
    procedure EventSet_BindReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdOrientation: TSFRotation;
    public property FdOrientation: TSFRotation read FFdOrientation;

    private FFdDirection: TMFVec3f;
    public property FdDirection: TMFVec3f read FFdDirection;

    private FFdUp: TMFVec3f;
    public property FdUp: TMFVec3f read FFdUp;

    private FFdGravityUp: TSFVec3f;
    public property FdGravityUp: TSFVec3f read FFdGravityUp;

    { Position of the viewpoint.

      This is an abstract function, not a simple FdPosition, because one of
      the descendants (TNodeX3DViewpointNode) doesn't have FdPosition field.
      For now, all non-abstract descendants (for both VRML 1.0 and VRML >= 2.0)
      override this and expose their position field correctly. }
    function Position: TSFVec3f; virtual; abstract;

    class function CameraKind: TVRMLCameraKind; virtual; abstract;

    { Calculate camera properties in the form of 3 vectors
      (position + direction + up) based on current field values of
      this node. Following VRML spec:

@preformatted(
  CamPos = FdPosition,
  CamDir = (0, 0, -1) rotated by FdOrientation,
  CamUp = (0, 1, 0) rotated by FdOrientation,
  GravityUp = (0, 1, 0) (not rotated by FdOrientation!),
  and everything is transformed by given CamTransform.
)
      (you should give here the actual VRML transformation at the point in file
      where camera is defined).

      Dodajemy do tego dodatki Kambiego :
      jesli FdDirection.Length > 0 to CamDirection nie jest liczone z
      FdOrientation ale jest brane wprost z FdDirection.Items[0].
      Podobnie dla FdUp.

      Zwraca zawsze znormalizowany CamDir i CamUp i GravityUp bo:
      @orderedList(
        @item(
          zeby zmusic cie do stosowania konsekwentnej zasady wyrazonej na
          poczatku VRMLNodes i nie pisania kodu ktory w jakis sposob bylby
          uzalezniony od podawanych tu CamDir. To dlatego ze
          pola FdOrientation / FdDirection / FdUp tego wezla NIE sluza do
          podawania czegokolwiek poza kierunkami, a wiec ich dlugosc jest
          niewazna. No i stosujac FdOrientation, a wiec uzywajac standardowego
          VRMLa 1.0, nie mozna nawet podac dlugosci FdDirection/FdUp innej
          niz 1.)
        @item(
          Normalizujemy tutaj bo w implementacji tej funkcji czesto wiemy
          ze nie trzeba normalizowac, np. gdy zwracamy standardowe dir/up
          kamery obrocone o orientation to nie potrzebujemy robic zadnej
          normalizacji bo wiemy ze wynik ma dlugosc 1. W ten sytuacji byloby
          nieoptymalne gdybys musial po wywolaniu tej procedury wywolac
          NormalizeTo1st(CamDir), bo przeciez czesto w ponizszej procedurze
          wiadomo ze nie trzea normalizowac (a wiec wywolanie NormalizeTo1st
          i wywolywany w jego srodku Sqrt sa zbedne).)
      )

      TODO: FocalDistance powinien tez byc tu zwracany (po przeliczeniu
      przez CamTransform) for TVRMLCameraNode_1. }
    procedure GetCameraVectors(const CamTransform: TMatrix4Single;
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single);
  end;

  TVRMLViewpointClassNode = class of TVRMLViewpointNode;

  TNodeX3DViewpointNode = class(TVRMLViewpointNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdcenterOfRotation: TSFVec3f;
    public property FdcenterOfRotation: TSFVec3f read FFdcenterOfRotation;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdjump: TSFBool;
    public property Fdjump: TSFBool read FFdjump;

    private FFdretainUserOffsets: TSFBool;
    public property FdretainUserOffsets: TSFBool read FFdretainUserOffsets;
  end;

  TNodeBillboard = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
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
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
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

  { LOD node.

    Compatibility node: this class is used for both VRML 97 and X3D.
    Although field name changed for X3D (in VRML 97, "children"
    was called "level"), this is handled seamlessly using our
    TVRMLFieldOrEvent.AddAlternativeField mechanism. In other words,
    programmer simply works with FdChildren field, and when reading/writing
    VRML file we take care to actually use either "children" or "level" name,
    depending on used VRML version. }
  TNodeLOD_2 = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdrange: TMFFloat;
    public property Fdrange: TMFFloat read FFdrange;

    private FFdforceTransitions: TSFBool;
    public property FdforceTransitions: TSFBool read FFdforceTransitions;

    { Event: SFInt32, out } { }
    private FEventlevel_changed: TVRMLEvent;
    public property Eventlevel_changed: TVRMLEvent read FEventlevel_changed;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function ChildrenField: TMFNode; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNavigationInfo = class(TNodeX3DBindableNode)
  private
    procedure EventSet_BindReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
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

    function Position: TSFVec3f; override;
    class function CameraKind: TVRMLCameraKind; override;
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

    function Position: TSFVec3f; override;
    class function CameraKind: TVRMLCameraKind; override;

    { This calculates proper angle of view for typical rectangular
      display, based on given fieldOfView field value.
      Result is in radians (just like fieldOfView VRML field).

      If you want to calculate horizontal angle of view then
      pass as ThisToOtherSizeRatio your window's width / height.
      If you want to calculate vertical angle of view then
      pass as ThisToOtherSizeRatio your window's height / width.
      For this method it doesn't really matter which is horizontal
      and which is vertical, both are treated the same.

      This works following VRML spec. So the angle of view for
      smaller window size is set to fieldOfViee. The other angle
      can always be calculated by AdjustViewAngleRadToAspectRatio
      (this implements the same equation that is mentioned in VRML spec).
      The larger angle cannot be larger than Pi, and may force the
      smaller angle to be smaller than fieldOfView. }
    function AngleOfView(const ThisToOtherSizeRatio: Single): Single;

    { This is like AngleOfView, but it allows you to specify
      FieldOfView as a parameter. }
    class function ViewpointAngleOfView(
      FieldOfView: Single;
      const ThisToOtherSizeRatio: Single): Single;
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
constructor TVRMLViewpointNode.Create(
  const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  { In X3D, this is part of X3DViewpointNode.
    In our engine, X3DViewpointNode descends from TVRMLViewpointNode
    inheriting "orientation" field this way. }
  FFdorientation := TSFRotation.Create(Self, 'orientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdorientation);
  { X3D specification comment: [-1,1],(-Inf,Inf) }

  FFddirection := TMFVec3f.Create(Self, 'direction', []);
  Fields.Add(FFddirection);

  FFdup := TMFVec3f.Create(Self, 'up', []);
  Fields.Add(FFdup);

  FFdgravityUp := TSFVec3f.Create(Self, 'gravityUp', StdVRMLGravityUp);
  Fields.Add(FFdgravityUp);

  Eventset_bind.OnReceive.AppendItem(@EventSet_BindReceive);
end;

procedure TVRMLViewpointNode.GetCameraVectors(
  const CamTransform: TMatrix4Single;
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single);
begin
  CamPos := Position.Value;

  if FdDirection.Items.Length > 0 then
  begin
    CamDir := FdDirection.Items.Items[0];
    if IsZeroVector(CamDir) then
    begin
      VRMLNonFatalError('Viewpoint "direction" must not be zero, assuming defaults');
      CamDir := FdOrientation.RotatedPoint( StdVRMLCamDir );
    end;
  end else
    CamDir := FdOrientation.RotatedPoint( StdVRMLCamDir );

  if FdUp.Items.Length > 0 then
  begin
    CamUp := FdUp.Items.Items[0];
    if IsZeroVector(CamUp) then
    begin
      VRMLNonFatalError('Viewpoint "up" must not be zero, assuming defaults');
      CamUp := FdOrientation.RotatedPoint( StdVRMLCamUp );
    end;
  end else
    CamUp := FdOrientation.RotatedPoint( StdVRMLCamUp );

  GravityUp := FdGravityUp.Value;
  if IsZeroVector(GravityUp) then
    GravityUp := StdVRMLGravityUp;

  { Niestety, macierz ponizej moze cos skalowac wiec nawet jesli powyzej
    uzylismy FdOrientation.RotatedPoint( StdVRMLCamDir/Up ) i wiemy ze CamDir/Up
    jest znormalizowane - to i tak musimy je tutaj znormalizowac.
    TODO: byloby dobrze uzyc tutaj czegos jak MultMatrixPointNoTranslationNoScale }
  CamPos := MultMatrixPoint(CamTransform, CamPos);
  CamDir := Normalized( MultMatrixPointNoTranslation(CamTransform, CamDir) );
  CamUp := Normalized( MultMatrixPointNoTranslation(CamTransform, CamUp) );
  GravityUp := Normalized( MultMatrixPointNoTranslation(CamTransform, GravityUp) );

  Assert(FloatsEqual(VectorLenSqr(CamDir), 1.0, 0.0001));
  Assert(FloatsEqual(VectorLenSqr(CamUp), 1.0, 0.0001));
end;

procedure TVRMLViewpointNode.EventSet_BindReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
begin
  if ParentEventsProcessor <> nil then
    (ParentEventsProcessor as TVRMLScene).
      ViewpointStack.Set_Bind(Self, (Value as TSFBool).Value);
end;

constructor TNodeX3DViewpointNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenterOfRotation := TSFVec3f.Create(Self, 'centerOfRotation', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenterOfRotation);
  { X3D specification comment: (-Inf,Inf) }

  FFddescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFddescription);

  FFdjump := TSFBool.Create(Self, 'jump', true);
  Fields.Add(FFdjump);

  FFdretainUserOffsets := TSFBool.Create(Self, 'retainUserOffsets', false);
  Fields.Add(FFdretainUserOffsets);

  DefaultContainerField := 'children';
end;

constructor TNodeBillboard.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdaxisOfRotation := TSFVec3f.Create(Self, 'axisOfRotation', Vector3Single(0, 1, 0));
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
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeBillboard.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeBillboard.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

constructor TNodeCollision.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  { In VRML 2.0, Collision didn't descent from X3DSensorName and had
    special field "collide". In X3D, "enabled" is used for the exact
    same purpose. }
  FFdenabled.AddAlternativeName('collide', 2);
  Fields.Add(FFdenabled);

  FEventcollideTime := TVRMLEvent.Create(Self, 'collideTime', TSFTime, false);
  Events.Add(FEventcollideTime);

  FEventisActive := TVRMLEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FFdproxy := TSFNode.Create(Self, 'proxy', INodeX3DChildNode);
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
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeCollision.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCollision.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

constructor TNodeLOD_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FdChildren.AddAlternativeName('level', 2);

  FFdcenter := TSFVec3f.Create(Self, 'center', ZeroVector3Single);
  FFdcenter.Exposed := false;
  Fields.Add(FFdcenter);

  FFdrange := TMFFloat.Create(Self, 'range', []);
  FFdrange.Exposed := false;
  Fields.Add(FFdrange);
  { X3D specification comment: [0,Inf) or -1 }

  FFdforceTransitions := TSFBool.Create(Self, 'forceTransitions', false);
  FFdforceTransitions.Exposed := false;
  Fields.Add(FFdforceTransitions);

  FEventlevel_changed := TVRMLEvent.Create(Self, 'level_changed', TSFInt32, false);
  Events.Add(FEventlevel_changed);

  DefaultContainerField := 'children';
end;

class function TNodeLOD_2.ClassNodeTypeName: string;
begin
  Result := 'LOD';
end;

class function TNodeLOD_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeLOD_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

function TNodeLOD_2.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

procedure TNodeLOD_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  { For now we simply always use the best LOD version,
    avoiding whole issue of choosing proper LOD child. }
  if FdChildren.Items.Count >= 1 then
    Func(Self, FdChildren.Items[0]);
end;

function TNodeLOD_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TNodeNavigationInfo.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdavatarSize := TMFFloat.Create(Self, 'avatarSize', [0.25, 1.6, 0.75]);
  Fields.Add(FFdavatarSize);
  { X3D specification comment: [0,Inf) }

  FFdheadlight := TSFBool.Create(Self, 'headlight', true);
  Fields.Add(FFdheadlight);

  FFdspeed := TSFFloat.Create(Self, 'speed', 1.0);
  Fields.Add(FFdspeed);
  { X3D specification comment: [0,Inf) }

  FFdtransitionTime := TSFTime.Create(Self, 'transitionTime', 1.0);
  Fields.Add(FFdtransitionTime);
  { X3D specification comment: [0, Inf) }

  FFdtransitionType := TMFString.Create(Self, 'transitionType', ['LINEAR']);
  Fields.Add(FFdtransitionType);
  { X3D specification comment: ["TELEPORT","LINEAR","ANIMATE",...] }

  FFdtype := TMFString.Create(Self, 'type', ['EXAMINE', 'ANY']);
  Fields.Add(FFdtype);
  { X3D specification comment: ["ANY","WALK","EXAMINE","FLY","LOOKAT","NONE",...] }

  FFdvisibilityLimit := TSFFloat.Create(Self, 'visibilityLimit', 0.0);
  Fields.Add(FFdvisibilityLimit);
  { X3D specification comment: [0,Inf) }

  FEventtransitionComplete := TVRMLEvent.Create(Self, 'transitionComplete', TSFBool, false);
  Events.Add(FEventtransitionComplete);

  DefaultContainerField := 'children';

  Eventset_bind.OnReceive.AppendItem(@EventSet_BindReceive);
end;

class function TNodeNavigationInfo.ClassNodeTypeName: string;
begin
  Result := 'NavigationInfo';
end;

function TNodeNavigationInfo.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 100);
end;

class function TNodeNavigationInfo.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeNavigationInfo.EventSet_BindReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
begin
  if ParentEventsProcessor <> nil then
    (ParentEventsProcessor as TVRMLScene).
      NavigationInfoStack.Set_Bind(Self, (Value as TSFBool).Value);
end;

constructor TNodeOrthoViewpoint.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfieldOfView := TMFFloat.Create(Self, 'fieldOfView', [-1, -1, 1, 1]);
  Fields.Add(FFdfieldOfView);
  { X3D specification comment:  (-Inf,Inf) }

  FFdposition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 10));
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

function TNodeOrthoViewpoint.Position: TSFVec3f;
begin
  Result := FdPosition;
end;

class function TNodeOrthoViewpoint.CameraKind: TVRMLCameraKind;
begin
  Result := ckOrthographic;
end;

constructor TNodeViewpoint.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfieldOfView := TSFFloat.Create(Self, 'fieldOfView', DefaultViewpointFieldOfView);
  Fields.Add(FFdfieldOfView);
  { X3D specification comment: (0,Pi) }

  FFdposition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 10));
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
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeViewpoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeViewpoint.Position: TSFVec3f;
begin
  Result := FdPosition;
end;

class function TNodeViewpoint.CameraKind: TVRMLCameraKind;
begin
  Result := ckPerspective;
end;

function TNodeViewpoint.AngleOfView(
  const ThisToOtherSizeRatio: Single): Single;
begin
  Result := ViewpointAngleOfView(FdFieldOfView.Value, ThisToOtherSizeRatio);
end;

class function TNodeViewpoint.ViewpointAngleOfView(
  FieldOfView: Single;
  const ThisToOtherSizeRatio: Single): Single;
var
  OtherAngle: Single;
begin
  Clamp(FieldOfView, 0.01, Pi - 0.01);

  if ThisToOtherSizeRatio < 1 then
  begin
    { So the resulting angle is the smaller one. }
    Result := FieldOfView;
    OtherAngle :=
      AdjustViewAngleRadToAspectRatio(Result, 1 / ThisToOtherSizeRatio);
    if OtherAngle > Pi then
      Result := AdjustViewAngleRadToAspectRatio(Pi, ThisToOtherSizeRatio);
  end else
  begin
    { So the resulting angle is the larger one. }
    OtherAngle := FieldOfView;
    Result :=
      AdjustViewAngleRadToAspectRatio(OtherAngle, ThisToOtherSizeRatio);
    if Result > Pi then
      Result := Pi;
  end;
end;

constructor TNodeViewpointGroup.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdchildren := TMFNode.Create(Self, 'children', [TNodeX3DViewpointNode, TNodeViewpointGroup]);
  Fields.Add(FFdchildren);

  FFddisplayed := TSFBool.Create(Self, 'displayed', true);
  Fields.Add(FFddisplayed);

  FFdsize := TSFVec3f.Create(Self, 'size', Vector3Single(0, 0, 0));
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
    TNodeLOD_2,
    TNodeNavigationInfo,
    TNodeOrthoViewpoint,
    TNodeViewpoint,
    TNodeViewpointGroup
  ]);
end;
{$endif read_implementation}
