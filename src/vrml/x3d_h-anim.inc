{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeHAnimDisplacer = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcoordIndex: TMFInt32;
    public property FdcoordIndex: TMFInt32 read FFdcoordIndex;

    private FFddisplacements: TMFVec3f;
    public property Fddisplacements: TMFVec3f read FFddisplacements;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdweight: TSFFloat;
    public property Fdweight: TSFFloat read FFdweight;
  end;

  TNodeHAnimHumanoid = class(TNodeX3DChildNode, INodeX3DBoundedObject, INodeTransform)
  private
    RestSkinCoord: TDynVector3SingleArray;
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure ParseAfter(Names: TVRMLNames); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdinfo: TMFString;
    public property Fdinfo: TMFString read FFdinfo;

    private FFdjoints: TMFNode;
    public property Fdjoints: TMFNode read FFdjoints;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdscaleOrientation: TSFRotation;
    public property FdscaleOrientation: TSFRotation read FFdscaleOrientation;

    private FFdsegments: TMFNode;
    public property Fdsegments: TMFNode read FFdsegments;

    private FFdsites: TMFNode;
    public property Fdsites: TMFNode read FFdsites;

    private FFdskeleton: TMFNode;
    public property Fdskeleton: TMFNode read FFdskeleton;

    private FFdskin: TMFNode;
    public property Fdskin: TMFNode read FFdskin;

    private FFdskinCoord: TSFNode;
    public property FdskinCoord: TSFNode read FFdskinCoord;

    private FFdskinNormal: TSFNode;
    public property FdskinNormal: TSFNode read FFdskinNormal;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;

    private FFdversion: TSFString;
    public property Fdversion: TSFString read FFdversion;

    private FFdviewpoints: TMFNode;
    public property Fdviewpoints: TMFNode read FFdviewpoints;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeHAnimJoint = class(TNodeX3DGroupingNode, INodeTransform)
  private
    HasRestTransform: boolean;
    RestTransform, RestInvertedTransform: TMatrix4Single;
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure ApplyTransform(State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFddisplacers: TMFNode;
    public property Fddisplacers: TMFNode read FFddisplacers;

    private FFdlimitOrientation: TSFRotation;
    public property FdlimitOrientation: TSFRotation read FFdlimitOrientation;

    private FFdllimit: TMFFloat;
    public property Fdllimit: TMFFloat read FFdllimit;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdscaleOrientation: TSFRotation;
    public property FdscaleOrientation: TSFRotation read FFdscaleOrientation;

    private FFdskinCoordIndex: TMFInt32;
    public property FdskinCoordIndex: TMFInt32 read FFdskinCoordIndex;

    private FFdskinCoordWeight: TMFFloat;
    public property FdskinCoordWeight: TMFFloat read FFdskinCoordWeight;

    private FFdstiffness: TMFFloat;
    public property Fdstiffness: TMFFloat read FFdstiffness;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;

    private FFdulimit: TMFFloat;
    public property Fdulimit: TMFFloat read FFdulimit;
  end;

  TNodeHAnimSegment = class(TNodeX3DGroupingNode, INodeTransform)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure ApplyTransform(State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdcenterOfMass: TSFVec3f;
    public property FdcenterOfMass: TSFVec3f read FFdcenterOfMass;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

    private FFddisplacers: TMFNode;
    public property Fddisplacers: TMFNode read FFddisplacers;

    private FFdmass: TSFFloat;
    public property Fdmass: TSFFloat read FFdmass;

    private FFdmomentsOfInertia: TMFFloat;
    public property FdmomentsOfInertia: TMFFloat read FFdmomentsOfInertia;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    { Segment.rotation, scale, translation are extensions of Blaxxun
      avatars, at least that's what
      [http://tecfa.unige.ch/vrml/objects/avatars/blaxxun/]
      suggests.
      They are not conforming to HAnim 1.0 or 200x specification. }

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;
  end;

  TNodeHAnimSite = class(TNodeX3DGroupingNode, INodeTransform)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure ApplyTransform(State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdscaleOrientation: TSFRotation;
    public property FdscaleOrientation: TSFRotation read FFdscaleOrientation;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeHAnimDisplacer.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcoordIndex := TMFInt32.Create(Self, 'coordIndex', []);
  Fields.Add(FFdcoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFddisplacements := TMFVec3f.Create(Self, 'displacements', []);
  Fields.Add(FFddisplacements);

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdweight := TSFFloat.Create(Self, 'weight', 0.0);
  Fields.Add(FFdweight);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'displacers';
end;

class function TNodeHAnimDisplacer.ClassNodeTypeName: string;
begin
  Result := 'HAnimDisplacer';
end;

function TNodeHAnimDisplacer.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeHAnimDisplacer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeHAnimHumanoid.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   Fdcenter.ChangesAlways := [chTransform];
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdinfo := TMFString.Create(Self, 'info', []);
  Fields.Add(FFdinfo);

  FFdjoints := TMFNode.Create(Self, 'joints', [TNodeHAnimJoint]);
  Fields.Add(FFdjoints);

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   Fdrotation.ChangesAlways := [chTransform];
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdscale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   Fdscale.ChangesAlways := [chTransform];
  Fields.Add(FFdscale);
  { X3D specification comment: (0,Inf) }

  FFdscaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
   FdscaleOrientation.ChangesAlways := [chTransform];
  Fields.Add(FFdscaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdsegments := TMFNode.Create(Self, 'segments', [TNodeHAnimSegment]);
  Fields.Add(FFdsegments);

  FFdsites := TMFNode.Create(Self, 'sites', [TNodeHAnimSite]);
  Fields.Add(FFdsites);

  FFdskeleton := TMFNode.Create(Self, 'skeleton', [TNodeHAnimJoint, TNodeHAnimSite]);
  Fields.Add(FFdskeleton);

  { HAnim 1.1 specificaion uses "humanoidBody" instead of "skeleton",
    also HAnim 200x specification examples show usage of this (although
    HAnim 200x specs talks only about "skeleton"). }
  Fdskeleton.AddAlternativeName('humanoidBody', 0);

  FFdskin := TMFNode.Create(Self, 'skin', [TNodeX3DChildNode]);
  Fields.Add(FFdskin);

  FFdskinCoord := TSFNode.Create(Self, 'skinCoord', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdskinCoord);

  FFdskinNormal := TSFNode.Create(Self, 'skinNormal', [TNodeX3DNormalNode]);
  Fields.Add(FFdskinNormal);

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   Fdtranslation.ChangesAlways := [chTransform];
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdversion := TSFString.Create(Self, 'version', '');
  Fields.Add(FFdversion);

  FFdviewpoints := TMFNode.Create(Self, 'viewpoints', [
    { X3D specificaion indicates that TNodeHAnimSite is allowed here,
      but textual description in HAnim specification indicates that
      it's an error: viewpoints should be allowed here.
      Confirmed by testing models from
      http://www.web3d.org/x3d/content/examples/Basic/HumanoidAnimation/

      To be on the safest side, I allow both TNodeHAnimSite and viewpoint
      classes.  }
    TNodeHAnimSite,
    TNodeX3DViewpointNode, TNodeViewpointGroup ]);
  Fields.Add(FFdviewpoints);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

destructor TNodeHAnimHumanoid.Destroy;
begin
  FreeAndNil(RestSkinCoord);
  inherited;
end;

class function TNodeHAnimHumanoid.ClassNodeTypeName: string;
begin
  Result := 'HAnimHumanoid';
end;

function TNodeHAnimHumanoid.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeHAnimHumanoid.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeHAnimHumanoid.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  { Between HAnim 1.0 and 200x hierarchy changed, in 1.0 there's no "skeleton"
    field but root Joint is just simply placed directly in VRML file.
    This means that I should simply render Joint node like a Transform,
    both inside "skeleton" field and outside, and both versions will be happy. }
  FdSkeleton.EnumerateValid(Func);

  { To support skinned body, pass skin as an active node too
    (if not nil, then it will be rendered.)
    Note that SkinCoord and SkinNormal are not passed as active
    (they should be referenced inside Skin object). }
  FdSkin.EnumerateValid(Func);

  FdViewpoints.EnumerateValid(Func);

  { Note that joints, segments and sites is not passed to Func --- they
    should already be part of Joint hierarchy (in skeleton for HAnim 200x
    or outside of Humanoid node for HAnim 1.0). }

  { Finally, the way we implement HAnimHumanoid is compatible with X3D
    prototypes for this in
    http://www.web3d.org/x3d/content/examples/Basic/HumanoidAnimation/BoxMan.wrl
    Which means that comments above are probably true, as I'm not the
    only one using this approach. }
end;

procedure TNodeHAnimHumanoid.BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;
  StateStack.Push;

  StateStack.Top.Humanoid := Self;
  StateStack.Top.HumanoidTransform := IdentityMatrix4Single;
  StateStack.Top.HumanoidInvertedTransform := IdentityMatrix4Single;

  TransformState(StateStack.Top, Fdcenter, Fdrotation, Fdscale, FdscaleOrientation,
    Fdtranslation);
end;

procedure TNodeHAnimHumanoid.AfterTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  StateStack.Pop;
  inherited;
end;

function TNodeHAnimHumanoid.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

procedure TNodeHAnimHumanoid.ParseAfter(Names: TVRMLNames);
begin
  inherited;

  { update RestSkinCoord }
  FreeAndNil(RestSkinCoord);
  if (FdSkinCoord.Value <> nil) and
     (FdSkinCoord.Value is TNodeCoordinate) then
  begin
    RestSkinCoord := TDynVector3SingleArray.Create;
    RestSkinCoord.Assign(TNodeCoordinate(FdSkinCoord.Value).FdPoint.Items);
  end;
end;

constructor TNodeHAnimJoint.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   Fdcenter.ChangesAlways := [chTransform];
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFddisplacers := TMFNode.Create(Self, 'displacers', [TNodeHAnimDisplacer]);
  Fields.Add(FFddisplacers);

  FFdlimitOrientation := TSFRotation.Create(Self, 'limitOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdlimitOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdllimit := TMFFloat.Create(Self, 'llimit', []);
  Fields.Add(FFdllimit);
  { X3D specification comment: (-Inf,Inf) }

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   Fdrotation.ChangesAlways := [chTransform];
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdscale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   Fdscale.ChangesAlways := [chTransform];
  Fields.Add(FFdscale);
  { X3D specification comment: (0,Inf) }

  FFdscaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
   FdscaleOrientation.ChangesAlways := [chTransform];
  Fields.Add(FFdscaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdskinCoordIndex := TMFInt32.Create(Self, 'skinCoordIndex', []);
  Fields.Add(FFdskinCoordIndex);

  FFdskinCoordWeight := TMFFloat.Create(Self, 'skinCoordWeight', []);
  Fields.Add(FFdskinCoordWeight);

  FFdstiffness := TMFFloat.Create(Self, 'stiffness', [0, 0, 0]);
  Fields.Add(FFdstiffness);
  { X3D specification comment: [0,1] }

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   Fdtranslation.ChangesAlways := [chTransform];
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdulimit := TMFFloat.Create(Self, 'ulimit', []);
  Fields.Add(FFdulimit);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeHAnimJoint.ClassNodeTypeName: string;
begin
  Result := 'HAnimJoint';
end;

function TNodeHAnimJoint.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeHAnimJoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeHAnimJoint.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  FdChildren.EnumerateValid(Func);
end;

procedure TNodeHAnimJoint.ApplyTransform(State: TVRMLGraphTraverseState);

  { Animate skin along with this joint. Returns if something actually changed. }
  function AnimateSkin(
    CurrentSkinCoord, RestSkinCoord: TDynVector3SingleArray): boolean;
  var
    Local: TVector4Single;
    I, CoordIndex: Integer;
  begin
    Result := false;

    if CurrentSkinCoord.Count <> RestSkinCoord.Count then
    begin
      if Log then
        WritelnLog('Humanoid', 'Cannot animate humanoid: skid coord count changed. Submit a bug with example model to have this fixed.');
      { fix is to update RestSkinCoord more often, like at every count
        change of HAnimHumanoid.skin or skin contents. }
      Exit;
    end;

    Result := true;

    for I := 0 to FdSkinCoordIndex.Count - 1 do
    begin
      CoordIndex := FdSkinCoordIndex.Items[I];
      if (CoordIndex < 0) or
         (CoordIndex >= RestSkinCoord.Count) then
      begin
        VRMLWarning(vwSerious, Format('HAnimJoint.skinCoord references not existing coordinate index %d (while we have only %d skin coordinates in HAnimHumanoid.skin)',
          [CoordIndex, RestSkinCoord.Count]));
        Continue;
      end;

      { get vertex rest position in local Joint coordinate space }
      Local := State.HumanoidInvertedTransform *
        Vector4Single(RestSkinCoord.Items[CoordIndex]);

      { our new State.HumanoidTransform is RestTransform * TransformChange.
        So TransformChange = RestInvertedTransform * State.HumanoidTransform.
        Transform local by TransformChange (we don't want to apply
        RestTransform again, as this transformation was already encoded
        in initial vertex position). }
      Local := RestInvertedTransform * State.HumanoidTransform * Local;

      { get vertex position back in Humanoid coordinate space }
      { TODO: make VRMLWarning when this fails with div by zero }
      CurrentSkinCoord.Items[CoordIndex] :=
        Vector3SinglePoint(State.HumanoidTransform * Local);
    end;
  end;

var
  IgnoreHumanoidTransformScale: Single;
begin
  inherited;
  TransformState(State, Fdcenter, Fdrotation, Fdscale, FdscaleOrientation,
    Fdtranslation);

  { update State.HumanoidTransform, State.HumanoidInvertedTransform. }
  IgnoreHumanoidTransformScale := 1;
  TransformMatrices(State.HumanoidTransform, State.HumanoidInvertedTransform,
    IgnoreHumanoidTransformScale,
    Fdcenter, Fdrotation, Fdscale, FdscaleOrientation, Fdtranslation);

  if not HasRestTransform then
  begin
    RestTransform := State.HumanoidTransform;
    RestInvertedTransform := State.HumanoidInvertedTransform;
    HasRestTransform := true;
  end else
  if (FdSkinCoordIndex.Count <> 0) and
     (State.Humanoid <> nil) and
     (State.Humanoid.FdSkinCoord.Value <> nil) and
     (State.Humanoid.FdSkinCoord.Value is TNodeCoordinate) and
     (State.Humanoid.RestSkinCoord <> nil) then
  begin
    if AnimateSkin(
      TNodeCoordinate(State.Humanoid.FdSkinCoord.Value).FdPoint.Items,
                      State.Humanoid.RestSkinCoord) then
      TNodeCoordinate(State.Humanoid.FdSkinCoord.Value).FdPoint.Changed;
  end;
end;

function TNodeHAnimJoint.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

constructor TNodeHAnimSegment.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenterOfMass := TSFVec3f.Create(Self, 'centerOfMass', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenterOfMass);
  { X3D specification comment: (-Inf,Inf) }

  FFdcoord := TSFNode.Create(Self, 'coord', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcoord);

  FFddisplacers := TMFNode.Create(Self, 'displacers', [TNodeHAnimDisplacer]);
  Fields.Add(FFddisplacers);

  FFdmass := TSFFloat.Create(Self, 'mass', 0);
  Fields.Add(FFdmass);
  { X3D specification comment: [0,Inf) }

  FFdmomentsOfInertia := TMFFloat.Create(Self, 'momentsOfInertia', [0, 0, 0, 0, 0, 0, 0, 0, 0]);
  Fields.Add(FFdmomentsOfInertia);
  { X3D specification comment: [0,Inf) }

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   Fdrotation.ChangesAlways := [chTransform];
  Fields.Add(FFdrotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdscale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   Fdscale.ChangesAlways := [chTransform];
  Fields.Add(FFdscale);
  { X3D specification comment: (-Inf, Inf) }

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   Fdtranslation.ChangesAlways := [chTransform];
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeHAnimSegment.ClassNodeTypeName: string;
begin
  Result := 'HAnimSegment';
end;

function TNodeHAnimSegment.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeHAnimSegment.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeHAnimSegment.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  FdChildren.EnumerateValid(Func);
end;

procedure TNodeHAnimSegment.ApplyTransform(State: TVRMLGraphTraverseState);
begin
  inherited;
  SimpleTransformState(State, Fdrotation, Fdscale, Fdtranslation);
end;

function TNodeHAnimSegment.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

constructor TNodeHAnimSite.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   Fdcenter.ChangesAlways := [chTransform];
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   Fdrotation.ChangesAlways := [chTransform];
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdscale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   Fdscale.ChangesAlways := [chTransform];
  Fields.Add(FFdscale);
  { X3D specification comment: (0,Inf) }

  FFdscaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
   FdscaleOrientation.ChangesAlways := [chTransform];
  Fields.Add(FFdscaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   Fdtranslation.ChangesAlways := [chTransform];
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  DefaultContainerField := 'children';
end;

class function TNodeHAnimSite.ClassNodeTypeName: string;
begin
  Result := 'HAnimSite';
end;

function TNodeHAnimSite.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeHAnimSite.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeHAnimSite.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  FdChildren.EnumerateValid(Func);
end;

procedure TNodeHAnimSite.ApplyTransform(State: TVRMLGraphTraverseState);
begin
  inherited;
  TransformState(State, Fdcenter, Fdrotation, Fdscale, FdscaleOrientation,
    Fdtranslation);
end;

function TNodeHAnimSite.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

procedure RegisterHAnimNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeHAnimDisplacer,
    TNodeHAnimHumanoid,
    TNodeHAnimJoint,
    TNodeHAnimSegment,
    TNodeHAnimSite
  ]);
end;

{$endif read_implementation}
