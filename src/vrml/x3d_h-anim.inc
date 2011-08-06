{
  Copyright 2008-2011 Michalis Kamburelis.

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
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;

    private FFdDisplacements: TMFVec3f;
    public property FdDisplacements: TMFVec3f read FFdDisplacements;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    private FFdWeight: TSFFloat;
    public property FdWeight: TSFFloat read FFdWeight;
  end;

  TNodeHAnimJoint = class;

  TVertexJoints = object
    RestPosition: TVector3Single;
    Count: Cardinal;
    Joints: array of TNodeHAnimJoint;
    Weights: array of Single;
    procedure Add(Joint: TNodeHAnimJoint; const Weight: Single);
  end;
  PVertexJoints = ^TVertexJoints;
  TDynVertexJointsArray = specialize TGenericStructList<TVertexJoints>;

  TNodeHAnimHumanoid = class(TNodeX3DChildNode, INodeX3DBoundedObject, INodeTransform)
  private
    VertexJoints: TDynVertexJointsArray;
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure ParseAfter(Names: TVRMLNames); override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdInfo: TMFString;
    public property FdInfo: TMFString read FFdInfo;

    private FFdJoints: TMFNode;
    public property FdJoints: TMFNode read FFdJoints;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    private FFdSegments: TMFNode;
    public property FdSegments: TMFNode read FFdSegments;

    private FFdSites: TMFNode;
    public property FdSites: TMFNode read FFdSites;

    private FFdSkeleton: TMFNode;
    public property FdSkeleton: TMFNode read FFdSkeleton;

    private FFdSkin: TMFNode;
    public property FdSkin: TMFNode read FFdSkin;

    private FFdSkinCoord: TSFNode;
    public property FdSkinCoord: TSFNode read FFdSkinCoord;

    private FFdSkinNormal: TSFNode;
    public property FdSkinNormal: TSFNode read FFdSkinNormal;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    private FFdVersion: TSFString;
    public property FdVersion: TSFString read FFdVersion;

    private FFdViewpoints: TMFNode;
    public property FdViewpoints: TMFNode read FFdViewpoints;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    { Change the skin (in skinCoord), to follow the animation of joints.
      Joints that changed are assumed to be already traversed by TVRMLNode.Traverse
      (this prepares field values for AnimateSkinPoint call).

      This method is used by TVRMLScene. Don't call this yourself.

      Returns changed VRML/X3D coordinates field (or @nil if there is no skin).
      You should call TVRMLField.Changed on it (unless you use it from some
      TVRMLScene initialization, when there's no point in notifying about this). }
    function AnimateSkin: TMFVec3f;
  end;

  TNodeHAnimJoint = class(TNodeX3DGroupingNode, INodeTransform)
  private
    AnimateSkinTransform: TMatrix4Single;
    RestCenter: TVector3Single;
    { Animate point, based on it's rest position.

      We used, at one point, to apply here a relative change since last position.
      - Advantage: no need to remember
        TVertexJoints.RestPosition or RestCenter.
      - Disadvantage: cummulation of errors was causing shrinking elbows
        on seamless3d test model Lucy.
      - Disadvantage 2: Relative change was causing
        more complicated code, with the need to switch previous/new
        transformations (since we cannot apply changes to vertexes during
        traversing of joints, we had to be very careful: a change must
        be applied only once, and then relative change must be reset to identity).
        Without this, we can repply AnimateSkinPoint many times, and it's
        always valid. }
    function AnimateSkinPoint(const Point: TVector3Single): TVector3Single;
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure ApplyTransform(State: TVRMLGraphTraverseState); override;
    procedure ParseAfter(Names: TVRMLNames); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdDisplacers: TMFNode;
    public property FdDisplacers: TMFNode read FFdDisplacers;

    private FFdLimitOrientation: TSFRotation;
    public property FdLimitOrientation: TSFRotation read FFdLimitOrientation;

    private FFdLlimit: TMFFloat;
    public property FdLlimit: TMFFloat read FFdLlimit;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    private FFdSkinCoordIndex: TMFInt32;
    public property FdSkinCoordIndex: TMFInt32 read FFdSkinCoordIndex;

    private FFdSkinCoordWeight: TMFFloat;
    public property FdSkinCoordWeight: TMFFloat read FFdSkinCoordWeight;

    private FFdStiffness: TMFFloat;
    public property FdStiffness: TMFFloat read FFdStiffness;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    private FFdUlimit: TMFFloat;
    public property FdUlimit: TMFFloat read FFdUlimit;
  end;

  TNodeHAnimSegment = class(TNodeX3DGroupingNode, INodeTransform)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure ApplyTransform(State: TVRMLGraphTraverseState); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdCenterOfMass: TSFVec3f;
    public property FdCenterOfMass: TSFVec3f read FFdCenterOfMass;

    private FFdCoord: TSFNode;
    public property FdCoord: TSFNode read FFdCoord;

    private FFdDisplacers: TMFNode;
    public property FdDisplacers: TMFNode read FFdDisplacers;

    private FFdMass: TSFFloat;
    public property FdMass: TSFFloat read FFdMass;

    private FFdMomentsOfInertia: TMFFloat;
    public property FdMomentsOfInertia: TMFFloat read FFdMomentsOfInertia;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    { Segment.rotation, scale, translation are extensions of Blaxxun
      avatars, at least that's what
      [http://tecfa.unige.ch/vrml/objects/avatars/blaxxun/]
      suggests.
      They are not conforming to HAnim 1.0 or 200x specification. }

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;
  end;

  TNodeHAnimSite = class(TNodeX3DGroupingNode, INodeTransform)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure ApplyTransform(State: TVRMLGraphTraverseState); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TNodeHAnimDisplacer.CreateNode;
begin
  inherited;

  FFdCoordIndex := TMFInt32.Create(Self, 'coordIndex', []);
  Fields.Add(FFdCoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdDisplacements := TMFVec3f.Create(Self, 'displacements', []);
  Fields.Add(FFdDisplacements);

  FFdName := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdName);

  FFdWeight := TSFFloat.Create(Self, 'weight', 0.0);
  Fields.Add(FFdWeight);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'displacers';
end;

class function TNodeHAnimDisplacer.ClassNodeTypeName: string;
begin
  Result := 'HAnimDisplacer';
end;

class function TNodeHAnimDisplacer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TVertexJoints.Add(Joint: TNodeHAnimJoint; const Weight: Single);
begin
  Inc(Count);
  SetLength(Joints , Count);
  SetLength(Weights, Count);
  Joints [Count - 1] := Joint;
  Weights[Count - 1] := Weight;
end;

procedure TNodeHAnimHumanoid.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   FdCenter.ChangesAlways := [chTransform];
  Fields.Add(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdInfo := TMFString.Create(Self, 'info', []);
  Fields.Add(FFdInfo);

  FFdJoints := TMFNode.Create(Self, 'joints', [TNodeHAnimJoint]);
  Fields.Add(FFdJoints);

  FFdName := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdName);

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   FdRotation.ChangesAlways := [chTransform];
  Fields.Add(FFdRotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdScale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   FdScale.ChangesAlways := [chTransform];
  Fields.Add(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
   FdScaleOrientation.ChangesAlways := [chTransform];
  Fields.Add(FFdScaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdSegments := TMFNode.Create(Self, 'segments', [TNodeHAnimSegment]);
  Fields.Add(FFdSegments);

  FFdSites := TMFNode.Create(Self, 'sites', [TNodeHAnimSite]);
  Fields.Add(FFdSites);

  FFdSkeleton := TMFNode.Create(Self, 'skeleton', [TNodeHAnimJoint, TNodeHAnimSite]);
  Fields.Add(FFdSkeleton);

  { HAnim 1.1 specificaion uses "humanoidBody" instead of "skeleton",
    also HAnim 200x specification examples show usage of this (although
    HAnim 200x specs talks only about "skeleton"). }
  Fdskeleton.AddAlternativeName('humanoidBody', 0);

  FFdSkin := TMFNode.Create(Self, 'skin', [TNodeX3DChildNode]);
  Fields.Add(FFdSkin);

  FFdSkinCoord := TSFNode.Create(Self, 'skinCoord', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdSkinCoord);

  FFdSkinNormal := TSFNode.Create(Self, 'skinNormal', [TNodeX3DNormalNode]);
  Fields.Add(FFdSkinNormal);

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   FdTranslation.ChangesAlways := [chTransform];
  Fields.Add(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdVersion := TSFString.Create(Self, 'version', '');
  Fields.Add(FFdVersion);

  FFdViewpoints := TMFNode.Create(Self, 'viewpoints', [
    { X3D specificaion indicates that TNodeHAnimSite is allowed here,
      but textual description in HAnim specification indicates that
      it's an error: viewpoints should be allowed here.
      Confirmed by testing models from
      http://www.web3d.org/x3d/content/examples/Basic/HumanoidAnimation/

      To be on the safest side, I allow both TNodeHAnimSite and viewpoint
      classes.  }
    TNodeHAnimSite,
    TNodeX3DViewpointNode, TNodeViewpointGroup ]);
  Fields.Add(FFdViewpoints);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdBboxCenter.Exposed := false;
  Fields.Add(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdBboxSize.Exposed := false;
  Fields.Add(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

destructor TNodeHAnimHumanoid.Destroy;
begin
  FreeAndNil(VertexJoints);
  inherited;
end;

class function TNodeHAnimHumanoid.ClassNodeTypeName: string;
begin
  Result := 'HAnimHumanoid';
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

  TransformState(StateStack.Top, Fdcenter.Value, Fdrotation.Value,
    Fdscale.Value, FdscaleOrientation.Value, Fdtranslation.Value);
end;

function TNodeHAnimHumanoid.AnimateSkin: TMFVec3f;

  function UpdateVertex(const VJ: TVertexJoints): TVector3Single;
  var
    New: TVector3Single;
    J: Integer;
    WeightSum: Single;
  begin
    New := ZeroVector3Single;
    WeightSum := 0;

    for J := 0 to VJ.Count - 1 do
    begin
      WeightSum += VJ.Weights[J];
      New += VJ.Joints[J].AnimateSkinPoint(VJ.RestPosition) * VJ.Weights[J];
    end;

    { Take the possibility of non-1 weight sum into account.
      When no joints affect this point, WeightSum = 0, this also works then. }
    Result := VJ.RestPosition * (1 - WeightSum) + New;
  end;

var
  I: Integer;
  SkinCoord: TDynVector3SingleArray;
begin
  if not (
    (FdSkinCoord.Value <> nil) and
    (FdSkinCoord.Value is TNodeCoordinate) and
    (VertexJoints <> nil)) then Exit(nil);

  Result := TNodeCoordinate(FdSkinCoord.Value).FdPoint;
  SkinCoord := Result.Items;
  if SkinCoord.Count <> VertexJoints.Count then Exit(nil);

  for I := 0 to VertexJoints.Count - 1 do
    SkinCoord.List^[I] := UpdateVertex(VertexJoints.List^[I]);
end;

procedure TNodeHAnimHumanoid.AfterTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  AnimateSkin;
  StateStack.Pop;
  inherited;
end;

procedure TNodeHAnimHumanoid.ParseAfter(Names: TVRMLNames);

  procedure CalculateVertexJoints;
  var
    SkinCoord: TDynVector3SingleArray;
    I, J: Integer;
    SkinCoordIndex: TDynLongIntArray;
    CoordIndex: Integer;
    Weight: Single;
    Joint: TNodeHAnimJoint;
  begin
    FreeAndNil(VertexJoints);
    if (FdSkinCoord.Value <> nil) and
       (FdSkinCoord.Value is TNodeCoordinate) then
    begin
      SkinCoord := TNodeCoordinate(FdSkinCoord.Value).FdPoint.Items;
      VertexJoints := TDynVertexJointsArray.Create;
      VertexJoints.Count := SkinCoord.Count;

      for I := 0 to SkinCoord.Count - 1 do
      begin
        VertexJoints.List^[I].RestPosition := SkinCoord.List^[I];
        VertexJoints.List^[I].Count := 0; { initially }
      end;

      for I := 0 to FdJoints.Count - 1 do
        if FdJoints[I] is TNodeHAnimJoint then
        begin
          Joint := TNodeHAnimJoint(FdJoints[I]);
          SkinCoordIndex := Joint.FdSkinCoordIndex.Items;
          for J := 0 to SkinCoordIndex.Count - 1 do
          begin
            CoordIndex := SkinCoordIndex.Items[J];
            if (CoordIndex < 0) or
               (CoordIndex >= SkinCoord.Count) then
            begin
              OnWarning(wtMajor, 'VRML/X3D', Format('HAnimJoint.skinCoord references not existing coordinate index %d (while we have only %d skin coordinates in HAnimHumanoid.skin)',
                [CoordIndex, SkinCoord.Count]));
              Continue;
            end;

            if J < Joint.FdSkinCoordWeight.Items.Count then
              Weight := Joint.FdSkinCoordWeight.Items[J] else
              Weight := 1;

            VertexJoints.List^[CoordIndex].Add(Joint, Weight);
          end;
        end;
    end;
  end;

begin
  inherited;
  CalculateVertexJoints;
end;

function TNodeHAnimHumanoid.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

procedure TNodeHAnimJoint.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   FdCenter.ChangesAlways := [chTransform];
  Fields.Add(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdDisplacers := TMFNode.Create(Self, 'displacers', [TNodeHAnimDisplacer]);
  Fields.Add(FFdDisplacers);

  FFdLimitOrientation := TSFRotation.Create(Self, 'limitOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdLimitOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdLlimit := TMFFloat.Create(Self, 'llimit', []);
  Fields.Add(FFdLlimit);
  { X3D specification comment: (-Inf,Inf) }

  FFdName := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdName);

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   FdRotation.ChangesAlways := [chTransform];
  Fields.Add(FFdRotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdScale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   FdScale.ChangesAlways := [chTransform];
  Fields.Add(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
   FdScaleOrientation.ChangesAlways := [chTransform];
  Fields.Add(FFdScaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdSkinCoordIndex := TMFInt32.Create(Self, 'skinCoordIndex', []);
  Fields.Add(FFdSkinCoordIndex);

  FFdSkinCoordWeight := TMFFloat.Create(Self, 'skinCoordWeight', []);
  Fields.Add(FFdSkinCoordWeight);

  FFdStiffness := TMFFloat.Create(Self, 'stiffness', [0, 0, 0]);
  Fields.Add(FFdStiffness);
  { X3D specification comment: [0,1] }

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   FdTranslation.ChangesAlways := [chTransform];
  Fields.Add(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdUlimit := TMFFloat.Create(Self, 'ulimit', []);
  Fields.Add(FFdUlimit);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';

  AnimateSkinTransform := IdentityMatrix4Single;
end;

class function TNodeHAnimJoint.ClassNodeTypeName: string;
begin
  Result := 'HAnimJoint';
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

function TNodeHAnimJoint.AnimateSkinPoint(const Point: TVector3Single): TVector3Single;
begin
  Result := MatrixMultPoint(AnimateSkinTransform, Point);
end;

procedure TNodeHAnimJoint.ParseAfter(Names: TVRMLNames);
begin
  inherited;
  RestCenter := FdCenter.Value;
end;

procedure TNodeHAnimJoint.ApplyTransform(State: TVRMLGraphTraverseState);
var
  IgnoreHumanoidTransformScale: Single;
  I: Integer;
  ChildJoint: TNodeHAnimJoint;
  LocalCenter, NewChildCenter: TVector3Single;
begin
  inherited;

  { Spec says center / scale / scaleOrientation / translation
    are relative to humanoid root, but rotations are accumulated.
    http://h-anim.org/Specifications/H-Anim200x/ISO_IEC_FCD_19774/ObjectInterfaces.html
    So rotations are affected by center/etc. of joints above,
    which means that I have to apply here LocalCenter, LocalScale and such?
    But how? TODO: for now, we only apply LocalCenter.

    Fortunately, normal State isn't really used for skinned animation. }

  LocalCenter := MatrixMultPoint(State.HumanoidInvertedTransform, FdCenter.Value);
  TransformState(State, LocalCenter, Fdrotation.Value,
    Fdscale.Value, FdscaleOrientation.Value, Fdtranslation.Value);

  { update State.HumanoidTransform, State.HumanoidInvertedTransform
    for child joints. }
  IgnoreHumanoidTransformScale := 1;
  TransformMatrices(State.HumanoidTransform, State.HumanoidInvertedTransform,
    IgnoreHumanoidTransformScale,
    LocalCenter, Fdrotation.Value, Fdscale.Value,
    FdscaleOrientation.Value, Fdtranslation.Value);

  AnimateSkinTransform := State.HumanoidTransform;

  { animate centers of children joints }
  for I := 0 to FdChildren.Count - 1 do
    if FdChildren[I] is TNodeHAnimJoint then
    begin
      ChildJoint := TNodeHAnimJoint(FdChildren[I]);

      { TODO: it's a little hack to update ChildJoint.FdCenter this way.
        The cleaner way would be to just

          ChildJoint.FdCenter.Send(AnimateSkinPoint(ChildJoint.RestCenter))

        This would already do compare, send event if needed etc. (useful
        for view3dscene H-Anim joints visualization).
        But it would also notify the scene (events processor), and this causes
        slowdown and bugs (on lucy) right now. }

      NewChildCenter := AnimateSkinPoint(ChildJoint.RestCenter);
      if not VectorsPerfectlyEqual(ChildJoint.FdCenter.Value, NewChildCenter) then
      begin
        ChildJoint.FdCenter.Value := NewChildCenter;
        if ChildJoint.FdCenter.Exposed then
          ChildJoint.FdCenter.EventOut.Send(NewChildCenter);

        { Increasing here PlusTicks is a little hack.
          If you'll next want to traverse a parent joint,
          this center will again be changed, and should be send (otherwise
          routes e.g. in Humanoid Visualization in view3dscene will not work). }
        if Scene <> nil then
          Scene.IncreaseTimeTick;
      end;
    end;
end;

function TNodeHAnimJoint.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

procedure TNodeHAnimSegment.CreateNode;
begin
  inherited;

  FFdCenterOfMass := TSFVec3f.Create(Self, 'centerOfMass', Vector3Single(0, 0, 0));
  Fields.Add(FFdCenterOfMass);
  { X3D specification comment: (-Inf,Inf) }

  FFdCoord := TSFNode.Create(Self, 'coord', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdCoord);

  FFdDisplacers := TMFNode.Create(Self, 'displacers', [TNodeHAnimDisplacer]);
  Fields.Add(FFdDisplacers);

  FFdMass := TSFFloat.Create(Self, 'mass', 0);
  Fields.Add(FFdMass);
  { X3D specification comment: [0,Inf) }

  FFdMomentsOfInertia := TMFFloat.Create(Self, 'momentsOfInertia', [0, 0, 0, 0, 0, 0, 0, 0, 0]);
  Fields.Add(FFdMomentsOfInertia);
  { X3D specification comment: [0,Inf) }

  FFdName := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdName);

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   FdRotation.ChangesAlways := [chTransform];
  Fields.Add(FFdRotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   FdScale.ChangesAlways := [chTransform];
  Fields.Add(FFdScale);
  { X3D specification comment: (-Inf, Inf) }

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   FdTranslation.ChangesAlways := [chTransform];
  Fields.Add(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeHAnimSegment.ClassNodeTypeName: string;
begin
  Result := 'HAnimSegment';
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
  TransformState(State, ZeroVector3Single, Fdrotation.Value,
    Fdscale.Value, ZeroVector4Single, Fdtranslation.Value);
end;

function TNodeHAnimSegment.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

procedure TNodeHAnimSite.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   FdCenter.ChangesAlways := [chTransform];
  Fields.Add(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdName := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdName);

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   FdRotation.ChangesAlways := [chTransform];
  Fields.Add(FFdRotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdScale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   FdScale.ChangesAlways := [chTransform];
  Fields.Add(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
   FdScaleOrientation.ChangesAlways := [chTransform];
  Fields.Add(FFdScaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   FdTranslation.ChangesAlways := [chTransform];
  Fields.Add(FFdTranslation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  DefaultContainerField := 'children';
end;

class function TNodeHAnimSite.ClassNodeTypeName: string;
begin
  Result := 'HAnimSite';
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
  TransformState(State, Fdcenter.Value, Fdrotation.Value,
    Fdscale.Value, FdscaleOrientation.Value, Fdtranslation.Value);
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
