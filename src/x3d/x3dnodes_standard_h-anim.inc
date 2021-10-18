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
  THAnimSegmentNode = class;
  THAnimSiteNode = class;

  { Alter the shape of coordinate-based geometry within parent H-Anim nodes.
    For example, different HAnimDisplacer may be used for each facial expression. }
  THAnimDisplacerNode = class(TAbstractGeometricPropertyNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;

    strict private FFdDisplacements: TMFVec3f;
    public property FdDisplacements: TMFVec3f read FFdDisplacements;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdWeight: TSFFloat;
    public property FdWeight: TSFFloat read FFdWeight;

    {$I auto_generated_node_helpers/x3dnodes_hanimdisplacer.inc}
  end;

  THAnimJointNode = class;

  TVertexJoints = object
    RestPosition: TVector3;
    Count: Cardinal;
    Joints: array of THAnimJointNode;
    Weights: array of Single;
    procedure Add(Joint: THAnimJointNode; const Weight: Single);
  end;
  PVertexJoints = ^TVertexJoints;
  TVertexJointsList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TVertexJoints>;

  { Central node for moving the animated humanoid. It contains the skeleton,
    and other information necessary for animation. }
  THAnimHumanoidNode = class(TAbstractChildNode)
  strict private
    type
      THAnimHumanoidNodeTransformFunctionality = class(TTransformFunctionality)
      strict private
        FParent: THAnimHumanoidNode;
      public
        constructor Create(const AParent: THAnimHumanoidNode);
        property Parent: THAnimHumanoidNode read FParent;
        procedure ApplyTransform(var Transformation: TTransformation); override;
      end;
    var
      FTransformFunctionality: THAnimHumanoidNodeTransformFunctionality;
      VertexJoints: TVertexJointsList;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure ParseAfter(Reader: TX3DReaderNames); override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;
    function TransformationChange: TNodeTransformationChange; override;

    strict private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    strict private FFdInfo: TMFString;
    public property FdInfo: TMFString read FFdInfo;

    strict private FFdJoints: TMFNode;
    public property FdJoints: TMFNode read FFdJoints;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    strict private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    strict private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    strict private FFdSegments: TMFNode;
    public property FdSegments: TMFNode read FFdSegments;

    strict private FFdSites: TMFNode;
    public property FdSites: TMFNode read FFdSites;

    strict private FFdSkeleton: TMFNode;
    public property FdSkeleton: TMFNode read FFdSkeleton;

    strict private FFdSkin: TMFNode;
    public property FdSkin: TMFNode read FFdSkin;

    strict private FFdSkinCoord: TSFNode;
    public property FdSkinCoord: TSFNode read FFdSkinCoord;

    strict private FFdSkinNormal: TSFNode;
    public property FdSkinNormal: TSFNode read FFdSkinNormal;

    strict private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    strict private FFdVersion: TSFString;
    public property FdVersion: TSFString read FFdVersion;

    strict private FFdViewpoints: TMFNode;
    public property FdViewpoints: TMFNode read FFdViewpoints;

    strict private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    strict private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    { Change the skin (in skinCoord), to follow the animation of joints.
      Joints that changed are assumed to be already traversed by TX3DNode.Traverse
      (this prepares field values for AnimateSkinPoint call).

      This method is used by TCastleSceneCore. Don't call this yourself.

      Returns changed X3D coordinates field (or @nil if there is no skin).
      You should call TX3DField.Changed on it (unless you use it from some
      TCastleSceneCore initialization, when there's no point in notifying about this). }
    function AnimateSkin: TMFVec3f;

    {$I auto_generated_node_helpers/x3dnodes_hanimhumanoid.inc}
  end;

  { Joint of an animated humanoid.
    Joints are organized in a hierarchy, like bones. }
  THAnimJointNode = class(TAbstractX3DGroupingNode)
  strict private
    type
      THAnimJointNodeTransformFunctionality = class(TTransformFunctionality)
      strict private
        FParent: THAnimJointNode;
      public
        constructor Create(const AParent: THAnimJointNode);
        property Parent: THAnimJointNode read FParent;
        procedure ApplyTransform(const State: TX3DGraphTraverseState); override;
      end;
    var
      FTransformFunctionality: THAnimJointNodeTransformFunctionality;
      AnimateSkinTransform: TMatrix4;
      RestCenter: TVector3;
  private
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
    function AnimateSkinPoint(const Point: TVector3): TVector3;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure ParseAfter(Reader: TX3DReaderNames); override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    function TransformationChange: TNodeTransformationChange; override;

    strict private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    strict private FFdDisplacers: TMFNode;
    public property FdDisplacers: TMFNode read FFdDisplacers;

    strict private FFdLimitOrientation: TSFRotation;
    public property FdLimitOrientation: TSFRotation read FFdLimitOrientation;

    strict private FFdLlimit: TMFFloat;
    public property FdLlimit: TMFFloat read FFdLlimit;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    strict private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    strict private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    strict private FFdSkinCoordIndex: TMFInt32;
    public property FdSkinCoordIndex: TMFInt32 read FFdSkinCoordIndex;

    strict private FFdSkinCoordWeight: TMFFloat;
    public property FdSkinCoordWeight: TMFFloat read FFdSkinCoordWeight;

    strict private FFdStiffness: TMFFloat;
    public property FdStiffness: TMFFloat read FFdStiffness;

    strict private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    strict private FFdUlimit: TMFFloat;
    public property FdUlimit: TMFFloat read FFdUlimit;

    {$I auto_generated_node_helpers/x3dnodes_hanimjoint.inc}
  end;

  { Visible segment of an animated humanoid,
    always a child of a TJointNode. }
  THAnimSegmentNode = class(TAbstractX3DGroupingNode)
  strict private
    type
      THAnimSegmentNodeTransformFunctionality = class(TTransformFunctionality)
      strict private
        FParent: THAnimSegmentNode;
      public
        constructor Create(const AParent: THAnimSegmentNode);
        property Parent: THAnimSegmentNode read FParent;
        procedure ApplyTransform(var Transformation: TTransformation); override;
      end;
    var
      FTransformFunctionality: THAnimSegmentNodeTransformFunctionality;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    function TransformationChange: TNodeTransformationChange; override;

    strict private FFdCenterOfMass: TSFVec3f;
    public property FdCenterOfMass: TSFVec3f read FFdCenterOfMass;

    strict private FFdCoord: TSFNode;
    public property FdCoord: TSFNode read FFdCoord;

    strict private FFdDisplacers: TMFNode;
    public property FdDisplacers: TMFNode read FFdDisplacers;

    strict private FFdMass: TSFFloat;
    public property FdMass: TSFFloat read FFdMass;

    strict private FFdMomentsOfInertia: TMFFloat;
    public property FdMomentsOfInertia: TMFFloat read FFdMomentsOfInertia;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    { Segment.rotation, scale, translation are extensions of Blaxxun
      avatars, at least that's what
      [http://tecfa.unige.ch/vrml/objects/avatars/blaxxun/]
      suggests.
      They are not conforming to HAnim 1.0 or 200x specification. }

    strict private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    strict private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    strict private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    {$I auto_generated_node_helpers/x3dnodes_hanimsegment.inc}
  end;

  { An attachment point of an animated humanoid,
    to attach something that held, worm or such by a humanoid. }
  THAnimSiteNode = class(TAbstractX3DGroupingNode)
  strict private
    type
      THAnimSiteNodeTransformFunctionality = class(TTransformFunctionality)
      strict private
        FParent: THAnimSiteNode;
      public
        constructor Create(const AParent: THAnimSiteNode);
        property Parent: THAnimSiteNode read FParent;
        procedure ApplyTransform(var Transformation: TTransformation); override;
      end;
    var
      FTransformFunctionality: THAnimSiteNodeTransformFunctionality;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    function TransformationChange: TNodeTransformationChange; override;

    strict private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    strict private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    strict private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    strict private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    {$I auto_generated_node_helpers/x3dnodes_hanimsite.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure THAnimDisplacerNode.CreateNode;
begin
  inherited;

  FFdCoordIndex := TMFInt32.Create(Self, true, 'coordIndex', []);
  AddField(FFdCoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdDisplacements := TMFVec3f.Create(Self, true, 'displacements', []);
  AddField(FFdDisplacements);

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdWeight := TSFFloat.Create(Self, true, 'weight', 0.0);
  AddField(FFdWeight);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'displacers';
end;

class function THAnimDisplacerNode.ClassX3DType: string;
begin
  Result := 'HAnimDisplacer';
end;

procedure TVertexJoints.Add(Joint: THAnimJointNode; const Weight: Single);
begin
  Inc(Count);
  SetLength(Joints , Count);
  SetLength(Weights, Count);
  Joints [Count - 1] := Joint;
  Weights[Count - 1] := Weight;
end;

{ THAnimHumanoidNode.THAnimHumanoidNodeTransformFunctionality ------------------------------ }

constructor THAnimHumanoidNode.THAnimHumanoidNodeTransformFunctionality.Create(const AParent: THAnimHumanoidNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

procedure THAnimHumanoidNode.THAnimHumanoidNodeTransformFunctionality.ApplyTransform(var Transformation: TTransformation);
begin
  inherited;

  Transformation.Multiply(
    Parent.Center,
    Parent.Rotation,
    Parent.Scale,
    Parent.ScaleOrientation,
    Parent.Translation);
end;

{ THAnimHumanoidNode --------------------------------------------------------- }

constructor THAnimHumanoidNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FTransformFunctionality := THAnimHumanoidNodeTransformFunctionality.Create(Self);
  AddFunctionality(FTransformFunctionality);
end;

procedure THAnimHumanoidNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, true, 'center', Vector3(0, 0, 0));
   FdCenter.ChangeAlways := chTransform;
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdInfo := TMFString.Create(Self, true, 'info', []);
  AddField(FFdInfo);

  FFdJoints := TMFNode.Create(Self, true, 'joints', [THAnimJointNode]);
  AddField(FFdJoints);

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdRotation := TSFRotation.Create(Self, true, 'rotation', Vector3(0, 0, 1), 0);
   FdRotation.ChangeAlways := chTransform;
  AddField(FFdRotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdScale := TSFVec3f.Create(Self, true, 'scale', Vector3(1, 1, 1));
   FdScale.ChangeAlways := chTransform;
  AddField(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, true, 'scaleOrientation', Vector3(0, 0, 1), 0);
   FdScaleOrientation.ChangeAlways := chTransform;
  AddField(FFdScaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdSegments := TMFNode.Create(Self, true, 'segments', [THAnimSegmentNode]);
  AddField(FFdSegments);

  FFdSites := TMFNode.Create(Self, true, 'sites', [THAnimSiteNode]);
  AddField(FFdSites);

  FFdSkeleton := TMFNode.Create(Self, true, 'skeleton', [THAnimJointNode, THAnimSiteNode]);
  AddField(FFdSkeleton);

  { HAnim 1.1 specification uses "humanoidBody" instead of "skeleton",
    also HAnim 200x specification examples show usage of this (although
    HAnim 200x specs talks only about "skeleton"). }
  Fdskeleton.AddAlternativeName('humanoidBody', 0);

  FFdSkin := TMFNode.Create(Self, true, 'skin', [TAbstractChildNode]);
  AddField(FFdSkin);

  FFdSkinCoord := TSFNode.Create(Self, true, 'skinCoord', [TAbstractCoordinateNode]);
  AddField(FFdSkinCoord);

  FFdSkinNormal := TSFNode.Create(Self, true, 'skinNormal', [TAbstractNormalNode]);
  AddField(FFdSkinNormal);

  FFdTranslation := TSFVec3f.Create(Self, true, 'translation', Vector3(0, 0, 0));
   FdTranslation.ChangeAlways := chTransform;
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdVersion := TSFString.Create(Self, true, 'version', '');
  AddField(FFdVersion);

  FFdViewpoints := TMFNode.Create(Self, true, 'viewpoints', [
    { X3D specificaion indicates that THAnimSiteNode is allowed here,
      but textual description in HAnim specification indicates that
      it's an error: viewpoints should be allowed here.
      Confirmed by testing models from
      http://www.web3d.org/x3d/content/examples/Basic/HumanoidAnimation/

      To be on the safest side, I allow both THAnimSiteNode and viewpoint
      classes.  }
    THAnimSiteNode,
    TAbstractViewpointNode,
    TViewpointGroupNode
  ]);
  AddField(FFdViewpoints);

  FFdBboxCenter := TSFVec3f.Create(Self, false, 'bboxCenter', Vector3(0, 0, 0));
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, false, 'bboxSize', Vector3(-1, -1, -1));
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

destructor THAnimHumanoidNode.Destroy;
begin
  FreeAndNil(VertexJoints);
  inherited;
end;

class function THAnimHumanoidNode.ClassX3DType: string;
begin
  Result := 'HAnimHumanoid';
end;

function THAnimHumanoidNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  { Between HAnim 1.0 and 200x hierarchy changed, in 1.0 there's no "skeleton"
    field but root Joint is just simply placed directly in VRML file.
    This means that I should simply render Joint node like a Transform,
    both inside "skeleton" field and outside, and both versions will be happy. }
  Result := FdSkeleton.Enumerate(Func);
  if Result <> nil then Exit;

  { To support skinned body, pass skin as an active node too
    (if not nil, then it will be rendered.)
    Note that SkinCoord and SkinNormal are not passed as active
    (they should be referenced inside Skin object). }
  Result := FdSkin.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdViewpoints.Enumerate(Func);
  if Result <> nil then Exit;

  { Note that joints, segments and sites is not passed to Func --- they
    should already be part of Joint hierarchy (in skeleton for HAnim 200x
    or outside of Humanoid node for HAnim 1.0). }

  { Finally, the way we implement HAnimHumanoid is compatible with X3D
    prototypes for this in
    http://www.web3d.org/x3d/content/examples/Basic/HumanoidAnimation/BoxMan.wrl
    Which means that comments above are probably true, as I'm not the
    only one using this approach. }
end;

procedure THAnimHumanoidNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Push;

  StateStack.Top.Humanoid := Self;
  StateStack.Top.HumanoidTransformation.Init;

  FTransformFunctionality.ApplyTransform(StateStack.Top.Transformation);
end;

function THAnimHumanoidNode.AnimateSkin: TMFVec3f;

  function UpdateVertex(const VJ: TVertexJoints): TVector3;
  var
    New: TVector3;
    J: Integer;
    WeightSum: Single;
  begin
    New := TVector3.Zero;
    WeightSum := 0;

    for J := 0 to VJ.Count - 1 do
    begin
      WeightSum := WeightSum + VJ.Weights[J];
      New := New + (VJ.Joints[J].AnimateSkinPoint(VJ.RestPosition) * VJ.Weights[J]);
    end;

    { Take the possibility of non-1 weight sum into account.
      When no joints affect this point, WeightSum = 0, this also works then. }
    Result := VJ.RestPosition * (1 - WeightSum) + New;
  end;

var
  I: Integer;
  SkinCoord: TVector3List;
begin
  if not (
    (FdSkinCoord.Value <> nil) and
    (FdSkinCoord.Value is TCoordinateNode) and
    (VertexJoints <> nil)) then Exit(nil);

  Result := TCoordinateNode(FdSkinCoord.Value).FdPoint;
  SkinCoord := Result.Items;
  if SkinCoord.Count <> VertexJoints.Count then Exit(nil);

  for I := 0 to VertexJoints.Count - 1 do
    SkinCoord.List^[I] := UpdateVertex(VertexJoints.List^[I]);
end;

procedure THAnimHumanoidNode.AfterTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  AnimateSkin;
  StateStack.Pop;
  inherited;
end;

procedure THAnimHumanoidNode.ParseAfter(Reader: TX3DReaderNames);

  procedure CalculateVertexJoints;
  var
    SkinCoord: TVector3List;
    I, J: Integer;
    SkinCoordIndex: TLongIntList;
    CoordIndex: Integer;
    Weight: Single;
    Joint: THAnimJointNode;
  begin
    FreeAndNil(VertexJoints);
    if (FdSkinCoord.Value <> nil) and
       (FdSkinCoord.Value is TCoordinateNode) then
    begin
      SkinCoord := TCoordinateNode(FdSkinCoord.Value).FdPoint.Items;
      VertexJoints := TVertexJointsList.Create;
      VertexJoints.Count := SkinCoord.Count;

      for I := 0 to SkinCoord.Count - 1 do
      begin
        VertexJoints.List^[I].RestPosition := SkinCoord.List^[I];
        VertexJoints.List^[I].Count := 0; { initially }
      end;

      for I := 0 to FdJoints.Count - 1 do
        if FdJoints[I] is THAnimJointNode then
        begin
          Joint := THAnimJointNode(FdJoints[I]);
          SkinCoordIndex := Joint.FdSkinCoordIndex.Items;
          for J := 0 to SkinCoordIndex.Count - 1 do
          begin
            CoordIndex := SkinCoordIndex.Items[J];
            if (CoordIndex < 0) or
               (CoordIndex >= SkinCoord.Count) then
            begin
              WritelnWarning('X3D', Format('HAnimJoint.skinCoord references not existing coordinate index %d (while we have only %d skin coordinates in HAnimHumanoid.skin)',
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

function THAnimHumanoidNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

{ THAnimJointNode.THAnimJointNodeTransformFunctionality -------------------------- }

constructor THAnimJointNode.THAnimJointNodeTransformFunctionality.Create(const AParent: THAnimJointNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

procedure THAnimJointNode.THAnimJointNodeTransformFunctionality.ApplyTransform(const State: TX3DGraphTraverseState);
var
  I: Integer;
  ChildJoint: THAnimJointNode;
  LocalCenter, NewChildCenter: TVector3;
  TempToSend: TSFVec3f;
begin
  inherited;

  { Spec says center / scale / scaleOrientation / translation
    are relative to humanoid root, but rotations are accumulated.
    http://h-anim.org/Specifications/H-Anim200x/ISO_IEC_FCD_19774/ObjectInterfaces.html
    So rotations are affected by center/etc. of joints above,
    which means that I have to apply here LocalCenter, LocalScale and such?
    But how? TODO: for now, we only apply LocalCenter.

    Fortunately, normal State isn't really used for skinned animation. }

  LocalCenter := State.HumanoidTransformation.InverseTransform.MultPoint(Parent.Center);
  State.Transformation.Multiply(
    LocalCenter,
    Parent.Rotation,
    Parent.Scale,
    Parent.ScaleOrientation,
    Parent.Translation);

  { update State.HumanoidTransformation for child joints. }
  State.HumanoidTransformation.Multiply(
    LocalCenter,
    Parent.Rotation,
    Parent.Scale,
    Parent.ScaleOrientation,
    Parent.Translation);

  Parent.AnimateSkinTransform := State.HumanoidTransformation.Transform;

  { animate centers of children joints }
  for I := 0 to FdChildren.Count - 1 do
    if FdChildren[I] is THAnimJointNode then
    begin
      ChildJoint := THAnimJointNode(FdChildren[I]);

      { TODO: it's very hacky to update ChildJoint.FdCenter this way.
        The cleaner way would be to just

          ChildJoint.FdCenter.Send(AnimateSkinPoint(ChildJoint.RestCenter))

        This would already do compare, send event if needed etc. (useful
        for view3dscene H-Anim joints visualization).
        But it would also notify the scene (events processor), and this causes
        slowdown and bugs (on lucy) right now. }

      NewChildCenter := Parent.AnimateSkinPoint(ChildJoint.RestCenter);
      if not TVector3.PerfectlyEquals(ChildJoint.FdCenter.Value, NewChildCenter) then
      begin
        ChildJoint.FdCenter.Value := NewChildCenter;

        if ChildJoint.FdCenter.Exposed then
        begin
          TempToSend := TSFVec3f.Create(ChildJoint, false, 'center', NewChildCenter);
          try
            ChildJoint.FdCenter.EventOut.Send(TempToSend);
          finally FreeAndNil(TempToSend) end;
        end;

        { Note that Send() above will use Scene.NextEventTime
          which with increase time by a "tick".
          This is good --- if you'll next want to traverse a parent joint,
          this center will again be changed, and should be send (otherwise
          routes e.g. in Humanoid Visualization in view3dscene will not work).

          Note that Scene may be nil now --- reproduce by:
          view3dscene ~/NextCloud/3dmodels/vrmlx3d/hanim/kambi_hanim_200_spec/hanim_loa_3.wrl }
      end;
    end;
end;

{ THAnimJointNode ------------------------------------------------------------ }

constructor THAnimJointNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FTransformFunctionality := THAnimJointNodeTransformFunctionality.Create(Self);
  AddFunctionality(FTransformFunctionality);
end;

procedure THAnimJointNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, true, 'center', Vector3(0, 0, 0));
   FdCenter.ChangeAlways := chTransform;
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdDisplacers := TMFNode.Create(Self, true, 'displacers', [THAnimDisplacerNode]);
  AddField(FFdDisplacers);

  FFdLimitOrientation := TSFRotation.Create(Self, true, 'limitOrientation', Vector3(0, 0, 1), 0);
  AddField(FFdLimitOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdLlimit := TMFFloat.Create(Self, true, 'llimit', []);
  AddField(FFdLlimit);
  { X3D specification comment: (-Inf,Inf) }

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdRotation := TSFRotation.Create(Self, true, 'rotation', Vector3(0, 0, 1), 0);
   FdRotation.ChangeAlways := chTransform;
  AddField(FFdRotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdScale := TSFVec3f.Create(Self, true, 'scale', Vector3(1, 1, 1));
   FdScale.ChangeAlways := chTransform;
  AddField(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, true, 'scaleOrientation', Vector3(0, 0, 1), 0);
   FdScaleOrientation.ChangeAlways := chTransform;
  AddField(FFdScaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdSkinCoordIndex := TMFInt32.Create(Self, true, 'skinCoordIndex', []);
  AddField(FFdSkinCoordIndex);

  FFdSkinCoordWeight := TMFFloat.Create(Self, true, 'skinCoordWeight', []);
  AddField(FFdSkinCoordWeight);

  FFdStiffness := TMFFloat.Create(Self, true, 'stiffness', [0, 0, 0]);
  AddField(FFdStiffness);
  { X3D specification comment: [0,1] }

  FFdTranslation := TSFVec3f.Create(Self, true, 'translation', Vector3(0, 0, 0));
   FdTranslation.ChangeAlways := chTransform;
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdUlimit := TMFFloat.Create(Self, true, 'ulimit', []);
  AddField(FFdUlimit);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';

  AnimateSkinTransform := TMatrix4.Identity;
end;

class function THAnimJointNode.ClassX3DType: string;
begin
  Result := 'HAnimJoint';
end;

function THAnimJointNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

function THAnimJointNode.AnimateSkinPoint(const Point: TVector3): TVector3;
begin
  Result := AnimateSkinTransform.MultPoint(Point);
end;

procedure THAnimJointNode.ParseAfter(Reader: TX3DReaderNames);
begin
  inherited;
  RestCenter := FdCenter.Value;
end;

function THAnimJointNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

{ THAnimSegmentNode.THAnimSegmentNodeTransformFunctionality ---------------------------------------- }

constructor THAnimSegmentNode.THAnimSegmentNodeTransformFunctionality.Create(const AParent: THAnimSegmentNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

procedure THAnimSegmentNode.THAnimSegmentNodeTransformFunctionality.ApplyTransform(var Transformation: TTransformation);
begin
  inherited;
  Transformation.Multiply(
    Parent.Rotation,
    Parent.Scale,
    Parent.Translation);
end;

{ THAnimSegmentNode ---------------------------------------------------------- }

constructor THAnimSegmentNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FTransformFunctionality := THAnimSegmentNodeTransformFunctionality.Create(Self);
  AddFunctionality(FTransformFunctionality);
end;

procedure THAnimSegmentNode.CreateNode;
begin
  inherited;

  FFdCenterOfMass := TSFVec3f.Create(Self, true, 'centerOfMass', Vector3(0, 0, 0));
  AddField(FFdCenterOfMass);
  { X3D specification comment: (-Inf,Inf) }

  FFdCoord := TSFNode.Create(Self, true, 'coord', [TAbstractCoordinateNode]);
  AddField(FFdCoord);

  FFdDisplacers := TMFNode.Create(Self, true, 'displacers', [THAnimDisplacerNode]);
  AddField(FFdDisplacers);

  FFdMass := TSFFloat.Create(Self, true, 'mass', 0);
  AddField(FFdMass);
  { X3D specification comment: [0,Inf) }

  FFdMomentsOfInertia := TMFFloat.Create(Self, true, 'momentsOfInertia', [0, 0, 0, 0, 0, 0, 0, 0, 0]);
  AddField(FFdMomentsOfInertia);
  { X3D specification comment: [0,Inf) }

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdRotation := TSFRotation.Create(Self, true, 'rotation', Vector3(0, 0, 1), 0);
   FdRotation.ChangeAlways := chTransform;
  AddField(FFdRotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TSFVec3f.Create(Self, true, 'scale', Vector3(1, 1, 1));
   FdScale.ChangeAlways := chTransform;
  AddField(FFdScale);
  { X3D specification comment: (-Inf, Inf) }

  FFdTranslation := TSFVec3f.Create(Self, true, 'translation', Vector3(0, 0, 0));
   FdTranslation.ChangeAlways := chTransform;
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function THAnimSegmentNode.ClassX3DType: string;
begin
  Result := 'HAnimSegment';
end;

function THAnimSegmentNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

function THAnimSegmentNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

{ THAnimSiteNode.THAnimSiteNodeTransformFunctionality ---------------------------------------- }

constructor THAnimSiteNode.THAnimSiteNodeTransformFunctionality.Create(const AParent: THAnimSiteNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

procedure THAnimSiteNode.THAnimSiteNodeTransformFunctionality.ApplyTransform(var Transformation: TTransformation);
begin
  inherited;
  Transformation.Multiply(
    Parent.Center,
    Parent.Rotation,
    Parent.Scale,
    Parent.ScaleOrientation,
    Parent.Translation);
end;

{ THAnimSiteNode ---------------------------------------------------------- }

constructor THAnimSiteNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FTransformFunctionality := THAnimSiteNodeTransformFunctionality.Create(Self);
  AddFunctionality(FTransformFunctionality);
end;

procedure THAnimSiteNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, true, 'center', Vector3(0, 0, 0));
   FdCenter.ChangeAlways := chTransform;
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdRotation := TSFRotation.Create(Self, true, 'rotation', Vector3(0, 0, 1), 0);
   FdRotation.ChangeAlways := chTransform;
  AddField(FFdRotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdScale := TSFVec3f.Create(Self, true, 'scale', Vector3(1, 1, 1));
   FdScale.ChangeAlways := chTransform;
  AddField(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, true, 'scaleOrientation', Vector3(0, 0, 1), 0);
   FdScaleOrientation.ChangeAlways := chTransform;
  AddField(FFdScaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdTranslation := TSFVec3f.Create(Self, true, 'translation', Vector3(0, 0, 0));
   FdTranslation.ChangeAlways := chTransform;
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  DefaultContainerField := 'children';
end;

class function THAnimSiteNode.ClassX3DType: string;
begin
  Result := 'HAnimSite';
end;

function THAnimSiteNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

function THAnimSiteNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

procedure RegisterHAnimNodes;
begin
  NodesManager.RegisterNodeClasses([
    THAnimDisplacerNode,
    THAnimHumanoidNode,
    THAnimJointNode,
    THAnimSegmentNode,
    THAnimSiteNode
  ]);
end;

{$endif read_implementation}
