{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2023 Michalis Kamburelis.

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
  {$I auto_generated_node_helpers/x3dnodes_hanimdisplacer.inc}
  end;

  THAnimJointNode = class;

  TVertexJoints = record
    RestPosition: TVector3;
    Count: Cardinal;
    Joints: array of THAnimJointNode;
    Weights: array of Single;
    procedure Add(Joint: THAnimJointNode; const Weight: Single);
  end;
  PVertexJoints = ^TVertexJoints;
  TVertexJointsList = {$ifdef FPC}specialize{$endif} TStructList<TVertexJoints>;

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
    procedure AddJoint(Node: TX3DNode);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure ParseAfter(Reader: TX3DReaderNames); override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    destructor Destroy; override;
    function TransformationChange: TNodeTransformationChange; override;

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
  THAnimJointNode = class(TAbstractGroupingNode)
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
    WarningSkinCoordReferences: Boolean;

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
    function TransformationChange: TNodeTransformationChange; override;

  {$I auto_generated_node_helpers/x3dnodes_hanimjoint.inc}
  end;

  { Visible segment of an animated humanoid,
    always a child of a TJointNode. }
  THAnimSegmentNode = class(TAbstractGroupingNode)
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
    function TransformationChange: TNodeTransformationChange; override;

  {$I auto_generated_node_helpers/x3dnodes_hanimsegment.inc}
  end;

  { An attachment point of an animated humanoid,
    to attach something that held, worm or such by a humanoid. }
  THAnimSiteNode = class(TAbstractGroupingNode)
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
    function TransformationChange: TNodeTransformationChange; override;

  {$I auto_generated_node_helpers/x3dnodes_hanimsite.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TVertexJoints -------------------------------------------------------------- }

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

  { HAnim 1.1 specification uses "humanoidBody" instead of "skeleton",
    also HAnim 200x specification examples show usage of this (although
    HAnim 200x specs talks only about "skeleton"). }
  Fdskeleton.AddAlternativeName('humanoidBody', 0);
end;

destructor THAnimHumanoidNode.Destroy;
begin
  FreeAndNil(VertexJoints);
  inherited;
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
    SkinCoord.L[I] := UpdateVertex(VertexJoints.L[I]);
end;

procedure THAnimHumanoidNode.AfterTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  AnimateSkin;
  StateStack.Pop;
  inherited;
end;

procedure THAnimHumanoidNode.AddJoint(Node: TX3DNode);
begin
  FdJoints.Add(Node);
end;

procedure THAnimHumanoidNode.ParseAfter(Reader: TX3DReaderNames);

  { It is a common mistake to leave HAnimHumanoid.joints empty,
    which means that CGE would not animate humanoid at all (contrary to other
    X3D browsers that don't rely on "joints").
    So we fix the HAnimHumanoid.joints list. }
  procedure CalculateJoints;
  begin
    if FdJoints.Count = 0 then
    begin
      EnumerateNodes(THAnimJointNode, {$ifdef FPC}@{$endif} AddJoint, false);
      if FdJoints.Count <> 0 then
        WritelnWarning('HAnimHumanoid.joints list was empty, fixed to list all %d joints', [
          FdJoints.Count
        ]);
    end;
  end;

  procedure CalculateVertexJoints;
  var
    SkinCoord: TVector3List;
    I, J: Integer;
    SkinCoordIndex: TInt32List;
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
        VertexJoints.L[I].RestPosition := SkinCoord.L[I];
        VertexJoints.L[I].Count := 0; { initially }
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
              if not Joint.WarningSkinCoordReferences then
              begin
                WritelnWarning('X3D', Format('"%s": skinCoord refers to not existing coordinate index %d (while we have only %d skin coordinates in HAnimHumanoid.skin)', [
                  Joint.NiceName,
                  CoordIndex,
                  SkinCoord.Count
                ]));
                Joint.WarningSkinCoordReferences := true;
              end;
              Continue;
            end;

            if J < Joint.FdSkinCoordWeight.Items.Count then
              Weight := Joint.FdSkinCoordWeight.Items[J] else
              Weight := 1;

            VertexJoints.L[CoordIndex].Add(Joint, Weight);
          end;
        end;
    end;
  end;

begin
  inherited;
  CalculateJoints;
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
  for I := 0 to Parent.FdChildren.Count - 1 do
    if Parent.FdChildren[I] is THAnimJointNode then
    begin
      ChildJoint := THAnimJointNode(Parent.FdChildren[I]);

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
  AnimateSkinTransform := TMatrix4.Identity;
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
