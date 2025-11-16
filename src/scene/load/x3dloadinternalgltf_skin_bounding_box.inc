{%MainUnit x3dloadinternalgltf.pas}
{
  Copyright 2018-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Calculating bounding boxes for shapes affected by the skinned animation.

  Proper bounding box is necessary to have correct frustum culling,
  as explained in
  https://castle-engine.io/skin#_you_need_to_assign_proper_bounding_box_explicitly .

  We want to calculate such bounding box,
  but without effectively simulating all the animation frames at loading time,
  which would make loading slow.
}

{ TAnimationSampler --------------------------------------------------------------- }

type
  { Sample animation, imported from glTF into X3D.
    Part of the information here is in X3D data structures,
    part is in glTF data structures.
    "Sampling animation" means that we calculate transformations at
    the given time. }
  TAnimationSampler = class
  strict private
    { Internal in SetTime. }
    CurrentTranslation: TVector3List;
    CurrentRotation: TVector4List;
    CurrentScale: TVector3List;
  public
    { List of TTransformNode nodes, ordered just list glTF nodes.
      Only initialized (non-nil and enough Count) for nodes that we created in ReadNode.

      Set this before @link(SetTime). }
    TransformNodes: TX3DNodeList;

    { glTF transformation nodes, ordered just list @link(TransformNodes).
      In PasGLTF, this comes from Document.Nodes.

      Set this before @link(SetTime). }
    TransformNodesGltf: TPasGLTF.TNodes;

    { Indexes of root nodes in TransformNodesGltf.
      In PasGLTF, this comes from Scene.Nodes.

      Set this before @link(SetTime). }
    TransformNodesRoots: TPasGLTF.TScene.TNodes;

    { Current animation applied by @link(SetTime).

      Set this before @link(SetTime). }
    Animation: TAnimation;

    { Owned by this object, calculated by @link(SetTime).
      Has the same size as TransformNodes, contains accumulated transformation matrix
      for each node.
      Contains undefined value for nodes that are @nil. }
    Transformations: TTransformationList;

    constructor Create;
    destructor Destroy; override;
    procedure SetTime(const Time: TFloatTime);
  end;

constructor TAnimationSampler.Create;
begin
  inherited;
  Transformations := TTransformationList.Create;
  CurrentTranslation := TVector3List.Create;
  CurrentRotation := TVector4List.Create;
  CurrentScale := TVector3List.Create;
end;

destructor TAnimationSampler.Destroy;
begin
  FreeAndNil(Transformations);
  FreeAndNil(CurrentTranslation);
  FreeAndNil(CurrentRotation);
  FreeAndNil(CurrentScale);
  inherited;
end;

procedure TAnimationSampler.SetTime(const Time: TFloatTime);

{ The implementation of this somewhat duplicates the logic
  of the animation and transformation at runtime,
  done by TTransformNode in CastleShapes, CastleSceneCore units.
  At one point I considered just using TTimeSensor.FakeTime
  or even TCastleSceneCore.ForceAnimationPose to set scene
  to given state in each SetTime, and then read resulting transformations
  from Scene.Shapes.
  However, this causes new complications:
  It would modify the nodes hierarchy, which means we should save/restore it.
  And it's not really much simpler, since the transformation hierarchy is quite simple.
}

  { Set all CurrentXxx values to reflect initial transformations of TransformNodes }
  procedure ResetCurrentTransformation;
  var
    I: Integer;
    Transform: TTransformNode;
  begin
    // initialize CurrentXxx lists
    for I := 0 to TransformNodes.Count - 1 do
      if TransformNodes[I] <> nil then
      begin
        Transform := TransformNodes[I] as TTransformNode;
        CurrentTranslation[I] := Transform.FdTranslation.Value;
        CurrentRotation   [I] := Transform.FdRotation   .Value;
        CurrentScale      [I] := Transform.FdScale      .Value;
      end;
  end;

  { Update all CurrentXxx values affected by this animation. }
  procedure UpdateCurrentTransformation;
  var
    Interpolator: TInterpolator;
    TargetIndex: Integer;
  begin
    for Interpolator in Animation.Interpolators do
    begin
      TargetIndex := TransformNodes.IndexOf(Interpolator.Target);
      if TargetIndex = -1 then
        raise EInternalError.Create('Interpolator.Target not on Nodes list');

      { Below we process Time,
        similar to TAbstractSingleInterpolatorNode.EventSet_FractionReceive. }

      case Interpolator.Path of
        gsTranslation:
          CurrentTranslation[TargetIndex] :=
            (Interpolator.Node as TPositionInterpolatorNode).Interpolate(Time);
        gsRotation:
          CurrentRotation[TargetIndex] :=
            (Interpolator.Node as TOrientationInterpolatorNode).Interpolate(Time);
        gsScale:
          CurrentScale[TargetIndex] :=
            (Interpolator.Node as TPositionInterpolatorNode).Interpolate(Time);
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('Unexpected glTF Interpolator.Path value');
        {$endif}
      end;
    end;
  end;

  { Calculate contents of Transformations, based on CurrentXxx and parent-child relationships. }
  procedure UpdateMatrix;

    procedure UpdateChildMatrix(const NodeIndex: Integer;
      const ParentT: TTransformation);
    var
      T: PTransformation;
      ChildNodeIndex: Integer;
    begin
      if not Between(NodeIndex, 0, Transformations.Count - 1) then
        Exit; // warning about it was already done by ReadNodes

      T := PTransformation(Transformations.Ptr(NodeIndex));
      T^ := ParentT;
      T^.Multiply(
        CurrentRotation[NodeIndex],
        CurrentScale[NodeIndex],
        CurrentTranslation[NodeIndex]);

      for ChildNodeIndex in TransformNodesGltf[NodeIndex].Children do
        UpdateChildMatrix(ChildNodeIndex, T^);
    end;

  var
    T: PTransformation;
    RootNodeIndex, ChildNodeIndex: Integer;
  begin
    for RootNodeIndex in TransformNodesRoots do
    begin
      if not Between(RootNodeIndex, 0, Transformations.Count - 1) then
        Continue; // warning about it was already done by ReadScene

      T := PTransformation(Transformations.Ptr(RootNodeIndex));
      T^.Init;
      T^.Multiply(
        CurrentRotation[RootNodeIndex],
        CurrentScale[RootNodeIndex],
        CurrentTranslation[RootNodeIndex]);

      for ChildNodeIndex in TransformNodesGltf[RootNodeIndex].Children do
        UpdateChildMatrix(ChildNodeIndex, T^);
    end;
  end;

begin
  { Since in practice TransformNodes.Count is constant during glTF file reading,
    this sets the size only at first SetTime call (for this glTF model). }
  Transformations.Count := TransformNodes.Count;
  CurrentTranslation.Count := TransformNodes.Count;
  CurrentRotation.Count := TransformNodes.Count;
  CurrentScale.Count := TransformNodes.Count;

  ResetCurrentTransformation;
  UpdateCurrentTransformation;
  UpdateMatrix;
end;

{ Routines ------------------------------------------------------------------ }

{ Gather all key times (in 0..1 range) from Interpolators, place them in AllKeys.
  If you have animation that uses multiple interpolators,
  then this routine calculates *all* key points within this animation.
  This routine always clears AllKeys at the beginning,
  so their initial contents don't matter. }
procedure GatherAnimationKeysToSample(const AllKeys: TSingleList;
  const Interpolators: TInterpolatorList);
var
  I: Integer;
  Interpolator: TAbstractInterpolatorNode;
begin
  AllKeys.Clear;
  for I := 0 to Interpolators.Count - 1 do
  begin
    Interpolator := Interpolators[I].Node;
    AllKeys.AddRange(Interpolator.FdKey.Items);
  end;
  AllKeys.SortAndRemoveDuplicates;
end;

{ Sample animation AnimationSampler.Anim at time TimeFraction (in 0..1 range)
  to determine how does a skin look like at this moment of time.
  OriginalCoords contains original (not animated) coords.
  We will enlarge Box to account for all vertexes at this time.

  JointMatrix must be already allocated to hold Joints.Count items. }
procedure SampleSkinAnimation(const AnimationSampler: TAnimationSampler;
  const KeyIndex: Integer;
  const TimeFraction: Single;
  const OriginalCoords: TVector3List; var Box: TBox3D;
  const Joints: TX3DNodeList; const JointsGltf: TPasGLTF.TSkin.TJoints;
  const JointMatrix: TMatrix4List;
  const InverseBindMatrices: TMatrix4List;
  const SkeletonRootIndex: Integer;
  const Joints0: TInt32List;
  const Weights0: TVector4List);
var
  I: Integer;
  NewCoordinate: TVector3;
  SkinMatrix, SkeletonRootInverse: TMatrix4;
  VertexJointsPtr: PVector4Integer;
  VertexWeights: TVector4;
begin
  AnimationSampler.SetTime(TimeFraction);

  if SkeletonRootIndex <> -1 then
    SkeletonRootInverse := AnimationSampler.Transformations.L[SkeletonRootIndex].InverseTransform
  else
    SkeletonRootInverse := TMatrix4.Identity;

  { For each Joint, we calculate JointMatrix following
    https://www.slideshare.net/Khronos_Group/gltf-20-reference-guide }
  for I := 0 to Joints.Count - 1 do
    JointMatrix[I] := SkeletonRootInverse *
      AnimationSampler.Transformations.L[JointsGltf[I]].Transform *
      InverseBindMatrices[I];

  { For each vertex, calculate SkinMatrix as linear combination of JointMatrix[...]
    for all joints indicated by Joints0 values for this vertex.
    TODO: Support JOINTS_1, WEIGHTS_1 etc. }
  for I := 0 to OriginalCoords.Count - 1 do
  begin
    VertexWeights := Weights0[I];
    VertexJointsPtr := PVector4Integer(Joints0.Ptr(I * 4));
    if VertexWeights.IsPerfectlyZero then
    begin
      { Workarond wrong weights produced by old Blender -> glTF exporter.
        See for explanation and testcase:
        https://github.com/castle-engine/demo-models/tree/master/animation/blender_skinned_animation/blender_zero_weights_bug }
      SkinMatrix := JointMatrix.L[0];
    end else
    begin
      SkinMatrix :=
        JointMatrix.L[VertexJointsPtr^.X] * VertexWeights.X +
        JointMatrix.L[VertexJointsPtr^.Y] * VertexWeights.Y +
        JointMatrix.L[VertexJointsPtr^.Z] * VertexWeights.Z +
        JointMatrix.L[VertexJointsPtr^.W] * VertexWeights.W;
    end;
    NewCoordinate := SkinMatrix.MultPoint(OriginalCoords.L[I]);
    Box.Include(NewCoordinate);
  end;
end;

{ When animation TimeSensor starts, set Shape.BBox using X3D routes.

  ParentGroup can be really any grouping node,
  we add there only trigger node and routes. }
procedure SetBBoxWhenAnimationStarts(const TimeSensor: TTimeSensorNode;
  const Shape: TShapeNode; const BBox: TBox3D;
  const ParentGroup: TAbstractGroupingNode);
var
  ValueTrigger: TValueTriggerNode;
  Center, Size: TVector3;
  F: TX3DField;
begin
  BBox.ToCenterSize(Center, Size);

  ValueTrigger := TValueTriggerNode.Create;
  ValueTrigger.X3DName := 'ValueTrigger_setBBox_' +
    TimeSensor.X3DName + '_' + Shape.X3DName;
  ParentGroup.AddChildren(ValueTrigger);
  ParentGroup.AddRoute(TimeSensor.EventIsActive, ValueTrigger.EventTrigger);

  F := TSFVec3f.Create(nil, true, 'bboxCenter', Center);
  ValueTrigger.AddCustomField(F);
  ParentGroup.AddRoute(F, Shape.FdBboxCenter);

  F := TSFVec3f.Create(nil, true, 'bboxSize', Size);
  ValueTrigger.AddCustomField(F);
  ParentGroup.AddRoute(F, Shape.FdBboxSize);
end;

{ Calculate bounding box for all shapes for all animation frames. }
procedure CalculateSkinnedShapesBoundingBoxes(const Skin: TSkinNode;
  const Animations: TAnimationList; const TransformNodes: TX3DNodeList;
  const JointsGltf: TPasGLTF.TSkin.TJoints;
  const TransformNodesGltf: TPasGLTF.TNodes;
  const TransformNodesRoots: TPasGLTF.TScene.TNodes;
  const SkeletonRootIndex: Integer);
var
  AnimationSampler: TAnimationSampler;
  JointMatrix: TMatrix4List; //< coult be local to each SampleSkinAnimation, but created once to avoid wasting time on allocation
  KeyTimes: TSingleList;
  FramesSampled, FramesAll: Cardinal;
  TimeStart: TTimerResult;

  { Calculate shape bounding box for all animation frames. }
  procedure CalculateOneShape(const Shape: TShapeNode);
  var
    CoordField: TSFNode;
    Coord: TCoordinateNode;
    Anim: TAnimation;
    Weights0: TVector4List;
    Joints0: TInt32List;
    Box: TBox3D;
    I: Integer;
  begin
    CoordField := Shape.Geometry.CoordField;
    if CoordField = nil then
    begin
      WritelnWarning('Cannot animate using skin geometry %s, it does not have coordinates', [
        Shape.Geometry.NiceName
      ]);
      Exit;
    end;

    if not (CoordField.Value is TCoordinateNode) then
    begin
      WritelnWarning('Cannot animate using skin geometry %s, the coordinates are not expressed as Coordinate node', [
        Shape.Geometry.NiceName
      ]);
      Exit;
    end;
    Coord := CoordField.Value as TCoordinateNode;

    if not Shape.Geometry.SkinWeightsJoints(Weights0, Joints0) then
    begin
      WritelnWarning('Cannot animate using skin geometry %s, no JOINTS_0 and WEIGHTS_0 information in the mesh', [
        Shape.Geometry.NiceName
      ]);
      Exit;
    end;

    for Anim in Animations do
    begin
      AnimationSampler.Animation := Anim;

      GatherAnimationKeysToSample(KeyTimes, Anim.Interpolators);

      Box := TBox3D.Empty;

      FramesAll := FramesAll + KeyTimes.Count;

      I := 0;
      while I < KeyTimes.Count do
      begin
        SampleSkinAnimation(AnimationSampler, I, KeyTimes[I],
          Coord.FdPoint.Items, Box,
          Skin.FdJoints.InternalItems, JointsGltf, JointMatrix,
          Skin.FdInverseBindMatrices.Items,
          SkeletonRootIndex,
          Joints0, Weights0);
        Inc(FramesSampled);
        I := I + TSkinNode.AnimationSamplingForBox;
      end;

      { Sample also last frame }
      if I <> KeyTimes.Count - 1 then
      begin
        I := KeyTimes.Count - 1;
        SampleSkinAnimation(AnimationSampler, I, KeyTimes[I],
          Coord.FdPoint.Items, Box,
          Skin.FdJoints.InternalItems, JointsGltf, JointMatrix,
          Skin.FdInverseBindMatrices.Items,
          SkeletonRootIndex,
          Joints0, Weights0);
        Inc(FramesSampled);
      end;

      { We want to use Shape.BBox for optimization (to avoid recalculating bbox).
        Simple version:

          Shape.BBox := Shape.BBox + Box;

        But it's more efficient to set bbox for specific animation
        once the animation starts.
        It also looks a bit more intuitive when you view bbox
        (otherwise you would see a large bbox accounting for *all* animations,
        but with mesh transformed with current animation, testcase: Bee, Monster).

        This matters, because bbox is also used for collisions.
        E.g. knight in fps_game when Walking should not have huge bbox
        because of Dying animation. }
      SetBBoxWhenAnimationStarts(Anim.TimeSensor, Shape, Box, Skin.Skeleton);
    end;
  end;

var
  I: Integer;
begin
  { AnimationSamplingForBox = 0 is a special case, leave shapes bbox
    unassigned then. Engine will assume bounding box of the animation
    is equal to the value of TCoordinateNode contents on CPU (not accounting
    for skinned animation on GPU). }
  if TSkinNode.AnimationSamplingForBox = 0 then
    Exit;

  // nil local variables, to have easy finally section
  AnimationSampler := nil;
  JointMatrix := nil;
  KeyTimes := nil;

  try
    AnimationSampler := TAnimationSampler.Create;
    AnimationSampler.TransformNodes := TransformNodes;
    AnimationSampler.TransformNodesGltf := TransformNodesGltf;
    AnimationSampler.TransformNodesRoots := TransformNodesRoots;

    JointMatrix := TMatrix4List.Create;
    JointMatrix.Count := Skin.FdJoints.Count;

    KeyTimes := TSingleList.Create;

    FramesSampled := 0;
    FramesAll := 0;
    TimeStart := Timer;

    for I := 0 to Skin.FdShapes.Count - 1 do
      CalculateOneShape(Skin.FdShapes[I] as TShapeNode);

    WritelnLog('Calculated skinned animation bounding box in %f secs, sampling %d animation frames from total %d frames. Shapes count: %d', [
      TimeStart.ElapsedTime,
      FramesSampled,
      FramesAll,
      Skin.FdShapes.Count
    ]);
  finally
    FreeAndNil(JointMatrix);
    FreeAndNil(AnimationSampler);
    FreeAndNil(KeyTimes);
  end;
end;
