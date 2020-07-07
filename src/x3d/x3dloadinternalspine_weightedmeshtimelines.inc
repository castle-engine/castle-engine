{
  Copyright 2020 Trung Le (kagamma).

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine weighted mesh timelines. }

{$ifdef read_interface}
  TWeightedMeshVertexList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSingleList>)
  end;

  TWeightedMeshTimeline = class
    Name: String;
    Attachment: TMeshAttachment;
    AffectedBoneTimelines: TBoneTimelineList;
    Vertices: TWeightedMeshVertexList;
    Time: TSingleList;
    Node: TCoordinateInterpolatorNode;
    constructor Create;
    destructor Destroy; override;
    procedure Parse;
    procedure BuildNodes(const MaxAnimationTime: Single; const Container: TX3DRootNode);
  end;

  TWeightedMeshTimelineList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TWeightedMeshTimeline>)
  end;
{$endif}

{$ifdef read_implementation}

constructor TWeightedMeshTimeline.Create;
begin
  inherited;
  Time := TSingleList.Create;
  Vertices := TWeightedMeshVertexList.Create;
end;

destructor TWeightedMeshTimeline.Destroy;
begin
  FreeAndNil(Vertices);
  FreeAndNil(Time);
  inherited;
end;

{ We now performing 'parse':
  - Find all possible timeframes by combining timeframes from every bone timelines
  - Perform interpolation for those bone timelines that doesnt have necesssary timeframes
  - Perform vertex calculation
}
procedure TWeightedMeshTimeline.Parse;
  { Example:
    - Time1: [0, 0.2, 0.6, 1]
    - Time2: [0, 0.3, 0.6, 0.7. 1]
    - Result: [0, 0.2, 0.3, 0.6, 0.7, 1]
  }
  procedure CombineTimeframes;
  var
    I, J: Integer;
    Value: Single;
  begin
    for I := 0 to AffectedBoneTimelines.Count - 1 do
    begin
      for J := 0 to AffectedBoneTimelines[I].Time.Count - 1 do
      begin
        Value := AffectedBoneTimelines[I].Time[J];
        if Time.IndexOf(Value) < 0 then
        begin
          Time.Add(Value);
        end;
      end;
    end;
    Time.Sort;
  end;

  procedure InterpolateTimeframes;
  var
    I, J, K, P: Integer;
    BoneTimeline: TBoneTimeline;
    BoneTimelineTranslate: TBoneTimelineTranslate;
    BoneTimelineScale: TBoneTimelineScale;
    BoneTimelineRotate: TBoneTimelineRotate;
    Timeframe, Timeframe1, Timeframe2: Single;
    IsExists: Boolean;
  begin
    for I := 0 to AffectedBoneTimelines.Count - 1 do
    begin
      BoneTimeline := AffectedBoneTimelines[I];
      for J := 0 to Time.Count - 1 do
      begin
        Timeframe := Time[J];
        // Check to see if this timeframe exists in bone timeline
        IsExists := False;
        for K := 0 to BoneTimeline.Time.Count - 1 do
        begin
          if Timeframe = BoneTimeline.Time[K] then
          begin
            IsExists := True;
            Break;
          end;
        end;
        // If timeframe does not exist, we insert it into bone timeline and then
        // generate new values for this timeframe by interpolating 2 values between it
        if not IsExists then
        begin
          BoneTimeline.Time.Add(Timeframe);
          BoneTimeline.Time.Sort;
          P := BoneTimeline.Time.IndexOf(Timeframe);
          if P = 0 then
          begin
            if BoneTimeline is TBoneTimelineTranslate then
            begin
              BoneTimelineTranslate := BoneTimeline as TBoneTimelineTranslate;
              BoneTimelineTranslate.Vectors.Insert(P, BoneTimelineTranslate.Vectors[P]);
            end;
            if BoneTimeline is TBoneTimelineRotate then
            begin
              BoneTimelineRotate := BoneTimeline as TBoneTimelineRotate;
              BoneTimelineRotate.Angles.Insert(P, BoneTimelineRotate.Angles[P]);
            end;;
            if BoneTimeline is TBoneTimelineScale then
            begin
              BoneTimelineScale := BoneTimeline as TBoneTimelineScale;
              BoneTimelineScale.Vectors.Insert(P, BoneTimelineScale.Vectors[P]);
            end;
          end else
          if P = BoneTimeline.Time.Count - 1 then
          begin
            if BoneTimeline is TBoneTimelineTranslate then
            begin
              BoneTimelineTranslate := BoneTimeline as TBoneTimelineTranslate;
              BoneTimelineTranslate.Vectors.Add(BoneTimelineTranslate.Vectors[BoneTimelineTranslate.Vectors.Count - 1]);
            end;
            if BoneTimeline is TBoneTimelineRotate then
            begin
              BoneTimelineRotate := BoneTimeline as TBoneTimelineRotate;
              BoneTimelineRotate.Angles.Add(BoneTimelineRotate.Angles[BoneTimelineRotate.Angles.Count - 1]);
            end;;
            if BoneTimeline is TBoneTimelineScale then
            begin
              BoneTimelineScale := BoneTimeline as TBoneTimelineScale;
              BoneTimelineScale.Vectors.Add(BoneTimelineScale.Vectors[BoneTimelineScale.Vectors.Count - 1]);
            end;
          end else
          begin
            Timeframe1 := BoneTimeline.Time[P - 1];
            Timeframe2 := BoneTimeline.Time[P + 1];
            if BoneTimeline is TBoneTimelineTranslate then
            begin
              BoneTimelineTranslate := BoneTimeline as TBoneTimelineTranslate;
              BoneTimelineTranslate.Vectors.Insert(P,
                  Lerp((Timeframe - Timeframe1) / (Timeframe2 - Timeframe1),
                  BoneTimelineTranslate.Vectors[P - 1],
                  BoneTimelineTranslate.Vectors[P]));
            end;
            if BoneTimeline is TBoneTimelineRotate then
            begin
              BoneTimelineRotate := BoneTimeline as TBoneTimelineRotate;
              BoneTimelineRotate.Angles.Insert(P,
                  Lerp((Timeframe - Timeframe1) / (Timeframe2 - Timeframe1),
                  BoneTimelineRotate.Angles[P - 1],
                  BoneTimelineRotate.Angles[P]));
            end;;
            if BoneTimeline is TBoneTimelineScale then
            begin
              BoneTimelineScale := BoneTimeline as TBoneTimelineScale;
              BoneTimelineScale.Vectors.Insert(0,
                  Lerp((Timeframe - Timeframe1) / (Timeframe2 - Timeframe1),
                  BoneTimelineScale.Vectors[P - 1],
                  BoneTimelineScale.Vectors[P]));
            end;
          end;
        end;
      end;
    end;
  end;

  procedure CalculateVertices;
  var
    I, J, K, L: Integer;
    VB: TMeshVertexBone;
    V: TVector2;
    VertexList: TSingleList;
    BoneParent: TBone;
    Bones: TBoneList;
  begin
    Bones := TBoneList.Create;
    Bones.OwnsObjects := False;
    try
      for I := 0 to Time.Count - 1 do
      begin
        VertexList := TSingleList.Create;
        VertexList.Count := Attachment.Weights.Count * 2;
        for J := 0 to Attachment.Weights.Count - 1 do
        begin
          V := TVector2.Zero;
          for K := 0 to Attachment.Weights[J].Bones.Count - 1 do
          begin
            VB := Attachment.Weights[J].Bones[K];
            //
            Bones.Clear;
            BoneParent := VB.Bone;
            while BoneParent <> nil do
            begin
              BoneParent.Backup;
              Bones.Add(BoneParent);
              for L := 0 to AffectedBoneTimelines.Count - 1 do
              begin
                if AffectedBoneTimelines[L].Bone.Name = BoneParent.Name then
                begin
                  if AffectedBoneTimelines[L] is TBoneTimelineTranslate then
                    BoneParent.XY := BoneParent.XY + (AffectedBoneTimelines[L] as TBoneTimelineTranslate).Vectors[I];
                  if AffectedBoneTimelines[L] is TBoneTimelineRotate then
                    BoneParent.Rotation := BoneParent.Rotation + (AffectedBoneTimelines[L] as TBoneTimelineRotate).Angles[I];
                  if AffectedBoneTimelines[L] is TBoneTimelineScale then
                    BoneParent.Scale := BoneParent.Scale * (AffectedBoneTimelines[L] as TBoneTimelineScale).Vectors[I];
                end;
              end;
              BoneParent := BoneParent.Parent;
            end;
            for L := Bones.Count - 1 downto 0 do
            begin
              Bones[L].UpdateWorldTransform;
            end;
            //
            V := V + (VB.Bone.ToWorldSpace(VB.V) * VB.Weight);
            //
            for L := Bones.Count - 1 downto 0 do
            begin
              Bones[L].Restore;
              Bones[L].UpdateWorldTransform;
            end;
          end;
          VertexList[J * 2    ] := V.X;
          VertexList[J * 2 + 1] := V.Y;
        end;
        Vertices.Add(VertexList);
      end;
    finally
      FreeAndNil(Bones);
    end;
  end;

begin
  CombineTimeframes;
  InterpolateTimeframes;
  CalculateVertices;
end;

procedure TWeightedMeshTimeline.BuildNodes(const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I, J: Integer;
  Route: TX3DRoute;
  VertexList: TSingleList;
begin
  Node := TCoordinateInterpolatorNode.Create('WeightedMeshTimeline_' + Name);
  for I := 0 to Time.Count - 1 do
  begin
    Node.FdKey.Items.Add(Time[I] / MaxAnimationTime);
    VertexList := Vertices.Items[I];
    Assert(not Odd(VertexList.Count)); // VertexList is a list of 2D coordinates
    for J := 0 to VertexList.Count div 2 - 1 do
      Node.FdKeyValue.Items.Add(Vector3(
        VertexList.Items[2 * J],
        VertexList.Items[2 * J + 1],
        0
      ));
  end;
  Container.AddChildren(Node);

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(Node.EventValue_changed);
  Route.SetDestinationDirectly(Attachment.Coord.FdPoint.EventIn);
  Container.AddRoute(Route);
end;

{$endif}
