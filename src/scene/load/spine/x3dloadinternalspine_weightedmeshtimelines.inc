{%MainUnit x3dloadinternalspine.pas}
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
  TWeightedMeshVertexList = class({$ifdef FPC}specialize{$endif} TObjectList<TSingleList>)
  end;

  TWeightedMeshTimeline = class
    Name: String;
    Attachment: TMeshAttachment;
    AffectedBoneTimelines: TBoneTimelineList;
    AffectedDeformTimeline: TDeformTimeline;
    Vertices: TWeightedMeshVertexList;
    Time: TSingleList;
    Node: TCoordinateInterpolatorNode;
    constructor Create;
    destructor Destroy; override;
    procedure Parse;
    procedure BuildNodes(const MaxAnimationTime: Single; const Container: TX3DRootNode);
  end;

  TWeightedMeshTimelineList = class({$ifdef FPC}specialize{$endif} TObjectList<TWeightedMeshTimeline>)
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
  - Find all possible timeframes by combining timeframes from every bone/deform timelines
  - Perform interpolation for those bone/deform timelines that doesnt have necesssary timeframes
  - Perform vertex calculation
}
procedure TWeightedMeshTimeline.Parse;
  { Example:
    - Time1: [0, 0.2, 0.65, 1]
    - Time2: [0, 0.3, 0.65, 0.7. 1]
    * Add additional steps to make the animation more accurate, default is 0.15
      Since we are doing animation by using CoordinateInterpolator, in many
      situations, like rotation or non-linear interpolation types, the result is
      not accurate. By adding more steps inbetween we we can achieve a more accurate
      result, as a cost of longer loading time and more memory usage
    - Result: [0, 0.15, 0.2, 0.3, 0.45, 0.6, 0.65, 0.7, 0.75, 0.8, 0.9, 1]
  }
  procedure CombineTimeframes;
  var
    I, J: Integer;
    Value: Single;
    IsRotationRequired: Boolean;
    IsCurved: Boolean;
    IsTimeframeTooLow: Boolean;
  begin
    IsRotationRequired := False;
    IsCurved := False;
    // For bone timelines
    for I := 0 to AffectedBoneTimelines.Count - 1 do
    begin
      if AffectedBoneTimelines[I] is TBoneTimelineRotate then
        IsRotationRequired := True;
      if AffectedBoneTimelines[I].Curve then
        IsCurved := True;
      for J := 0 to AffectedBoneTimelines[I].Time.Count - 1 do
      begin
        Value := AffectedBoneTimelines[I].Time[J];
        if Time.IndexOf(Value) < 0 then
        begin
          Time.Add(Value);
        end;
      end;
    end;
    // For deform timeline
    if AffectedDeformTimeline <> nil then
    begin
      if AffectedDeformTimeline.Curve then
        IsCurved := True;
      for I := 0 to AffectedDeformTimeline.Time.Count - 1 do
      begin
        Value := AffectedDeformTimeline.Time[I];
        if Time.IndexOf(Value) < 0 then
        begin
          Time.Add(Value);
        end;
      end;
    end;
    Time.Sort;
    // Make the animation more accurate by adding more steps inbetween
    // Default step is 0.15
    if IsCurved or IsRotationRequired or (AffectedBoneTimelines.Count > 0) then
    begin
      Value := 0;
      while Value < Time[Time.Count - 1] do
      begin
        IsTimeframeTooLow := False;
        for I := 0 to Time.Count - 1 do
        begin
          if Abs(Time[I] - Value) < 0.15 then
          begin
            IsTimeframeTooLow := True;
            Break;
          end;
        end;
        if not IsTimeframeTooLow then
          Time.Insert(0, Value);
        Value := Value + 0.15;
      end;
      Time.Sort;
    end;
  end;

  procedure InterpolateTimeframes;
  var
    I, J, K: Integer;
    P, T1, T2, OldT1, OldT2: Integer;
    BoneTimeline: TBoneTimeline;
    BoneTimelineTranslate: TBoneTimelineTranslate absolute BoneTimeline;
    BoneTimelineScale: TBoneTimelineScale absolute BoneTimeline;
    BoneTimelineRotate: TBoneTimelineRotate absolute BoneTimeline;
    Timeframe, Timeframe1, Timeframe2: Single;
    IsExists: Boolean;
    VertexList, VertexList1, VertexList2: TSingleList;
    T: Single;
    ControlPoints: TCubicBezier2DPoints;
    CachedBezierCurve: array [0..19] of TVector2;
  begin
    ControlPoints[0] := Vector2(0, 0);
    ControlPoints[3] := Vector2(1, 1);
    OldT1 := 0;
    OldT2 := 0;
    // For bone timeline
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
          // TODO: If there's missing timeframe at the start of the animation,
          // we take the value from the next timeframe. This is not correct
          // as default behavior is to take the setup value and then make a
          // "sudden jump" to next frame
          if P = 0 then
          begin
            BoneTimeline.BackupTime.Insert(0, Timeframe);
            if BoneTimeline is TBoneTimelineTranslate then
            begin
              BoneTimelineTranslate.Vectors.Insert(P, BoneTimelineTranslate.Vectors[P]);
              // Add missing value to backup values as safeguard
              BoneTimelineTranslate.BackupVectors.Insert(P, BoneTimelineTranslate.Vectors[P]);
            end else
            if BoneTimeline is TBoneTimelineRotate then
            begin
              BoneTimelineRotate.Angles.Insert(P, BoneTimelineRotate.Angles[P]);
              // Add missing value to backup values as safeguard
              BoneTimelineRotate.BackupAngles.Insert(P, BoneTimelineRotate.BackupAngles[P]);
            end else
            if BoneTimeline is TBoneTimelineScale then
            begin
              BoneTimelineScale.Vectors.Insert(P, BoneTimelineScale.Vectors[P]);
              // Add missing value to backup values as safeguard
              BoneTimelineScale.BackupVectors.Insert(P, BoneTimelineScale.BackupVectors[P]);
            end;
            BoneTimeline.CurveControlPoints.Insert(P, Vector4(0, 0, 1, 1));
          end else
          if P = BoneTimeline.Time.Count - 1 then
          begin
            BoneTimeline.BackupTime.Add(Timeframe);
            if BoneTimeline is TBoneTimelineTranslate then
            begin
              BoneTimelineTranslate.Vectors.Add(BoneTimelineTranslate.Vectors[BoneTimelineTranslate.Vectors.Count - 1]);
              // Add missing value to backup values as safeguard
              BoneTimelineTranslate.BackupVectors.Add(BoneTimelineTranslate.BackupVectors[BoneTimelineTranslate.BackupVectors.Count - 1]);
            end else
            if BoneTimeline is TBoneTimelineRotate then
            begin
              BoneTimelineRotate.Angles.Add(BoneTimelineRotate.Angles[BoneTimelineRotate.Angles.Count - 1]);
              // Add missing value to backup values as safeguard
              BoneTimelineRotate.BackupAngles.Add(BoneTimelineRotate.BackupAngles[BoneTimelineRotate.BackupAngles.Count - 1]);
            end else
            if BoneTimeline is TBoneTimelineScale then
            begin
              BoneTimelineScale.Vectors.Add(BoneTimelineScale.Vectors[BoneTimelineScale.Vectors.Count - 1]);
              // Add missing value to backup values as safeguard
              BoneTimelineScale.BackupVectors.Add(BoneTimelineScale.BackupVectors[BoneTimelineScale.BackupVectors.Count - 1]);
            end;
            // Add missing value to backup values as safeguard
            BoneTimeline.CurveControlPoints.Add(Vector4(0, 0, 1, 1));
          end else
          begin
            // If this timeline has control points, then we need to search timeframe in original
            // time instead for accurary result
            if BoneTimeline.Curve then
            begin
              T1 := 0;
              T2 := 0;
              for K := BoneTimeline.BackupTime.Count - 2 downto 0 do
              begin
                if (Timeframe > BoneTimeline.BackupTime[K]) then
                begin
                  T1 := K;
                  T2 := K + 1;
                  Break;
                end;
              end;
              // This is a different timeframe compare to previous one?
              // Then we need to generate a bezier curve in order to use it
              // later for finding step value
              if (T1 <> OldT1) or (T2 <> OldT2) then
              begin;
                ControlPoints[1] := Vector2(BoneTimeline.CurveControlPoints[T1][0], BoneTimeline.CurveControlPoints[T1][1]);
                ControlPoints[2] := Vector2(BoneTimeline.CurveControlPoints[T1][2], BoneTimeline.CurveControlPoints[T1][3]);
                for K := 0 to High(CachedBezierCurve) do
                begin
                  CachedBezierCurve[K] := CubicBezier2D(K / High(CachedBezierCurve), ControlPoints);
                end;
                OldT1 := T1;
                OldT2 := T2;
              end;
              Timeframe1 := BoneTimeline.BackupTime[T1];
              Timeframe2 := BoneTimeline.BackupTime[T2];
              T := (Timeframe - Timeframe1) / (Timeframe2 - Timeframe1);
              for K := 1 to High(CachedBezierCurve) do
              begin
                if CachedBezierCurve[K].X > T then
                begin
                  T := Lerp(
                       (T - CachedBezierCurve[K - 1].X) / (CachedBezierCurve[K].X - CachedBezierCurve[K - 1].X),
                       CachedBezierCurve[K - 1].Y, CachedBezierCurve[K].Y);
                  Break;
                end;
              end;
              if BoneTimeline is TBoneTimelineTranslate then
              begin
                BoneTimelineTranslate.Vectors.Insert(P,
                    Lerp(T, BoneTimelineTranslate.BackupVectors.List^[T1], BoneTimelineTranslate.BackupVectors.List^[T2]));
              end else
              if BoneTimeline is TBoneTimelineRotate then
              begin
                BoneTimelineRotate.Angles.Insert(P,
                    Lerp(T, BoneTimelineRotate.BackupAngles.List^[T1], BoneTimelineRotate.BackupAngles.List^[T2]));
              end else
              if BoneTimeline is TBoneTimelineScale then
              begin
                BoneTimelineScale.Vectors.Insert(P,
                    Lerp(T, BoneTimelineScale.BackupVectors.List^[T1], BoneTimelineScale.BackupVectors.List^[T2]));
              end;
            end else
            begin
              T1 := P - 1;
              T2 := P + 1;
              Timeframe1 := BoneTimeline.Time[T1];
              Timeframe2 := BoneTimeline.Time[T2];
              T := (Timeframe - Timeframe1) / (Timeframe2 - Timeframe1);
              if BoneTimeline is TBoneTimelineTranslate then
              begin
                BoneTimelineTranslate.Vectors.Insert(P,
                    Lerp(T, BoneTimelineTranslate.Vectors.List^[T1], BoneTimelineTranslate.Vectors.List^[P]));
              end else
              if BoneTimeline is TBoneTimelineRotate then
              begin
                BoneTimelineRotate.Angles.Insert(P,
                    Lerp(T, BoneTimelineRotate.Angles.List^[T1], BoneTimelineRotate.Angles.List^[P]));
              end else
              if BoneTimeline is TBoneTimelineScale then
              begin
                BoneTimelineScale.Vectors.Insert(P,
                    Lerp(T, BoneTimelineScale.Vectors.List^[T1], BoneTimelineScale.Vectors.List^[P]));
              end;
            end;
          end;
        end;
      end;
    end;
    // For deform timeline
    if AffectedDeformTimeline <> nil then
    begin
      for J := 0 to Time.Count - 1 do
      begin
        Timeframe := Time[J];
        // Check to see if this timeframe exists in deform timeline
        IsExists := False;
        for K := 0 to AffectedDeformTimeline.Time.Count - 1 do
        begin
          if Timeframe = AffectedDeformTimeline.Time[K] then
          begin
            IsExists := True;
            Break;
          end;
        end;
        // If timeframe does not exist, we insert it into deform timeline and then
        // generate new values for this timeframe by interpolating 2 values between it
        if not IsExists then
        begin
          AffectedDeformTimeline.Time.Add(Timeframe);
          AffectedDeformTimeline.Time.Sort;
          P := AffectedDeformTimeline.Time.IndexOf(Timeframe);
          VertexList := TSingleList.Create;
          // TODO: If there's missing timeframe at the start of the animation,
          // we take the value from the next timeframe. This is not correct
          // as default behavior is to take the setup value and then make a
          // "sudden jump" to next frame
          if P = 0 then
          begin
            AffectedDeformTimeline.BackupTime.Insert(0, Timeframe);
            VertexList.AddRange(AffectedDeformTimeline.Vertices[P]);
            AffectedDeformTimeline.Vertices.Insert(P, VertexList);
            // Add missing value to backup values as safeguard
            AffectedDeformTimeline.BackupVertices.Insert(P, VertexList);
            AffectedDeformTimeline.CurveControlPoints.Insert(P, Vector4(0, 0, 1, 1));
          end else
          if P = AffectedDeformTimeline.Time.Count - 1 then
          begin
            AffectedDeformTimeline.BackupTime.Add(Timeframe);
            VertexList.AddRange(AffectedDeformTimeline.Vertices[AffectedDeformTimeline.Vertices.Count - 1]);
            AffectedDeformTimeline.Vertices.Add(VertexList);
            // Add missing value to backup values as safeguard
            AffectedDeformTimeline.BackupVertices.Add(VertexList);
            AffectedDeformTimeline.CurveControlPoints.Add(Vector4(0, 0, 1, 1));
          end else
          begin
            // If this timeline has control points, then we need to search timeframe in original
            // time instead for accurary result
            if AffectedDeformTimeline.Curve then
            begin
              T1 := 0;
              T2 := 0;
              for K := AffectedDeformTimeline.BackupTime.Count - 2 downto 0 do
              begin
                if (Timeframe > AffectedDeformTimeline.BackupTime[K]) then
                begin
                  T1 := K;
                  T2 := K + 1;
                  Break;
                end;
              end;
              // This is a different timeframe compare to previous one?
              // Then we need to generate a bezier curve in order to use it
              // later for finding step value
              if (T1 <> OldT1) or (T2 <> OldT2) then
              begin;
                ControlPoints[1] := Vector2(AffectedDeformTimeline.CurveControlPoints[T1][0], AffectedDeformTimeline.CurveControlPoints[T1][1]);
                ControlPoints[2] := Vector2(AffectedDeformTimeline.CurveControlPoints[T1][2], AffectedDeformTimeline.CurveControlPoints[T1][3]);
                for K := 0 to High(CachedBezierCurve) do
                begin
                  CachedBezierCurve[K] := CubicBezier2D(K / High(CachedBezierCurve), ControlPoints);
                end;
                OldT1 := T1;
                OldT2 := T2;
              end;
              Timeframe1 := AffectedDeformTimeline.BackupTime[T1];
              Timeframe2 := AffectedDeformTimeline.BackupTime[T2];
              VertexList1 := AffectedDeformTimeline.BackupVertices[T1];
              VertexList2 := AffectedDeformTimeline.BackupVertices[T2];
              VertexList.Count := VertexList1.Count;
              T := (Timeframe - Timeframe1) / (Timeframe2 - Timeframe1);
              for K := 1 to High(CachedBezierCurve) do
              begin
                if CachedBezierCurve[K].X > T then
                begin
                  T := Lerp(
                       (T - CachedBezierCurve[K - 1].X) / (CachedBezierCurve[K].X - CachedBezierCurve[K - 1].X),
                       CachedBezierCurve[K - 1].Y, CachedBezierCurve[K].Y);
                  Break;
                end;
              end;
            end else
            begin
              T1 := P - 1;
              T2 := P + 1;
              Timeframe1 := AffectedDeformTimeline.Time[T1];
              Timeframe2 := AffectedDeformTimeline.Time[T2];
              VertexList1 := AffectedDeformTimeline.Vertices[T1];
              VertexList2 := AffectedDeformTimeline.Vertices[P];
              VertexList.Count := VertexList1.Count;
              T := (Timeframe - Timeframe1) / (Timeframe2 - Timeframe1);
            end;
            for K := 0 to VertexList1.Count - 1 do
            begin
              VertexList[K] := Lerp(T, VertexList1[K], VertexList2[K]);
            end;
            AffectedDeformTimeline.Vertices.Insert(P, VertexList);
          end;
        end;
      end;
    end;
  end;

  procedure CalculateVertices;
  var
    I, J, K, L: Integer;
    Offset: Integer;
    VB: TMeshVertexBone;
    V, V2: TVector2;
    DeformVertexList,
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
            Offset := Attachment.Weights[J].Offset;
            //
            Bones.Clear;
            BoneParent := VB.Bone;
            // Recalculate bone structure based on timeline values
            while BoneParent <> nil do
            begin
              BoneParent.Backup;
              Bones.Add(BoneParent);
              for L := 0 to AffectedBoneTimelines.Count - 1 do
              begin
                if AffectedBoneTimelines[L].Bone.Name = BoneParent.Name then
                begin
                  if AffectedBoneTimelines[L] is TBoneTimelineTranslate then
                    BoneParent.XY := BoneParent.XY + (AffectedBoneTimelines[L] as TBoneTimelineTranslate).Vectors[I]
                  else
                  if AffectedBoneTimelines[L] is TBoneTimelineRotate then
                    BoneParent.Rotation := BoneParent.Rotation + (AffectedBoneTimelines[L] as TBoneTimelineRotate).Angles[I]
                  else
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
            if AffectedDeformTimeline <> nil then
            begin
              DeformVertexList := AffectedDeformTimeline.Vertices[I];
              V2.X := DeformVertexList[Offset + K * 2];
              V2.Y := DeformVertexList[Offset + K * 2 + 1];
              V := V + (VB.Bone.ToWorldSpace(V2) * VB.Weight);
            end
            else
              V := V + (VB.Bone.ToWorldSpace(VB.V) * VB.Weight);
            // Restore bone structure
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
