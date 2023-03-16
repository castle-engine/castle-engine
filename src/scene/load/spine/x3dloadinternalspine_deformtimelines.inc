{%MainUnit x3dloadinternalspine.pas}
{
  Copyright 2019 Trung Le (kagamma).

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine deform timelines. }

{$ifdef read_interface}
  TDeformVertexList = class({$ifdef FPC}specialize{$endif} TObjectList<TSingleList>)
  end;

  TDeformTimeline = class
  strict private
    FMaxTime: Single;
    IsBackup: Boolean;
  public
    { If this is true, then we will ignore this deform during building node }
    IsHandledByWeightedMeshTimeline: Boolean;
    Slot: TSlot;
    Attachment: TMeshAttachment;
    Time: TSingleList;
    BackupTime: TSingleList;
    Vertices: TDeformVertexList;
    BackupVertices: TDeformVertexList;
    Curve: Boolean;
    CurveControlPoints: TVector4List;
    Node: TAbstractInterpolatorNode;
    NodeUsedAsChild: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONArray;
      const NormalizeCurvesKeyValues: Boolean); virtual;
    procedure BuildNodes(const MaxAnimationTime: Single; const Container: TX3DRootNode);
    property MaxTime: Single read FMaxTime;
    procedure Backup;
    procedure Restore;
    procedure ParseControlPoints(const Json: TJSONArray;
      const NormalizeCurvesKeyValues: Boolean);
  end;

  TDeformTimelineList = class({$ifdef FPC}specialize{$endif} TObjectList<TDeformTimeline>)
  end;
{$endif}

{$ifdef read_implementation}

constructor TDeformTimeline.Create;
begin
  inherited;
  Time := TSingleList.Create;
  BackupTime := TSingleList.Create;
  Vertices := TDeformVertexList.Create(True);
  BackupVertices := TDeformVertexList.Create(False);
  CurveControlPoints := TVector4List.Create;
  Curve := False;
end;

destructor TDeformTimeline.Destroy;
begin
  FreeAndNil(Time);
  FreeAndNil(BackupTime);
  FreeAndNil(Vertices);
  FreeAndNil(BackupVertices);
  FreeAndNil(CurveControlPoints);
  inherited;
end;

procedure TDeformTimeline.Parse(const Json: TJSONArray;
  const NormalizeCurvesKeyValues: Boolean);
var
  I, J, K: Integer;
  Obj: TJSONObject;
  VertexJsonArray: TJSONArray;
  VertexList: TSingleList;
  Offset: Integer;
  VB: TMeshVertexBone;
begin
  Curve := false;

  for I := 0 to Json.Count - 1 do
  begin
    Obj := Json.Items[I] as TJSONObject;
    Time.Add(Obj.Get('time', 0.0));
    Offset := Obj.Get('offset', 0);

    // only detect whether we have a curve at this point
    if not Curve then
      Curve := Obj.Find('curve') is TJSONArray;

    VertexJsonArray := Obj.Get('vertices', TJSONArray(nil));
    VertexList := TSingleList.Create;

    { Layout of VertexList:

      - static vertices (until Offset)
      - animated vertices (from VertexJsonArray)
      - static vertices (until the end)

      Note that Offset and VertexJsonArray.Count may be odd
      (see https://github.com/castle-engine/demo-models/tree/master/spine/free_form_deformation
      for demo).
    }
    if Attachment.Weights.Count = 0 then
    // For mesh without weights
    begin
      VertexList.Count := Attachment.Vertices.Count * 2;
      for J := 0 to Attachment.Vertices.Count - 1 do
      begin
        VertexList[J * 2    ] := Attachment.Vertices[J].X;
        VertexList[J * 2 + 1] := Attachment.Vertices[J].Y;
      end;
    end else
    // Things are a bit different for weighted mesh
    begin
      VertexList.Count := Attachment.Weights[Attachment.Weights.Count - 1].Offset
          + Attachment.Weights[Attachment.Weights.Count - 1].Bones.Count * 2;
      for J := 0 to Attachment.Weights.Count - 1 do
      begin
        for K := 0 to Attachment.Weights[J].Bones.Count - 1 do
        begin
          VB := Attachment.Weights[J].Bones[K];
          VertexList[Attachment.Weights[J].Offset + K * 2    ] := VB.V.X;
          VertexList[Attachment.Weights[J].Offset + K * 2 + 1] := VB.V.Y;
        end;
      end;
    end;

    if VertexJsonArray <> nil then
      for J := 0 to VertexJsonArray.Count - 1 do
        VertexList[Offset + J] :=
          VertexList[Offset + J] + VertexJsonArray.Items[J].AsFloat;

    Vertices.Add(VertexList);
  end;
  FMaxTime := Time.Items[Time.Count - 1];

  if Curve then
    ParseControlPoints(Json, NormalizeCurvesKeyValues);
end;

procedure TDeformTimeline.BuildNodes(const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I, J: Integer;
  Route: TX3DRoute;
  VertexList: TSingleList;
  EventValueChanged: TX3DEvent;
  N: TCoordinateInterpolatorNode;
  NC: TCubicBezierCoordinateInterpolatorNode;
begin
  if Curve then
  begin
    NC := TCubicBezierCoordinateInterpolatorNode.Create('DeformTimeline_' + Slot.Name);
    for I := 0 to Time.Count - 1 do
    begin
      NC.FdKey.Items.Add(Time[I] / MaxAnimationTime);
      VertexList := Vertices.Items[I];
      Assert(not Odd(VertexList.Count)); // VertexList is a list of 2D coordinates
      for J := 0 to VertexList.Count div 2 - 1 do
        NC.FdKeyValue.Items.Add(Vector3(
          VertexList.Items[2 * J],
          VertexList.Items[2 * J + 1],
          0
        ));
    end;
    NC.SetControlPoints(CurveControlPoints);
    NC.OptimizeControlPoints;
    EventValueChanged := NC.EventValue_changed;
    Node := NC;
  end else
  begin
    N := TCoordinateInterpolatorNode.Create('DeformTimeline_' + Slot.Name);
    for I := 0 to Time.Count - 1 do
    begin
      N.FdKey.Items.Add(Time[I] / MaxAnimationTime);
      VertexList := Vertices.Items[I];
      Assert(not Odd(VertexList.Count)); // VertexList is a list of 2D coordinates
      for J := 0 to VertexList.Count div 2 - 1 do
        N.FdKeyValue.Items.Add(Vector3(
          VertexList.Items[2 * J],
          VertexList.Items[2 * J + 1],
          0
        ));
    end;
    EventValueChanged := N.EventValue_changed;
    Node := N;
  end;

  NodeUsedAsChild := true;
  Container.AddChildren(Node);

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(EventValueChanged);
  Route.SetDestinationDirectly(Attachment.Coord.FdPoint.EventIn);
  Container.AddRoute(Route);
end;

procedure TDeformTimeline.Backup;
begin
  if not IsBackup then
  begin
    BackupTime.Clear;
    BackupVertices.Clear;
    BackupTime.AddRange(Time);
    BackupVertices.AddRange(Vertices);
    IsBackup := True;
  end;
end;

procedure TDeformTimeline.Restore;
begin
  if IsBackup then
  begin
    Time.Clear;
    Vertices.Clear;
    Time.AddRange(BackupTime);
    Vertices.AddRange(BackupVertices);
    IsBackup := False;
  end;
end;

procedure TDeformTimeline.ParseControlPoints(const Json: TJSONArray;
  const NormalizeCurvesKeyValues: Boolean);
var
  I: Integer;
  PreviousTime, NextTime: Single;
  O: TJSONObject;
begin
  Assert(Time.Count = Vertices.Count);
  if Json.Count <> Time.Count then
  begin
    WritelnWarning('Invalid Spine JSON: Curve support requires that timeline JSON count is correct');
    Curve := false;
    Exit;
  end;
  for I := 0 to Time.Count - 2 do
  begin
    PreviousTime := Time[I];
    NextTime     := Time[I + 1];
    if not (Json[I] is TJSONObject) then
    begin
      WritelnWarning('Invalid Spine JSON: Curve support requires that all JSON timeline chldren are object');
      Curve := false;
      Exit;
    end;
    O := TJSONObject(Json[I]);
    CurveControlPoints.Add(ReadCurveControlPointsTimeOnly(O, NormalizeCurvesKeyValues, PreviousTime, NextTime));
  end;
  CurveControlPoints.Add(Vector4(0, 0, 1, 1)); // TODO: add last dummy value, because our TCubicBezier* may expect it
end;

{$endif}
