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
  TDeformVertexList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSingleList>)
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
    Node: TCoordinateInterpolatorNode;
    NodeUsedAsChild: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONArray);
    procedure BuildNodes(const MaxAnimationTime: Single; const Container: TX3DRootNode);
    property MaxTime: Single read FMaxTime;
    procedure Backup;
    procedure Restore;
  end;

  TDeformTimelineList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TDeformTimeline>)
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

procedure TDeformTimeline.Parse(const Json: TJSONArray);
var
  I, J, K: Integer;
  Obj: TJSONObject;
  VertexJsonArray: TJSONArray;
  VertexList: TSingleList;
  Offset: Integer;
  VB: TMeshVertexBone;
  ControlPoints: TVector4;
begin
  for I := 0 to Json.Count - 1 do
  begin
    Obj := Json.Items[I] as TJSONObject;
    Time.Add(Obj.Get('time', 0.0));
    Offset := Obj.Get('offset', 0);
    ReadCurveControlPoints('deform', Obj, ControlPoints, Curve);
    CurveControlPoints.Add(ControlPoints);

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
  begin
    { TODO: Take care of 'curve' field once we implement CubicBezier version of
      TCoordinateInterpolatorNode }
    if Attachment.Weights.Count = 0 then
      WritelnWarning('Spine', 'Deform timeline''s "curve" field is not supported for non-weighted mesh');
  end;
end;

procedure TDeformTimeline.BuildNodes(const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I, J: Integer;
  Route: TX3DRoute;
  VertexList: TSingleList;
begin
  Node := TCoordinateInterpolatorNode.Create('DeformTimeline_' + Slot.Name);
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

  NodeUsedAsChild := true;
  Container.AddChildren(Node);

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(Node.EventValue_changed);
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

{$endif}
