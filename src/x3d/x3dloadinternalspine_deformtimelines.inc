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
  public
    Slot: TSlot;
    Attachment: TMeshAttachment;
    Time: TSingleList;
    Vertices: TDeformVertexList;
    Node: TCoordinateInterpolatorNode;
    NodeUsedAsChild: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONArray);
    procedure BuildNodes(const MaxAnimationTime: Single; const Container: TX3DRootNode);
    property MaxTime: Single read FMaxTime;
  end;

  TDeformTimelineList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TDeformTimeline>)
  end;
{$endif}

{$ifdef read_implementation}

constructor TDeformTimeline.Create;
begin
  inherited;
  Time := TSingleList.Create;
  Vertices := TDeformVertexList.Create(true);
end;

destructor TDeformTimeline.Destroy;
begin
  FreeAndNil(Time);
  FreeAndNil(Vertices);
  inherited;
end;

procedure TDeformTimeline.Parse(const Json: TJSONArray);
var
  I, J: Integer;
  Obj: TJSONObject;
  CurveJson: TJSONData;
  VertexJsonArray: TJSONArray;
  VertexList: TSingleList;
  Offset: Integer;
begin
  for I := 0 to Json.Count - 1 do
  begin
    Obj := Json.Items[I] as TJSONObject;
    Time.Add(Obj.Get('time', 0.0));
    Offset := Obj.Get('offset', 0);
    { TODO: Take care of 'curve' field once we implement CubicBezier version of
      TCoordinateInterpolatorNode }
    CurveJson := Obj.Find('curve');
    if CurveJson <> nil then
      if CurveJson is TJSONArray then
        WritelnWarning('Spine', 'Deform timeline''s "curve" field is not supported');
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

    VertexList.Count := Attachment.Vertices.Count * 2;
    for J := 0 to Attachment.Vertices.Count - 1 do
    begin
      VertexList[J * 2    ] := Attachment.Vertices[J].X;
      VertexList[J * 2 + 1] := Attachment.Vertices[J].Y;
    end;

    if VertexJsonArray <> nil then
      for J := 0 to VertexJsonArray.Count - 1 do
        VertexList[Offset + J] :=
          VertexList[Offset + J] + VertexJsonArray.Items[J].AsFloat;

    Vertices.Add(VertexList);
  end;
  FMaxTime := Time.Items[Time.Count - 1];
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

{$endif}
