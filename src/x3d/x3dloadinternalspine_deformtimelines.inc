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
  TDeformVertexList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TVector3List>)
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
  Vertices := TDeformVertexList.Create;
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
  VertexJsonArray: TJSONArray;
  VertexList: TVector3List;
  V3: TVector3;
  Offset: Integer;
begin
  for I := 0 to Json.Count-1 do
  begin
    Obj := Json.Items[I] as TJSONObject;
    Time.Add(Obj.Get('time', 0.0));
    Offset := Obj.Get('offset', 0) div 2;
    { TODO: Take care of 'curve' field once we implement CubicBezier version of
      TCoordinateInterpolatorNode }
    VertexJsonArray := Obj.Get('vertices', TJSONArray.Create);
    VertexList := TVector3List.Create;
    { |---static vertices (offset)---|---anim vertices---|---static vertices---| }
    for J := 0 to Offset - 1 do
      VertexList.Add(Vector3(Attachment.Vertices.Items[J], 0));
    for J := 0 to VertexJsonArray.Count div 2 - 1 do
    begin
      V3 := Vector3(VertexJsonArray.Items[J * 2].AsFloat,
                    VertexJsonArray.Items[J * 2 + 1].AsFloat,
                    0);
      V3 := V3 + Vector3(Attachment.Vertices.Items[Offset + J], 0);
      VertexList.Add(V3);
    end;
    for J := Offset + VertexJsonArray.Count div 2 to Attachment.Vertices.Count - 1 do
      VertexList.Add(Vector3(Attachment.Vertices.Items[J], 0));
    Vertices.Add(VertexList);
  end;
  FMaxTime := Time.Items[Time.Count - 1];
end;

procedure TDeformTimeline.BuildNodes(const MaxAnimationTime: Single; const Container: TX3DRootNode);
var
  I, J: Integer;
  Route: TX3DRoute;
  VertexList: TVector3List;
begin
  Node := TCoordinateInterpolatorNode.Create('DeformTimeline_' + Slot.Name);
  for I := 0 to Time.Count - 1 do
  begin
    Node.FdKey.Items.Add(Time[I] / MaxAnimationTime);
    VertexList := Vertices.Items[I];
    for J := 0 to VertexList.Count - 1 do
      Node.FdKeyValue.Items.Add(VertexList.Items[J]);
  end;

  NodeUsedAsChild := true;
  Container.AddChildren(Node);

  Route := TX3DRoute.Create;
  Route.SetSourceDirectly(Node.EventValue_changed);
  Route.SetDestinationDirectly(Attachment.Coord.FdPoint.EventIn);
  Container.AddRoute(Route);
end;

{$endif}

