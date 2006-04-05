{
  Copyright 2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(For user-oriented description what are sectors and waypoints,
  when they should be used etc. see the README file with
  my game "The Castle".) }
unit VRMLSceneWaypoints;

interface

uses SysUtils, KambiUtils, KambiClassUtils, VectorMath, Boxes3d, VRMLNodes;

{$define read_interface}

type
  TSceneSectorsList = class;

  TSceneWaypoint = class
  private
    FSectors: TSceneSectorsList;
  public
    constructor Create;
    destructor Destroy; override;

    Position: TVector3Single;

    { Sectors that contain this waypoint. }
    property Sectors: TSceneSectorsList read FSectors;
  end;

  TObjectsListItem_1 = TSceneWaypoint;
  {$I objectslist_1.inc}
  TSceneWaypointsList = class(TObjectsList_1)
  private
    NodesToRemove: TVRMLNodesList;
    procedure TraverseForWaypoints(Node: TVRMLNode;
      State: TVRMLGraphTraverseState);
  public
    { Shapes placed under the name Waypoint<index>_<ignored>
      are removed from the Node, and are added as new waypoint with
      given index. Waypoint's Position is set to the middle point
      of shape's bounding box.

      Count of this list is enlarged, if necessary,
      to include all waypoints indicated in the Node.

      If the Node is part of some TVRMLFlatScene instance, remember to call
      scene's ChangedAll after this. }
    procedure ExtractPositions(Node: TVRMLNode);
  end;

  TSceneSector = class
  private
    FBoundingBoxes: TDynBox3dArray;
    FVisibleSectors: TDynBooleanArray;
    FWaypoints: TSceneWaypointsList;
  public
    constructor Create;
    destructor Destroy; override;

    property BoundingBoxes: TDynBox3dArray read FBoundingBoxes;

    { Returns whether Point is inside the sector.
      Implementation in TSceneSector just returns if Point is inside
      one of the BoundingBoxes. You can override this to define
      the sector geometry in a more flexible way.

      Remember to also override SectorsBoxesMargin. }
    function IsPointInside(const Point: TVector3Single): boolean; virtual;

    { This is like IsPointInside, but it's supposed to enlarge the geometry
      by SectorsBoxesMargin.

      This is used only by LinkToWaypoints.
      If you don't use LinkToWaypoints (because you create all links
      between waypoints and sectors some other way, e.g. manually
      code them in Pascal), you don't have to care about overriding this
      in descendants. }
    function IsPointInsideMargin(const Point: TVector3Single;
      const SectorsBoxesMargin: Single): boolean; virtual;

    { What sectors are visible from this sector.

      When reading this, you should generally be prepared that length
      of it may be smaller than all sectors of your scene.
      That's because sometimes we don't initialize VisibleSectors fully.

      In this case you should assume that not initialized sector
      indexes are visible. And always you can assume that
      Always Sectors[I].VisibleSectors[I] = @true, i.e. the sector
      is visible from itself (assuming that VisibleSectors has
      enough length to contain I). }
    property VisibleSectors: TDynBooleanArray read FVisibleSectors;

    { Waypoints that are included in this sector. }
    property Waypoints: TSceneWaypointsList read FWaypoints;
  end;

  ESectorNotInitialized = class(Exception);
  EWaypointNotInitialized = class(Exception);

  TObjectsListItem_2 = TSceneSector;
  {$I objectslist_2.inc}
  TSceneSectorsList = class(TObjectsList_2)
  private
    NodesToRemove: TVRMLNodesList;
    procedure TraverseForSectors(Node: TVRMLNode;
      State: TVRMLGraphTraverseState);
  public
    { Shapes placed under the name Sector<index>_<ignored>
      are removed from the Node, and are added to sector <index> BoundingBoxes.

      Count of this list is enlarged, if necessary,
      to include all sectors indicated in the Node.

      If the Node is part of some TVRMLFlatScene instance, remember to call
      scene's ChangedAll after this. }
    procedure ExtractBoundingBoxes(Node: TVRMLNode);

    { This adds appropriate Waypoints to all sectors on this list,
      and adds appropriate Sectors to all Waypoints on given list.

      A waypoint is considered to be within the sector, if
      Sector.IsPointInsideMargin(Waypoint.Position, SectorsBoxesMargin)
      is true. The SectorsBoxesMargin is needed to avoid any kind of
      uncertainty when the waypoint's position is at the very border
      of the sector.

      @raises ESectorNotInitialized When some sector is nil.
      @raises EWaypointNotInitialized When some waypoint is nil. }
    procedure LinkToWaypoints(Waypoints: TSceneWaypointsList;
      const SectorsBoxesMargin: Single);
  end;

{$undef read_interface}

implementation

uses KambiStringUtils;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

{ TSceneWaypoint ------------------------------------------------------------- }

constructor TSceneWaypoint.Create;
begin
  inherited Create;
  FSectors := TSceneSectorsList.Create;
end;

destructor TSceneWaypoint.Destroy;
begin
  FreeAndNil(FSectors);
  inherited;
end;

{ TSceneWaypointsList -------------------------------------------------------- }

procedure TSceneWaypointsList.TraverseForWaypoints(Node: TVRMLNode;
  State: TVRMLGraphTraverseState);

  procedure CreateNewWaypoint(const WaypointNodeName: string);
  var
    IgnoredBegin, WaypointIndex: Integer;
    WaypointPosition: TVector3Single;
  begin
    { Calculate WaypointIndex }
    IgnoredBegin := Pos('_', WaypointNodeName);
    if IgnoredBegin = 0 then
      WaypointIndex := StrToInt(WaypointNodeName) else
      WaypointIndex := StrToInt(Copy(WaypointNodeName, 1, IgnoredBegin - 1));

    WaypointPosition := Box3dMiddle(
      (Node as TNodeGeneralShape).BoundingBox(State));

    Count := Max(Count, WaypointIndex + 1);
    if Items[WaypointIndex] <> nil then
      raise Exception.CreateFmt('Waypoint %d is already initialized',
        [WaypointIndex]);

    Items[WaypointIndex] := TSceneWaypoint.Create;
    Items[WaypointIndex].Position := WaypointPosition;
  end;

const
  WaypointPrefix = 'Waypoint';
var
  ParentIndex: Integer;
  Parent: TVRMLNode;
begin
  for ParentIndex := 0 to Node.ParentsCount - 1 do
  begin
    Parent := Node.Parents[ParentIndex];
    if IsPrefix(WaypointPrefix, Parent.NodeName) then
    begin
      CreateNewWaypoint(SEnding(Parent.NodeName, Length(WaypointPrefix) + 1));
      { Don't remove Parent now --- will be removed later.
        This avoids problems with removing nodes while traversing. }
      NodesToRemove.Add(Parent);
      Break;
    end;
  end;
end;

procedure TSceneWaypointsList.ExtractPositions(Node: TVRMLNode);
var
  I: Integer;
begin
  NodesToRemove := TVRMLNodesList.Create;
  try
    Node.TraverseFromDefaultState(TNodeGeneralShape, TraverseForWaypoints);
    for I := 0 to NodesToRemove.Count - 1 do
      NodesToRemove.Items[I].FreeRemovingFromAllParents;
  finally NodesToRemove.Free end;
end;

{ TSceneSector --------------------------------------------------------------- }

constructor TSceneSector.Create;
begin
  inherited Create;
  FBoundingBoxes := TDynBox3dArray.Create;
  FVisibleSectors := TDynBooleanArray.Create;
  FWaypoints := TSceneWaypointsList.Create;
end;

destructor TSceneSector.Destroy;
begin
  FreeAndNil(FBoundingBoxes);
  FreeAndNil(FVisibleSectors);
  FreeAndNil(FWaypoints);
  inherited;
end;

function TSceneSector.IsPointInside(const Point: TVector3Single): boolean;
var
  I: Integer;
begin
  { This could be implemented as IsPointInsideMargin(Point, 0),
    but is not (for speed). }
  for I := 0 to BoundingBoxes.High do
    if Box3dPointInside(Point, BoundingBoxes.Items[I]) then
      Exit(true);
  Result := false;
end;

function TSceneSector.IsPointInsideMargin(const Point: TVector3Single;
  const SectorsBoxesMargin: Single): boolean;
var
  I: Integer;
begin
  for I := 0 to BoundingBoxes.High do
    if Box3dPointInside(Point,
      BoxExpand(BoundingBoxes.Items[I], SectorsBoxesMargin)) then
      Exit(true);
  Result := false;
end;

{ TSceneSectorsList -------------------------------------------------------- }

procedure TSceneSectorsList.TraverseForSectors(Node: TVRMLNode;
  State: TVRMLGraphTraverseState);

  procedure AddSectorBoundingBox(const SectorNodeName: string);
  var
    IgnoredBegin, SectorIndex: Integer;
    SectorBoundingBox: TBox3d;
  begin
    { Calculate SectorIndex }
    IgnoredBegin := Pos('_', SectorNodeName);
    if IgnoredBegin = 0 then
      SectorIndex := StrToInt(SectorNodeName) else
      SectorIndex := StrToInt(Copy(SectorNodeName, 1, IgnoredBegin - 1));

    SectorBoundingBox := (Node as TNodeGeneralShape).BoundingBox(State);

    Count := Max(Count, SectorIndex + 1);
    if Items[SectorIndex] = nil then
      Items[SectorIndex] := TSceneSector.Create;

    Items[SectorIndex].BoundingBoxes.AppendItem(SectorBoundingBox);
  end;

const
  SectorPrefix = 'Sector';
var
  ParentIndex: Integer;
  Parent: TVRMLNode;
begin
  for ParentIndex := 0 to Node.ParentsCount - 1 do
  begin
    Parent := Node.Parents[ParentIndex];
    if IsPrefix(SectorPrefix, Parent.NodeName) then
    begin
      AddSectorBoundingBox(SEnding(Parent.NodeName, Length(SectorPrefix) + 1));
      { Don't remove Parent now --- will be removed later.
        This avoids problems with removing nodes while traversing. }
      NodesToRemove.Add(Parent);
      Break;
    end;
  end;
end;

procedure TSceneSectorsList.ExtractBoundingBoxes(Node: TVRMLNode);
var
  I: Integer;
begin
  NodesToRemove := TVRMLNodesList.Create;
  try
    Node.TraverseFromDefaultState(TNodeGeneralShape, TraverseForSectors);
    for I := 0 to NodesToRemove.Count - 1 do
      NodesToRemove.Items[I].FreeRemovingFromAllParents;
  finally NodesToRemove.Free end;
end;

procedure TSceneSectorsList.LinkToWaypoints(Waypoints: TSceneWaypointsList;
  const SectorsBoxesMargin: Single);
var
  S: TSceneSector;
  W: TSceneWaypoint;
  SectorIndex, WaypointIndex: Integer;
begin
  { Note that this method is usually the first to be called after doing
    things like ExtractBoundingBoxes or ExtractPosotions,
    so we try here to make nice error messages when some sector
    or waypoint is not initialized yet (i.e. = nil). }

  for SectorIndex := 0 to High do
  begin
    S := Items[SectorIndex];
    if S = nil then
      raise ESectorNotInitialized.CreateFmt('Sector %d not initialized',
        [SectorIndex]);

    for WaypointIndex := 0 to Waypoints.High do
    begin
      W := Waypoints[WaypointIndex];
      if W = nil then
        raise EWaypointNotInitialized.CreateFmt('Waypoint %d not initialized',
          [WaypointIndex]);

      if S.IsPointInsideMargin(W.Position, SectorsBoxesMargin) then
      begin
        S.Waypoints.Add(W);
        W.Sectors.Add(S);
      end;
    end;
  end;
end;

end.