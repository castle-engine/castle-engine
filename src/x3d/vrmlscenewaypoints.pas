{
  Copyright 2006-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sectors and waypoints, to improve creature AI in 3D levels.
  For user-oriented description what are sectors and waypoints,
  when they should be used etc. see "The Castle" developer docs,
  [http://castle-engine.sourceforge.net/castle-development.php]. }
unit VRMLSceneWaypoints;

interface

uses SysUtils, CastleUtils, CastleClassUtils, Classes,
  VectorMath, Boxes3D, VRMLNodes, VRMLScene,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

type
  TSceneSectorList = class;

  TSceneWaypoint = class
  private
    FSectors: TSceneSectorList;
  public
    constructor Create;
    destructor Destroy; override;
  public
    Position: TVector3Single;

    { Sectors that contain this waypoint. }
    property Sectors: TSceneSectorList read FSectors;
  end;

  TSceneWaypointList = class(specialize TFPGObjectList<TSceneWaypoint>)
  public
    { Shapes placed under the name Waypoint<index>_<ignored>
      are removed from the Scene, and are added as new waypoint with
      given index. Waypoint's Position is set to the middle point
      of shape's bounding box.

      Count of this list is enlarged, if necessary,
      to include all waypoints indicated in the Scene. }
    procedure ExtractPositions(Scene: T3DSceneCore);
  end;

  TSceneSector = class
  private
    FBoundingBoxes: TBox3DList;
    FVisibleSectors: TBooleanList;
    FWaypoints: TSceneWaypointList;
  public
    constructor Create;
    destructor Destroy; override;

    property BoundingBoxes: TBox3DList read FBoundingBoxes;

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
    property VisibleSectors: TBooleanList read FVisibleSectors;

    { Waypoints that are included in this sector. }
    property Waypoints: TSceneWaypointList read FWaypoints;
  end;

  ESectorNotInitialized = class(Exception);
  EWaypointNotInitialized = class(Exception);

  TSceneSectorList = class(specialize TFPGObjectList<TSceneSector>)
  public
    { Shapes placed under the name Sector<index>_<ignored>
      are removed from the Scene, and are added to sector <index> BoundingBoxes.

      Count of this list is enlarged, if necessary,
      to include all sectors indicated in the Scene. }
    procedure ExtractBoundingBoxes(Scene: T3DSceneCore);

    { This adds appropriate Waypoints to all sectors on this list,
      and adds appropriate Sectors to all Waypoints on given list.

      A waypoint is considered to be within the sector, if
      Sector.IsPointInsideMargin(Waypoint.Position, SectorsBoxesMargin)
      is true. The SectorsBoxesMargin is needed to avoid any kind of
      uncertainty when the waypoint's position is at the very border
      of the sector.

      @raises ESectorNotInitialized When some sector is nil.
      @raises EWaypointNotInitialized When some waypoint is nil. }
    procedure LinkToWaypoints(Waypoints: TSceneWaypointList;
      const SectorsBoxesMargin: Single);

    { Returns sector with given point (using IsPointInside of each sector).
      Returns nil if no such sector. }
    function SectorWithPoint(const Point: TVector3Single): TSceneSector;

    { This sets Waypoints contents to the list of waypoints
      that must be passed to travel from sector SectorBegin to SectorEnd.

      Special cases: when either SectorBegin or SectorEnd are nil
      (this can easily happen if you pass here results
      of SectorWithPoint method), or when SectorBegin = SectorEnd,
      or when there is no possible way, it returns @false and
      just clears Waypoints (i.e. sets Waypoints.Count to 0).

      Otherwise (if a way is found) it returns @true and sets
      Waypoints items as appropriate. The order of Waypoints
      is significant: starting from SectorBegin, you should
      first travel to Waypoints[0], then to Waypoints[1] etc.
      In this case for sure we have at least one Waypoint.

      (So the result of this function is actually just a comfortable
      thing, you can get the same result just checking
      Waypoints.Count <> 0)

      TODO: This should use breadth-first search.
      Right now it uses depth-first search. For small sectors+waypoints
      graphs it doesn't matter. }
    class function FindWay(SectorBegin, SectorEnd: TSceneSector;
      Waypoints: TSceneWaypointList): boolean;
  end;

implementation

uses CastleStringUtils, Shape;

{ TSceneWaypoint ------------------------------------------------------------- }

constructor TSceneWaypoint.Create;
begin
  inherited Create;
  FSectors := TSceneSectorList.Create(false);
end;

destructor TSceneWaypoint.Destroy;
begin
  FreeAndNil(FSectors);
  inherited;
end;

{ TSceneWaypointList -------------------------------------------------------- }

procedure TSceneWaypointList.ExtractPositions(Scene: T3DSceneCore);
var
  NodesToRemove: TX3DNodeList;

  procedure TraverseForWaypoints(Shape: TShape);

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

      WaypointPosition := Shape.BoundingBox.Middle;

      Count := Max(Count, WaypointIndex + 1);
      if Items[WaypointIndex] <> nil then
        raise Exception.CreateFmt('Waypoint %d is already initialized',
          [WaypointIndex]);

      Items[WaypointIndex] := TSceneWaypoint.Create;
      Items[WaypointIndex].Position := WaypointPosition;

      { Tests:
      Writeln('Waypoint ', WaypointIndex, ': at position ',
        VectorToNiceStr(WaypointPosition));}
    end;

  const
    WaypointPrefix = 'Waypoint';
  begin
    if IsPrefix(WaypointPrefix, Shape.BlenderMeshName) then
    begin
      CreateNewWaypoint(SEnding(Shape.BlenderMeshName, Length(WaypointPrefix) + 1));
      { Don't remove BlenderObjectNode now --- will be removed later.
        This avoids problems with removing nodes while traversing. }
      NodesToRemove.Add(Shape.BlenderObjectNode);
    end;
  end;

var
  I: Integer;
  SI: TShapeTreeIterator;
begin
  NodesToRemove := TX3DNodeList.Create(false);
  try
    SI := TShapeTreeIterator.Create(Scene.Shapes, { OnlyActive } true);
    try
      while SI.GetNext do TraverseForWaypoints(SI.Current);
    finally SysUtils.FreeAndNil(SI) end;

    Scene.BeforeNodesFree;
    for I := 0 to NodesToRemove.Count - 1 do
      NodesToRemove.Items[I].FreeRemovingFromAllParents;
  finally NodesToRemove.Free end;

  Scene.ChangedAll;
end;

{ TSceneSector --------------------------------------------------------------- }

constructor TSceneSector.Create;
begin
  inherited Create;
  FBoundingBoxes := TBox3DList.Create;
  FVisibleSectors := TBooleanList.Create;
  FWaypoints := TSceneWaypointList.Create(false);
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
  for I := 0 to BoundingBoxes.Count - 1 do
    if BoundingBoxes.L[I].PointInside(Point) then
      Exit(true);
  Result := false;
end;

function TSceneSector.IsPointInsideMargin(const Point: TVector3Single;
  const SectorsBoxesMargin: Single): boolean;
var
  I: Integer;
begin
  for I := 0 to BoundingBoxes.Count - 1 do
    if BoundingBoxes.L[I].Expand(SectorsBoxesMargin).PointInside(Point) then
      Exit(true);
  Result := false;
end;

{ TSceneSectorList -------------------------------------------------------- }

procedure TSceneSectorList.ExtractBoundingBoxes(Scene: T3DSceneCore);
var
  NodesToRemove: TX3DNodeList;

  procedure TraverseForSectors(Shape: TShape);

    procedure AddSectorBoundingBox(const SectorNodeName: string);
    var
      IgnoredBegin, SectorIndex: Integer;
      SectorBoundingBox: TBox3D;
    begin
      { Calculate SectorIndex }
      IgnoredBegin := Pos('_', SectorNodeName);
      if IgnoredBegin = 0 then
        SectorIndex := StrToInt(SectorNodeName) else
        SectorIndex := StrToInt(Copy(SectorNodeName, 1, IgnoredBegin - 1));

      SectorBoundingBox := Shape.BoundingBox;

      Count := Max(Count, SectorIndex + 1);
      if Items[SectorIndex] = nil then
        Items[SectorIndex] := TSceneSector.Create;

      Items[SectorIndex].BoundingBoxes.Add(SectorBoundingBox);

      { Tests:
      Writeln('Sector ', SectorIndex, ': added box ',
        SectorBoundingBox.ToNiceStr); }
    end;

  const
    SectorPrefix = 'Sector';
  begin
    if IsPrefix(SectorPrefix, Shape.BlenderMeshName) then
    begin
      AddSectorBoundingBox(SEnding(Shape.BlenderMeshName, Length(SectorPrefix) + 1));
      { Don't remove BlenderObjectNode now --- will be removed later.
        This avoids problems with removing nodes while traversing. }
      NodesToRemove.Add(Shape.BlenderObjectNode);
    end;
  end;

var
  I: Integer;
  SI: TShapeTreeIterator;
begin
  NodesToRemove := TX3DNodeList.Create(false);
  try
    SI := TShapeTreeIterator.Create(Scene.Shapes, { OnlyActive } true);
    try
      while SI.GetNext do TraverseForSectors(SI.Current);
    finally SysUtils.FreeAndNil(SI) end;

    Scene.BeforeNodesFree;
    for I := 0 to NodesToRemove.Count - 1 do
      NodesToRemove.Items[I].FreeRemovingFromAllParents;
  finally NodesToRemove.Free end;

  Scene.ChangedAll;
end;

procedure TSceneSectorList.LinkToWaypoints(Waypoints: TSceneWaypointList;
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

  for SectorIndex := 0 to Count - 1 do
  begin
    S := Items[SectorIndex];
    if S = nil then
      raise ESectorNotInitialized.CreateFmt('Sector %d not initialized',
        [SectorIndex]);

    for WaypointIndex := 0 to Waypoints.Count - 1 do
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

function TSceneSectorList.SectorWithPoint(const Point: TVector3Single):
  TSceneSector;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.IsPointInside(Point) then
      Exit;
  end;
  Result := nil;
end;

class function TSceneSectorList.FindWay(SectorBegin, SectorEnd: TSceneSector;
  Waypoints: TSceneWaypointList): boolean;
var
  { This is used to avoid falling into loops. }
  SectorsVisited: TSceneSectorList;

  function FindWayToSectorEnd(SectorNow: TSceneSector;
    SectorDistance: Integer): boolean;
  var
    WaypointIndex, SectorIndex: Integer;
    W: TSceneWaypoint;
  begin
    if SectorsVisited.IndexOf(SectorNow) <> -1 then
      Exit(false);
    SectorsVisited.Add(SectorNow);

    if SectorNow = SectorEnd then
    begin
      Waypoints.Count := SectorDistance;
      Exit(true);
    end;

    for WaypointIndex := 0 to SectorNow.Waypoints.Count - 1 do
    begin
      W := SectorNow.Waypoints[WaypointIndex];
      for SectorIndex := 0 to W.Sectors.Count - 1 do
        if FindWayToSectorEnd(W.Sectors[SectorIndex], SectorDistance + 1) then
        begin
          Waypoints[SectorDistance] := W;
          Exit(true);
        end;
    end;

    Result := false;
  end;

begin
  Waypoints.Count := 0;
  if (SectorBegin = nil) or
     (SectorEnd = nil) or
     (SectorBegin = SectorEnd) then
    Exit(false);

  { Note that we know here that SectorBegin <> SectorEnd,
    so the first call to FindWayToSectorEnd will not immediately
    return with true, so Waypoints[0] will for sure be filled...
    so Waypoints.Count will have to be > 0 in this case.
    Just like I promised in the interface. }

  SectorsVisited := TSceneSectorList.Create(false);
  try
    Result := FindWayToSectorEnd(SectorBegin, 0);
  finally SectorsVisited.Free end;
end;

end.