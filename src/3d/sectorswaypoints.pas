{
  Copyright 2006-2012 Michalis Kamburelis.

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
unit SectorsWaypoints;

interface

uses SysUtils, CastleUtils, CastleClassUtils, Classes, VectorMath, Boxes3D, FGL;

type
  TSectorList = class;

  TWaypoint = class
  private
    FSectors: TSectorList;
  public
    constructor Create;
    destructor Destroy; override;
  public
    Position: TVector3Single;

    { Sectors that contain this waypoint. }
    property Sectors: TSectorList read FSectors;
  end;

  TWaypointList = class(specialize TFPGObjectList<TWaypoint>)
  end;

  TSector = class
  private
    FBoundingBoxes: TBox3DList;
    FVisibleSectors: TBooleanList;
    FWaypoints: TWaypointList;
  public
    constructor Create;
    destructor Destroy; override;

    property BoundingBoxes: TBox3DList read FBoundingBoxes;

    { Returns whether Point is inside the sector.
      Implementation in TSector just returns if Point is inside
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
    property Waypoints: TWaypointList read FWaypoints;
  end;

  ESectorNotInitialized = class(Exception);
  EWaypointNotInitialized = class(Exception);

  TSectorList = class(specialize TFPGObjectList<TSector>)
  public
    { This adds appropriate Waypoints to all sectors on this list,
      and adds appropriate Sectors to all Waypoints on given list.

      A waypoint is considered to be within the sector, if
      Sector.IsPointInsideMargin(Waypoint.Position, SectorsBoxesMargin)
      is true. The SectorsBoxesMargin is needed to avoid any kind of
      uncertainty when the waypoint's position is at the very border
      of the sector.

      @raises ESectorNotInitialized When some sector is nil.
      @raises EWaypointNotInitialized When some waypoint is nil. }
    procedure LinkToWaypoints(Waypoints: TWaypointList;
      const SectorsBoxesMargin: Single);

    { Returns sector with given point (using IsPointInside of each sector).
      Returns nil if no such sector. }
    function SectorWithPoint(const Point: TVector3Single): TSector;

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
    class function FindWay(SectorBegin, SectorEnd: TSector;
      Waypoints: TWaypointList): boolean;
  end;

implementation

uses CastleStringUtils;

{ TWaypoint ------------------------------------------------------------- }

constructor TWaypoint.Create;
begin
  inherited Create;
  FSectors := TSectorList.Create(false);
end;

destructor TWaypoint.Destroy;
begin
  FreeAndNil(FSectors);
  inherited;
end;

{ TSector --------------------------------------------------------------- }

constructor TSector.Create;
begin
  inherited Create;
  FBoundingBoxes := TBox3DList.Create;
  FVisibleSectors := TBooleanList.Create;
  FWaypoints := TWaypointList.Create(false);
end;

destructor TSector.Destroy;
begin
  FreeAndNil(FBoundingBoxes);
  FreeAndNil(FVisibleSectors);
  FreeAndNil(FWaypoints);
  inherited;
end;

function TSector.IsPointInside(const Point: TVector3Single): boolean;
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

function TSector.IsPointInsideMargin(const Point: TVector3Single;
  const SectorsBoxesMargin: Single): boolean;
var
  I: Integer;
begin
  for I := 0 to BoundingBoxes.Count - 1 do
    if BoundingBoxes.L[I].Expand(SectorsBoxesMargin).PointInside(Point) then
      Exit(true);
  Result := false;
end;

{ TSectorList -------------------------------------------------------- }

procedure TSectorList.LinkToWaypoints(Waypoints: TWaypointList;
  const SectorsBoxesMargin: Single);
var
  S: TSector;
  W: TWaypoint;
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

function TSectorList.SectorWithPoint(const Point: TVector3Single):
  TSector;
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

class function TSectorList.FindWay(SectorBegin, SectorEnd: TSector;
  Waypoints: TWaypointList): boolean;
var
  { This is used to avoid falling into loops. }
  SectorsVisited: TSectorList;

  function FindWayToSectorEnd(SectorNow: TSector;
    SectorDistance: Integer): boolean;
  var
    WaypointIndex, SectorIndex: Integer;
    W: TWaypoint;
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

  SectorsVisited := TSectorList.Create(false);
  try
    Result := FindWayToSectorEnd(SectorBegin, 0);
  finally SectorsVisited.Free end;
end;

end.