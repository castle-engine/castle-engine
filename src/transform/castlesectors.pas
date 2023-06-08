{
  Copyright 2006-2018 Michalis Kamburelis.

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
  see "The Castle" developer docs,
  [https://castle-engine.io/castle-development.php]. }
unit CastleSectors;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  CastleUtils, CastleClassUtils, CastleVectors, CastleBoxes;

type
  TSectorList = class;

  TWaypoint = class
  private
    FSectors: TSectorList;
  public
    constructor Create;
    destructor Destroy; override;
  public
    Position: TVector3;

    { Box of the waypoint is only used by TSectorList.LinkToWaypoints,
      to detect which sectors contain ths waypoint (presumably,
      on their border).
      This box is @italic(not) used by actual AI using waypoints/sectors
      (only @link(Position) matters then). }
    Box: TBox3D;

    { Sectors that contain this waypoint. }
    property Sectors: TSectorList read FSectors;
  end;

  TWaypointList = class({$ifdef FPC}specialize{$endif} TObjectList<TWaypoint>)
  end;

  TSector = class
  private
    FBoxes: TBox3DList;
    FVisibleSectors: TBooleanList;
    FWaypoints: TWaypointList;
  public
    constructor Create;
    destructor Destroy; override;

    property Boxes: TBox3DList read FBoxes;

    { Is Point inside the sector. }
    function Contains(const Point: TVector3): boolean;

    { Does the box collide (at least partially) with sector. }
    function Collision(const Box: TBox3D): boolean;

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

  TSectorList = class({$ifdef FPC}specialize{$endif} TObjectList<TSector>)
  public
    { Connect sectors and waypoints into a graph.
      Adds appropriate waypoints to sectors and sectors to waypoints,
      knowing which waypoint belongs (presumably, lies at the border of)
      to which sector.
      A waypoint belongs to the sector simply when TWaypoint.Box
      collides with one of the sector boxes.

      @raises ESectorNotInitialized When some sector is nil.
      @raises EWaypointNotInitialized When some waypoint is nil. }
    procedure LinkToWaypoints(Waypoints: TWaypointList);

    { Returns sector with given point (using @link(TSector.Contains) of each sector).
      Returns nil if no such sector. }
    function SectorWithPoint(const Point: TVector3): TSector;

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

var
  LogSectors: boolean = false;

implementation

uses CastleStringUtils, CastleLog;

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
  FBoxes := TBox3DList.Create;
  FVisibleSectors := TBooleanList.Create;
  FWaypoints := TWaypointList.Create(false);
end;

destructor TSector.Destroy;
begin
  FreeAndNil(FBoxes);
  FreeAndNil(FVisibleSectors);
  FreeAndNil(FWaypoints);
  inherited;
end;

function TSector.Contains(const Point: TVector3): boolean;
var
  I: Integer;
begin
  for I := 0 to Boxes.Count - 1 do
    if Boxes.L[I].Contains(Point) then
      Exit(true);
  Result := false;
end;

function TSector.Collision(const Box: TBox3D): boolean;
var
  I: Integer;
begin
  for I := 0 to Boxes.Count - 1 do
    if Boxes.L[I].Collision(Box) then
      Exit(true);
  Result := false;
end;

{ TSectorList -------------------------------------------------------- }

procedure TSectorList.LinkToWaypoints(Waypoints: TWaypointList);
var
  S: TSector;
  W: TWaypoint;
  SectorIndex, WaypointIndex: Integer;
begin
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

      if S.Collision(W.Box) then
      begin
        S.Waypoints.Add(W);
        W.Sectors.Add(S);
        if LogSectors then
          WritelnLog('Sectors', Format('Waypoint %d links to sector %d',
            [WaypointIndex, SectorIndex]));
      end;
    end;
  end;

  if LogSectors then
  begin
    for WaypointIndex := 0 to Waypoints.Count - 1 do
      if Waypoints[WaypointIndex].Sectors.Count <= 1 then
        WritelnWarning('Sectors', Format('Waypoint %d only links to %d sectors. Waypoints that link to 1 or 0 sectors are useless.',
          [WaypointIndex, Waypoints[WaypointIndex].Sectors.Count]));
    for SectorIndex := 0 to Count - 1 do
      if Items[SectorIndex].Waypoints.Count = 0 then
        WritelnWarning('Sectors', Format('Sector %d is not connected to any waypoint. Such sectors are useless.',
          [SectorIndex]));
  end;
end;

function TSectorList.SectorWithPoint(const Point: TVector3):
  TSector;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Contains(Point) then
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
