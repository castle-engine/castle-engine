{
  Copyright 2016-2018 Eugene Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Build a 3D tunnel using X3D mesh (IndexedFaceSet node). }
program build_3d_tunnel;

uses SysUtils,
  CastleWindow, CastleSceneCore, CastleScene, CastleVectors,
  X3dnodes, CastleKeysMouse, CastleRandom, CastleLog, CastleCameras,
  CastleViewport;

const
  { Amount of vertexes generated on the floor.
    Without randomization it doesn't matter. }
  FloorPoints = 12;
  { Amount of vertexes generated on the walls.
    It *must* be even because it's symmetric and each vertex must have a pair. }
  MaxPoints = FloorPoints + 20;

const
  { Length of the passage. }
  MaxSections = 10000;

type
  { Temporarily stores data about each section before they're added to global
    TCoordinateNode and TIndexedFaceSetNode nodes. }
  TSection = record
    { Average point and next average point for the section. }
    Median, Next: TVector3;
    { Vertexes constituting a section. }
    Pt: array [1..MaxPoints] of TVector3;
    { Absolute (global) indexes of each vertex. }
    Index: array [1..MaxPoints] of Integer;
    { Angle the section faces. }
    Angle: Single;
    { Section's width and height. }
    Width, Height: Single;
  end;

var
  { Global variables referencing important instances of CGE classes }
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
  Navigation: TCastleWalkNavigation;

  { 3D geometry containers for the passage }
  ROOT: TX3DRootNode;             // contains all the geometry
  Shape: TShapeNode;              // contains the shape (coords and geometry)
  coords: TCoordinateNode;        // contains x,y,z of all vertexes
  Geometry: TIndexedFaceSetNode;  // face set of each wall&floor
  Appearance: TAppearanceNode;    // contains material
  Material: TMaterialNode;        // colored material

  { Variables used for generation }
  last_sect, next_sect: TSection;
  core_section: TSection;
  NIndex: Integer;   // global indexes of vertexes

{ Creates a shape of the section.
  Section is created with center point at 0,0,0.
  all points have z=0 and faces y axis direction. }
procedure create_core_section;
var
  I: Integer;
begin
  with core_section do
  begin
    for I := 1 to FloorPoints do
    begin
      Pt[I][0] := -1 + (I-1)/(FloorPoints-1)*2; // this is x-location of the floor vertexes
      Pt[I][1] := 0;                            // y = 0
      Pt[I][2] := 0;                            // z = 0
    end;
    for I := FloorPoints+1 to MaxPoints do
    begin
      Pt[I][0] := 1 - (I-FloorPoints)/(MaxPoints-FloorPoints+1)*2;  //x and y of the vertexes
      Pt[I][1] := 1 - Sqr(Sqr(Abs(Pt[I][0])));
      Pt[I][2] := 0;                            // z = 0
    end;
  end;
end;

{ Generates x,y,z coordinates for each section vertexes }
procedure create_section;
var
  I: Integer;
begin
  with next_sect do
    for I := 1 to MaxPoints do
    begin
      Pt[I][0] := median[0]+((core_section.Pt[I][0])*Cos(Angle) + (core_section.pt[I][2])*Sin(Angle))*Width;
      Pt[I][2] := median[2]+((core_section.Pt[I][2])*Cos(Angle) + (core_section.pt[I][0])*Sin(Angle))*Width;
      Pt[I][1] := median[1]+(core_section.Pt[I][1])*Height;
      Inc(NIndex);
      Index[I] := NIndex;
      Coords.FdPoint.Items.Add(Vector3(Pt[I][0], Pt[I][1], Pt[I][2]));
    end;
end;

{ Add vertexes indexes counter-clockwise to generate faces lookin inwards }
procedure pass;
var
  I, next_i: Integer;
begin
  //amount of faces (quads) generated will be equal to amount of vertexes
  for I := 1 to MaxPoints do
  begin
    if I < MaxPoints then next_i := I+1 else next_i := 1; // handle last face correctly
    { Add vertexes indexes counter-clockwise }
    Geometry.FdCoordIndex.Items.Add(last_sect.index[I]);
    Geometry.FdCoordIndex.Items.Add(last_sect.index[next_i]);
    Geometry.FdCoordIndex.Items.Add(next_sect.index[next_i]);
    Geometry.FdCoordIndex.Items.Add(next_sect.index[I]);
    { And finish the face by -1 }
    Geometry.FdCoordIndex.Items.Add(-1);
  end;
end;

{ this is the main procedure of the algorithm: makes a passage }
procedure MakePassage;
var
  J: Integer;
begin
  for J := 1 to MaxSections do
  begin
    { Determine next section parameters }
    next_sect.angle := last_sect.angle+(Rand.Random-0.5)/10;
    next_sect.median[0] := last_sect.median[0]+sin(next_sect.angle);
    next_sect.median[1] := last_sect.median[1]-(Rand.Random-0.5)/3;
    next_sect.median[2] := last_sect.median[2]-cos(next_sect.angle);
    next_sect.width := last_sect.width+(Rand.Random-0.5)/3;
    if next_sect.width < 1 then next_sect.width:=1;
    if next_sect.width > 3 then next_sect.width:=3;
    next_sect.height := last_sect.height+(Rand.Random-0.5)/3;
    if next_sect.height < 2 then next_sect.height:=2;
    if next_sect.height > 5 then next_sect.height:=5;
    { Now create the section }
    create_section;
    { And connect it to previous section }
    pass;
    last_sect := next_sect;
  end;
end;


procedure generatemap;
begin
  { Creates the passage shape }
  create_core_section;

  { Initialize globai index counter }
  NIndex := -1;

  { Make first section to start with }
  //first set parameters
  last_sect.median[0] := 0;
  last_sect.median[1] := -2;
  last_sect.median[2] := 2;
  last_sect.width := 1+Rand.Random*2;
  last_sect.height := 2+Rand.Random*2;
  last_sect.angle := 0;
  //and generate the section
  create_section;

  { Finally make the passage by generating the sequential sections }
  MakePassage;
end;

begin
  InitializeLog;

  { Initialize TCoordinateNode and TIndexedFaceSetNode}
  Coords := TCoordinateNode.Create;
  Geometry := TIndexedFaceSetNode.Create;

  { Generate the passage }
  GenerateMap;

  { Merge Coords and Geometry }
  Geometry.Coord := Coords;

  { Create some simple material }
  Material := TMaterialNode.Create;
  Material.DiffuseColor := Vector3(1, 0.9, 0.9);
  Material.AmbientIntensity := 2;

  { And add it to Appearance node }
  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  { pack everything inside the Shape (geometry + appearance) }
  Shape := TShapeNode.Create;
  Shape.Geometry := Geometry;
  Shape.Appearance := Appearance;

  { and finally add everything to the Root node }
  Root := TX3DRootNode.Create;
  Root.AddChildren(shape);

  { Initialize Window }
  Window := TCastleWindowBase.Create(Application);

  { Initialize Viewport }
  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Window.Controls.InsertFront(Viewport);

  { Create a Scene based on Root node }
  Scene := TCastleScene.Create(Application);
  scene.load(Root,true);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  { Add the scene to Viewport }
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  { Initialize Viewport.Navigation }
  Navigation := TCastleWalkNavigation.Create(Application);
  Navigation.MouseLook := true;
  Navigation.PreferredHeight := 1;
  Navigation.MoveHorizontalSpeed := 10;
  Navigation.Radius := 0.1;
  Viewport.Navigation := Navigation;
  // We could also set AutoNavigation to false,
  // but it's not necessary since we assign Navigation explicitly.

  { Initialize Viewport.Camera }
  Viewport.Camera.Position := Vector3(0, 0, -1);
  Viewport.Camera.ProjectionNear := Navigation.Radius * 0.5;

  { Finally run the application }
  Window.OpenAndRun;
end.
