{
  Copyright 2016-2017 Eugene Loza, Michalis Kamburelis.

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

uses SysUtils, CastleWindow, CastleSceneCore, CastleScene, CastleVectors,
  X3dnodes, CastlePlayer, CastleKeysMouse, CastleRandom;

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
    median, next: Tvector3single;
    { Vertexes constituting a section. }
    pt: array [1..MaxPoints] of Tvector3single;
    { Absolute (global) indexes of each vertex. }
    index: array [1..MaxPoints] of integer;
    { Angle the section faces. }
    angle: single;
    { Section's width and height. }
    width, height: single;
  end;

var
  { global variables of the Engine }
  Window: TCastleWindow;
  Scene: TCastleScene;
  player: TPlayer;

  { 3D geometry containers for the passage }
  ROOT: TX3DRootNode;             // contains all the geometry
  Shape: TShapeNode;              // contains the shape (coords and geometry)
  coords: TCoordinateNode;        // contains x,y,z of all vertexes
  Geometry: TIndexedFaceSetNode;  // face set of each wall&floor
  Appearance: TAppearanceNode;    // contains material
  Material: TMaterialNode;        // colored material

  { local variables used for generation }
  last_sect, next_sect: TSection;
  core_section: TSection;
  nindex: integer;   // global indexes of vertexes
  RND: TCastleRandom; //random number generator

{ Creates a shape of the section.
  Section is created with center point at 0,0,0.
  all points have z=0 and faces y axis direction. }
procedure create_core_section;
var
  i: integer;
begin
  with core_section do
  begin
    for i := 1 to FloorPoints do
    begin
      pt[i][0] := -1 + (i-1)/(FloorPoints-1)*2; // this is x-location of the floor vertexes
      pt[i][1] := 0;                            // y = 0
      pt[i][2] := 0;                            // z = 0
    end;
    for i := FloorPoints+1 to MaxPoints do
    begin
      pt[i][0] := 1 - (i-FloorPoints)/(MaxPoints-FloorPoints+1)*2;  //x and y of the vertexes
      pt[i][1] := 1 - sqr(sqr(abs(pt[i][0])));
      pt[i][2] := 0;                            // z = 0
    end;
  end;
end;

{ generates x,y,z coordinates for each section vertexes }
procedure create_section;
var
  i: integer;
begin
  with next_sect do
    for i := 1 to MaxPoints do
    begin
      pt[i][0] := median[0]+((core_section.pt[i][0])*cos(angle) + (core_section.pt[i][2])*sin(angle))*width;
      pt[i][2] := median[2]+((core_section.pt[i][2])*cos(angle) + (core_section.pt[i][0])*sin(angle))*width;
      pt[i][1] := median[1]+(core_section.pt[i][1])*height;
      inc(nindex);
      index[i] := nindex;
      Coords.FdPoint.Items.Add(Vector3Single(pt[i][0], pt[i][1], pt[i][2]));
    end;
end;

{ add vertexes indexes counter-clockwise to generate faces lookin inwards }
procedure pass;
var
  i, next_i: integer;
begin
  //amount of faces (quads) generated will be equal to amount of vertexes
  for i := 1 to MaxPoints do
  begin
    if i < MaxPoints then next_i := i+1 else next_i := 1; // handle last face correctly
    { add vertexes indexes counter-clockwise }
    Geometry.FdCoordIndex.Items.Add(last_sect.index[i]);
    Geometry.FdCoordIndex.Items.Add(last_sect.index[next_i]);
    Geometry.FdCoordIndex.Items.Add(next_sect.index[next_i]);
    Geometry.FdCoordIndex.Items.Add(next_sect.index[i]);
    { and finish the face by -1 }
    Geometry.FdCoordIndex.Items.Add(-1);
  end;
end;

{ this is the main procedure of the algorithm: makes a passage }
procedure MakePassage;
var
  j: integer;
begin
  for j := 1 to MaxSections do
  begin
    { determine next section parameters }
    next_sect.angle := last_sect.angle+(RND.random-0.5)/10;
    next_sect.median[0] := last_sect.median[0]+sin(next_sect.angle);
    next_sect.median[1] := last_sect.median[1]-(RND.random-0.5)/3;
    next_sect.median[2] := last_sect.median[2]-cos(next_sect.angle);
    next_sect.width := last_sect.width+(RND.random-0.5)/3;
    if next_sect.width < 1 then next_sect.width:=1;
    if next_sect.width > 3 then next_sect.width:=3;
    next_sect.height := last_sect.height+(RND.random-0.5)/3;
    if next_sect.height < 2 then next_sect.height:=2;
    if next_sect.height > 5 then next_sect.height:=5;
    { now create the section }
    create_section;
    { and connect it to previous section }
    pass;
    last_sect := next_sect;
  end;
end;


procedure generatemap;
begin
  { creates the passage shape }
  create_core_section;

  { initialize globai index counter }
  nindex := -1;

  { make first section to start with }
  //first set parameters
  last_sect.median[0] := 0;
  last_sect.median[1] := -2;
  last_sect.median[2] := 2;
  last_sect.width := 1+RND.random*2;
  last_sect.height := 2+RND.random*2;
  last_sect.angle := 0;
  //and generate the section
  create_section;

  { finally make the passage by generating the sequential sections }
  makePassage;
end;

begin
  { initialize random number generator }
  RND := TCastleRandom.Create;

  { initialize TCoordinateNode and TIndexedFaceSetNode}
  Coords := TCoordinateNode.Create('', '');
  Geometry := TIndexedFaceSetNode.Create('', '');

  { generate the passage }
  generatemap;

  { merge Coords and Geometry }
  Geometry.FdCoord.Value := Coords;

  { create some simple material }
  Material := TMaterialNode.Create('', '');
  Material.FdDiffuseColor.value:=vector3single(1,0.9,0.9);
  material.FdAmbientIntensity.value:=2;

  { and add it to Appearance node }
  Appearance := TAppearanceNode.Create('', '');
  Appearance.FdMaterial.Value := Material;

  { pack everything inside the Shape (geometry + appearance) }
  Shape := TShapeNode.Create('', '');
  Shape.FdGeometry.Value := Geometry;
  Shape.Appearance := Appearance;

  { and finally add everything to the Root node }
  Root := TX3DRootNode.Create('', '');
  Root.FdChildren.Add(shape);

  { Initialize Castle Window }

  Window := TCastleWindow.Create(Application);

  { Create player }

  Player := TPlayer.Create(Window.SceneManager);
  Window.SceneManager.Items.Add(Player);
  Window.SceneManager.Player := Player;

  { Create a scene based on Root node }

  Scene := TCastleScene.Create(Application);
  scene.load(Root,true);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  { Add the scene to Scene Manager }

  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  { Set some parameters for the player }

  Player.Position := Vector3Single(0,0,-1);
  Player.Camera.MouseLook := true;
  Player.DefaultPreferredHeight := 1;
  Player.DefaultMoveHorizontalSpeed := 10;
  Window.SceneManager.Camera := Player.camera;

  { finally run the application }

  Window.OpenAndRun;

  FreeAndNil(RND);
end.
