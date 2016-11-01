program LongTunnel;

{$mode objfpc}{$H+}

uses CastleWindow, CastleSceneCore, CastleScene, CastleVectors,
     X3dnodes, castleplayer, CastleKeysMouse, CastleRandom;

const
  { amount of vertexes generated on the floor. Without randomization it doesn't
    matter }
  floorpoints = 12;
  { amount of vertexes generated on the walls. It *must* be even because it'
    symmetric and each vertex must have a pair }
  maxpoints = floorpoints+20;

{ length of the passage }
const maxsections = 10000;

type
  { temporarily stores data about each section before they're added to global
    TCoordinateNode and TIndexedFaceSetNode nodes }
  TSection = record
  { average point and next average point for the section }
  median,next: Tvector3single;
  { vertexes contsituting a section }
  pt: array[1..maxpoints] of Tvector3single;
  { absolute (global) indexes of each vertex }
  index: array[1..maxpoints] of integer;
  { angle the section faces }
  angle: single;
  { section's width and height }
  width,height: single;
end;

var { global variables of the Engine }
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

{$R+}{$Q+}

var { local variables used for generation }
    last_sect, next_sect: TSection;
    core_section: TSection;
    nindex: integer;   // global indexes of vertexes
    RND: TCastleRandom; //random number generator

{ creates a shape of the section
  section is created with center point at 0,0,0;
  all points have z=0 and faces y axis direction}
procedure create_core_section;
var i: integer;
begin
 with core_section do begin
  for i := 1 to floorpoints do begin
    pt[i][0] := -1 + (i-1)/(floorpoints-1)*2; // this is x-location of the floor vertexes
    pt[i][1] := 0;                            // y = 0
    pt[i][2] := 0;                            // z = 0
  end;
  for i := floorpoints+1 to maxpoints do begin
    pt[i][0] := 1 - (i-floorpoints)/(maxpoints-floorpoints+1)*2;  //x and y of the vertexes
    pt[i][1] := 1 - sqr(sqr(abs(pt[i][0])));
    pt[i][2] := 0;                            // z = 0
  end;
 end;
end;

{ generates x,y,z coordinates for each section vertexes }
procedure create_section;
var i: integer;
begin
 with next_sect do
   for i := 1 to maxpoints do begin
     pt[i][0] := median[0]+((core_section.pt[i][0])*cos(angle) + (core_section.pt[i][2])*sin(angle))*width;
     pt[i][2] := median[2]+((core_section.pt[i][2])*cos(angle) + (core_section.pt[i][0])*sin(angle))*width;
     pt[i][1] := median[1]+(core_section.pt[i][1])*height;
     inc(nindex);
     index[i] := nindex;
     Coords.FdPoint.Items.Add(Vector3Single(pt[i][0],pt[i][1],pt[i][2]));
   end;
end;

{ add vertexes indexes counter-clockwise to generate faces lookin inwards }
procedure pass;
var i,next_i: integer;
begin
 //amount of faces (quads) generated will be equal to amount of vertexes
 for i := 1 to maxpoints do begin
   if i < maxpoints then next_i := i+1 else next_i := 1; // handle last face correctly
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
var j: integer;
begin
 for j := 1 to maxsections do begin
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
   {now create the section}
   create_section;
   {and connect it to previous section}
   pass;
   last_sect:=next_sect;
 end;
end;


procedure generatemap;
begin
 { creates the passage shape }
 create_core_section;

 { initialize globai index counter }
 nindex:=-1;

 { make first section to start with }
 //first set parameters
 last_sect.median[0]:=0;
 last_sect.median[1]:=-2;
 last_sect.median[2]:=2;
 last_sect.width:=1+RND.random*2;
 last_sect.height:=2+RND.random*2;
 last_sect.angle:=0;
 //and generate the section
 create_section;

 {finally make the passage by generating the sequential sections }
 makePassage;
end;

begin
 { initialize random number generator }
 RND := TCastleRandom.create;

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

 player.position:=Vector3Single(0,0,-1);
 player.Camera.MouseLook:=true;
 player.DefaultPreferredHeight:=1;
 player.DefaultMoveHorizontalSpeed:=10;
 window.scenemanager.camera:=player.camera;

 { finally run the application }

 Window.OpenAndRun;
end.

