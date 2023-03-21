{
  Copyright 2016-2021 Eugene Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  CastleCameras;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainScene: TCastleScene;
    WalkNavigation: TCastleWalkNavigation;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  X3DNodes;

{ ----------------------------------------------------------------------------
  Generation algorithm, expressed as one simple BuildNode function. }

// TODO: move these vars to be local in BuildNode, make sure to zero

const
  { Amount of vertexes generated on the floor.
    Without randomization it doesn't matter. }
  FloorPoints = 12;
  { Amount of vertexes generated on the walls.
    It *must* be even because it's symmetric and each vertex must have a pair. }
  MaxPoints = FloorPoints + 20;
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
  { Nodes }
  Root: TX3DRootNode;             // contains everything
  Shape: TShapeNode;              // contains the Geometry and Appearance
  coords: TCoordinateNode;        // lists 3D positions of all mesh vertexes
  Geometry: TIndexedFaceSetNode;  // mesh - faces, for each wall & floor
  Appearance: TAppearanceNode;    // contains Material
  Material: TMaterialNode;        // sets color etc.

  { variables used for generation }
  LastSection, NextSection: TSection;
  CoreSection: TSection;
  NIndex: Integer;   // global indexes of vertexes

function BuildNode: TX3DRootNode;

  { Creates a shape of the section.
    Section is created with center point at 0,0,0.
    all points have z=0 and faces y axis direction. }
  procedure CreateCoreSection;
  var
    i: Integer;
  begin
    with CoreSection do
    begin
      for i := 1 to FloorPoints do
      begin
        pt[i].X := -1 + (i-1)/(FloorPoints-1)*2; // this is x-location of the floor vertexes
        pt[i].Y := 0;                            // y = 0
        pt[i].Z := 0;                            // z = 0
      end;
      for i := FloorPoints+1 to MaxPoints do
      begin
        pt[i].X := 1 - (i-FloorPoints)/(MaxPoints-FloorPoints+1)*2;  //x and y of the vertexes
        pt[i].Y := 1 - sqr(sqr(abs(pt[i].X)));
        pt[i].Z := 0;                            // z = 0
      end;
    end;
  end;

  { Generates x,y,z coordinates for each section vertexes. }
  procedure CreateSection;
  var
    i: Integer;
  begin
    with NextSection do
      for i := 1 to MaxPoints do
      begin
        pt[i].X := median.X+((CoreSection.pt[i].X)*cos(angle) + (CoreSection.pt[i].Z)*sin(angle))*width;
        pt[i].Z := median.Z+((CoreSection.pt[i].Z)*cos(angle) + (CoreSection.pt[i].X)*sin(angle))*width;
        pt[i].Y := median.Y+(CoreSection.pt[i].Y)*height;
        inc(NIndex);
        index[i] := NIndex;
        Coords.FdPoint.Items.Add(Vector3(pt[i].X, pt[i].Y, pt[i].Z));
      end;
  end;

  { Add vertexes indexes counter-clockwise to generate faces looking inwards. }
  procedure Pass;
  var
    i, NextI: Integer;
  begin
    // amount of faces (quads) generated will be equal to amount of vertexes
    for i := 1 to MaxPoints do
    begin
      if i < MaxPoints then NextI := i+1 else NextI := 1; // handle last face correctly
      { add vertexes indexes counter-clockwise }
      Geometry.FdCoordIndex.Items.Add(LastSection.index[i]);
      Geometry.FdCoordIndex.Items.Add(LastSection.index[NextI]);
      Geometry.FdCoordIndex.Items.Add(NextSection.index[NextI]);
      Geometry.FdCoordIndex.Items.Add(NextSection.index[i]);
      { and finish the face by -1 }
      Geometry.FdCoordIndex.Items.Add(-1);
    end;
  end;

  { Main iteration of the algorithm: make a passage from multiple sections. }
  procedure MakePassage;
  var
    j: Integer;
  begin
    for j := 1 to MaxSections do
    begin
      { determine next section parameters }
      NextSection.angle := LastSection.angle+(Random-0.5)/10;
      NextSection.median.X := LastSection.median.X+sin(NextSection.angle);
      NextSection.median.Y := LastSection.median.Y-(Random-0.5)/3;
      NextSection.median.Z := LastSection.median.Z-cos(NextSection.angle);
      NextSection.width := LastSection.width+(Random-0.5)/3;
      if NextSection.width < 1 then NextSection.width:=1;
      if NextSection.width > 3 then NextSection.width:=3;
      NextSection.height := LastSection.height+(Random-0.5)/3;
      if NextSection.height < 2 then NextSection.height:=2;
      if NextSection.height > 5 then NextSection.height:=5;
      { now create the section }
      CreateSection;
      { and connect it to previous section }
      Pass;
      LastSection := NextSection;
    end;
  end;

  procedure GenerateMap;
  begin
    { creates the passage shape }
    CreateCoreSection;

    { initialize globai index counter }
    NIndex := -1;

    { make first section to start with }
    //first set parameters
    LastSection.median.X := 0;
    LastSection.median.Y := -2;
    LastSection.median.Z := 2;
    LastSection.width := 1+Random*2;
    LastSection.height := 2+Random*2;
    LastSection.angle := 0;
    //and generate the section
    CreateSection;

    { finally make the passage by generating the sequential sections }
    MakePassage;
  end;

begin
  { initialize TCoordinateNode and TIndexedFaceSetNode }
  Coords := TCoordinateNode.Create;
  Geometry := TIndexedFaceSetNode.Create;

  { generate the passage }
  GenerateMap;

  { merge Coords and Geometry }
  Geometry.Coord := Coords;

  { create some simple material }
  Material := TMaterialNode.Create;
  Material.DiffuseColor := Vector3(1, 0.9, 0.9);
  material.AmbientIntensity := 2;

  { and add it to Appearance node }
  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  { pack everything inside the Shape (geometry + appearance) }
  Shape := TShapeNode.Create;
  Shape.Geometry := Geometry;
  Shape.Appearance := Appearance;

  { and finally add everything to the Root node }
  Root := TX3DRootNode.Create;
  Root.AddChildren(shape);

  Result := Root;
end;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  MainScene.Load(BuildNode, true);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyF4) then
  begin
    WalkNavigation.MouseLook := not WalkNavigation.MouseLook;
    Exit(true); // key was handled
  end;
end;

end.
