{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "The Rift".

  "The Rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "The Rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "The Rift"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Locations in game (Locations). }
unit GameLocations;

interface

uses Classes, Generics.Collections,
  CastleUtils, CastleClassUtils, CastleScene, CastleVectors, CastleTransform,
  CastleGLImages, X3DNodes;

type
  TLocation = class
  private
    FName: string;
    FImageURL: string;
    FShadowedImageURL: string;
    FSceneURL: string;
    FScene: TCastleScene;
    FImage, FShadowedImage: TGLImage;
    FSceneCameraDescription: string;
    FInitialPosition: TVector3;
    FInitialDirection: TVector3;
    FInitialUp: TVector3;
    Loaded: boolean;
    procedure EnumerateLights(Node: TX3DNode);
  public
    destructor Destroy; override;

    { Create things necessary for playing (displaying this location). }
    procedure Load(const PrepareParams: TPrepareParams);

    { Internal name, must be simple (needs to be valid XML element
      name and X3D node name, for easy data reading). }
    property Name: string read FName;

    property ImageURL: string read FImageURL;
    property ShadowedImageURL: string read FShadowedImageURL;
    property SceneURL: string read FSceneURL;

    property SceneCameraDescription: string read FSceneCameraDescription;

    property InitialPosition: TVector3 read FInitialPosition;
    property InitialDirection: TVector3 read FInitialDirection;
    property InitialUp: TVector3 read FInitialUp;

    property Scene: TCastleScene read FScene;
    property Image: TGLImage read FImage;
    property ShadowedImage: TGLImage read FShadowedImage;
  end;

  TLocationList = class(specialize TObjectList<TLocation>)
  public
    StartLocation: TLocation;

    { Create locations and set their parameters from GameConfig. }
    constructor Create;
  end;

var
  { List of all locations, should be created in Application.OnInitialize. }
  Locations: TLocationList;

implementation

uses SysUtils, DOM, CastleProgress, CastleImages, CastleRenderer,
  CastleUIControls, CastleGLUtils, CastleWindow, CastleXMLUtils,
  CastleSceneCore, CastleApplicationProperties, X3DLoad,
  GameConfiguration;

{ TLocation ------------------------------------------------------------------ }

destructor TLocation.Destroy;
begin
  FreeAndNil(FScene);
  FreeAndNil(FImage);
  FreeAndNil(FShadowedImage);
  inherited;
end;

procedure TLocation.EnumerateLights(Node: TX3DNode);
begin
  { This is not necessary for normal game display,
    but it is useful in DebugDisplay = ddOnly3D or ddBlend3D.
    It means that shadows (through shadow volumes) are also visible on 3D rendering. }
  (Node as TAbstractLightNode).ShadowVolumes := true;
end;

procedure TLocation.Load(const PrepareParams: TPrepareParams);
var
  SceneRoot: TX3DRootNode;
begin
  if Loaded then Exit;
  Loaded := true;

  FScene := TCastleScene.Create(nil);
  FScene.Spatial := [ssRendering, ssDynamicCollisions];
  // TODO
  { in normal (non-debug) circumstances, scene is only rendered to depth buffer }
  FScene.Attributes.Mode := rmDepth;
  SceneRoot := Load3D(SceneURL);
  // TODO
  SceneRoot.EnumerateNodes(TAbstractLightNode, @EnumerateLights, false);
  FScene.Load(SceneRoot, true);

  FScene.PrepareResources([prRender, prBoundingBox], false, PrepareParams);

  FImage := TGLImage.Create(ImageURL, [TRGBImage]);
  FShadowedImage := TGLImage.Create(ShadowedImageURL, [TRGBImage]);
end;

{ TLocationList ------------------------------------------------------------- }

constructor TLocationList.Create;

  procedure MissingLocationAttribute(const AttrName: string);
  begin
    raise Exception.CreateFmt('Location doesn''t have a required attribute "%s"',
      [AttrName]);
  end;

var
  I: TXMLElementIterator;
  LocationsElement: TDOMElement;
  Location: TLocation;
  StartLocationName: string;
begin
  inherited Create(true);

  LocationsElement := GameConfig.PathElement('locations');
  if LocationsElement = nil then
    raise Exception.Create('Unable to find XML <locations> element');

  if not LocationsElement.AttributeString('start_name', StartLocationName) then
    raise Exception.Create(
      '<locations> doesn''t have a required attribute "start_name"');

  I := LocationsElement.ChildrenIterator;
  try
    while I.GetNext do
    begin
      Location := TLocation.Create;
      Add(Location);

      if not I.Current.AttributeString('name', Location.FName) then
        MissingLocationAttribute('name');
      if Location.Name = StartLocationName then
        StartLocation := Location;

      Location.FImageURL := I.Current.AttributeURL('image_url', GameConfig.URL);
      Location.FShadowedImageURL := I.Current.AttributeURL('shadowed_image_url', GameConfig.URL);
      Location.FSceneURL := I.Current.AttributeURL('scene_url', GameConfig.URL);

      I.Current.AttributeString('scene_camera_description',
        Location.FSceneCameraDescription);

      Location.FInitialPosition := I.Current.AttributeVector3Def(
        'initial_position', TVector3.Zero);
      Location.FInitialDirection := I.Current.AttributeVector3Def(
        'initial_direction', Vector3(1, 0, 0));
      Location.FInitialUp := I.Current.AttributeVector3Def(
        'initial_up', Vector3(0, 0, 1));
    end;
  finally FreeAndNil(I) end;

  if StartLocation = nil then
    raise Exception.CreateFmt('Start location name "%s" not found',
      [StartLocationName]);
end;

finalization
  FreeAndNil(Locations);
end.
