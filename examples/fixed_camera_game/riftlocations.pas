{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "the rift".

  "the rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "the rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "the rift"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftLocations;

interface

uses CastleUtils, CastleClassUtils, Classes, CastleScene, CastleVectors,
  RiftLoadable, X3DNodes, FGL, CastleGLImages;

type
  TLocation = class(TLoadable)
  private
    FName: string;
    FImageURL: string;
    FShadowedImageURL: string;
    FSceneURL: string;
    FScene: TCastleScene;
    FGLImage, FGLShadowedImage: TGLImage;
    FSceneCameraDescription: string;
    FInitialPosition: TVector3Single;
    FInitialDirection: TVector3Single;
    FInitialUp: TVector3Single;
    procedure EnumerateLights(Node: TX3DNode);
  protected
    procedure LoadInternal(const BaseLights: TLightInstancesList); override;
    procedure UnLoadInternal; override;
  public
    { Internal creature name, must be simple (needs to be valid XML element
      name and VRML node name, for easy data reading). }
    property Name: string read FName;

    function LoadSteps: Cardinal; override;

    property ImageURL: string read FImageURL;
    property ShadowedImageURL: string read FShadowedImageURL;
    property SceneURL: string read FSceneURL;

    property SceneCameraDescription: string read FSceneCameraDescription;

    property InitialPosition: TVector3Single read FInitialPosition;
    property InitialDirection: TVector3Single read FInitialDirection;
    property InitialUp: TVector3Single read FInitialUp;

    property Scene: TCastleScene read FScene;
    property GLImage: TGLImage read FGLImage;
    property GLShadowedImage: TGLImage read FGLShadowedImage;
  end;

  TLocationList = class(specialize TFPGObjectList<TLocation>)
    { Call Load on all items, producing also appropriate progress bar. }
    procedure Load(const BaseLights: TLightInstancesList);
  end;

var
  { List of all locations, created and destroyed in GL open/close. }
  Locations: TLocationList;

  StartLocation: TLocation;

implementation

uses SysUtils, DOM, CastleProgress, CastleImages, CastleRenderer,
  CastleUIControls, CastleGLUtils, CastleWindow, CastleXMLUtils,
  CastleSceneCore, CastleApplicationProperties, X3DLoad,
  RiftWindow, RiftData;

{ TLocation ------------------------------------------------------------------ }

procedure TLocation.EnumerateLights(Node: TX3DNode);
begin
  { This is not necessary for normal game display,
    but it is useful in DebugDisplay = ddOnly3D or ddBlend3D.
    It means that shadows (through shadow volumes) are also visible on 3D rendering. }
  (Node as TAbstractLightNode).ShadowVolumes := true;
end;

procedure TLocation.LoadInternal(const BaseLights: TLightInstancesList);
var
  SceneRoot: TX3DRootNode;
begin
  inherited;

  FScene := TCastleScene.Create(nil);
  SceneRoot := Load3D(SceneURL);
  SceneRoot.EnumerateNodes(TAbstractLightNode, @EnumerateLights, false);
  FScene.Load(SceneRoot, true);
  Progress.Step;

  { in normal (non-debug) circumstances, scene is only rendered to depth buffer }
  Scene.Attributes.Mode := rmDepth;

  Scene.PrepareResources([prRender, prBoundingBox], false, BaseLights);
  Progress.Step;

  { prepare octrees }

  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  Progress.Step;

  FGLImage := TGLImage.Create(ImageURL, [TRGBImage]);
  FGLShadowedImage := TGLImage.Create(ShadowedImageURL, [TRGBImage]);
  Progress.Step;
end;

function TLocation.LoadSteps: Cardinal;
begin
  Result := inherited LoadSteps + 4;
end;

procedure TLocation.UnLoadInternal;
begin
  FreeAndNil(FScene);
  FreeAndNil(FGLImage);
  FreeAndNil(FGLShadowedImage);
  inherited;
end;

{ TLocationList ------------------------------------------------------------- }

procedure TLocationList.Load(const BaseLights: TLightInstancesList);
var
  I: Integer;
  ProgressCount: Cardinal;
begin
  ProgressCount := 0;
  for I := 0 to Count - 1 do
    ProgressCount += Items[I].LoadSteps;

  Progress.Init(ProgressCount, 'Loading locations');
  try
    for I := 0 to Count - 1 do
      Items[I].Load(BaseLights);
  finally Progress.Fini end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure ContextOpen;

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
  V: string;
begin
  Locations := TLocationList.Create(true);

  LocationsElement := DataConfig.PathElement('locations');
  if LocationsElement = nil then
    raise Exception.Create('Unable to find XML <locations> element');

  if not LocationsElement.AttributeString('start_name', StartLocationName) then
    raise Exception.CreateFmt(
      '<locations> doesn''t have a required attribute "start_name"', []);

  I := LocationsElement.ChildrenIterator;
  try
    while I.GetNext do
    begin
      Location := TLocation.Create;
      Locations.Add(Location);

      if not I.Current.AttributeString('name', Location.FName) then
        MissingLocationAttribute('name');
      if Location.Name = StartLocationName then
        StartLocation := Location;

      Location.FImageURL := I.Current.AttributeURL('image_url', DataConfig.URL);
      Location.FShadowedImageURL := I.Current.AttributeURL('shadowed_image_url', DataConfig.URL);
      Location.FSceneURL := I.Current.AttributeURL('scene_url', DataConfig.URL);

      I.Current.AttributeString('scene_camera_description',
        Location.FSceneCameraDescription);

      if I.Current.AttributeString('initial_position', V) then
        Location.FInitialPosition := Vector3SingleFromStr(V) else
        Location.FInitialPosition := Vector3Single(0, 0, 0);

      if I.Current.AttributeString('initial_direction', V) then
        Location.FInitialDirection := Vector3SingleFromStr(V) else
        Location.FInitialDirection := Vector3Single(1, 0, 0);

      if I.Current.AttributeString('initial_up', V) then
        Location.FInitialUp := Vector3SingleFromStr(V) else
        Location.FInitialUp := Vector3Single(0, 0, 1);
    end;
  finally FreeAndNil(I) end;

  if StartLocation = nil then
    raise Exception.CreateFmt('Start location name "%s" not found',
      [StartLocationName]);
end;

procedure ContextClose;
begin
  FreeAndNil(Locations);
end;

initialization
  ApplicationProperties.OnGLContextOpen.Add(@ContextOpen);
  ApplicationProperties.OnGLContextClose.Add(@ContextClose);
end.
