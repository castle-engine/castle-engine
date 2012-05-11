{
  Copyright 2008-2012 Michalis Kamburelis.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftLocations;

interface

uses CastleUtils, CastleClassUtils, Classes, CastleScene, GL, VectorMath,
  RiftLoadable, X3DNodes, FGL;

type
  TLocation = class(TLoadable)
  private
    FName: string;
    FImageFileName: string;
    FShadowedImageFileName: string;
    FSceneFileName: string;
    FScene: TCastleScene;
    FGLList_Image, FGLList_ShadowedImage: TGLuint;
    FSceneCameraDescription: string;
    FInitialPosition: TVector3Single;
    FInitialDirection: TVector3Single;
    FInitialUp: TVector3Single;
  protected
    procedure LoadInternal(const BaseLights: TLightInstancesList); override;
    procedure UnLoadInternal; override;
  public
    { Internal creature name, must be simple (needs to be valid XML element
      name and VRML node name, for easy data reading). }
    property Name: string read FName;

    function LoadSteps: Cardinal; override;

    property ImageFileName: string read FImageFileName;
    property ShadowedImageFileName: string read FShadowedImageFileName;
    property SceneFileName: string read FSceneFileName;

    property SceneCameraDescription: string read FSceneCameraDescription;

    property InitialPosition: TVector3Single read FInitialPosition;
    property InitialDirection: TVector3Single read FInitialDirection;
    property InitialUp: TVector3Single read FInitialUp;

    property Scene: TCastleScene read FScene;
    property GLList_Image: TGLuint read FGLList_Image;
    property GLList_ShadowedImage: TGLuint read FGLList_ShadowedImage;
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

uses SysUtils, DOM, GLImages, ProgressUnit, Images, GLRenderer,
  CastleGLUtils, CastleWindow, CastleXMLUtils, CastleSceneCore, RiftWindow, RiftData;

{ TLocation ------------------------------------------------------------------ }

procedure TLocation.LoadInternal(const BaseLights: TLightInstancesList);
begin
  inherited;

  FScene := TCastleScene.CreateCustomCache(nil, GLContextCache);
  FScene.Load(SceneFileName);
  Progress.Step;

  { in normal (non-debug) circumstances, scene is only rendered to depth buffer }
  Scene.Attributes.Mode := rmDepth;

  Scene.PrepareResources([prRender, prBoundingBox], false, BaseLights,
    1 { MultiSampling = 1, it is ignored as we do not include prScreenEffects });
  Progress.Step;

  { prepare octrees }

  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  { TrianglesList was created for triangle octree. We don't need it anymore.

    Hm, for now we actually don't create ssCollidableTriangles,
    so TrianglesList is not actually used.
  Scene.FreeResources([frTrianglesListNotOverTriangulate]); }

  Progress.Step;

  FGLList_Image := LoadImageToDisplayList(ImageFileName,
    [TRGBImage], [], Window.Width, Window.Height);
  FGLList_ShadowedImage := LoadImageToDisplayList(ShadowedImageFileName,
    [TRGBImage], [], Window.Width, Window.Height);
  Progress.Step;
end;

function TLocation.LoadSteps: Cardinal;
begin
  Result := inherited LoadSteps + 4;
end;

procedure TLocation.UnLoadInternal;
begin
  FreeAndNil(FScene);
  glFreeDisplayList(FGLList_Image);
  glFreeDisplayList(FGLList_ShadowedImage);
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

procedure WindowOpen(Window: TCastleWindowBase);

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

  if not DOMGetAttribute(LocationsElement, 'start_name', StartLocationName) then
    raise Exception.CreateFmt(
      '<locations> doesn''t have a required attribute "start_name"', []);

  I := TXMLElementIterator.Create(LocationsElement);
  try
    while I.GetNext do
    begin
      Location := TLocation.Create;
      Locations.Add(Location);

      if not DOMGetAttribute(I.Current, 'name', Location.FName) then
        MissingLocationAttribute('name');
      if Location.Name = StartLocationName then
        StartLocation := Location;

      if not DOMGetAttribute(I.Current, 'image_file_name', Location.FImageFileName) then
        MissingLocationAttribute('image_file_name');
      Location.FImageFileName := DataFileNameFromConfig(Location.FImageFileName);

      if not DOMGetAttribute(I.Current, 'shadowed_image_file_name', Location.FShadowedImageFileName) then
        MissingLocationAttribute('shadowed_image_file_name');
      Location.FShadowedImageFileName := DataFileNameFromConfig(Location.FShadowedImageFileName);

      if not DOMGetAttribute(I.Current, 'scene_file_name', Location.FSceneFileName) then
        MissingLocationAttribute('scene_file_name');
      Location.FSceneFileName := DataFileNameFromConfig(Location.FSceneFileName);

      DOMGetAttribute(I.Current, 'scene_camera_description',
        Location.FSceneCameraDescription);

      if DOMGetAttribute(I.Current, 'initial_position', V) then
        Location.FInitialPosition := Vector3SingleFromStr(V) else
        Location.FInitialPosition := Vector3Single(0, 0, 0);

      if DOMGetAttribute(I.Current, 'initial_direction', V) then
        Location.FInitialDirection := Vector3SingleFromStr(V) else
        Location.FInitialDirection := Vector3Single(1, 0, 0);

      if DOMGetAttribute(I.Current, 'initial_up', V) then
        Location.FInitialUp := Vector3SingleFromStr(V) else
        Location.FInitialUp := Vector3Single(0, 0, 1);
    end;
  finally FreeAndNil(I) end;

  if StartLocation = nil then
    raise Exception.CreateFmt('Start location name "%s" not found',
      [StartLocationName]);
end;

procedure WindowClose(Window: TCastleWindowBase);
begin
  FreeAndNil(Locations);
end;

initialization
  Window.OnOpenList.Add(@WindowOpen);
  Window.OnCloseList.Add(@WindowClose);
end.
