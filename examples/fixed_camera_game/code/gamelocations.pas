{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Locations in game (Locations). }
unit GameLocations;

interface

uses Classes, Generics.Collections,
  CastleUtils, CastleClassUtils, CastleScene, CastleVectors, CastleTransform,
  CastleGLImages, X3DNodes, CastleRectangles, CastleRenderer;

type
  TLocation = class
  private
  public
    type
      TLocationScene = class(TCastleScene)
      private
        Image, ShadowedImage: TGLImage;
      public
        SceneManagerRect: TRectangle;
        RenderInternalModel: boolean;
        procedure LocalRender(const Params: TRenderParams); override;
      end;
  private
    FName: string;
    FImageURL: string;
    FShadowedImageURL: string;
    FSceneURL: string;
    FScene: TLocationScene;
    FImage, FShadowedImage: TGLImage;
    FSceneCameraDescription: string;
    FPlayerPosition: TVector3;
    FPlayerDirection: TVector3;
    FPlayerUp: TVector3;
    Loaded: boolean;
  public
    destructor Destroy; override;

    { Create things necessary for playing (displaying this location). }
    procedure Load(const PrepareParams: TPrepareParams);

    { Location short name, must be simple (needs to be valid XML element
      name and X3D node name, for easy data reading). }
    property Name: string read FName;

    property ImageURL: string read FImageURL;
    property ShadowedImageURL: string read FShadowedImageURL;
    property SceneURL: string read FSceneURL;

    property SceneCameraDescription: string read FSceneCameraDescription;

    property PlayerPosition: TVector3 read FPlayerPosition;
    property PlayerDirection: TVector3 read FPlayerDirection;
    property PlayerUp: TVector3 read FPlayerUp;

    property Scene: TLocationScene read FScene;
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

uses SysUtils, DOM,
  CastleProgress, CastleImages, CastleUIControls, CastleGLUtils, CastleXMLUtils,
  CastleSceneCore, CastleApplicationProperties, X3DLoad,
  GameConfiguration;

{ TLocation.TLocationScene --------------------------------------------------- }

procedure TLocation.TLocationScene.LocalRender(const Params: TRenderParams);

  { Draw Image centered on screen, to fit inside the scene manager rect,
    matching the 3D scene projection. }
  procedure DrawImage(const Image: TGLImage);
  var
    DrawRect: TRectangle;
  begin
    { Draw Image such that Image.Height always fills the SceneManagerRect.Height,
      because that is the field of view of 3D scene,
      and that was the field of view used to render the image in Blender. }
    DrawRect := Image.Rect.ScaleToHeight(SceneManagerRect.Height);
    { above calculated DrawRect size OK, but DrawRect position (Left, Bottom)
      should be fixed now. }
    DrawRect := SceneManagerRect.CenterInside(DrawRect.Width, DrawRect.Height);
    Image.Draw(DrawRect);
  end;

var
  SavedProjectionMatrix: TMatrix4;
begin
  if RenderInternalModel then
  begin
    Attributes.Mode := rmFull;
    inherited;
  end else
  begin
    { this makes a tiny (not important in case of our trivial location 3D model)
      optimization: since we only care about filling the depth buffer,
      rendering with rmDepth will not initialize some material stuff. }
    Attributes.Mode := rmDepth;

    RenderContext.ColorChannels := [];
    inherited;
    RenderContext.ColorChannels := [0..3];

    { Render the 2D image covering the location.

      This cooperates correctly with shadow volumes, because the Image.Draw call
      is correctly masked with the stencil buffer settings of the shadow volume
      renderer.

      To work correctly, the location scene must be rendered before creatures
      (like Player) scenes (otherwise Image.Draw would unconditionally cover
      the the Player). This is satisfied, since CurrentLocation.Scene
      is first in SceneManager.Items. }

    if (not Params.Transparent) and
       (Params.ShadowVolumesReceivers = ReceiveShadowVolumes) then
    begin
      { Nole that the 3 lines that save, set and restore RenderContext.ProjectionMatrix
        are necessary only in case GLFeatures.EnableFixedFunction = true,
        which will be false on all modern GPUs and OpenGLES.
        When GLFeatures.EnableFixedFunction = false,
        then rendering Image doesn't need to have a projection matrix set. }
      SavedProjectionMatrix := RenderContext.ProjectionMatrix;
      OrthoProjection(FloatRectangle(SceneManagerRect)); // need 2D projection

      if Params.InShadow then
        DrawImage(ShadowedImage)
      else
        DrawImage(Image);

      RenderContext.ProjectionMatrix := SavedProjectionMatrix; // restore 3D projection
    end;
  end;
end;

{ TLocation ------------------------------------------------------------------ }

destructor TLocation.Destroy;
begin
  FreeAndNil(FScene);
  FreeAndNil(FImage);
  FreeAndNil(FShadowedImage);
  inherited;
end;

procedure TLocation.Load(const PrepareParams: TPrepareParams);
begin
  if Loaded then Exit;
  Loaded := true;

  FImage := TGLImage.Create(ImageURL, [TRGBImage]);
  FShadowedImage := TGLImage.Create(ShadowedImageURL, [TRGBImage]);

  FScene := TLocationScene.Create(nil);
  FScene.Spatial := [ssRendering, ssDynamicCollisions];
  // two-sided lighting
  FScene.Attributes.PhongShading := true;
  { The shadows are already drawn on location Image,
    so no need to cast them on location again.
    TODO: This also means that location cannot cast shadows on Player.
    A better approach would be to leave CastShadowVolumes = true (default),
    and change location Image to *not* contain location shadows "baked". }
  FScene.CastShadowVolumes := false;
  FScene.Load(SceneURL, true);
  FScene.PrepareResources([prRender, prBoundingBox], false, PrepareParams);
  FScene.Image := Image;
  FScene.ShadowedImage := ShadowedImage;
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

      Location.FPlayerPosition := I.Current.AttributeVector3Def(
        'player_position', TVector3.Zero);
      Location.FPlayerDirection := I.Current.AttributeVector3Def(
        'player_direction', Vector3(1, 0, 0));
      Location.FPlayerUp := I.Current.AttributeVector3Def(
        'player_up', Vector3(0, 0, 1));
    end;
  finally FreeAndNil(I) end;

  if StartLocation = nil then
    raise Exception.CreateFmt('Start location name "%s" not found',
      [StartLocationName]);
end;

finalization
  FreeAndNil(Locations);
end.
