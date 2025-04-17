{
  Copyright 2008-2024 Michalis Kamburelis.

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
  CastleGLImages, X3DNodes, CastleRectangles, CastleRenderOptions;

type
  TLocation = class
  private
  public
    type
      TLocationScene = class(TCastleScene)
      end;

      TLocationImageTransform = class(TCastleTransform)
      private
        Image, ShadowedImage: TDrawableImage;
        procedure RenderLocation(const Transformation: TTransformation;
          const PassParams: TRenderOnePassParams);
      public
        ViewportRect: TRectangle;
        procedure LocalRender(const Params: TRenderParams); override;
      end;

  private
    FName: string;
    FImageUrl: String;
    FShadowedImageUrl: String;
    FSceneUrl: String;
    FViewpoint: String;
    FScene: TLocationScene;
    FImageTransform: TLocationImageTransform;
    FImage, FShadowedImage: TDrawableImage;
    FSceneCameraDescription: string;
    FPlayerPosition: TVector3;
    FPlayerDirection: TVector3;
    Loaded: boolean;
    FRenderInternalModel: Boolean;
    procedure SetRenderInternalModel(const Value: Boolean);
    procedure RenderInternalModelChanged;
  public
    constructor Create;
    destructor Destroy; override;

    { Create things necessary for playing (displaying this location). }
    procedure Load(const PrepareParams: TPrepareParams);

    { Location short name, must be simple (needs to be valid XML element
      name and X3D node name, for easy data reading). }
    property Name: string read FName;

    property ImageUrl: String read FImageUrl;
    property ShadowedImageUrl: String read FShadowedImageUrl;
    property SceneUrl: String read FSceneUrl;

    property SceneCameraDescription: string read FSceneCameraDescription;

    property PlayerPosition: TVector3 read FPlayerPosition;
    property PlayerDirection: TVector3 read FPlayerDirection;

    property Scene: TLocationScene read FScene;
    property ImageTransform: TLocationImageTransform read FImageTransform;

    // Not needed publicly.
    // property Image: TDrawableImage read FImage;
    // property ShadowedImage: TDrawableImage read FShadowedImage;

    { Should we render the internal location 3D model as usual
      for debug purposes.
      By default, when this is @false, the location 3D model
      is used for depth information, but not for colors. }
    property RenderInternalModel: Boolean read FRenderInternalModel
      write SetRenderInternalModel default false;
  end;

  TLocationList = class({$ifdef FPC}specialize{$endif} TObjectList<TLocation>)
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
  CastleImages, CastleUIControls, CastleGLUtils, CastleXmlUtils, CastleUriUtils,
  CastleSceneCore, CastleApplicationProperties, X3DLoad, CastleRenderContext,
  GameConfiguration;

{ TLocation.TLocationImage --------------------------------------------------- }

procedure TLocation.TLocationImageTransform.LocalRender(const Params: TRenderParams);
begin
  inherited;
  Params.AddRenderEvent({$ifdef FPC}@{$endif} RenderLocation);
end;

procedure TLocation.TLocationImageTransform.RenderLocation(
  const Transformation: TTransformation;
  const PassParams: TRenderOnePassParams);

  { Draw Image centered on screen, to fit inside the TCastleViewport,
    matching the 3D scene projection. }
  procedure DrawImage(const Image: TDrawableImage);
  var
    DrawRect: TRectangle;
  begin
    { Draw Image such that Image.Height always fills the ViewportRect.Height,
      because that is the field of view of 3D scene,
      and that was the field of view used to render the image in Blender. }
    DrawRect := Image.Rect.ScaleToHeight(ViewportRect.Height);
    { above calculated DrawRect size OK, but DrawRect position (Left, Bottom)
      should be fixed now. }
    DrawRect := ViewportRect.CenterInside(DrawRect.Width, DrawRect.Height);
    Image.Draw(DrawRect);
  end;

var
  SavedProjectionMatrix: TMatrix4;
  SavedDepthTest: Boolean;
begin
  { Render the 2D image covering the location.

    This cooperates correctly with shadow volumes, because when
    PassParams.DisableShadowVolumeCastingLights = true then also the stencil
    buffer settings force drawing only when the shadows are.
    The call to Image.Draw (when DisableShadowVolumeCastingLights)
    thus renders only pixels that are detected as "in shadow".

    The order of rendering is important also. Our Image.Draw must be drawn
    unconditionaly right where the Scene is drawn, such that
    - color buffer set by Image.Draw
    - ...corresponds to the depth buffer set by the Scene.
    This way player 3D character is correctly behind / in front of the rendered
    location image.
    This works now, because
    - event added by AddRenderEvent happens before everything now.
    - then CurrentLocation.Scene is first in Viewport.Items.
    - then the rest, like Player, scenes are rendered (that cannot be
      overdrawn by Image.Draw). }

  if (not PassParams.UsingBlending) and
     (true in PassParams.FilterShadowVolumesReceivers) then
  begin
    { Note that the 3 lines that save, set and restore RenderContext.ProjectionMatrix
      are necessary only in case GLFeatures.EnableFixedFunction = true,
      which will be false on all modern GPUs and OpenGLES.
      When GLFeatures.EnableFixedFunction = false,
      then rendering Image doesn't need to have a projection matrix set. }
    SavedProjectionMatrix := RenderContext.ProjectionMatrix;
    OrthoProjection(FloatRectangle(ViewportRect)); // need 2D projection

    // do not test or change Z buffer
    SavedDepthTest := RenderContext.DepthTest;
    RenderContext.DepthTest := false;

    if PassParams.DisableShadowVolumeCastingLights then
      DrawImage(ShadowedImage)
    else
      DrawImage(Image);

    RenderContext.ProjectionMatrix := SavedProjectionMatrix; // restore 3D projection
    RenderContext.DepthTest := SavedDepthTest;
  end;
end;

{ TLocation ------------------------------------------------------------------ }

constructor TLocation.Create;
begin
  inherited;
end;

destructor TLocation.Destroy;
begin
  FreeAndNil(FScene);
  FreeAndNil(FImageTransform);
  FreeAndNil(FImage);
  FreeAndNil(FShadowedImage);
  inherited;
end;

procedure TLocation.Load(const PrepareParams: TPrepareParams);

  function HasViewpoint(const Scene: TCastleSceneCore; const ViewpointName: string): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to Scene.ViewpointsCount - 1 do
      if Scene.GetViewpointName(I) = ViewpointName then
        Exit(true);
    Result := false;
  end;

begin
  if Loaded then Exit;
  Loaded := true;

  FImage := TDrawableImage.Create(ImageUrl, [TRGBImage]);
  FShadowedImage := TDrawableImage.Create(ShadowedImageUrl, [TRGBImage]);

  FScene := TLocationScene.Create(nil);
  FScene.PreciseCollisions := true;
  { The shadows are already drawn on location Image,
    so no need to cast them on location again.
    TODO: This also means that location cannot cast shadows on Player.
    A better approach would be to leave CastShadows = true (default),
    and change location Image to *not* contain location shadows "baked". }
  FScene.CastShadows := false;
  FScene.InitialViewpointName := FViewpoint;
  FScene.Load(SceneUrl);
  FScene.PrepareResources([prRenderSelf, prBoundingBox], PrepareParams);

  // check FScene.InitialViewpointName valid
  if not HasViewpoint(FScene, FViewpoint) then
    raise Exception.CreateFmt('Viewpoint name "%s" not found in scene "%s"', [
      FViewpoint,
      UriDisplay(SceneUrl)
    ]);

  FImageTransform := TLocationImageTransform.Create(nil);
  FImageTransform.Image := FImage;
  FImageTransform.ShadowedImage := FShadowedImage;

  RenderInternalModelChanged;
end;

procedure TLocation.SetRenderInternalModel(const Value: Boolean);
begin
  if FRenderInternalModel <> Value then
  begin
    FRenderInternalModel := Value;
    RenderInternalModelChanged;
  end;
end;

procedure TLocation.RenderInternalModelChanged;
begin
  FImageTransform.Exists := not RenderInternalModel;

  if RenderInternalModel then
  begin
    FScene.RenderOptions.Mode := rmFull;
    FScene.RenderOptions.InternalColorChannels := AllColorChannels;
  end else
  begin
    // render scene only to depth buffer
    FScene.RenderOptions.Mode := rmDepth;
    FScene.RenderOptions.InternalColorChannels := [];
  end;
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

      Location.FImageUrl := I.Current.AttributeUrl('image_url', GameConfig.Url);
      Location.FShadowedImageUrl := I.Current.AttributeUrl('shadowed_image_url', GameConfig.Url);
      Location.FSceneUrl := I.Current.AttributeUrl('scene_url', GameConfig.Url);
      Location.FViewpoint := I.Current.AttributeStringDef('viewpoint', '');

      I.Current.AttributeString('scene_camera_description',
        Location.FSceneCameraDescription);

      Location.FPlayerPosition := I.Current.AttributeVector3Def(
        'player_position', TVector3.Zero);
      Location.FPlayerDirection := I.Current.AttributeVector3Def(
        'player_direction', Vector3(1, 0, 0));
    end;
  finally FreeAndNil(I) end;

  if StartLocation = nil then
    raise Exception.CreateFmt('Start location name "%s" not found',
      [StartLocationName]);
end;

initialization
finalization
  FreeAndNil(Locations);
end.
