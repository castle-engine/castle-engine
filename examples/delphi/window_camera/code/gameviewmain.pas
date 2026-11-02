{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place.

  Shows the device camera (using FMX TCameraComponent) as a texture
  on a 3D box, with TCastleButton components to change
  the basic camera properties (kind, focus mode, torch mode). }
unit GameViewMain;

interface

uses Classes, System.Types,
  { FMX units, to access the camera.
    We can use them, because TCastleWindow on Delphi (on Android, iOS and more)
    is implemented using FMX. }
  System.Permissions, FMX.Media, FMX.Graphics,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene, CastleViewport, X3DNodes;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    LabelStatus: TCastleLabel;
    ButtonKind: TCastleButton;
    ButtonFocusMode: TCastleButton;
    ButtonTorchMode: TCastleButton;
    SceneBox: TCastleScene;
    RootGroup: TCastleUserInterface;
  private
    { FMX component to access the device camera. }
    DeviceCamera: TCameraComponent;
    { Last frame from the camera, as FMX bitmap. }
    CameraBitmap: TBitmap;
    { Box and texture on the box, updated with each camera frame. }
    Box: TBoxNode;
    BoxTexture: TImageTextureNode;
    LifeTime: Single;
    { Did we receive a frame from the current camera (since the view
      started or since we changed the camera Kind).
      Only then we query the camera capabilities and modes. }
    CameraReady: Boolean;
    procedure CreateBox;
    procedure CameraPermissionsResult(Sender: TObject;
      const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
    { Update buttons to show current camera properties,
      disable buttons for features not supported by the current camera. }
    procedure UpdateCameraUi;
    procedure ClickKind(Sender: TObject);
    procedure ClickFocusMode(Sender: TObject);
    procedure ClickTorchMode(Sender: TObject);
    procedure CameraSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, System.TypInfo,
  CastleRenderOptions, CastleFmxUtils;

const
  { Android permission name. }
  PermissionCamera = 'android.permission.CAMERA';

  { Width of the box face that shows the camera image.
    Chosen to fill the viewport width (see the camera position in the design). }
  BoxWidth = 2.8;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  CreateBox;

  ButtonKind.OnClick := ClickKind;
  ButtonFocusMode.OnClick := ClickFocusMode;
  ButtonTorchMode.OnClick := ClickTorchMode;

  CameraBitmap := TBitmap.Create;
  CameraReady := false;

  DeviceCamera := TCameraComponent.Create(FreeAtStop);
  DeviceCamera.OnSampleBufferReady := CameraSampleBufferReady;
  DeviceCamera.Kind := TCameraKind.BackCamera;
  UpdateCameraUi;

  LabelStatus.Caption := 'Waiting for the camera permission...';

  { Ask user for the permission to use the camera.
    The result (also when the permission is already granted)
    is passed to CameraPermissionsResult. }
  PermissionsService.RequestPermissions([PermissionCamera],
    CameraPermissionsResult);
end;

procedure TViewMain.CameraPermissionsResult(Sender: TObject;
  const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
begin
  if (Length(AGrantResults) = 1) and
     (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    { Start the camera. Camera frames are then passed to
      CameraSampleBufferReady. }
    DeviceCamera.Quality := TVideoCaptureQuality.MediumQuality;
    DeviceCamera.Active := true;
    LabelStatus.Caption := 'Waiting for the camera...';
  end else
    LabelStatus.Caption := 'No permission to use the camera';
end;

procedure TViewMain.Stop;
begin
  DeviceCamera.Active := false;
  FreeAndNil(CameraBitmap);
  inherited;
end;

procedure TViewMain.CreateBox;
var
  Shape: TShapeNode;
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
  RootNode: TX3DRootNode;
  TexProperties: TTexturePropertiesNode;
begin
  { Build a box using X3D nodes, this way we have access to
    the texture node (BoxTexture) and we can change the texture contents
    at any time. See https://castle-engine.io/viewport_and_scenes_from_code
    about building scenes by code. }

  TexProperties := TTexturePropertiesNode.Create;
  TexProperties.MagnificationFilter := magDefault;
  TexProperties.MinificationFilter := minDefault;
  TexProperties.BoundaryModeS := bmClampToEdge;
  TexProperties.BoundaryModeT := bmClampToEdge;
  { Do not force "power of 2" size of the image.
    Disables mipmaps, but avoids distortion of non-power-of-2 images
    (and wasting time scaling them). }
  TexProperties.GuiTexture := true;

  BoxTexture := TImageTextureNode.Create;
  BoxTexture.TextureProperties := TexProperties;

  { Unlit material means that the box doesn't need any lights,
    it will just display the texture colors. }
  Material := TUnlitMaterialNode.Create;
  Material.EmissiveTexture := BoxTexture;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Box := TBoxNode.CreateWithShape(Shape);
  Box.Size := Vector3(BoxWidth, BoxWidth * 4 / 3, 1);
  Shape.Appearance := Appearance;

  RootNode := TX3DRootNode.Create;
  RootNode.AddChildren(Shape);

  SceneBox.Load(RootNode, true);
end;

procedure TViewMain.CameraSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin
  { This is called by FMX in the main thread, for each new camera frame. }

  DeviceCamera.SampleBufferToBitmap(CameraBitmap, true);
  if (CameraBitmap.Width = 0) or (CameraBitmap.Height = 0) then
    Exit;

  if not CameraReady then
  begin
    CameraReady := true;
    UpdateCameraUi;
  end;

  { Display camera frame as a texture on the box.
    BitmapToCastleImage converts FMX TBitmap to the engine image
    (TRGBAlphaImage). The texture node takes ownership of the image
    (2nd parameter is true), so we don't need to free it. }
  BoxTexture.LoadFromImage(BitmapToCastleImage(CameraBitmap), true, '');

  { Make the box front face have the same proportions as the camera image. }
  Box.Size := Vector3(
    BoxWidth, BoxWidth * CameraBitmap.Height / CameraBitmap.Width, 1);

  LabelStatus.Caption := Format('Image size: %d x %d', [
    CameraBitmap.Width,
    CameraBitmap.Height
  ]);
end;

procedure TViewMain.UpdateCameraUi;
var
  HasTorch, HasFocusMode: Boolean;
begin
  ButtonKind.Caption := 'Kind: ' + GetEnumName(TypeInfo(TCameraKind),
    Ord(DeviceCamera.Kind));

  { Query the camera only once it works (CameraReady).
    Before that, the user possibly didn't give us the permission to use
    the camera yet (and then querying it raises an exception on Android). }
  HasTorch := CameraReady and DeviceCamera.HasTorch;
  { FMX implements FocusMode only on Android and iOS.
    On other platforms, setting it is ignored and it's always AutoFocus.
    There's no property like HasFocusMode in TCameraComponent to query this. }
  HasFocusMode := CameraReady
    {$if not (defined(ANDROID) or defined(IOS))} and false {$endif};

  ButtonFocusMode.Enabled := HasFocusMode;
  ButtonTorchMode.Enabled := HasTorch;

  if CameraReady then
  begin
    ButtonFocusMode.Caption := 'Focus Mode: ' +
      GetEnumName(TypeInfo(TFocusMode), Ord(DeviceCamera.FocusMode));
    ButtonTorchMode.Caption := 'Torch Mode: ' +
      GetEnumName(TypeInfo(TTorchMode), Ord(DeviceCamera.TorchMode));
  end else
  begin
    ButtonFocusMode.Caption := 'Focus Mode';
    ButtonTorchMode.Caption := 'Torch Mode';
  end;
end;

procedure TViewMain.ClickKind(Sender: TObject);
begin
  { Changing Kind automatically restarts the camera.
    It may be a different camera now, with different capabilities,
    so wait for the next frame to query it. }
  CameraReady := false;
  if DeviceCamera.Kind = High(TCameraKind) then
    DeviceCamera.Kind := Low(TCameraKind)
  else
    DeviceCamera.Kind := Succ(DeviceCamera.Kind);
  UpdateCameraUi;
end;

procedure TViewMain.ClickFocusMode(Sender: TObject);
begin
  if DeviceCamera.FocusMode = High(TFocusMode) then
    DeviceCamera.FocusMode := Low(TFocusMode)
  else
    DeviceCamera.FocusMode := Succ(DeviceCamera.FocusMode);
  UpdateCameraUi;
end;

procedure TViewMain.ClickTorchMode(Sender: TObject);
begin
  {$ifdef ANDROID}
  { FMX on Android ignores TTorchMode.ModeAuto. That is, trying to set it
    is silently ignored, as TAndroidVideoCaptureDevice.SetTorchMode does
      if ATorchMode = TTorchMode.ModeAuto then
        Exit;
    So we avoid this value on Android. }
  if DeviceCamera.TorchMode = TTorchMode.ModeOn then
    DeviceCamera.TorchMode := TTorchMode.ModeOff
  else
    DeviceCamera.TorchMode := TTorchMode.ModeOn;
  {$else}
  if DeviceCamera.TorchMode = High(TTorchMode) then
    DeviceCamera.TorchMode := Low(TTorchMode)
  else
    DeviceCamera.TorchMode := Succ(DeviceCamera.TorchMode);
  {$endif}
  UpdateCameraUi;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Swing the box a little, to show that it is really a 3D object. }
  LifeTime := LifeTime + SecondsPassed;
  SceneBox.Rotation := Vector4(0, 1, 0, 0.8 * Sin(LifeTime));

  { Apply the Container.SafeBorder, to not draw UI over mobile status bars,
    notches, etc. }
  RootGroup.Border.Assign(Container.SafeBorder);
end;

end.
