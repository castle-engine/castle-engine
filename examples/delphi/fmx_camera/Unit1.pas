unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Permissions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Objects, FMX.Media, FMX.Controls.Presentation,
  Fmx.CastleControl,
  CastleScene, CastleViewport, CastleControls, CastleUiControls, X3DNodes;

type
  TForm1 = class(TForm)
    ImageCamera: TImage;
    LayoutKind: TLayout;
    ButtonKind: TButton;
    LabelKind: TLabel;
    LayoutFlashMode: TLayout;
    ButtonFlashMode: TButton;
    LabelFlashMode: TLabel;
    LayoutFocusMode: TLayout;
    ButtonFocusMode: TButton;
    LabelFocusMode: TLabel;
    LayoutTorchMode: TLayout;
    ButtonTorchMode: TButton;
    LabelTorchMode: TLabel;
    CastleControl1: TCastleControl;
    CameraComponent1: TCameraComponent;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonKindClick(Sender: TObject);
    procedure ButtonFlashModeClick(Sender: TObject);
    procedure ButtonFocusModeClick(Sender: TObject);
    procedure ButtonTorchModeClick(Sender: TObject);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
  private
    { Components designed using CGE editor, in data/main.castle-user-interface. }
    SceneBox: TCastleScene;
    LabelFps: TCastleLabel;
    { Texture on the box, updated with each camera frame. }
    BoxTexture: TImageTextureNode;
    BoxRotation: Single;
    { Did we receive a frame from the current camera (since the application
      started or since we changed the camera Kind).
      Only then we query the camera capabilities and modes. }
    CameraReady: Boolean;
    procedure CreateBox;
    procedure CameraPermissionsResult(Sender: TObject;
      const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
    { Update labels to show current camera properties,
      disable UI for features not supported by the current camera. }
    procedure UpdateCameraUi;
    procedure DoUpdate(const Sender: TCastleUserInterface;
      const SecondsPassed: Single; var HandleInput: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses System.TypInfo,
  CastleVectors, CastleFmxUtils;

{$R *.fmx}

const
  { Android permission name. }
  PermissionCamera = 'android.permission.CAMERA';

procedure TForm1.FormCreate(Sender: TObject);
begin
  CastleControl1.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Initialize references to components designed using CGE editor,
    saved in data/main.castle-user-interface. }
  SceneBox := CastleControl1.Container.DesignedComponent('SceneBox') as TCastleScene;
  LabelFps := CastleControl1.Container.DesignedComponent('LabelFps') as TCastleLabel;

  CreateBox;

  { Assign event to some OnUpdate, to rotate the box and update FPS display. }
  LabelFps.OnUpdate := DoUpdate;

  CameraComponent1.Kind := TCameraKind.BackCamera;
  UpdateCameraUi;

  { Ask user for the permission to use the camera.
    The result (also when the permission is already granted)
    is passed to CameraPermissionsResult.

    This is necessary on Android: without the permission, most operations
    on TCameraComponent (even setting Quality) raise EPermissionException.
    On other platforms, PermissionsService just reports that the permission
    is granted (and on iOS, FMX asks for the camera permission
    automatically when we activate the camera). }
  PermissionsService.RequestPermissions([PermissionCamera],
    CameraPermissionsResult);

  { To hacky pretent that we have permissions, test this.
    It will work on Windows (where permissions are just granted)
    but not Android. }
  //   CameraPermissionsResult(Self, [PermissionCamera], [TPermissionStatus.Granted]);
end;

procedure TForm1.CameraPermissionsResult(Sender: TObject;
  const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
begin
  if (Length(AGrantResults) = 1) and
     (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    { Start the camera. Camera frames are then passed to
      CameraComponent1SampleBufferReady. }
    CameraComponent1.Quality := TVideoCaptureQuality.MediumQuality;
    CameraComponent1.Active := true;
  end else
    ShowMessage('No permission to use the camera');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CameraComponent1.Active := false;
end;

procedure TForm1.CreateBox;
var
  Box: TBoxNode;
  Shape: TShapeNode;
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
  RootNode: TX3DRootNode;
begin
  { Build a box using X3D nodes, this way we have access to
    the texture node (BoxTexture) and we can change the texture contents
    at any time. See https://castle-engine.io/viewport_and_scenes_from_code
    about building scenes by code. }

  BoxTexture := TImageTextureNode.Create;

  { Unlit material means that the box doesn't need any lights,
    it will just display the texture colors. }
  Material := TUnlitMaterialNode.Create;
  Material.EmissiveTexture := BoxTexture;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Box := TBoxNode.CreateWithShape(Shape);
  Box.Size := Vector3(2, 2, 2);
  Shape.Appearance := Appearance;

  RootNode := TX3DRootNode.Create;
  RootNode.AddChildren(Shape);

  SceneBox.Load(RootNode, true);
end;

procedure TForm1.CameraComponent1SampleBufferReady(Sender: TObject;
  const ATime: TMediaTime);
begin
  { This is called by FMX in the main thread, for each new camera frame. }

  if not CameraReady then
  begin
    CameraReady := true;
    UpdateCameraUi;
  end;

  { Display camera frame in FMX TImage. }
  CameraComponent1.SampleBufferToBitmap(ImageCamera.Bitmap, true);

  { Display camera frame as a texture on the box in TCastleControl.
    BitmapToCastleImage converts FMX TBitmap to the engine image
    (TRGBAlphaImage). The texture node takes ownership of the image
    (2nd parameter is true), so we don't need to free it. }
  BoxTexture.LoadFromImage(BitmapToCastleImage(ImageCamera.Bitmap), true, '');
end;

procedure TForm1.UpdateCameraUi;
var
  HasFlash, HasTorch, HasFocusMode: Boolean;
begin
  LabelKind.Text := GetEnumName(TypeInfo(TCameraKind),
    Ord(CameraComponent1.Kind));

  { Query the camera only once it works (CameraReady).
    Before that, the user possibly didn't give us the permission to use
    the camera yet (and then querying it raises an exception on Android). }
  HasFlash := CameraReady and CameraComponent1.HasFlash;
  HasTorch := CameraReady and CameraComponent1.HasTorch;
  { FMX implements FocusMode only on Android and iOS.
    On other platforms, setting it is ignored and it's always AutoFocus.
    There's no property like HasFocusMode in TCameraComponent to query this. }
  HasFocusMode := CameraReady
    {$if not (defined(ANDROID) or defined(IOS))} and false {$endif};

  ButtonFlashMode.Enabled := HasFlash;
  LabelFlashMode.Enabled := HasFlash;
  ButtonFocusMode.Enabled := HasFocusMode;
  LabelFocusMode.Enabled := HasFocusMode;
  ButtonTorchMode.Enabled := HasTorch;
  LabelTorchMode.Enabled := HasTorch;

  if CameraReady then
  begin
    LabelFlashMode.Text := GetEnumName(TypeInfo(TFlashMode),
      Ord(CameraComponent1.FlashMode));
    LabelFocusMode.Text := GetEnumName(TypeInfo(TFocusMode),
      Ord(CameraComponent1.FocusMode));
    LabelTorchMode.Text := GetEnumName(TypeInfo(TTorchMode),
      Ord(CameraComponent1.TorchMode));
  end;
end;

procedure TForm1.ButtonKindClick(Sender: TObject);
begin
  { Changing Kind automatically restarts the camera.
    It may be a different camera now, with different capabilities,
    so wait for the next frame to query it. }
  CameraReady := false;
  if CameraComponent1.Kind = High(TCameraKind) then
    CameraComponent1.Kind := Low(TCameraKind)
  else
    CameraComponent1.Kind := Succ(CameraComponent1.Kind);
  UpdateCameraUi;
end;

procedure TForm1.ButtonFlashModeClick(Sender: TObject);
begin
  if CameraComponent1.FlashMode = High(TFlashMode) then
    CameraComponent1.FlashMode := Low(TFlashMode)
  else
    CameraComponent1.FlashMode := Succ(CameraComponent1.FlashMode);
  UpdateCameraUi;
end;

procedure TForm1.ButtonFocusModeClick(Sender: TObject);
begin
  if CameraComponent1.FocusMode = High(TFocusMode) then
    CameraComponent1.FocusMode := Low(TFocusMode)
  else
    CameraComponent1.FocusMode := Succ(CameraComponent1.FocusMode);
  UpdateCameraUi;
end;

procedure TForm1.ButtonTorchModeClick(Sender: TObject);
begin
  {$ifdef ANDROID}
  { FMX on Android ignores TTorchMode.ModeAuto. That is, trying to set it
    is silently ignored, as TAndroidVideoCaptureDevice.SetTorchMode does
      if ATorchMode = TTorchMode.ModeAuto then
        Exit;
    So we avoid this value on Android. }
  if CameraComponent1.TorchMode = TTorchMode.ModeOn then
    CameraComponent1.TorchMode := TTorchMode.ModeOff
  else
    CameraComponent1.TorchMode := TTorchMode.ModeOn;
  {$else}
  if CameraComponent1.TorchMode = High(TTorchMode) then
    CameraComponent1.TorchMode := Low(TTorchMode)
  else
    CameraComponent1.TorchMode := Succ(CameraComponent1.TorchMode);
  {$endif}
  UpdateCameraUi;
end;

procedure TForm1.DoUpdate(const Sender: TCastleUserInterface;
  const SecondsPassed: Single; var HandleInput: Boolean);
begin
  LabelFps.Caption := 'FPS: ' + CastleControl1.Container.Fps.ToString;

  { Rotate the box, to show all the faces. }
  BoxRotation := BoxRotation + SecondsPassed * 0.5;
  SceneBox.Rotation := Vector4(1, 1, 0, BoxRotation);
end;

end.
