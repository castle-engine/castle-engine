{
  Copyright 2024-2024 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleViewport, CastleScene,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTimeUtils,
  CastleCameras;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start.
      TODO: web: For now they are created manually in Start, not loaded. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    SpotLight: TCastleSpotLight;
    ButtonDropBox: TCastleButton;
  private
    LifeTime: TFloatTime;
    procedure ClickDropBox(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleColors, CastleUtils, CastleLog, CastleTransform;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: web: Loading castle-data:/ not supported yet on web
  // DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  PointLight: TCastlePointLight;
  Plane: TCastlePlane;
  PlaneBody, ConeBody: TCastleRigidBody;
  PlaneCollider: TCastlePlaneCollider;
  ConeCollider: TCastleSphereCollider; // close enough for cone
  Background: TCastleBackground;
  Navigation: TCastleExamineNavigation;
  Cone: TCastleCone;
  I: Integer;
  LabelInfo: TCastleLabel;
begin
  inherited;

  // TODO: web: No file reading on web yet, https://castle-engine.io/web
  // Would be easier to design this in gameviewmain.castle-user-interface
  // and just load here, but web target cannot load data files yet.

  MainViewport := TCastleViewport.Create(FreeAtStop);
  MainViewport.FullSize := true;
  // MainViewport.Camera.Translation := Vector3(0.00, 2.00, 4.00);
  MainViewport.Camera.SetWorldView(
    Vector3(14.38, 6.97, 21.14), // position
    Vector3(-0.54, -0.28, -0.80), // direction
    Vector3(-0.11, 0.96, -0.26)  // up
  );
  InsertFront(MainViewport);

  PointLight := TCastlePointLight.Create(FreeAtStop);
  PointLight.Translation := Vector3(4.00, 3.00, 1.00);
  MainViewport.Items.Add(PointLight);

  SpotLight := TCastleSpotLight.Create(FreeAtStop);
  SpotLight.Shadows := true;
  SpotLight.Color := YellowRgb;
  SpotLight.Translation := Vector3(0, 1, 0);
  MainViewport.Items.Add(SpotLight);

  Plane := TCastlePlane.Create(FreeAtStop);
  Plane.Size := Vector2(30, 30);
  MainViewport.Items.Add(Plane);

  { Add TCastleRigidBody and TCastlePlaneCollider to Plane,
    to make it collide with physical boxes (dropped by ClickDropBox) OK. }
  PlaneBody := TCastleRigidBody.Create(FreeAtStop);
  Plane.AddBehavior(PlaneBody);
  PlaneCollider := TCastlePlaneCollider.Create(FreeAtStop);
  Plane.AddBehavior(PlaneCollider);

  Background := TCastleBackground.Create(FreeAtStop);
  MainViewport.Background := Background;

  Navigation := TCastleExamineNavigation.Create(FreeAtStop);
  MainViewport.InsertFront(Navigation);

  for I := 0 to 10 do
  begin
    Cone := TCastleCone.Create(FreeAtStop);
    // random position with Y = 0 (ground level)
    Cone.Translation := Vector3(
      RandomFloatRange(-10, 10),
      1,
      RandomFloatRange(-10, 10)
    );
    // random light color
    Cone.Color := Vector4(
      RandomFloatRange(0.5, 1),
      RandomFloatRange(0.5, 1),
      RandomFloatRange(0.5, 1),
      1
    );
    { Add TCastleRigidBody and TCastlePlaneCollider to Cone,
      to make it collide with physical boxes (dropped by ClickDropBox) OK. }
    ConeBody := TCastleRigidBody.Create(FreeAtStop);
    Cone.AddBehavior(ConeBody);
    ConeCollider := TCastleSphereCollider.Create(FreeAtStop);
    Cone.AddBehavior(ConeCollider);

    MainViewport.Items.Add(Cone);
  end;

  LabelFps := TCastleLabel.Create(FreeAtStop);
  { smaller font, because web area is smaller, also lack of "texture swizzle"
    makes it look worse on web now. }
  LabelFps.FontSize := 15;
  LabelFps.Anchor(hpRight, -5);
  LabelFps.Anchor(vpTop, -5);
  LabelFps.Color := Vector4(0.1, 0.5, 0.1, 1); // dark green
  InsertFront(LabelFps);

  LabelInfo := TCastleLabel.Create(FreeAtStop);
  { smaller font, because web area is smaller, also lack of "texture swizzle"
    makes it look worse on web now. }
  LabelInfo.FontSize := 15;
  LabelInfo.Anchor(hpLeft, 5);
  LabelInfo.Anchor(vpTop, -5);
  LabelInfo.Caption := 'Drag with mouse to:' + NL +
    '- Rotate: left button' + NL +
    '- Move: middle button (or left button and hold Shift)' + NL +
    '- Zoom: left button and hold Ctrl' + NL +
    '- See also: Castle Model Viewer docs about Examine mode.' + NL +
    '- Press C to print camera settings (to console).';
  LabelInfo.Color := Vector4(0.1, 0.1, 0.1, 1); // almost black
  //LabelInfo.Alignment := hpRight;
  InsertFront(LabelInfo);

  ButtonDropBox := TCastleButton.Create(FreeAtStop);
  { smaller font, because web area is smaller, also lack of "texture swizzle"
    makes it look worse on web now. }
  ButtonDropBox.FontSize := 15;
  ButtonDropBox.CustomBackground := true;
  ButtonDropBox.CustomColorNormal := HexToColor('CCCCCC');
  ButtonDropBox.CustomColorFocused := HexToColor('EEEEEE');
  ButtonDropBox.CustomColorPressed := HexToColor('A9A9A9');
  ButtonDropBox.Caption := 'Drop Box (With Physics)';
  ButtonDropBox.OnClick := {$ifdef FPC}@{$endif} ClickDropBox;
  ButtonDropBox.Anchor(hpLeft, 5);
  ButtonDropBox.Anchor(vpBottom, 5);
  InsertFront(ButtonDropBox);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  S, C: Single;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  LifeTime := LifeTime + SecondsPassed;
  SinCos(LifeTime, S, C);
  SpotLight.Direction := Vector3(S, -0.3 { a little downward }, C);
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;

  { Print Pascal code to set given camera position/direction/up,
    useful to copy-paste to have the desired initial camera. }
  procedure PrintCameraSettings;

    function Vector3ToPascal(const V: TVector3): String;
    begin
      Result := FormatDot('Vector3(%f, %f, %f)', [V[0], V[1], V[2]]);
    end;

  var
    Pos, Dir, Up: TVector3;
  begin
    MainViewport.Camera.GetWorldView(Pos, Dir, Up);
    WritelnLog(Format('// Set camera vectors using Castle Game Engine.' + NL +
      'MainViewport.Camera.SetWorldView(' + NL +
      '  %s, // position' + NL +
      '  %s, // direction' + NL +
      '  %s  // up' + NL +
      ');', [
        Vector3ToPascal(Pos),
        Vector3ToPascal(Dir),
        Vector3ToPascal(Up)
      ]));
  end;


begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyC) then
  begin
    PrintCameraSettings;
    Exit(true); // key was handled
  end;
end;

procedure TViewMain.ClickDropBox(Sender: TObject);
var
  Box: TCastleBox;
  Body: TCastleRigidBody;
  Collider: TCastleBoxCollider;
begin
  Box := TCastleBox.Create(FreeAtStop);
  Box.Translation := Vector3(
    RandomFloatRange(-10, 10),
    10,
    RandomFloatRange(-10, 10)
  );

  Body := TCastleRigidBody.Create(FreeAtStop);
  Box.AddBehavior(Body);

  Collider := TCastleBoxCollider.Create(FreeAtStop);
  Box.AddBehavior(Collider);

  MainViewport.Items.Add(Box);
end;

end.
