{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Example how to build a scene with a TouchSensor, and listen on TouchSensor
  "click" events.

  This builds a scene with a simple Cone and a TouchSensor, using X3D nodes.
  TouchSensor is a node defined by X3D that can "sense" when user clicks
  on a shape.
  See http://www.web3d.org/documents/specifications/19775-1/V3.2/Part01/components/pointingsensor.html#TouchSensor
  and https://castle-engine.io/x3d_implementation_pointingdevicesensor.php
  for the details about the TouchSensor.

  Then it registers simple callbacks on the TouchSensor touchTime
  and isActive events. You register callbacks simply by event's AddNotification.

  Logs TouchSensor clicks on the console. }

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses Classes,SysUtils,
  X3DNodes, X3DFields, X3DTIme, CastleWindow, CastleSceneCore, CastleScene,
  CastleViewport, CastleNotifications, CastleUIControls, CastleColors;

var
  Notifications: TCastleNotifications;

type
  TMyEventListener = class(TComponent)
    procedure ReceivedTouchTime(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure ReceivedIsActive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  end;

procedure TMyEventListener.ReceivedTouchTime(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Val: Double;
  Scene: TCastleScene;
begin
  Val := (Value as TSFTime).Value;

  { In case you need it, you can get the Scene reference from Event.
    Typecasts below are unfortunately necessary for now.
    But you can be sure they are correct, if this is part of some TCastleScene with events working. }
  Scene := (Event.ParentNode as TX3DNode).Scene as TCastleScene;

  Notifications.Show(Format('Received TouchSensor.touchTime event: time %f, scene: %s', [Val, Scene.Name]));
end;

procedure TMyEventListener.ReceivedIsActive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Val: Boolean;
begin
  Val := (Value as TSFBool).Value;
  Notifications.Show(Format('Received TouchSensor.isActive event: %s', [BoolToStr(Val, true)]));
end;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
  EventListener: TMyEventListener;

  { X3D nodes used to build the scene: }
  Root: TX3DRootNode;
  Cone: TConeNode;
  Shape: TShapeNode;
  TouchSensor: TTouchSensorNode;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  Notifications  := TCastleNotifications.Create(Application);
  Notifications.Anchor(hpMiddle);
  Notifications.Anchor(vpBottom);
  Notifications.TextAlignment := hpMiddle; // looks best, when anchor is also in the middle
  Notifications.MaxMessages := 20;
  Notifications.Timeout := 20000;
  Notifications.Color := Yellow;
  Window.Controls.InsertFront(Notifications);

  EventListener := TMyEventListener.Create(Application);

  Cone := TConeNode.Create;

  Shape := TShapeNode.Create;
  { line below is a shortcut to create Shape.Appearance,
    and assign Shape.Appearance.Material. Adding a default material
    makes the shape appear lit. }
  Shape.Material := TMaterialNode.Create;
  Shape.Geometry := Cone;

  TouchSensor := TTouchSensorNode.Create('MyTouchSensor');
  TouchSensor.Enabled := true;
  TouchSensor.EventTouchTime.AddNotification(@EventListener.ReceivedTouchTime);
  TouchSensor.EventIsActive.AddNotification(@EventListener.ReceivedIsActive);

  Root := TX3DRootNode.Create;
  Root.AddChildren(Shape);
  Root.AddChildren(TouchSensor);

  Scene := TCastleScene.Create(Application);
  Scene.Name := 'MyScene';
  Scene.Load(Root, true);
  Scene.ProcessEvents := true;
  { Remember to initialize Scene.Spatial,
    otherwise we cannot detect clicks on the touch sensor,
    as we have no necessary collision detection structures. }
  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Application.Run;
end.
