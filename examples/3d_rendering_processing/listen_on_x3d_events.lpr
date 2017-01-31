{
  Copyright 2016-2017 Michalis Kamburelis.

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
  and http://castle-engine.sourceforge.net/x3d_implementation_pointingdevicesensor.php
  for the details about the TouchSensor.

  Then it registers simple callbacks on the TouchSensor touchTime
  and isActive events. You register callbacks simply by adding them to
  event's OnReceive list.

  Logs TouchSensor clicks on the console. }

{$apptype CONSOLE}

uses Classes,SysUtils,
  X3DNodes, X3DFields, X3DTIme, CastleWindow, CastleSceneCore, CastleScene;

type
  TMyEventListener = class(TComponent)
    procedure ReceivedTouchTime(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure ReceivedIsActive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  end;

procedure TMyEventListener.ReceivedTouchTime(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Val: Double;
begin
  Val := (Value as TSFTime).Value;
  Writeln('Received TouchSensor.touchTime event with value ', Val:1:2);
end;

procedure TMyEventListener.ReceivedIsActive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Val: boolean;
begin
  Val := (Value as TSFBool).Value;
  Writeln('Received TouchSensor.isActive event with value ', BoolToStr(Val, true));
end;

var
  Window: TCastleWindow;
  Scene: TCastleScene;
  EventListener: TMyEventListener;

  { X3D nodes used to build the scene: }
  Root: TX3DRootNode;
  Cone: TConeNode;
  Shape: TShapeNode;
  TouchSensor: TTouchSensorNode;
begin
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
  TouchSensor.EventTouchTime.OnReceive.Add(@EventListener.ReceivedTouchTime);
  TouchSensor.EventIsActive.OnReceive.Add(@EventListener.ReceivedIsActive);

  Root := TX3DRootNode.Create;
  Root.FdChildren.Add(Shape);
  Root.FdChildren.Add(TouchSensor);

  Scene := TCastleScene.Create(Application);
  Scene.Load(Root, true);
  Scene.ProcessEvents := true;
  { Remember to initialize Scene.Spatial,
    otherwise we cannot detect clicks on the touch sensor,
    as we have no necessary collision detection structures. }
  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  Window := TCastleWindow.Create(Application);
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Window.Open;
  Application.Run;
end.
