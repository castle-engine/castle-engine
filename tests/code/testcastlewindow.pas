// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestWindow" -*-
{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleWindow. }
unit TestCastleWindow;

interface

uses {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry, CastleTestCase
     {$else}CastleTester{$endif};

type
  TTestWindow = class(TCastleTestCase)
  published
    procedure Test1;
    procedure TestNotifications;
    procedure TestMenu;
    { Test TCastleUserInterface.AutoSizeToChildren.
      In this unit, as it requires TCastleWindow (UI container) to make sense. }
    procedure TestAutoSizeToChildren;
    { Test TCastleContainer.Focus.
      In this unit, as it requires TCastleWindow (UI container) to make sense. }
    procedure TestFocus;
    procedure TestEventLoop;
    procedure TestViewportPositionTo;
    procedure TestStateAutoStop;
    procedure TestStateSize;
    procedure TestStateSize2;
  end;

implementation

uses SysUtils, Classes, Math,
  CastleWindow, CastleControls, CastleStringUtils, CastleKeysMouse,
  CastleUIControls, CastleRectangles, CastleOnScreenMenu, CastleComponentSerialize,
  CastleCameras, {$ifdef FPC}CastleSceneManager,{$endif} CastleVectors,
  CastleTransform, CastleScene, CastleApplicationProperties, CastleUIState,
  CastleViewport;

procedure TTestWindow.Test1;
var
  Window: TCastleWindowBase;
begin
  Window := TCastleWindowBase.Create(nil);
  try
    Window.Open;
    Window.Close;
  finally
    FreeAndNil(Window)
  end;
end;

procedure TTestWindow.TestNotifications;
var
  Window: TCastleWindowBase;
  C: TCastleButton;
begin
  Window := TCastleWindowBase.Create(nil);
  try
    C := TCastleButton.Create(Window);
    FreeAndNil(C);
  finally FreeAndNil(Window) end;
end;

procedure TTestWindow.TestMenu;
var
  M: TMenuItem;
begin
  M := TMenuItem.Create('blah', 123, 'a');
  AssertTrue(M.KeyMatches(keyA, 'a', []));
  { Below may be improved in the future, for now our KeyMatches is probably too forgiving.
    Below combination is not even usually possible, with mkCtrl you would get CtrlA
    character usually. }
  AssertTrue(M.KeyMatches(keyA, 'a', [mkCtrl]));
  FreeAndNil(M);

  M := TMenuItem.Create('blah', 123, keyF11);
  AssertTrue(M.KeyMatches(keyF11, #0, []));
  AssertTrue(M.KeyMatches(keyF11, '', [])); // string '' and #0 should be treated equal, for backward compat
  { below may be improved in the future, for now our KeyMatches is probably too forgiving }
  AssertTrue(M.KeyMatches(keyF11, #0, [mkCtrl]));
  AssertTrue(M.KeyMatches(keyF11, '', [mkCtrl])); // string '' and #0 should be treated equal, for backward compat
  FreeAndNil(M);

  M := TMenuItem.Create('blah', 123, keyF11);
  M.Modifiers := [mkCtrl];
  AssertTrue(not M.KeyMatches(keyF11, #0, []));
  AssertTrue(M.KeyMatches(keyF11, #0, [mkCtrl]));
  FreeAndNil(M);
end;

procedure TTestWindow.TestAutoSizeToChildren;
var
  Window: TCastleWindowBase;
  Parent, Child1, Child2: TCastleUserInterface;
begin
  Window := nil;
  Parent := nil;
  Child1 := nil;
  Child2 := nil;
  try
    Window := TCastleWindowBase.Create(nil);
    Window.Width := 500;
    Window.Height := 500;
    Window.ResizeAllowed := raNotAllowed;
    Window.Open;

    Parent := TCastleUserInterface.Create(nil);
    Window.Controls.InsertFront(Parent);

    Child1 := TCastleUserInterface.Create(nil);
    Child2 := TCastleUserInterface.Create(nil);

    AssertRectsEqual(FloatRectangle(0, 0, 100, 100), Parent.RenderRect);

    Parent.Anchor(hpLeft, 50);
    Parent.Anchor(vpTop, -60);
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 100, 100, 100), Parent.RenderRect);

    Parent.AutoSizeToChildren := true;
    AssertRectsEqual(TFloatRectangle.Empty, Parent.RenderRect);

    Parent.InsertFront(Child1);
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 100, 100, 100), Parent.RenderRect);

    Child1.Width := 40;
    Child1.Height := 30;
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 30, 40, 30), Parent.RenderRect);

    Child2.Width := 200;
    Child2.Height := 100;
    Child2.Anchor(vpTop, -10);
    Parent.InsertFront(Child2);
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 110, 200, 110), Parent.RenderRect);

    Child1.Width := 10;
    Child1.Height := 10;
    Child1.Anchor(hpLeft, 200);
    Child2.Width := 10;
    Child2.Height := 10;
    Child2.Anchor(vpBottom, 400);
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 410, 210, 410), Parent.RenderRect);

    Child1.Width := 80;
    Child1.Height := 30;
    Child1.Anchor(hpLeft, 30);
    Child1.Anchor(vpBottom, 40);
    Child2.Width := 80;
    Child2.Height := 30;
    Child2.Anchor(hpLeft, 40);
    Child2.Anchor(vpBottom, 60);
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 90, 120, 90), Parent.RenderRect);
  finally
    FreeAndNil(Window);
    FreeAndNil(Parent);
    FreeAndNil(Child1);
    FreeAndNil(Child2);
  end;
end;

procedure TTestWindow.TestFocus;
var
  Window: TCastleWindowBase;
  ManualButton, Button2: TCastleButton;
  OnScreenMenu1: TCastleOnScreenMenu;
  SceneManager1: TCastleSceneManager;

  { Based on examples/lazarus/model_3d_with_2d_controls/ }

  procedure AddUserInterfaceDesigned;
  var
    UiRoot: TCastleUserInterface;
    UiOwner: TComponent;
  begin
    UiOwner := TComponent.Create(Window);
    UiRoot := UserInterfaceLoad('castle-data:/focus_test/main.castle-user-interface', UiOwner);
    Window.Controls.InsertFront(UiRoot);
    Button2 := UiOwner.FindRequiredComponent('Button2') as TCastleButton;
    SceneManager1 := UiOwner.FindRequiredComponent('SceneManager1') as TCastleSceneManager;
    SceneManager1.AssignDefaultNavigation; // adds TCastleWalkNavigation
  end;

  procedure AddUserInterfaceFromCode;
  begin
    ManualButton := TCastleButton.Create(Window);
    ManualButton.Caption := 'Button added from code';
    ManualButton.Anchor(hpRight, -10);
    ManualButton.Anchor(vpBottom, 120);
    Window.Controls.InsertFront(ManualButton);

    OnScreenMenu1 := TCastleOnScreenMenu.Create(Window);
    OnScreenMenu1.Bottom := 140;
    OnScreenMenu1.Left := 400;
    OnScreenMenu1.Add('one');
    OnScreenMenu1.Add('two');
    OnScreenMenu1.Add('three');
    Window.Controls.InsertFront(OnScreenMenu1);
  end;

  procedure MoveMouse(const Pos: TVector2);
  // var
  //   C: TCastleUserInterface;
  begin
    Window.InternalFakeMotion(InputMotion(Window.MousePosition, Pos, [], 0));
    Window.Container.UpdateFocusAndMouseCursor;
    (* Useful to test current Focus value:
    Writeln('Focus now ', Window.Container.Focus.Count);
    for C in Window.Container.Focus do
      Writeln('  ', C.Name, ':', C.ClassName);
    *)
  end;

begin
  Window := TCastleWindowBase.Create(nil);
  try
    Window.Width := 800;
    Window.Height := 800;
    Window.ResizeAllowed := raNotAllowed;
    Window.Open;

    AddUserInterfaceDesigned;
    AddUserInterfaceFromCode;

    MoveMouse(FloatRectangle(Window.Rect).Middle);
    AssertEquals(3, Window.Container.Focus.Count);
    AssertTrue(Window.Container.Focus[0].Name = 'Group1');
    AssertTrue(Window.Container.Focus[1].Name = 'SceneManager1');
    AssertTrue(Window.Container.Focus[2] is TCastleWalkNavigation); // internal in SceneManager1

    MoveMouse(ManualButton.RenderRect.Middle);
    AssertEquals(4, Window.Container.Focus.Count);
    AssertTrue(Window.Container.Focus[0].Name = 'Group1');
    AssertTrue(Window.Container.Focus[1].Name = 'SceneManager1');
    AssertTrue(Window.Container.Focus[2] is TCastleWalkNavigation); // internal in SceneManager1
    AssertTrue(Window.Container.Focus[3] = ManualButton);

    MoveMouse(Button2.RenderRect.Middle);
    AssertEquals(4, Window.Container.Focus.Count);
    AssertTrue(Window.Container.Focus[0].Name = 'Group1');
    AssertTrue(Window.Container.Focus[1].Name = 'SceneManager1');
    AssertTrue(Window.Container.Focus[2] is TCastleWalkNavigation); // internal in SceneManager1
    AssertTrue(Window.Container.Focus[3] = Button2);
  finally FreeAndNil(Window) end;
end;

procedure TTestWindow.TestEventLoop;
var
  Window: TCastleWindowBase;

  procedure SimulateEventLoop(const T: TCastleTransform);
  var
    RenderParams: TRenderParams;
    RemoveMe: TRemoveType;
  begin
    RenderParams := TBasicRenderParams.Create;
    try
      T.Render(RenderParams);
    finally FreeAndNil(RenderParams) end;

    RemoveMe := rtNone;
    T.Update(1/60, RemoveMe);

    //Window.MessageOK('Press OK to finish this event loop run', mtInfo);
  end;

var
  Box: TCastleBox;
  Viewport: TCastleViewport;
begin
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif}OnWarningRaiseException);
  try
    Window := TCastleWindowBase.Create(nil);
    try
      // for rendering, OpenGL context must be ready, with GLFeatures initialized
      Window.Visible := false;
      Window.Open;

      Viewport := TCastleViewport.Create(nil);
      try
        Viewport.FullSize := true;
        Viewport.AutoCamera := true;
        Window.Controls.InsertFront(Viewport);

        Box := TCastleBox.Create(nil);
        try
          Viewport.Items.Add(Box);

          SimulateEventLoop(Box);
          Box.Material := pmUnlit;
          SimulateEventLoop(Box);
          Box.Material := pmPhysical;
          SimulateEventLoop(Box);
        finally FreeAndNil(Box) end;
      finally FreeAndNil(Viewport) end;
    finally FreeAndNil(Window) end;
  finally
    ApplicationProperties.OnWarning.Remove({$ifdef FPC}@{$endif}OnWarningRaiseException);
  end;
end;

procedure TTestWindow.TestViewportPositionTo;
var
  Viewport: TCastleViewport;

{ Non-interactive version of a testcase on
  https://gist.github.com/michaliskambi/2b8faee73df9f3a12351736cabcad1ee
  for https://github.com/castle-engine/castle-engine/issues/295 :
  Viewport.PositionToXxx should return correct values even when used before
  and resize/render event. }

  procedure TestQueryPosition(const ScreenPos: TVector2;
    const CorrectRayOrigin, CorrectRayDirection, CorrectCameraPlaneResult, CorrectWorldPlaneResult: TVector3;
    const CorrectPos2D: TVector2);
  var
    CameraPlaneResult, RayOrigin, RayDirection, WorldPlaneResult: TVector3;
    Pos2D: TVector2;
  begin
    //WritelnLog('Testing on ', ScreenPos.ToString);

    Viewport.PositionToRay(ScreenPos, true, RayOrigin, RayDirection);
    AssertVectorEquals(CorrectRayOrigin, RayOrigin, 0.1);
    AssertVectorEquals(CorrectRayDirection, RayDirection, 0.1);

    AssertTrue(Viewport.PositionToCameraPlane(ScreenPos, true, 2, CameraPlaneResult));
    AssertVectorEquals(CorrectCameraPlaneResult, CameraPlaneResult, 0.1);

    AssertTrue(Viewport.PositionToWorldPlane(ScreenPos, true, -10, WorldPlaneResult));
    AssertVectorEquals(CorrectWorldPlaneResult, WorldPlaneResult, 0.1);

    Pos2D := Viewport.PositionTo2DWorld(ScreenPos, true);
    AssertVectorEquals(CorrectPos2D, Pos2D, 0.1);
  end;

var
  Window: TCastleWindowBase;
begin
  Window := TCastleWindowBase.Create(nil);
  try
    Window.Width := 300;
    Window.Height := 300;
    Window.Open;

    Viewport := TCastleViewport.Create(Window);
    Viewport.FullSize := true;

    // too early to use
    // TestQueryPosition(Vector2(100, 100));
    // TestQueryPosition(Vector2(Window.Width / 2, Window.Height / 2));

    Window.Controls.InsertFront(Viewport);

    TestQueryPosition(Vector2(100, 100),
      Vector3(0.00, 0.00, 0.00),
      Vector3(-0.13, -0.13, -0.98),
      Vector3(-0.27, -0.27, -2.00),
      Vector3(-1.37, -1.37, -10.00),
      Vector2(100.00, 100.00)
    );
    TestQueryPosition(Vector2(Window.Width / 2, Window.Height / 2),
      Vector3(0.00, 0.00, 0.00),
      Vector3(0.00, 0.00, -1.00),
      Vector3(0.00, 0.00, -2.00),
      Vector3(0.01, 0.01, -10.00),
      Vector2(150.00, 150.00)
    );
  finally FreeAndNil(Window) end;
end;

procedure TTestWindow.TestStateAutoStop;

{ Test something similar to https://github.com/castle-engine/castle-engine/issues/307 .

  However, this was always working, it does *not* reproduce what
  https://github.com/castle-engine/castle-engine/issues/307 did.
  There is TCastleControlBase that does

    procedure TCastleForm.WindowOpen(Sender: TObject);
    begin
      TCastleControlBase.MainControl := Window;
      CastleApp := TCastleApp.Create(Window);
      TUIState.Current := CastleApp;
      Window.Container.UIScaling := usNone;
    end;
}

var
  Window: TCastleWindowBase;
  SomeState: TUIState;
begin
  {$ifdef CASTLE_TESTER}
  if not IsConsoleMode then
    Fail('Curretnly we can test TUIState only in console mode.');
  {$endif}

  {$ifndef CASTLE_TESTER}
  Window := TCastleWindowBase.Create(nil);
  {$else}
  Window := CreateWindowForTest;
  {$endif}
  try
    {$ifndef CASTLE_TESTER}
    Application.MainWindow := Window;
    {$endif}

    Window.Open;
    SomeState := TUIState.Create(Window);
    TUIState.Current := SomeState;
  finally
    { let freeing Window cause everything else:
      - freeing of SomeState
      - stopping of SomeState
      - closing of Window
    }
    {$ifndef CASTLE_TESTER}
    FreeAndNil(Window);
    {$else}
    DestroyWindowForTest;
    {$endif}
  end;

  {$ifndef CASTLE_TESTER}
  Application.MainWindow := nil;
  {$endif}
end;

type
  TStateTestingSize = class(TUIState)
  public
    W, H: Single;
    TestCase: TCastleTestCase;
    procedure Start; override;
    procedure Resize; override;
  end;

procedure TStateTestingSize.Start;
begin
  inherited;
  W := EffectiveWidth;
  H := EffectiveHeight;
  TestCase.AssertTrue(
    SameValue(EffectiveWidth, 160) or
    SameValue(EffectiveHeight, 90));
  TestCase.AssertTrue(
    SameValue(EffectiveRect.Width, 160) or
    SameValue(EffectiveRect.Height, 90));
end;

procedure TStateTestingSize.Resize;
begin
  inherited;
  TestCase.AssertTrue(
    SameValue(EffectiveWidth, 160) or
    SameValue(EffectiveHeight, 90));
  TestCase.AssertTrue(
    SameValue(EffectiveRect.Width, 160) or
    SameValue(EffectiveRect.Height, 90));
  // size didn't change from Start to 1st Resize call
  TestCase.AssertSameValue(W, EffectiveWidth);
  TestCase.AssertSameValue(H, EffectiveHeight);
end;

procedure TTestWindow.TestStateSize;
var
  Window: TCastleWindowBase;
  StateTesting: TStateTestingSize;
begin
  {$ifdef CASTLE_TESTER}
  if not IsConsoleMode then
    Fail('Curretnly we can test TUIState only in console mode.');
  {$endif}

  {$ifndef CASTLE_TESTER}
  Window := TCastleWindowBase.Create(nil);
  {$else}
  Window := CreateWindowForTest;
  {$endif}
  try
    {$ifndef CASTLE_TESTER}
    Application.MainWindow := Window;
    {$endif}

    Window.Open;
    Window.Container.UIScaling := usEncloseReferenceSize;
    Window.Container.UIReferenceWidth := 160;
    Window.Container.UIReferenceHeight := 90;

    StateTesting := TStateTestingSize.Create(Window);
    StateTesting.TestCase := Self;

    // already state EffectiveXxx for size should work
    AssertTrue(
      SameValue(StateTesting.EffectiveWidth, 160) or
      SameValue(StateTesting.EffectiveHeight, 90));
    AssertTrue(
      SameValue(StateTesting.EffectiveRect.Width, 160) or
      SameValue(StateTesting.EffectiveRect.Height, 90));

    TUIState.Current := StateTesting;
  finally
    {$ifndef CASTLE_TESTER}
    FreeAndNil(Window);
    {$else}
    DestroyWindowForTest;
    {$endif}
  end;
  {$ifndef CASTLE_TESTER}
  Application.MainWindow := nil;
  {$endif}
end;

type
  TStateTestingSize2 = class(TUIState)
  public
    W, H: Single;
    TestCase: TCastleTestCase;
    procedure Start; override;
    procedure Resize; override;
  end;

procedure TStateTestingSize2.Start;
begin
  inherited;
  W := EffectiveWidth;
  H := EffectiveHeight;
  TestCase.AssertTrue(
    SameValue(EffectiveWidth, 200) or
    SameValue(EffectiveHeight, 400));
  TestCase.AssertTrue(
    SameValue(EffectiveRect.Width, 200) or
    SameValue(EffectiveRect.Height, 400));
end;

procedure TStateTestingSize2.Resize;
begin
  inherited;
  TestCase.AssertTrue(
    SameValue(EffectiveWidth, 200) or
    SameValue(EffectiveHeight, 400));
  TestCase.AssertTrue(
    SameValue(EffectiveRect.Width, 200) or
    SameValue(EffectiveRect.Height, 400));
  // size didn't change from Start to 1st Resize call
  TestCase.AssertSameValue(W, EffectiveWidth);
  TestCase.AssertSameValue(H, EffectiveHeight);
end;

procedure TTestWindow.TestStateSize2;
var
  Window: TCastleWindowBase;
  StateTesting: TStateTestingSize2;
begin
  {$ifdef CASTLE_TESTER}
  if not IsConsoleMode then
    Fail('Curretnly we can test TUIState only in console mode.');
  {$endif}

  {$ifndef CASTLE_TESTER}
  Window := TCastleWindowBase.Create(nil);
  {$else}
  Window := CreateWindowForTest;
  {$endif}
  try
    {$ifndef CASTLE_TESTER}
    Application.MainWindow := Window;
    {$endif}

    Window.Width := 200;
    Window.Height := 400;
    Window.Open;
    { No UI scaling this time. }

    StateTesting := TStateTestingSize2.Create(Window);
    StateTesting.TestCase := Self;

    // already state EffectiveXxx for size should work
    AssertTrue(
      SameValue(StateTesting.EffectiveWidth, 200) or
      SameValue(StateTesting.EffectiveHeight, 400));
    AssertTrue(
      SameValue(StateTesting.EffectiveRect.Width, 200) or
      SameValue(StateTesting.EffectiveRect.Height, 400));

    TUIState.Current := StateTesting;
  finally
    {$ifndef CASTLE_TESTER}
    FreeAndNil(Window);
    {$else}
    DestroyWindowForTest;
    {$endif}
  end;

  {$ifndef CASTLE_TESTER}
  Application.MainWindow := nil;
  {$endif}
end;

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestWindow);
{$endif}
end.
