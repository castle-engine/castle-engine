{
  Copyright 2008-2013 Michalis Kamburelis, Jan Adamec.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Class with the complete functionality to draw and navigate in 3D and 2D using
  of "Castle Game Engine". Requires pre-initialized OpenGL context.}
unit CastleFrame;

{$I castleconf.inc}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes,
  CastleVectors, CastleRectangles, CastleKeysMouse, CastleUtils, CastleTimeUtils,
  CastleUIControls, CastleCameras, X3DNodes, CastleScene, CastleLevels,
  CastleImages, CastleGLVersion, CastleSceneManager, CastleControls, pk3DConnexion;

const
  DefaultLimitFPS = 100.0;

  // library callback codes
  ecgelibNeedsDisplay          = 0;
  ecgelibSetMouseCursor        = 1;  // sends mouse cursor code in iParam1
  ecgelibNavigationTypeChanged = 2;  // sends ECgeNavigationType in iParam1 (see castleengine.h)

  // mouse cursor codes
  ecgecursorDefault   = 0;
  ecgecursorWait      = 1;
  ecgecursorHand      = 2;
  ecgecursorText      = 3;
  ecgecursorNone      = 4;

type
  TCgeLibraryCallbackProc = function (eCode, iParam1, iParam2: cInt32):cInt32; cdecl;

  TUserInterface = (euiDesktop, euiTouch);
  TTouchCtlInterface = (etciNone, etciCtlWalkCtlRotate, etciCtlWalkDragRotate,
                        etciCtlFlyCtlWalkDragRotate, etciCtlPanXYDragRotate);

  TCastleFrame = class(TComponent, IUIContainer)
  private
    FSceneManager: TGameSceneManager;
    FControls: TUIControlList;
    FUseControls: boolean;
    FOnDrawStyle: TUIControlDrawStyle;
    FFocus: TUIControl;
    FCaptureInput: TUIControl;

    FWidth, FHeight: Integer;
    FDpi: Integer;
    FUserInterface: TUserInterface;
    FMouseX: Integer;
    FMouseY: Integer;
    FPressed: TKeysPressed;
    FMousePressed: TMouseButtons;
    Mouse3d: T3DConnexionDevice;
    Mouse3dPollTimer: Single;

    FFps: TFramesPerSecond;
    FGLInitialized: boolean;

    { manually track when we need to be repainted, useful for AggressiveUpdate }
    Invalidated: boolean;
    FLibraryCallbackProc: TCgeLibraryCallbackProc;

    procedure UpdateShiftState(const Shift: TShiftState);
    procedure ControlsVisibleChange(Sender: TObject);
    procedure SetUseControls(const Value: boolean);
    procedure UpdateFocusAndMouseCursor;
    function GetTooltipX: Integer;
    function GetTooltipY: Integer;
    { Called when the control C is destroyed or just removed from Controls list. }
    procedure DetachNotification(const C: TUIControl);
  protected
    procedure DoBeforeDraw;
    procedure DoDraw;
    procedure Resize;
    procedure DoGLContextOpen;
    procedure DoGLContextClose;

    function KeyDownEvent(const MyKey: TKey; const Ch: char): boolean;
    function KeyUpEvent(const MyKey: TKey; const Ch: char): boolean;
    procedure MouseDownEvent(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure MouseUpEvent(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure MouseMoveEvent(Shift: TShiftState; NewX, NewY: Integer);
    function MouseWheelEvent(const Scroll: Single; const Vertical: boolean): boolean;
    procedure UpdateEvent;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { Enable @link(Controls) list processing.

      @italic(Messing with this is very dangerous), that's why it's
      visibility is only protected (although could be even pubilshed, technically).
      This makes all controls miss all their events, including some critical
      notification events like TUIControl.GLContextOpen, TUIControl.GLContextClose,
      TUIControl.ContainerResize.

      You can reliably only turn this off temporarily, when you know that
      no events (or at least no meaningful events, like resize or control
      add/remove) will reach the window during this time. }
    property UseControls: boolean
      read FUseControls write SetUseControls default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint;
    procedure Invalidate;
    procedure SetRenderSize(const NewWidth, NewHeight: Integer);
    procedure SetLibraryCallbackProc(aProc: TCgeLibraryCallbackProc);
    procedure Update;
    procedure GLContextOpen;
    procedure GLContextClose;

    { IUIContainer required functions }
    procedure SetMousePosition(const NewMouseX, NewMouseY: Integer);
    function GetMouseX: Integer;
    function GetMouseY: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function Rect: TRectangle;
    function GetMousePressed: TMouseButtons;
    function GetPressed: TKeysPressed;

    property MouseX: Integer read GetMouseX;
    property MouseY: Integer read GetMouseY;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Fps: TFramesPerSecond read FFps;
    property GLInitialized: boolean read FGLInitialized;

    function Mouse3dLoaded: boolean;

    function OnKeyDown(const MyKey: TKey; Ch: char): boolean;
    function OnKeyUp(const MyKey: TKey; Ch: char): boolean;
    procedure OnMouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure OnMouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure OnMouseMove(Shift: TShiftState; NewX, NewY: Integer);
    function OnMouseWheel(const Scroll: Single; const Vertical: boolean): boolean;

    { Returns the control that should receive input events first,
      or @nil if none. More precisely, this is the first on Controls
      list that is enabled and under the mouse cursor.
      @nil is returned when there's no enabled control under the mouse cursor,
      or when UseControls = @false. }
    property Focus: TUIControl read FFocus;

  published
    { How OnDraw callback fits within various Draw methods of our
      @link(Controls).
      See TCastleWindowCustom.OnDrawStyle for full description. }
    property OnDrawStyle: TUIControlDrawStyle
      read FOnDrawStyle write FOnDrawStyle default dsNone;

    { Controls listening for user input (keyboard / mouse) to this window.

      Usually you explicitly add / delete controls to this list.
      Also, freeing the control that is on this list
      automatically removes it from this list (using the TComponent.Notification
      mechanism). }
    property Controls: TUIControlList read FControls;

  private
    function GetShadowVolumes: boolean;
    function GetShadowVolumesDraw: boolean;
    function GetOnCameraChanged: TNotifyEvent;
    procedure SetShadowVolumes(const Value: boolean);
    procedure SetShadowVolumesDraw(const Value: boolean);
    procedure SetOnCameraChanged(const Value: TNotifyEvent);
  public

    procedure Load(const SceneURL: string);
    procedure Load(ARootNode: TX3DRootNode; const OwnsRootNode: boolean);

    function MainScene: TCastleScene;
    function Camera: TCamera;
  published
    property SceneManager: TGameSceneManager read FSceneManager;

    property OnCameraChanged: TNotifyEvent
      read GetOnCameraChanged write SetOnCameraChanged;

    { See TCastleAbstractViewport.ShadowVolumes. }
    property ShadowVolumes: boolean
      read GetShadowVolumes write SetShadowVolumes
      default TCastleAbstractViewport.DefaultShadowVolumes;

    { See TCastleAbstractViewport.ShadowVolumesDraw. }
    property ShadowVolumesDraw: boolean
      read GetShadowVolumesDraw write SetShadowVolumesDraw default false;

  { Navigation info helper functions }
  public
    function GetCurrentNavigationType(): TCameraNavigationType;
    procedure SetNavigationType(NewType: TCameraNavigationType);
  private
    procedure NavigationInfoChanged(Sender: TObject);

  { Touch interface methods }
  public
    LeftTouchCtl, RightTouchCtl: TCastleTouchControl;
    procedure UpdateTouchController(LeftSide, CtlVisible: boolean; Mode: TCastleTouchCtlMode = ctcmWalking);

    { This function should be called every time the navigation type changes. }
    procedure UpdateTouchInterface(Mode: TTouchCtlInterface);

    { Sets touch controls depending on the current navigation mode. Should
      be called each time after navigation mode changed. }
    procedure UpdateUserInterface();

  published
    property UserInterface: TUserInterface
      read FUserInterface write FUserInterface default euiDesktop;
    property Dpi: integer
      read FDpi write FDpi default 96;
  end;


var
  LimitFPS: Single = DefaultLimitFPS;

implementation

uses CastleGL, CastleGLUtils, CastleStringUtils, X3DLoad,
  CastleGLImages, CastleSceneCore;

{ TControlledUIControlList ----------------------------------------------------- }

type
  { TUIControlList descendant that takes care to react to list add/remove
    notifications, doing appropriate operations with parent Container. }
  TControlledUIControlList = class(TUIControlList)
  private
    Container: TCastleFrame;
  public
    constructor Create(const FreeObjects: boolean; const AContainer: TCastleFrame);
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

constructor TControlledUIControlList.Create(const FreeObjects: boolean;
  const AContainer: TCastleFrame);
begin
  inherited Create(FreeObjects);
  Container := AContainer;
end;

procedure TControlledUIControlList.Notify(Ptr: Pointer; Action: TListNotification);
var
  C: TUIControl absolute Ptr;
begin
  C := TUIControl(Ptr);
  case Action of
    lnAdded:
      begin
        { Make sure Container.ControlsVisibleChange will be called
          when a control calls OnVisibleChange. }
        if C.OnVisibleChange = nil then
          C.OnVisibleChange := @Container.ControlsVisibleChange;

        { Register Container to be notified of control destruction. }
        C.FreeNotification(Container);

        C.Container := Container;

        { Call initial ContainerResize for control.
          If Container OpenGL context is not yet initialized, defer it to
          the Init time, then our initial EventResize will be called
          that will do ContainerResize on every control. }
        if Container.GLInitialized then
        begin
          C.GLContextOpen;
          C.ContainerResize(Container.Width, Container.Height);
        end;
      end;
    lnExtracted, lnDeleted:
      begin
        if Container.GLInitialized then
          C.GLContextClose;

        if C.OnVisibleChange = @Container.ControlsVisibleChange then
          C.OnVisibleChange := nil;

        C.RemoveFreeNotification(Container);
        Container.DetachNotification(C);

        C.Container := nil;
      end;
    else raise EInternalError.Create('TControlledUIControlList.Notify action?');
  end;

  if Container.FControls <> nil then
    Container.UpdateFocusAndMouseCursor;
end;

{ Limit FPS ------------------------------------------------------------------ }

var
  LastLimitFPSTime: TTimerResult;

procedure DoLimitFPS;
var
  NowTime: TTimerResult;
  TimeRemainingFloat: Single;
begin
  if LimitFPS > 0 then
  begin
    NowTime := Timer;

    { When this is run for the 1st time, LastLimitFPSTime is zero,
      so NowTime - LastLimitFPSTime is huge, so we will not do any Sleep
      and only update LastLimitFPSTime.

      For the same reason, it is not a problem if you do not call DoLimitFPS
      often enough (for example, you do a couple of ProcessMessage calls
      without DoLimitFPS for some reason), or when user temporarily sets
      LimitFPS to zero and then back to 100.0.
      In every case, NowTime - LastLimitFPSTime will be large, and no sleep
      will happen. IOW, in the worst case --- we will not limit FPS,
      but we will *never* slow down the program when it's not really necessary. }

    TimeRemainingFloat :=
      { how long I should wait between _LimitFPS calls }
      1 / LimitFPS -
      { how long I actually waited between _LimitFPS calls }
      (NowTime - LastLimitFPSTime) / TimerFrequency;
    { Don't do Sleep with too small values.
      It's better to have larger FPS values than limit,
      than to have them too small. }
    if TimeRemainingFloat > 0.001 then
    begin
      Sleep(Round(1000 * TimeRemainingFloat));
      LastLimitFPSTime := Timer;
    end else
      LastLimitFPSTime := NowTime;
  end;
end;

{ Castle Frame --------------------------------------------------------------- }

constructor TCastleFrame.Create(AOwner: TComponent);
begin
  inherited;
  FFps := TFramesPerSecond.Create;
  FPressed := TKeysPressed.Create;

  FControls := TControlledUIControlList.Create(false, Self);
  FUseControls := true;
  FOnDrawStyle := dsNone;

  Invalidated := false;
  FLibraryCallbackProc := nil;
  LeftTouchCtl := nil;
  RightTouchCtl := nil;
  FDpi := 96;
  FUserInterface := euiDesktop;

  FSceneManager := TGameSceneManager.Create(Self);
  { SetSubComponent and Name setting (must be unique only within TCastleControl,
    so no troubles) are necessary to store it in LFM and display in object inspector
    nicely. }
  FSceneManager.SetSubComponent(true);
  FSceneManager.Name := 'SceneManager';
  FSceneManager.OnBoundNavigationInfoChanged := @NavigationInfoChanged;
  Controls.Add(SceneManager);

  { connect 3D device - 3Dconnexion device }
  Mouse3dPollTimer := 0;
  try
    Mouse3d := T3DConnexionDevice.Create('Castle Control');
  except
  end;

  if not GLInitialized then
  begin
    FGLInitialized := true; {JA TODO: ???}
  end;
end;

destructor TCastleFrame.Destroy;
begin
  FreeAndNil(FControls);
  FreeAndNil(Mouse3d);
  FreeAndNil(FPressed);
  FreeAndNil(FFps);
  inherited;
end;

function TCastleFrame.Mouse3dLoaded: boolean;
begin
  Result := Assigned(Mouse3d) and Mouse3d.Loaded;
end;

procedure TCastleFrame.Invalidate;
begin
  Invalidated := true;
  if (FLibraryCallbackProc<>nil) then  // tell the parent window to repaint
    FLibraryCallbackProc(ecgelibNeedsDisplay, 0, 0);
end;

procedure TCastleFrame.SetLibraryCallbackProc(aProc: TCgeLibraryCallbackProc);
begin
  FLibraryCallbackProc := aProc;
end;

procedure TCastleFrame.Notification(AComponent: TComponent; Operation: TOperation);
begin
  { We have to remove a reference to the object from Controls list.
    This is crucial: TControlledUIControlList.Notify,
    and some Controls.MakeSingle calls, assume that all objects on
    the Controls list are always valid objects (no invalid references,
    even for a short time).

    Check "Controls <> nil" is not needed here, it's just in case
    this code will be moved to TUIControl.Notification some day.
    See T3D.Notification for explanation. }

  if (Operation = opRemove) and (AComponent is TUIControl) {and (Controls <> nil)} then
  begin
    Controls.DeleteAll(AComponent);
    DetachNotification(TUIControl(AComponent));
  end;
end;

procedure TCastleFrame.DetachNotification(const C: TUIControl);
begin
  if C = FFocus        then FFocus := nil;
  if C = FCaptureInput then FCaptureInput := nil;
end;

procedure TCastleFrame.UpdateFocusAndMouseCursor;

  function CalculateFocus: TUIControl;
  var
    I: Integer;
  begin
    if not UseControls then Exit(nil);

    for I := 0 to Controls.Count - 1 do
    begin
      Result := Controls[I];
      if Result.PositionInside(MouseX, MouseY) then
        Exit;
    end;

    Result := nil;
  end;

  function CalculateMouseCursor: TMouseCursor;
  begin
    if Focus <> nil then
      Result := Focus.Cursor else
      Result := mcDefault;
  end;

var
  NewFocus: TUIControl;
  NewCursor: TMouseCursor;
  CursorCode: cInt32;
begin
  if FCaptureInput <> nil then
    NewFocus := FCaptureInput else
    NewFocus := CalculateFocus;

  if NewFocus <> Focus then
  begin
    if (Focus <> nil) and UseControls then Focus.Focused := false;
    FFocus := NewFocus;
    { No need to check UseControls above: if Focus <> nil then we know
      UseControls was true during CalculateFocus. }
    if (Focus <> nil) then Focus.Focused := true;
  end;

  if FLibraryCallbackProc <> nil then
  begin
    NewCursor := CalculateMouseCursor;
    // send to client
    case NewCursor of
      mcNone: CursorCode := ecgecursorNone;
      mcWait: CursorCode := ecgecursorWait;
      mcHand: CursorCode := ecgecursorHand;
      mcText: CursorCode := ecgecursorText;
      else CursorCode := ecgecursorDefault;
    end;
    FLibraryCallbackProc(ecgelibSetMouseCursor, CursorCode, 0);
  end;
end;

procedure TCastleFrame.Update;
begin
  UpdateEvent;
  DoLimitFPS;
end;

procedure TCastleFrame.UpdateEvent;
var
  I: Integer;
  C: TUIControl;
  HandleInput: boolean;
  Dummy: boolean;
  Tx, Ty, Tz, TLength, Rx, Ry, Rz, RAngle: Double;
  Mouse3dPollSpeed: Single;
const
  Mouse3dPollDelay = 0.05;
begin
  Fps._UpdateBegin;

  { 3D Mouse }
  if (Assigned(Mouse3D) and Mouse3D.Loaded) or (LeftTouchCtl<>nil) or (RightTouchCtl<>nil) then
  begin
    Mouse3dPollTimer -= Fps.UpdateSecondsPassed;
    if Mouse3dPollTimer < 0 then
    begin
      Tx := 0; Ty := 0; Tz := 0; TLength := 0;
      Rx := 0; Ry := 0; Rz := 0; RAngle := 0;
      { get values from sensor }
      Mouse3dPollSpeed := -Mouse3dPollTimer + Mouse3dPollDelay;
      if (Assigned(Mouse3D) and Mouse3D.Loaded) then
      begin
        Mouse3D.GetTranslationValues(Tx, Ty, Tz, TLength);
        Mouse3D.GetRotationValues(Rx, Ry, Rz, RAngle);
      end;
      if (LeftTouchCtl<>nil) then
      begin
        LeftTouchCtl.GetTranslationValues(Tx, Ty, Tz, TLength);
        LeftTouchCtl.GetRotationValues(Rx, Ry, Rz, RAngle);
      end;
      if (RightTouchCtl<>nil) then
      begin
        RightTouchCtl.GetTranslationValues(Tx, Ty, Tz, TLength);
        RightTouchCtl.GetRotationValues(Rx, Ry, Rz, RAngle);
      end;

      { send to all 2D controls, including viewports }
      for I := 0 to Controls.Count - 1 do
      begin
        C := Controls[I];
        if C.PositionInside(MouseX, MouseY) then
        begin
          C.Mouse3dTranslation(Tx, Ty, Tz, TLength, Mouse3dPollSpeed);
          C.Mouse3dRotation(Rx, Ry, Rz, RAngle, Mouse3dPollSpeed);
        end;
      end;

      { set timer.
        The "repeat ... until" below should not be necessary under normal
        circumstances, as Mouse3dPollDelay should be much larger than typical
        frequency of how often this is checked. But we do it for safety
        (in case something else, like AI or collision detection,
        slows us down *a lot*). }
      repeat Mouse3dPollTimer += Mouse3dPollDelay until Mouse3dPollTimer > 0;
    end;
  end;

  if UseControls then
  begin
    { Although we call Update for all the controls, we look
      at PositionInside and track HandleInput values.
      See TUIControl.Update for explanation. }

    HandleInput := true;

    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
      begin
        C.Update(Fps.UpdateSecondsPassed, HandleInput);
      end else
      begin
        Dummy := false;
        C.Update(Fps.UpdateSecondsPassed, Dummy);
      end;
    end;
  end;

  inherited;
end;

procedure TCastleFrame.UpdateShiftState(const Shift: TShiftState);
begin
  FPressed.Keys[K_Shift] := ssShift in Shift;
  FPressed.Keys[K_Alt  ] := ssAlt   in Shift;
  FPressed.Keys[K_Ctrl ] := ssCtrl  in Shift;
end;

function TCastleFrame.OnKeyDown(const MyKey: TKey; Ch: char): boolean;
begin
  if (MyKey <> K_None) or (Ch <> #0) then
    FPressed.KeyDown(MyKey, Ch);
  //UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }
  if (MyKey <> K_None) or (Ch <> #0) then
     Result := KeyDownEvent(MyKey, Ch)
  else
     Result := false;
end;

function TCastleFrame.KeyDownEvent(const MyKey: TKey; const Ch: char): boolean;
var
  C: TUIControl;
  I: Integer;
begin
  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.Press(InputKey(MyKey, Ch)) then
          Exit(true);
    end;
  end;
end;

function TCastleFrame.OnKeyUp(const MyKey: TKey; Ch: char): boolean;
begin
  if MyKey <> K_None then
    FPressed.KeyUp(MyKey, Ch);
  //UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }
  if (MyKey <> K_None) or (Ch <> #0) then
    Result := KeyUpEvent(MyKey, Ch)
  else
    Result := false;
end;

function TCastleFrame.KeyUpEvent(const MyKey: TKey; const Ch: char): boolean;
var
  C: TUIControl;
  I: Integer;
begin
  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.Release(InputKey(MyKey, Ch)) then
          Exit(true);
    end;
  end;
end;

procedure TCastleFrame.OnMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;
  Include(FMousePressed, Button);

  MouseDownEvent(Button, Shift, X, Y);
end;

procedure TCastleFrame.MouseDownEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  C: TUIControl;
  I: Integer;
begin
  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.Press(InputMouseButton(Button)) then
        begin
          if C.Container = Self as IUIContainer then // for reasons of this check, see CastleWindow
            FCaptureInput := C;
          Exit;
        end;
    end;
  end;

  inherited;
end;

procedure TCastleFrame.OnMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;
  Exclude(FMousePressed, Button);

  MouseUpEvent(Button, Shift, X, Y);
end;

procedure TCastleFrame.MouseUpEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  C, Capture: TUIControl;
  I: Integer;
begin
  Capture := FCaptureInput;
  if FMousePressed = [] then
    FCaptureInput := nil;

  if UseControls then
  begin
    if Capture <> nil then
    begin
      Capture.Release(InputMouseButton(Button));
      Exit;
    end;

    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.Release(InputMouseButton(Button)) then
          Exit;
    end;
  end;

  inherited;
end;

procedure TCastleFrame.OnMouseMove(Shift: TShiftState; NewX, NewY: Integer);

  procedure AggressiveUpdate;
  const
    MaxDesiredFPS = DefaultLimitFPS;
  var
    DesiredFPS: Single;
  begin
    if LimitFPS <= 0 then
      DesiredFPS := MaxDesiredFPS else
      DesiredFPS := Min(MaxDesiredFPS, LimitFPS);
    if Timer - Fps.UpdateStartTime > TimerFrequency / DesiredFPS then
    begin
      UpdateEvent;
      if Invalidated then Paint;
    end;
  end;

begin
  MouseMoveEvent(Shift, NewX, NewY);
  FMouseX := NewX;
  FMouseY := NewY;

  AggressiveUpdate;
end;

procedure TCastleFrame.MouseMoveEvent(Shift: TShiftState; NewX, NewY: Integer);
var
  C: TUIControl;
  I: Integer;
begin
  UpdateFocusAndMouseCursor;

  if UseControls then
  begin
    if FCaptureInput <> nil then
    begin
      FCaptureInput.MouseMove(MouseX, MouseY, NewX, NewY);
      Exit;
    end;

    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.MouseMove(MouseX, MouseY, NewX, NewY) then Exit;
    end;
  end;

  inherited;
end;

function TCastleFrame.OnMouseWheel(const Scroll: Single; const Vertical: boolean): boolean;
begin
  Result := MouseWheelEvent(Scroll, Vertical);
end;

function TCastleFrame.MouseWheelEvent(const Scroll: Single; const Vertical: boolean): boolean;
var
  C: TUIControl;
  I: Integer;
begin
  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.Press(InputMouseWheel(Scroll, true)) then
        begin
          Result := true;
          Exit;
        end;
    end;
  end;
  Result := false;
end;

procedure TCastleFrame.ControlsVisibleChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TCastleFrame.DoBeforeDraw;
var
  I: Integer;
begin
  inherited;

  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].BeforeDraw;
  end;
end;

procedure TCastleFrame.DoDraw;

  { Call Draw for all controls having DrawStyle = ds3D.

    Also (since we call DrawStyle for everything anyway)
    calculates AnythingWants2D = if any control returned DrawStyle = ds2D.
    If not, you can later avoid even changing projection to 2D. }
  procedure Draw3D(out AnythingWants2D: boolean);
  var
    I: Integer;
    C: TUIControl;
  begin
    AnythingWants2D := false;

    if UseControls then
    begin
      { draw controls in "downto" order, back to front }
      for I := Controls.Count - 1 downto 0 do
      begin
        C := Controls[I];
        case C.DrawStyle of
          ds2D: AnythingWants2D := true;
          { Set OpenGL state that may be changed carelessly, and has some
            guanteed value, for TUIControl.Draw calls. }
          ds3D: begin {$ifndef OpenGLES} glLoadIdentity; {$endif} C.Draw; end;
        end;
      end;
    end;

    case OnDrawStyle of
      ds2D: AnythingWants2D := true;
      ds3D: begin {$ifndef OpenGLES} glLoadIdentity; {$endif} {JA inherited DoDraw; }end;
    end;
  end;

  procedure Draw2D;
  var
    C: TUIControl;
    I: Integer;
  begin
    { Set state that is guaranteed for Draw2D calls,
      but TUIControl.Draw cannot change it carelessly. }
    {$ifndef OpenGLES}
    glDisable(GL_LIGHTING);
    glDisable(GL_FOG);
    {$endif}
    glDisable(GL_DEPTH_TEST);
    ScissorDisable;
    GLEnableTexture(CastleGLUtils.etNone);
    glViewport(Rect);

    OrthoProjection(0, Width, 0, Height);

    if UseControls then
    begin
      { draw controls in "downto" order, back to front }
      for I := Controls.Count - 1 downto 0 do
      begin
        C := Controls[I];

        if C.DrawStyle = ds2D then
        begin
          { Set OpenGL state that may be changed carelessly, and has some
            guanteed value, for Draw2d calls. }
          {$ifndef OpenGLES} glLoadIdentity; {$endif}
          WindowPos := Vector2LongInt(0, 0);
          C.Draw;
        end;
      end;
    end;

    if OnDrawStyle = ds2D then
    begin
      {$ifndef OpenGLES} glLoadIdentity; {$endif}
      WindowPos := Vector2LongInt(0, 0);
      {JA inherited DoDraw;}
    end;
  end;

var
  AnythingWants2D: boolean;
begin
  Draw3D(AnythingWants2D);

  if AnythingWants2D then
    Draw2D;

  if OnDrawStyle = dsNone then
    inherited;
end;

procedure TCastleFrame.Paint;
begin
  //if MakeCurrent then
  begin
    DoBeforeDraw;
    Fps._RenderBegin;
    try
      DoDraw;
      if GLVersion.BuggySwapNonStandardViewport then
        glViewport(Rect);
      {JA SwapBuffers;}
    finally Fps._RenderEnd end;
    Invalidated := false;
  end;
end;

procedure TCastleFrame.Resize;
var
  I, CtlBorder: Integer;
begin
  inherited;

  { Call MakeCurrent here, to make sure CastleUIControls always get
    ContainerResize with good GL context. }
  if GLInitialized and UseControls {and MakeCurrent} then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].ContainerResize(Width, Height);
  end;

  CtlBorder := Round(24*FDpi/96);
  if LeftTouchCtl<>nil then
  begin
    LeftTouchCtl.Left := CtlBorder;
    LeftTouchCtl.Bottom := CtlBorder;
  end;
  if RightTouchCtl<>nil then
  begin
    RightTouchCtl.Left := Width - RightTouchCtl.Width - CtlBorder;
    RightTouchCtl.Bottom := CtlBorder;
  end;
end;

procedure TCastleFrame.GLContextOpen;
begin
  {MakeCurrent;}
  LoadAllExtensions();
  DoGLContextOpen();
end;

var
  ControlsOpen: Cardinal;

procedure TCastleFrame.DoGLContextOpen;
var
  I: Integer;
begin
  inherited;
  Inc(ControlsOpen);
  if ControlsOpen = 1 then
    CastleUIControls.OnGLContextOpen.ExecuteAll;

  { call GLContextOpen on controls after inherited (OnGLContextOpen). }
  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].GLContextOpen;
  end;
end;

procedure TCastleFrame.GLContextClose;
begin
  DoGLContextClose();
end;

procedure TCastleFrame.DoGLContextClose;
var
  I: Integer;
begin
  { call GLContextClose on controls before inherited (OnGLContextClose).
    This may be called from Close, which may be called from TCastleWindowBase destructor,
    so prepare for Controls being possibly nil now. }
  if UseControls and (Controls <> nil) then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].GLContextClose;
  end;

  if ControlsOpen = 1 then
    CastleUIControls.OnGLContextClose.ExecuteAll;
  Dec(ControlsOpen);
  inherited;
end;

procedure TCastleFrame.SetUseControls(const Value: boolean);
begin
  if Value <> UseControls then
  begin
    FUseControls := Value;
    { Focus must always be @nil when UseControls = false }
    UpdateFocusAndMouseCursor;
  end;
end;

procedure TCastleFrame.Load(const SceneURL: string);
begin
  Load(Load3D(SceneURL, false), true);
  MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  MainScene.ProcessEvents := true;

  UpdateUserInterface();
end;

procedure TCastleFrame.Load(ARootNode: TX3DRootNode; const OwnsRootNode: boolean);
begin
  { destroy MainScene and Camera, we will recreate them }
  SceneManager.MainScene.Free;
  SceneManager.MainScene := nil;
  SceneManager.Items.Clear;
  SceneManager.Camera.Free;

  SceneManager.MainScene := TCastleScene.Create(Self);
  SceneManager.MainScene.Load(ARootNode, OwnsRootNode);
  SceneManager.Items.Add(SceneManager.MainScene);

  { initialize octrees titles }
  MainScene.TriangleOctreeProgressTitle := 'Building triangle octree';
  MainScene.ShapeOctreeProgressTitle := 'Building shape octree';

  { just to make our Camera always non-nil.
    Useful for model_3d_viewer that wants to initialize NavigationType
    from camera. }
  SceneManager.Camera := SceneManager.CreateDefaultCamera;
end;

function TCastleFrame.MainScene: TCastleScene;
begin
  Result := SceneManager.MainScene;
end;

function TCastleFrame.Camera: TCamera;
begin
  Result := SceneManager.Camera;
end;

function TCastleFrame.GetShadowVolumes: boolean;
begin
  Result := SceneManager.ShadowVolumes;
end;

procedure TCastleFrame.SetShadowVolumes(const Value: boolean);
begin
  SceneManager.ShadowVolumes := Value;
end;

function TCastleFrame.GetShadowVolumesDraw: boolean;
begin
  Result := SceneManager.ShadowVolumesDraw;
end;

procedure TCastleFrame.SetShadowVolumesDraw(const Value: boolean);
begin
  SceneManager.ShadowVolumesDraw := Value;
end;

function TCastleFrame.GetOnCameraChanged: TNotifyEvent;
begin
  Result := SceneManager.OnCameraChanged;
end;

procedure TCastleFrame.SetOnCameraChanged(const Value: TNotifyEvent);
begin
  SceneManager.OnCameraChanged := Value;
end;

function TCastleFrame.GetTooltipX: Integer;
begin
  Result := 0;
end;

function TCastleFrame.GetTooltipY: Integer;
begin
  Result := 0;
end;

procedure TCastleFrame.SetRenderSize(const NewWidth, NewHeight: Integer);
begin
  if (FWidth <> NewWidth) or (FHeight <> NewHeight) then
  begin
    FWidth := NewWidth;
    FHeight := NewHeight;
    Resize();
  end;
end;

{ IUIContainer required functions }
procedure TCastleFrame.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  FMouseX := NewMouseX;
  FMouseY := NewMouseY;
end;

function TCastleFrame.GetMouseX: Integer;
begin
  Result := FMouseX;
end;

function TCastleFrame.GetMouseY: Integer;
begin
  Result := FMouseY;
end;

function TCastleFrame.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TCastleFrame.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TCastleFrame.Rect: TRectangle;
begin
  Result := Rectangle(0, 0, FWidth, FHeight);
end;

function TCastleFrame.GetMousePressed: TMouseButtons;
begin
  Result := FMousePressed;
end;

function TCastleFrame.GetPressed: TKeysPressed;
begin
  Result := FPressed;
end;

function TCastleFrame.GetCurrentNavigationType(): TCameraNavigationType;
begin
  if Camera is TUniversalCamera then
    Result := (Camera as TUniversalCamera).NavigationType else
  if Camera is TWalkCamera then
    Result := ntWalk else
    Result := ntExamine;
end;

procedure TCastleFrame.SetNavigationType(NewType: TCameraNavigationType);
begin
  if (Camera<>nil) AND (Camera is TUniversalCamera) then
     (Camera as TUniversalCamera).NavigationType := NewType;
  UpdateUserInterface();
end;

procedure TCastleFrame.NavigationInfoChanged(Sender: TObject);
var
  NavType: TCameraNavigationType;
  NavValue: integer;
begin
  UpdateUserInterface();

  // send into to parent app (to update navigation buttons state)
  if (FLibraryCallbackProc<>nil) then
  begin
    NavType := GetCurrentNavigationType();
    case NavType of
      ntWalk:         NavValue := 0;
      ntFly:          NavValue := 1;
      ntExamine:      NavValue := 2;
      ntArchitecture: NavValue := 3;
    end;
    FLibraryCallbackProc(ecgelibNavigationTypeChanged, NavValue, 0);
  end;
end;

procedure TCastleFrame.UpdateTouchController(LeftSide, CtlVisible: boolean; Mode: TCastleTouchCtlMode);
var
  aNewCtl: TCastleTouchControl;
begin
  // left controller
  if LeftSide and (LeftTouchCtl<>nil) then
  begin
    if CtlVisible then
      LeftTouchCtl.TouchMode := Mode
    else begin
      Controls.Remove(LeftTouchCtl);
      FreeAndNil(LeftTouchCtl);
    end;
    Exit;
  end;

  // right controller
  if (not LeftSide) and (RightTouchCtl<>nil) then
  begin
    if CtlVisible then
      RightTouchCtl.TouchMode := Mode
    else begin
      Controls.Remove(RightTouchCtl);
      FreeAndNil(RightTouchCtl);
    end;
    Exit;
  end;

  if not CtlVisible then Exit;

  aNewCtl := TCastleTouchControl.Create(self);
  aNewCtl.TouchMode := Mode;
  aNewCtl.SizeScale := FDpi / 96;
  Controls.InsertFront(aNewCtl);
  if LeftSide then
    LeftTouchCtl := aNewCtl
  else
    RightTouchCtl := aNewCtl;
  Resize();
end;

procedure TCastleFrame.UpdateTouchInterface(Mode: TTouchCtlInterface);
var
  WalkCamera: TWalkCamera;
begin
  if (Camera<>nil) and (Camera is TUniversalCamera) then
    WalkCamera := (Camera as TUniversalCamera).Walk
  else
    WalkCamera := nil;

  if Mode = etciCtlWalkCtlRotate then
  begin
    UpdateTouchController(true, true, ctcmWalking);
    UpdateTouchController(false, true, ctcmHeadRotation);
    if WalkCamera<>nil then
      WalkCamera.MouseDragMode := cwdmNone;
  end
  else if Mode = etciCtlWalkDragRotate then
  begin
    UpdateTouchController(true, false);
    UpdateTouchController(false, true, ctcmWalking);
    if WalkCamera<>nil then
      WalkCamera.MouseDragMode := cwdmDragToRotate;
  end
  else if Mode = etciCtlFlyCtlWalkDragRotate then
  begin
    UpdateTouchController(true, true, ctcmFlyUpdown);
    UpdateTouchController(false, true, ctcmWalking);
    if WalkCamera<>nil then
      WalkCamera.MouseDragMode := cwdmDragToRotate;
  end
  else if Mode = etciCtlPanXYDragRotate then
  begin
    UpdateTouchController(true, false);
    UpdateTouchController(false, true, ctcmPanXY);
    if WalkCamera<>nil then
      WalkCamera.MouseDragMode := cwdmDragToRotate;
  end
  else
  begin
    UpdateTouchController(true, false);
    UpdateTouchController(false, false);
    if WalkCamera<>nil then
      WalkCamera.MouseDragMode := cwdmDragToWalk;
  end;
  Resize();
  Invalidate();
end;

procedure TCastleFrame.UpdateUserInterface();
var
  NavType: TCameraNavigationType;
begin
  if UserInterface = euiTouch then
  begin
    NavType := GetCurrentNavigationType();
    case NavType of
      ntWalk:         UpdateTouchInterface(etciCtlWalkDragRotate);
      ntFly:          UpdateTouchInterface(etciCtlFlyCtlWalkDragRotate);
      ntExamine:      UpdateTouchInterface(etciCtlPanXYDragRotate);
      ntArchitecture: UpdateTouchInterface(etciCtlPanXYDragRotate);
    end;
  end;
end;

end.
