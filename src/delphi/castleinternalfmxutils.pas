{
  Copyright 2022-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities specific to FMX in Castle Game Engine.
  This allows sharing of solutions between FMX TOpenGLControl and FMX TCastleControl. }
unit CastleInternalFmxUtils;

{$I castleconf.inc}

{ Platforms using TGLContextExisting with FMX (FireMonkey) for
  the rendering context.
  In other words, just relying that FMX will create context for us. }
{$if defined(DELPHI) and (defined(ANDROID) or defined(IOS))}
  {$define CASTLE_MOBILE_FMX}
{$endif}

interface

uses Types, Generics.Collections, UITypes, System.Messaging,
  FMX.Controls, FMX.Controls.Presentation, FMX.Types, FMX.Graphics,
  FMX.Forms,
  {$ifdef ANDROID}
  // used by TFmxTouchDispatcher
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText,
  {$endif}
  {$ifdef MSWINDOWS} FMX.Presentation.Win, {$endif}
  {$ifdef LINUX} FMX.Platform.Linux, {$endif}
  {$ifdef CASTLE_MOBILE_FMX} CastleGLES, {$endif}
  CastleInternalContextBase, CastleVectors, CastleRenderContext,
  CastleKeysMouse;

type
  THandleEvent = procedure of object;

  { Utility for FMX controls to help them initialize OpenGL context.

    This tries to abstract as much as possible the platform-specific ways
    how to get OpenGL context (and native handle) on a given control,
    in a way that is useful for both FMX TOpenGLControl and FMX TCastleControl.

    The current state:

    - On Windows: make sure native Windows handle is initialized when necessary,
      and pass it to TGLContextWgl.

    - On Linux: we have to create our own Gtk widget (since FMXLinux only ever
      creates native handle for the whole form, it seems).
      And insert it into FMX form, keeping the existing FMX drawing area too.
      And then use TGLContextEgl to connect to our own Gtk widget.

    - On other platforms: just rely on FMX to create / release the OpenGL context.
      This makes sense on Delphi/Android and Delphi/iOS.

    Note: We could not make TCastleControl descend from TOpenGLControl on FMX
    (like we did on LCL), since the GL work of TCastleControl is partially
    done by the container (to be shared, in turn,
    with VCL and in future LCL implementations).
    So to have enough flexibility how to organize hierarchy, this is rather
    a separate class that is just created and used by both
    FMX TOpenGLControl and FMX TCastleControl. }
  TFmxOpenGLUtility = class
  {$define read_TFmxOpenGLUtility_interface}
  {$I castleinternalfmxutils_utility.inc}
  {$undef read_TFmxOpenGLUtility_interface}
  public
    { Set before calling HandleNeeded.
      Cannot change during lifetime of this instance, for now. }
    Control: TPresentedControl;

    { Called, if assigned, after creation (OnHandleAfterCreateEvent)
      or before destruction (OnHandleBeforeDestroyEvent)
      of a native handle.

      @bold(This is called only on platforms where
      FMX Presentation is not available.)
      This means platforms where

      - our code creates the native handle (in general:
        any system-specific resources) we need.
        E.g. Gtk handle on Linux, created by Delphi/Linux.

      - or when there's no need to create anything..E.g. FMX on Android or iOS
        just uses the existing context, so we don't need to create anything more.
        So this applies to Delphi/Android and Delphi/iOS.

      TODO: Delphi/macOS: to be figured out.

      In contrast, on platforms where FMX Presentation is available
      (like Delphi/Windows), we use FMX Presentation features,
      and we don't need extra notifications from this class when handle
      is created/destroyed. }
    OnHandleAfterCreateEvent: THandleEvent;
    OnHandleBeforeDestroyEvent: THandleEvent;

    { Is initializing internal resources, needed by HandleNeeded,
      possible now. Use this if calling HandleNeeded is not necessary now,

      This does something only on platforms where
      FMX Presentation is not available and we need to take care ourselves
      of the necessary internal per-control handle where OpenGL context
      exists. In practice: only on Delphi/Linux now.

      On other platforms, it always returns true. }
    // Not needed in the end // function HandlePossible: Boolean;

    { Make sure that Control has initialized internal handle,
      necessary to later initialize OpenGL context for this.
      This must be called before ContextAdjustEarly.

      - On some platforms, like Windows,
        creating a handle should provoke ContextAdjustEarly
        and TGLContext.Initialize.
        The caller should make it happen: see
        TPresentationProxyFactory.Current.Register and our presentation classes.
        This works nicely when FMX platform defines
        Presentation and classes like TWinPresentation.

      - On other platforms, like Linux, we create handle ourselves.
        Manually make sure context is created
        after handle is obtained.
        Needed for Delphi/Linux that doesn't define any "presentation"
        stuff and only creates a handle for the entire form. }
    procedure HandleNeeded;

    { Release handle that was created by @link(HandleNeeded).

      This does something only on platforms where
      FMX Presentation is not available and we need to take care ourselves
      of the necessary internal per-control handle where OpenGL context
      exists. }
    procedure HandleRelease;

    { Adjust TGLContext parameters before calling TGLContext.CreateContext.
      You must call HandleNeeded earlier.

      Extracts platform-specific bits from given FMX Control,
      and puts in platform-specific bits of TGLContext descendants.
      This is used by TCastleControl and TOpenGLControl.
      It does quite low-level and platform-specific job, dictated
      by the necessity of how FMX works, to be able to get context.

      This is synchronized with what ContextCreateBestInstance does on this platform. }
    procedure ContextAdjustEarly(const PlatformContext: TGLContext);

    { Call this often to perform platform-specific adjustments.

      At this point, this is required on Linux: we need to synchronize
      internal GTK control with desired position and size of FMX TCastleControl. }
    procedure Update;

    { Size reported by FMX controls needs to be multiplied by this
      to get size in physical pixels (which we need e.g. for OpenGL context). }
    function Scale: Single;
  end;

const
  { On some platforms (Windows), the engine control must always have "native style",
    which means it has ControlType = Platform. See FMX docs about native controls:
    https://docwiki.embarcadero.com/RADStudio/Sydney/en/FireMonkey_Native_Windows_Controls
    Native controls are always on top of non-native controls.

    On other platforms (Android and iOS), it must be Styled
    (which is also default for FMX controls).
    TControlType.Platform for mobile would cause creation of "native view"
    that intercepts touches, making OnTouch not work properly. }
  DefaultControlType =
    {$if (not defined(ANDROID)) and (not defined(IOS))}
      TControlType.Platform
    {$else}
      TControlType.Styled
    {$endif};

type
  { Utility to help with rendering OpenGL in FMX controls. }
  TFmxOpenGLRenderingUtility = record
  {$ifdef CASTLE_MOBILE_FMX}
  strict private
    SavedViewport: array [0..3] of TGLint;
  {$endif CASTLE_MOBILE_FMX}
  public
    { Perform necessary preparations before direct OpenGL(ES) rendering
      that must cooperate with FMX's rendering state. }
    procedure BeforeDirectRendering(const Canvas: TCanvas;
      const RenderContext: TRenderContext);

    { Perform necessary cleanup after direct OpenGL(ES) rendering
      that must cooperate with FMX's rendering state. }
    procedure AfterDirectRendering(const Canvas: TCanvas;
      const RenderContext: TRenderContext);
  end;

{$ifdef CASTLE_HANDLE_FMX_TOUCH}

type
  { Touch down or up reported by TFmxTouchDispatcher. }
  TFmxTouchEvent = procedure (const FingerIndex: TFingerIndex;
    const Position: TVector2) of object;

  { Touch motion reported by TFmxTouchDispatcher. }
  TFmxTouchMotionEvent = procedure (const FingerIndex: TFingerIndex;
    const OldPosition, NewPosition: TVector2) of object;

  { Handle touch events on an FMX form, calling in turn proper events
    with parameters matching what our engine expects.

    The main information about touch events in FMX is provided in
    TForm.OnTouch event.
    FMX has a number of issues though, some platform-specific,
    like being unable to accurately determine which finger is released on Android.
    This class workarounds these problems, by interpreting touch events
    in a way matching FMX Android / iOS implementation, and adding Android-specific
    handling to detect "what causes up". }
  TFmxTouchDispatcher = class
  strict private
    {$ifdef ANDROID}
    type
      { Listen to raw Android touch events on the form view. }
      TAndroidTouchListener = class(TJavaLocal, JView_OnTouchListener)
      strict private
        FTouchDispatcher: TFmxTouchDispatcher;
      public
        constructor Create(const ATouchDispatcher: TFmxTouchDispatcher);
        { Raw touch event from Android. }
        function onTouch(V: JView; Event: JMotionEvent): Boolean; cdecl;
      end;
    var
      { Java listener, attached to Android view.
        Note: TJavaLocal (descendant of TJInterfacedObject) keeps one
        reference to itself, so it's not freed when interface references
        to it drop to zero. We manage its lifetime explicitly,
        freeing it in destructor (after detaching from Android view). }
      FAndroidTouchListener: TAndroidTouchListener;
      { Android view we have attached TAndroidTouchListener to, @nil if none. }
      FAttachedView: JView;
    {$endif ANDROID}

    var
      { Current form given to AttachToForm. }
      FForm: TCommonCustomForm;

      { Map FMX finger id to CGE TFingerIndex, for all pressed fingers.

        Finger index (both CGE TFingerIndex and FMX Id:NativeInt) stay persistent,
        so if you touch with fingers 0 and 1, then release 0,
        you are still pressing finger 1.

        On Android, FMX "finger id" is actually already exactly what we need for
        our TFingerIndex.
        Numbers are small, like 0 and 1. They come from Android API
        AMotionEvent_getPointerId, so FMX does this just like our
        castlewindow_android.inc .

        On iOS: FMX "finger id" comes from iOS touch hash.
        These numbers can be very large.
        And they can never hit "zero".

        - For CGE, "very large numbers" is not a problem, we could still use it for
          FingerIndex. Our structures, like TFingerIndexCaptureMap,
          are prepared that FingerIndex is just arbitrary number.

        - But "never zero" is a problem -> some of our code relies that
          "first finger pressed" is zero (e.g. CastleCameras check
          "Event.FingerIndex <> 0").

        Solution: map to consecutive numbers starting from zero,
        maintain mapping FMX id -> our indexes.
      }
      FingerIndexes: TDictionary<NativeInt, TFingerIndex>;

      { For all pressed fingers, store their last known positions. }
      FingerPositions: TDictionary<TFingerIndex, TVector2>;

    { On Android, attaches TAndroidTouchListener to FForm.
      Call only when FForm.Handle <> nil. }
    procedure AttachListenerToForm;

    { On Android, detach TAndroidTouchListener from FForm.
      Call at any time, this is safe, only detaches if was attached. }
    procedure DetachListenerFromForm;

    procedure AfterCreateFormHandle(const Sender: TObject; const M: TMessage);
    procedure BeforeDestroyFormHandle(const Sender: TObject; const M: TMessage);

    { Handler of TForm.OnTouch, assigned by @link(AttachToForm).
      This will call @link(OnDown), @link(OnUp), or @link(OnMotion) as appropriate. }
    procedure FormTouch(Sender: TObject; const Touches: TTouches;
      const Action: TTouchAction);
  private
    { At next FormTouch, these will correspond to TTouch.Id for the finger
      that went down/up. }
    DownId, UpId: NativeInt;
    HasDownId, HasUpId: Boolean;
  protected
    procedure DoDown(const FingerIndex: TFingerIndex; const Position: TVector2); virtual;
    procedure DoUp(const FingerIndex: TFingerIndex; const Position: TVector2); virtual;
    procedure DoMotion(const FingerIndex: TFingerIndex;
      const OldPosition, NewPosition: TVector2); virtual;
  public
    OnDown, OnUp: TFmxTouchEvent;
    OnMotion: TFmxTouchMotionEvent;

    // Resolve positions relative to this control.
    PositionsInControl: TControl;

    constructor Create;
    destructor Destroy; override;

    { Start listening to touches on the given form.
      This assigns the form's OnTouch event to our @link(FormTouch). }
    procedure AttachToForm(const AForm: TCommonCustomForm);

    { Stop listening to touches on the form given to @link(AttachToForm).
      This clears the form's OnTouch event.
      Called automatically from the destructor. }
    procedure DetachFromForm;
  end;

{$endif CASTLE_HANDLE_FMX_TOUCH}

implementation

uses SysUtils,
  {$if defined(CASTLE_HANDLE_FMX_TOUCH) and defined(ANDROID)}
  // for FAndroidTouchListener in TFmxTouchDispatcher
  Androidapi.Input, FMX.Platform.Android, FMX.Platform.UI.Android,
  {$endif}
  {$ifdef CASTLE_MOBILE_FMX}
  FMX.Canvas.GPU, FMX.Types3D,
  {$endif CASTLE_MOBILE_FMX}
  {$define read_implementation_uses}
    {$I castleinternalfmxutils_utility.inc}
  {$undef read_implementation_uses}
  CastleInternalGLUtils, CastleLog;

{ TFmxOpenGLUtility ---------------------------------------------------------- }

{$define read_implementation}
  {$I castleinternalfmxutils_utility.inc}
{$undef read_implementation}

{ TFmxOpenGLRenderingUtility ------------------------------------------------- }

procedure TFmxOpenGLRenderingUtility.BeforeDirectRendering(const Canvas: TCanvas;
  const RenderContext: TRenderContext);
begin
  {$ifdef CASTLE_MOBILE_FMX}
  { We need TCustomCanvasGpu. Raise (early) if this is not the case.
    TODO: Confirm: This will likely happen if you try to use CGE with Skia
    on mobile. }
  if not (Canvas is TCustomCanvasGpu) then
    raise Exception.CreateFmt('Canvas class unsupported: %s, we cannot render using Castle Game Engine',
      [Canvas.ClassName]);
  {$endif}

  { Flush the FMX canvas BEFORE calling any raw GL.
    FMX batches draw calls; if we call glDrawArrays while FMX still has
    pending batched commands the interleaving causes visual corruption.
    Testcase: run on Android, using Delphi, e.g. platformer (or any other demo)
    -- without this, it looks like our rendering is ignored. }
  Canvas.Flush;

  {$ifdef CASTLE_MOBILE_FMX}
  { Clear scissor, matching RenderContext.
    TODO: Instead, read current scissor, and make RenderContext aware of it.
    Note that at end, PopContextStates will restore the previous
    scissor state, so FMX scissor state is already restored OK. }
  glDisable(GL_SCISSOR_TEST);

  { Save + restore FMX viewport.
    FMX sets this once at frame start (TContextAndroid.DoBeginScene,
    TContextIOS.DoBeginScene) and controls rendering expect it. }
  glGetIntegerv(GL_VIEWPORT, @SavedViewport[0]);

  RenderContext.SynchronizeState;
  {$endif}
end;

procedure TFmxOpenGLRenderingUtility.AfterDirectRendering(const Canvas: TCanvas;
  const RenderContext: TRenderContext);
{$ifdef CASTLE_MOBILE_FMX}
var
  Ctx: TContext3D;
{$endif}
begin
  {$ifdef CASTLE_MOBILE_FMX}
  // Restore FMX viewport
  glViewport(SavedViewport[0], SavedViewport[1], SavedViewport[2], SavedViewport[3]);

  { Clean state, that FMX never touches, just assumes it is clean. }

  { FMX doesn't draw using VBOs, make sure it doesn't access our
    by accident. }
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  RenderContext.CurrentVao := nil; // paranoid; this does nothing on OpenGLES
  RenderContext.CurrentProgram := nil;

  { FMX never sets glStencilMask, but it can do
    glEnable(GL_STENCIL_TEST), and glClear -> assuming glStencilMask.
    Hm, although CGE doesn't call glStencilMask now, so it doesn't matter. }
  glStencilMask($FFFFFFFF);

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, 0); // make sure our texture does not leak

  { Check GL errors after all CGE rendering.
    Don't limit this to debug-only, as FMX does glGetError always,
    see TGlesDiagnostic.RaiseIfHasError .
    So it's better to catch GL errors at the end of CGE rendering,
    and report them as CGE rendering issues,
    not let FMX worry about them (and warn/abort depending on
    OpenGlErrorReporting).

    TODO: CheckGLErrorsAll.
    Like CheckGLErrors, but calls glGetError repeatedly until all errors are
    collected, and glGetError reports GL_NO_ERROR.
    Then we make exception or log, about all collected errors,
    just like CheckGLErrors.
  }
  try
    CheckGLErrors('after Castle Game Engine rendering (in the middle of FMX rendering)');
  finally
    { Make OpenGLES context state correspond to what FMX thinks
      it should be (TContext3D.CurrentStates).

      We do this by TContext3D.ResetStates + Ctx.PopContextStates.
      - TContext3D.ResetStates sets TContext3D.CurrentStates
        "unknown state now"
      - Ctx.PopContextStates will execute necessary
        TCustomContextOpenGL.DoSetContextState that make
        all OpenGLES calls.
        It also sets proper scissor.

      This way rest of FMX rendering (even if FMX renders more controls
      right after Castle Game Engine FMX control) will use correct
      state.

      Note that placement of Ctx.PushContextStates doesn't matter much.
      We could do it before "try", before all CGE rendering.
      But actually nothing changes FMX cached state (CurrentStates),
      so we may as well just do PushContextStates here.
      We really call PushContextStates only to pair it with PopContextStates.
    }
    Ctx := TCustomCanvasGpu(Canvas).Context;
    Ctx.PushContextStates;
    TContext3D.ResetStates;
    Ctx.PopContextStates;
  end;

  {$endif CASTLE_MOBILE_FMX}
end;

{$ifdef CASTLE_HANDLE_FMX_TOUCH}

{ TFmxTouchDispatcher.TAndroidTouchListener --------------------------------- }

{$ifdef ANDROID}
constructor TFmxTouchDispatcher.TAndroidTouchListener.Create(
  const ATouchDispatcher: TFmxTouchDispatcher);
begin
  inherited Create;
  FTouchDispatcher := ATouchDispatcher;
end;

function TFmxTouchDispatcher.TAndroidTouchListener.onTouch(
  V: JView; Event: JMotionEvent): Boolean;
begin
  case Event.getActionMasked of
    AMOTION_EVENT_ACTION_DOWN,
    AMOTION_EVENT_ACTION_POINTER_DOWN:
      begin
        FTouchDispatcher.DownId := Event.getPointerId(Event.getActionIndex);
        FTouchDispatcher.HasDownId := true;
      end;
    AMOTION_EVENT_ACTION_UP,
    AMOTION_EVENT_ACTION_POINTER_UP:
      begin
        FTouchDispatcher.UpId := Event.getPointerId(Event.getActionIndex);
        FTouchDispatcher.HasUpId := true;
      end;
  end;

  { Android calls View.OnTouchListener.onTouch *before* View.onTouchEvent.
    FMX handles touches in onTouchEvent (TFormViewListener.onTouchEvent
    in FMX.Platform.UI.Android), so returning @false here
    lets FMX process the event as usual (OnTouch, mouse events, gestures). }
  Result := false;
end;
{$endif}

{ TFmxTouchDispatcher -------------------------------------------------------- }

constructor TFmxTouchDispatcher.Create;
begin
  inherited;
  FingerIndexes := TDictionary<NativeInt, TFingerIndex>.Create;
  FingerPositions := TDictionary<TFingerIndex, TVector2>.Create;
  {$ifdef ANDROID}
  FAndroidTouchListener := TAndroidTouchListener.Create(Self);
  {$endif ANDROID}
end;

destructor TFmxTouchDispatcher.Destroy;
begin
  DetachFromForm;
  {$ifdef ANDROID}
  FreeAndNil(FAndroidTouchListener);
  {$endif ANDROID}
  FreeAndNil(FingerIndexes);
  FreeAndNil(FingerPositions);
  inherited;
end;

procedure TFmxTouchDispatcher.AttachToForm(const AForm: TCommonCustomForm);
begin
  DetachFromForm; // in case we were attached to some form already
  FForm := AForm;
  FForm.OnTouch := FormTouch;
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateFormHandle);
  TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyFormHandle);
  { Form handle may already exist (e.g. when adding this to a form that is
    already created), then TAfterCreateFormHandle was already sent. }
  if FForm.Handle <> nil then
    AttachListenerToForm;
end;

procedure TFmxTouchDispatcher.DetachFromForm;
begin
  if FForm = nil then
    Exit;
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, AfterCreateFormHandle);
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, BeforeDestroyFormHandle);
  DetachListenerFromForm;
  { Otherwise the form would call FormTouch on a freed instance,
    if we are destroyed before the form. }
  FForm.OnTouch := nil;
  FForm := nil;
end;

procedure TFmxTouchDispatcher.AfterCreateFormHandle(const Sender: TObject; const M: TMessage);
begin
  { These messages are broadcast for all forms, filter to our form. }
  if TAfterCreateFormHandle(M).Value = FForm then
    AttachListenerToForm;
end;

procedure TFmxTouchDispatcher.BeforeDestroyFormHandle(const Sender: TObject; const M: TMessage);
begin
  if TBeforeDestroyFormHandle(M).Value = FForm then
    DetachListenerFromForm;
end;

procedure TFmxTouchDispatcher.AttachListenerToForm;
begin
  {$ifdef ANDROID}
  { Attach when we have form handle (from TAfterCreateFormHandle),
    as form handle (and its Android view) may be recreated,
    e.g. when changing some form properties. }
  Assert(FForm.Handle <> nil);
  DetachListenerFromForm; // in case we're attached to some old view
  FAttachedView := WindowHandleToPlatform(FForm.Handle).View;
  FAttachedView.setOnTouchListener(FAndroidTouchListener);
  {$endif}
end;

procedure TFmxTouchDispatcher.DetachListenerFromForm;
begin
  {$ifdef ANDROID}
  if FAttachedView <> nil then
  begin
    FAttachedView.setOnTouchListener(nil);
    FAttachedView := nil;
  end;
  {$endif}
end;

procedure TFmxTouchDispatcher.FormTouch(
  Sender: TObject; const Touches: TTouches; const Action: TTouchAction);

  { Find TTouch index with given Id. Returns -1 if not found. }
  function FindTouchWithId(const Id: NativeInt): Integer;
  var
    I: Integer;
  begin
    for I := 0 to High(Touches) do
    begin
      if Touches[I].Id = Id then
        Exit(I);
    end;
    Result := -1;
  end;

  { Convert Position from TForm space to PositionsInControl space.
    Also convert types (TPointF -> TVector2) by the way. }
  function PositionToLocal(const Position: TPointF): TVector2;
  var
    P: TPointF;
  begin
    P := PositionsInControl.AbsoluteToLocal(Position);
    Result := Vector2(P.X, P.Y);
  end;

  { Finger with given TTouch index is pressed. }
  procedure FingerDown(const TouchIndex: Integer);

    function NewFingerIndex: TFingerIndex;
    begin
      Result := 0;
      while FingerPositions.ContainsKey(Result) do
        Inc(Result);
    end;

  var
    Id: NativeInt;
    FingerIndex: TFingerIndex;
    Position: TVector2;
  begin
    Id := Touches[TouchIndex].Id;

    { Ignore if this finger is already pressed.
      This happens in practice on iOS, testcase: any CGE application really,
      like platformer.

      Reason (based on reading Delphi code): FMX reports all touches of the UIEvent
      (UIEvent.allTouches) in each touchesBegan / touchesMoved call.
      So when UIKit calls touchesBegan (for new finger) and touchesMoved
      (for other finger) with the same UIEvent, the new finger is reported
      2 times (2 calls to SendTouches).

      And (this seems FMX bug) it is reported 2 times with Action=Down.
      I.e. the Touches[..].Action = Down, and the Action parameter is also
      = Down (FMX changes it from Move to Down,
      see TFMXViewBase.SendTouches). }
    if FingerIndexes.ContainsKey(Id) then
      Exit;

    // calculate FingerIndex from Id, update FingerIndexes
    FingerIndex := NewFingerIndex;
    FingerIndexes.Add(Id, FingerIndex);

    Position := PositionToLocal(Touches[TouchIndex].Location);
    FingerPositions.AddOrSetValue(FingerIndex, Position);
    DoDown(FingerIndex, Position);
  end;

  { Finger with given TTouch index is released. }
  procedure FingerUp(const TouchIndex: Integer);
  var
    Id: NativeInt;
    FingerIndex: TFingerIndex;
    Position: TVector2;
  begin
    Id := Touches[TouchIndex].Id;

    // calculate FingerIndex from Id, update FingerIndexes
    if not FingerIndexes.TryGetValue(Id, FingerIndex) then
    begin
      WritelnWarning('TFmxTouchDispatcher', 'Finger with id %d released, but was not pressed', [Id]);
      Exit;
    end;
    FingerIndexes.Remove(Id);

    Position := PositionToLocal(Touches[TouchIndex].Location);
    FingerPositions.Remove(FingerIndex);
    DoUp(FingerIndex, Position);
  end;

  { Handle Action = Down.
    On Action = Down, new touch appears in the Touches list. }
  procedure HandleDown;
  var
    TouchIndex: Integer;
  begin
    if HasDownId then
    begin
      { Android: we know exactly which finger is down from DownId. }
      HasDownId := false;
      TouchIndex := FindTouchWithId(DownId);
      if TouchIndex = -1 then
      begin
        WritelnWarning('TFmxTouchDispatcher', 'Could not find touch with DownId %d', [DownId]);
        Exit;
      end;
      FingerDown(TouchIndex);
    end else
    begin
      {$ifdef ANDROID}
      WritelnWarning('TFmxTouchDispatcher', 'HandleDown called without DownId on Android');
      {$endif ANDROID}

      { iOS: Touches[..].Action is reliable.
        And multiple fingers may go down at once, this can happen in reality. }
      for TouchIndex := 0 to High(Touches) do
        if Touches[TouchIndex].Action = TTouchAction.Down then
          FingerDown(TouchIndex);
    end;
  end;

  { Handle Action = Up (on iOS, this can also indicate Cancel, but this is
    only specified in particular touch).

    On Action = Up, old touch is still present in the Touches list.
    Same for Action = Cancel.

    How to recognize which finger is released?

    - On iOS: See per-touch actions.
      There will be Touches[..].Action=Up only for the actual finger being up.
      Multiple fingers may go up at once.

    - On Android: Unfortunately, when releasing one finger, we get OnTouch
      with Touches[..].Action=Up for *all* fingers.
      If multiple fingers were released, we really don't know which one is up
      based on the parameters FMX gives us.

      To fix it, we rely on UpId.
  }
  procedure HandleUp;
  var
    TouchIndex: Integer;
  begin
    if HasUpId then
    begin
      { Android: we know exactly which finger is up from UpId. }
      HasUpId := false;
      TouchIndex := FindTouchWithId(UpId);
      if TouchIndex = -1 then
      begin
        WritelnWarning('TFmxTouchDispatcher', 'Could not find touch with UpId %d', [UpId]);
        Exit;
      end;
      FingerUp(TouchIndex);
    end else
    begin
      {$ifdef ANDROID}
      WritelnWarning('TFmxTouchDispatcher', 'HandleUp called without UpId on Android. This makes handling released fingers slightly wrong on Android with FMX, as FMX (without UpId hack) does not provide exact information which finger is released.');
      {$endif ANDROID}

      { iOS: Touches[..].Action is reliable. }
      for TouchIndex := 0 to High(Touches) do
        if Touches[TouchIndex].Action in [TTouchAction.Up, TTouchAction.Cancel] then
          FingerUp(TouchIndex);
    end;
  end;

  { Handle Action = Cancel: send Up for all fingers.

    From user perspective: You can cause Action=Cancel, on Android and iOS,
    when switching apps while holding finger.

    Note that on iOS, only Touches[..].Action can be Cancel,
    the Action parameter will be still "Up" in this case.
    This is handled by HandleUp, not HandleCancel.
    FMX sends Action = Cancel only on Android. }
  procedure HandleCancel;
  var
    FingerIndex: TFingerIndex;
  begin
    HasUpId := false;

    for FingerIndex in FingerIndexes.Values do
      DoUp(FingerIndex, FingerPositions[FingerIndex]);

    FingerIndexes.Clear;
    FingerPositions.Clear;
  end;

  { Handle Action = Move.

    Note that all fingers (still pressed) are passed here.
    We detect movement of each finger individually by comparing
    with previous position. }
  procedure HandleMove;
  var
    TouchIndex: Integer;
    FingerIndex: TFingerIndex;
    OldPosition, NewPosition: TVector2;
  begin
    for TouchIndex := 0 to High(Touches) do
      if Touches[TouchIndex].Action = TTouchAction.Move then
      begin
        if not FingerIndexes.TryGetValue(Touches[TouchIndex].Id, FingerIndex) then
        begin
          WritelnWarning('TFmxTouchDispatcher', 'Finger with id %d moved, but was not pressed', [Touches[TouchIndex].Id]);
          Continue;
        end;
        NewPosition := PositionToLocal(Touches[TouchIndex].Location);
        if not FingerPositions.TryGetValue(FingerIndex, OldPosition) then
        begin
          WritelnWarning('TFmxTouchDispatcher', 'Could not find previous position for finger index %d', [FingerIndex]);
          OldPosition := NewPosition;
          // the condition below will mean we don't send OnMove for this finger this time.
        end;
        if not TVector2.PerfectlyEquals(OldPosition, NewPosition) then
        begin
          FingerPositions.AddOrSetValue(FingerIndex, NewPosition);
          DoMotion(FingerIndex, OldPosition, NewPosition);
        end;
      end;
  end;

begin
  Assert(PositionsInControl <> nil);
  case Action of
    TTouchAction.Down:
      HandleDown;
    TTouchAction.Up:
      HandleUp;
    TTouchAction.Cancel:
      HandleCancel;
    TTouchAction.Move:
      HandleMove;
  end;
end;

procedure TFmxTouchDispatcher.DoDown(const FingerIndex: TFingerIndex; const Position: TVector2);
begin
  if Assigned(OnDown) then
    OnDown(FingerIndex, Position);
end;

procedure TFmxTouchDispatcher.DoUp(const FingerIndex: TFingerIndex; const Position: TVector2);
begin
  if Assigned(OnUp) then
    OnUp(FingerIndex, Position);
end;

procedure TFmxTouchDispatcher.DoMotion(const FingerIndex: TFingerIndex;
  const OldPosition, NewPosition: TVector2);
begin
  if Assigned(OnMotion) then
    OnMotion(FingerIndex, OldPosition, NewPosition);
end;

{$endif CASTLE_HANDLE_FMX_TOUCH}

end.
