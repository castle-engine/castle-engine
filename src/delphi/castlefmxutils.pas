{
  Copyright 2023-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for Delphi FMX (FireMonkey). }
unit CastleFmxUtils;

{$I castleconf.inc}

interface

uses Types, Generics.Collections, System.Messaging,
  FMX.Dialogs, FMX.Forms, FMX.Types, FMX.Controls,
  {$ifdef ANDROID}
  // used by TFmxTouchDispatcher
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText,
  {$endif}
  CastleFileFilters, CastleVectors, CastleKeysMouse;

{ Convert file filters into FMX Dialog.Filter, Dialog.FilterIndex.
  Suitable for both open and save dialogs (in FMX, TSaveDialog
  descends from TOpenDialog).

  Input filters are either given as a string FileFilters
  (encoded just like for TFileFilterList.AddFiltersFromString),
  or as TFileFilterList instance.

  Output filters are set as appropriate properties of given Dialog instance.

  When AllFields is false, then filters starting with "All " in the name,
  like "All files", "All images", are not included in the output.

  @groupBegin }
procedure FileFiltersToDialog(const FileFilters: string;
  const Dialog: TOpenDialog; const AllFields: boolean = true); overload;
procedure FileFiltersToDialog(FFList: TFileFilterList;
  const Dialog: TOpenDialog; const AllFields: boolean = true); overload;
{ @groupEnd }

{$ifdef LINUX}
{ Set mouse position, in screen coordinates.
  WidgetNativeHandle is a GTK widget pointer, used to determine
  display and screen where to set the pointer. }
procedure FmxSetMousePos(const WidgetNativeHandle: Pointer;
  const Point: TPointF);
{$endif}

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

    procedure AttachToForm(const AForm: TCommonCustomForm);
    procedure DetachFromForm;

    { Call this when TForm.OnTouch occured.
      (It can be attached directly, like Form.OnTouch := FTouchDispatcher.FormTouch.)

      This will call @link(OnDown), @link(OnUp), or @link(OnMotion) as appropriate. }
    procedure FormTouch(Sender: TObject; const Touches: TTouches;
      const Action: TTouchAction);
  end;

implementation

uses
  {$ifdef ANDROID}
  // for FAndroidTouchListener in TFmxTouchDispatcher
  Androidapi.Input, FMX.Platform.Android, FMX.Platform.UI.Android,
  {$endif ANDROID}
  SysUtils, CTypes, CastleLog;

procedure FileFiltersToDialog(const FileFilters: string;
  const Dialog: TOpenDialog; const AllFields: boolean = true);
var
  OutFilter: String;
  OutFilterIndex: Integer;
begin
  TFileFilterList.LclFmxFiltersFromString(FileFilters,
    OutFilter, OutFilterIndex, AllFields);
  Dialog.Filter := OutFilter;
  Dialog.FilterIndex := OutFilterIndex;
end;

procedure FileFiltersToDialog(FFList: TFileFilterList;
  const Dialog: TOpenDialog; const AllFields: boolean = true);
var
  OutFilter: String;
  OutFilterIndex: Integer;
begin
  FFList.LclFmxFilters(OutFilter, OutFilterIndex, AllFields);
  Dialog.Filter := OutFilter;
  Dialog.FilterIndex := OutFilterIndex;
end;

{$ifdef LINUX}
type
  PGdkDevice = Pointer;
  PGdkScreen = Pointer;
  PGdkDisplay = Pointer;
  PGdkDeviceManager = Pointer;
  PGtkWidget = Pointer;

procedure gdk_device_warp(Device: PGdkDevice; Screen: PGdkScreen; X, Y: CInt); cdecl; external 'libgtk-3.so.0';

//function gdk_display_get_default: PGdkDisplay; cdecl; external 'libgtk-3.so.0';
function gtk_widget_get_display(widget: PGtkWidget): PGdkDisplay; cdecl; external 'libgtk-3.so.0';
function gdk_display_get_device_manager(display: PGdkDisplay): PGdkDeviceManager; cdecl; external 'libgtk-3.so.0';
function gdk_device_manager_get_client_pointer(manager: PGdkDeviceManager): PGdkDevice; cdecl; external 'libgtk-3.so.0';

//function gdk_screen_get_default: PGdkScreen; cdecl; external 'libgtk-3.so.0';
function gtk_widget_get_screen(widget: PGtkWidget): PGdkScreen; cdecl; external 'libgtk-3.so.0';

procedure FmxSetMousePos(const WidgetNativeHandle: Pointer;
  const Point: TPointF);
var
  Display: PGdkDisplay;
  DeviceManager: PGdkDeviceManager;
  Device: PGdkDevice;
  Screen: PGdkScreen;
begin
  { Get main device (mouse) following
    https://stackoverflow.com/questions/24844489/how-to-use-gdk-device-get-position .
    We use Display and Screen that correspond to the WidgetNativeHandle .
    Then we "warp" (set mouse position) using
    https://docs.gtk.org/gdk3/method.Device.warp.html . }

  Display := gtk_widget_get_display(WidgetNativeHandle);
  DeviceManager := gdk_display_get_device_manager(Display);
  Device := gdk_device_manager_get_client_pointer(DeviceManager);

  Screen := gtk_widget_get_screen(WidgetNativeHandle);

  //WritelnLog('Mouse', 'Warping mouse to %f %f', [Point.X, Point.Y]);
  gdk_device_warp(Device, Screen, Round(Point.X), Round(Point.Y));
end;
{$endif}

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

end.