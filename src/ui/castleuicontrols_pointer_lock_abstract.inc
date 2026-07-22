{%MainUnit castleuicontrols.pas}
{
  Copyright 2010-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Abstract API to perform "pointer lock". }

{$ifdef read_interface}

//type

  { Logic of "pointer lock".

    When @link(Active) is @true:

    @orderedList(
      @item(Mouse cursor is hidden.)

      @item(Mouse movement is unbounded by the window/canvas borders.
        At least it @italic(seems to be unbounded),
        from the perspective of user dragging something with mouse
        or rotating 3D view in FPS game.)

      @item(The only way to get useful "delta of move" from @link(TInputMotion)
        is to use @link(TCastleAbstractPointerLock.Delta) like this:

        @longCode(
        function TViewMain.Motion(const Event: TInputMotion): Boolean;
        begin
          Result := inherited;
          if Result then Exit; // allow the ancestor to handle event

          MoveSomething := MoveSomething + Container.PointerLock.Delta(Event);
        end;
        )
      )

      @item(
        The position of the mouse, as reported by @link(TInputMotion.Position)
        and @link(TInputMotion.OldPosition), are undefined.

        Some implementations (desktop) may expose there "chaotic movement"
        (as we forcefully reposition mouse to the middle of the screen).
        Some implementations (web) may expose there "no movement at all"
        (as the "pointer lock API" on WWW browsers pretends that mouse didn't
        move at all).
      )
    )

    This is automatically used by @link(TCastleWalkNavigation.MouseLook).

    You can use it yourself explicitly for custom dragging.
    Follow the template below.
    See the engine example examples/user_interface/dragging_test/ for
    a working code demonstrating this to drag 2D objects with mouse.

    @longCode(#

    function TViewPlayGame.Press(const Event: TInputPressRelease): Boolean;
    begin
      Result := inherited;
      if Result then Exit;

      if Event.IsMouseButton(buttonLeft) then
        Container.PointerLock.Active := true;
    end;

    function TViewPlayGame.Release(const Event: TInputPressRelease): Boolean;
    begin
      Result := inherited;
      if Result then Exit;

      if Event.IsMouseButton(buttonLeft) then
        Container.PointerLock.Active := false;
    end;

    function TViewPlayGame.Motion(const Event: TInputMotion): Boolean;
    var
      Delta: TVector2;
    begin
      Result := inherited;
      if Result then Exit;

      if Container.PointerLock.Active then
      begin
        Delta := Container.PointerLock.Delta(Event);
        // Use Delta to perform any logic you want.
      end;
    end;
    #)

    This is an abstract class, as the implementation of pointer lock is
    platform-specific.

    @unorderedList(
      @item(On some backends (non-web right now, using @link(TCastleDesktopPointerLock)),
        we actually hide the mouse cursor and keep repositioning mouse
        to the middle of the screen.)

      @item(On web, we use the
        @url(https://developer.mozilla.org/en-US/docs/Web/API/Pointer_Lock_API
        pointer lock API) of browsers, which does all the work for us,
        but comes with some quirks:

        @unorderedList(
          @item(Pointer lock is activated with some delay after setting
            @link(Active) to @true, and potentially user may reject it
            altogether.)

          @item(It has to be activated in response (or shortly after)
            to a user interaction with a page (e.g. mouse click).)

          @item(It may be cancelled by the user at any time
            (typically by pressing @italic(Escape) key).
            When this happens, @link(Active) is set to @false and
            callbacks registered with @link(AddUserCancelledListener)
            are called.)

          @item(See @url(https://castle-engine.io/web#_known_limitations
            known limitations of web backend) for more details about pointer
            lock on web.)
        )
      )
    )

    Never create instances of this class directly.
    Use only the instance provided by @link(TCastleContainer.PointerLock),
    which provides the correct implementation for the current platform,
    tied with the current @link(TCastleContainer container) and window. }
  TCastleAbstractPointerLock = class
  strict private
    FContainer: TCastleContainer;
    FActive: Boolean;
    FOnUserCancelled: TNotifyEventList;
    FController: TCastleUserInterface;
    FControllerObserver: TFreeNotificationObserver;
    procedure SetActive(const Value: Boolean);
    procedure SetController(const Value: TCastleUserInterface);
    procedure ControllerFreeNotification(const Sender: TFreeNotificationObserver);
  protected
    { Start or stop mouse look. }
    procedure ActiveChanged(const NewValue: Boolean); virtual; abstract;

    { Container performing this pointer lock logic. }
    property Container: TCastleContainer read FContainer;

    { Set @link(Active) to @false without calling virtual @link(ActiveChanged).
      Calls callbacks registered with @link(AddUserCancelledListener).
      To be used by descendants when we lost pointer lock because of user action,
      e.g. on web when user cancels pointer lock by pressing Escape key.

      Descendants are responsible to reset their own state before/after calling
      this, since this does not call @link(ActiveChanged), so descendants
      are not notified about the cancellation in any other way.
      E.g. web implementation sets "FEffective := false" before calling this. }
    procedure CancelActive;
  public
    constructor Create(const AContainer: TCastleContainer);
    destructor Destroy; override;

    { Enable or disable pointer lock.
      Note that on some platforms (web) user can cancel pointer lock at any time,
      in which case this property turns to @false.
      On other platforms (desktop) this stays completely under control
      of the application. }
    property Active: Boolean read FActive write SetActive;

    { Read current delta of mouse movement, assuming that mouse look is active.

      Use this only from @link(TCastleUserInterface.Motion) method overrides,
      as it needs TInputMotion as input argument to calculate the mouse movement
      delta. Typically, this means you call this from your
      @url(https://castle-engine.io/views view)'s @code(Motion) method.

      Always returns zero if @link(Active) is @false.
      Remember that on some platforms (web) user may cancel the "pointer lock"
      mode at any moment, so be sure to tolerate it by doing nothing when
      this is zero.

      It also returns zero if @link(Active) is @true but
      we have not yet prepared for this mode.
      On desktops, "not prepared" means that mouse was not positioned
      correctly yet. On web, "not prepared" means that browser/user didn't yet
      confirm pointer lock mode.
      Again, the simple solution is to do nothing when this is zero.

      The returned value is in final device pixels, so it is not affected by
      UI scale. If you want to adjust to UI scale (so user will have to move
      mouse by more pixels, when on a larger screen, when UI is larger)
      then just use @code(Delta / UiScale). }
    function Delta(const Event: TInputMotion): TVector2; virtual; abstract;

    { Next motion event is expected to have wild movement, so ignore it.
      This is internal, used by container implementations.
      @exclude }
    procedure InternalIgnoreNextMotion; virtual;

    { Called when user cancels pointer lock, e.g. by pressing Escape key on web.
      When this is triggered, the @link(Active) property is already set to @false.
      @groupBegin }
    procedure AddUserCancelledListener(const Event: TNotifyEvent);
    procedure RemoveUserCancelledListener(const Event: TNotifyEvent);
    { @groupEnd }

    { Optionally, when requesting pointer lock (by setting @link(Active) to @true),
      you can set this property to indicate what component is responsible
      for the pointer lock.

      This controller, and not others, should process deltas during pointer lock.
      This controller should also set @link(Active) to @false when no longer needed.
      This is just a convention: since in practice it doesn't make sense for
      mutliple components to process pointer lock deltas at the same time,
      this property allows them to coordinate "which one processes deltas now"
      and avoid conflicts.
      E.g. when we have multiple viewports and multiple TCastleWalkNavigation
      instances, only one of them can effectively process pointer lock deltas.

      There is one guaranteed effect of setting this:
      we will make sure to pass @link(Motion) events to this controller,
      first, even if it is not focused. This allows to capture motion events
      even by TCastleWalkNavigation that doesn't fill the middle of the screen.
      This makes sense, since mouse position in general should not matter
      when pointer lock is active. }
    property Controller: TCastleUserInterface read FController write SetController;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleAbstractPointerLock.Create(const AContainer: TCastleContainer);
begin
  inherited Create;
  FContainer := AContainer;
  FOnUserCancelled := TNotifyEventList.Create;
  FControllerObserver := TFreeNotificationObserver.Create(nil);
  FControllerObserver.OnFreeNotification := {$ifdef FPC}@{$endif} ControllerFreeNotification;
end;

destructor TCastleAbstractPointerLock.Destroy;
begin
  FreeAndNil(FOnUserCancelled);
  Controller := nil;
  FreeAndNil(FControllerObserver);
  inherited;
end;

procedure TCastleAbstractPointerLock.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    // flush before changing Active
    if not (csDestroying in Container.ComponentState) then
      Container.FlushCollectedMotion;
    FActive := Value;
    ActiveChanged(Value);
  end;
end;

procedure TCastleAbstractPointerLock.CancelActive;
begin
  // flush before changing Active
  if not (csDestroying in Container.ComponentState) then
    Container.FlushCollectedMotion;
  FActive := false;
  FOnUserCancelled.ExecuteAll(Self);
end;

procedure TCastleAbstractPointerLock.InternalIgnoreNextMotion;
begin
  // nothing in this class
end;

procedure TCastleAbstractPointerLock.AddUserCancelledListener(const Event: TNotifyEvent);
begin
  FOnUserCancelled.Add(Event);
end;

procedure TCastleAbstractPointerLock.RemoveUserCancelledListener(const Event: TNotifyEvent);
begin
  FOnUserCancelled.Remove(Event);
end;

procedure TCastleAbstractPointerLock.SetController(const Value: TCastleUserInterface);
begin
  if FController <> Value then
  begin
    FController := Value;
    FControllerObserver.Observed := Value;
  end;
end;

procedure TCastleAbstractPointerLock.ControllerFreeNotification(const Sender: TFreeNotificationObserver);
begin
  Controller := nil;
end;

{$endif read_implementation}
