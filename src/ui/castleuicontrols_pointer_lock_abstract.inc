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

    Between @link(Start) and @link(Stop):

    @orderedList(
      @item(Mouse cursor is hidden.)

      @item(Mouse movement is unbounded by the window/canvas borders.
        At least it @italic(seems to be unbounded),
        from the perspective of user dragging something with mouse
        or rotating 3D view in FPS game.)

      @item(Only the "delta" of mouse movement (@link(TInputMotion.Delta)) is useful.

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
    On some backends (non-web, see @link(TDesktopPointerLock)),
    we actually hide the mouse cursor and
    keep repositioning mouse to the middle of the screen.
    On web, we use the @url(https://developer.mozilla.org/en-US/docs/Web/API/Pointer_Lock_API
    pointer lock API) of browsers, which does all the work for us,
    and also is activated with potential delay,
    and also may be cancelled by the user at any time (e.g. by pressing Escape
    key).

    Never create instances of this class directly.
    Use @link(TCastleContainer.PointerLock) instead which provides the correct
    implementation for the current platform, tied with the current container
    and window. }
  TCastleAbstractPointerLock = class
  strict private
    FContainer: TCastleContainer;
    FActive: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    { Start or stop mouse look. }
    procedure ActiveChanged(const NewValue: Boolean); virtual; abstract;

    { Container performing this pointer lock logic. }
    property Container: TCastleContainer read FContainer;
  public
    constructor Create(const AContainer: TCastleContainer);

    { Enable or disable pointer lock. See @link(PointerLockEffective).
      Note that on some platforms (web) user can cancel pointer lock at any time,
      in which case this property turns to @false.
      On other platforms (desktop) this stays completely under control
      of the application. }
    property Active: Boolean read FActive write SetActive;

    { Read current delta of mouse movement, assuming that mouse look is active.
      Use this only from @link(TCastleContainer.Motion) overrides,
      it needs TInputMotion as input argument to calculate the mouse movement delta.

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
      then just use @code(Delta / UiScale).

      @param(ControlRectControlRect
        The rectangle (in final device coordinates,
        like from @link(TCastleUserInterface.RenderRect)) of control over which we
        should do pointer lock.
        Some implementations (web) may ignore the ControlRect.
        Others (desktop) may use it to reposition mouse to the middle
        of ControlRect.) }
    function Delta(const Event: TInputMotion;
      const ControlRect: TFloatRectangle): TVector2; overload; virtual; abstract;

    { Same as @link(Delta) but without ControlRect. This is useful when you
      want to use the default ControlRect (e.g. container rectangle). }
    function Delta(const Event: TInputMotion): TVector2; overload;

    { Next motion event is expected to have wild movement, so ignore it.
      This is internal, used by container implementations.
      @exclude }
    procedure InternalIgnoreNextMotion; virtual; abstract;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleAbstractPointerLock.Create(const AContainer: TCastleContainer);
begin
  inherited Create;
  FContainer := AContainer;
end;

procedure TCastleAbstractPointerLock.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    ActiveChanged(Value);
  end;
end;

function TCastleAbstractPointerLock.Delta(const Event: TInputMotion): TVector2;
begin
  Result := Delta(Event, FloatRectangle(FContainer.PixelsRect));
end;

{$endif read_implementation}
