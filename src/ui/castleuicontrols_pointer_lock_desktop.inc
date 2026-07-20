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
{ Pointer lock implementation on desktops. }

{$ifdef read_interface}

//type

  { Pointer lock implementation on desktops.
    We hide the mouse cursor (using @link(TCastleContainer)
    internal mechanism, so we override whatever cursor is set by
    @link(TCastleUserInterface.Cursor) or @link(TCastleContainer.OverrideCursor)).
    We keep repositioning mouse to the middle of the screen (or specified
    ControlRect). }
  TCastleDesktopPointerLock = class(TCastleAbstractPointerLock)
  strict private
    FIgnoreNextMotion: Boolean;
    FWaitingForMiddle: Boolean;
    FMotionToSubtract: TVector2;
  protected
    procedure ActiveChanged(const NewValue: Boolean); override;
  public
    function Delta(const Event: TInputMotion;
      const ControlRect: TFloatRectangle): TVector2; override;
    procedure InternalIgnoreNextMotion; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TCastleDesktopPointerLock.ActiveChanged(const NewValue: Boolean);
begin
  if NewValue then
  begin
    FIgnoreNextMotion := false;
    FWaitingForMiddle := false;
    FMotionToSubtract := TVector2.Zero;
    Container.FInternalOverrideCursor := mcForceNone;
  end else
  begin
    Container.FInternalOverrideCursor := mcDefault;
  end;
end;

function TCastleDesktopPointerLock.Delta(const Event: TInputMotion;
  const ControlRect: TFloatRectangle): TVector2;

  { There's a lot of tricky things in implementing this correctly.

    1. Initially we tried to set mouse position to container center very often
       (in every Update, and/or in every MouseLookDelta).

       But then:
       Doing "Result := Event.Position - Event.OldPosition" is bad.
       Instead "Result := Event.Position - Middle" is better:

       That is because setting MousePosition may (but doesn't have to)
       generate another Motion in the container to destination position.

       Subtracting Middle (instead of Container.Position, previous
       known mouse position) solves it. This way

       - The Motion caused by MakeMousePositionedForMouseLook will not do
         anything bad, as MouseChange wil be 0 then.

       - In case MakeMousePositionedForMouseLook does not cause Motion,
         we will not measure the changes as too much. Consider this:

          - player moves mouse to MiddleX-10
          - Motion is generated, I rotate camera by "-10" horizontally
          - Setting MousePosition sets mouse to the Middle,
            but this time no Motion is generated
          - player moved mouse to MiddleX+10. Although mouse was
            positioned on Middle, TCastleWindow thinks that the mouse
            is still positioned on Middle-10, and I will get "+20" move
            for player (while I should get only "+10")

    2. Unfortunately "Result := Event.Position - Middle"
       is also bad, in case there's unspecified delay in applying SetMousePosition.
       Effectively, we would apply some differences in positions many times,
       making the speed of MouseLookDelta faster than normal dragging
       (and the exact factor "how much faster" can depend on mouse sensitivity,
       how much is SetMousePosition delayed (on Windows 10 it can be delayed a lot,
       more than one frame very often)).

       This was easily seen in Unholy fight, and dragging_test example in CGE.

       The new fix is that we allow mouse in a large area in the center,
       and we do SetMousePosition only when it's outside of this large area.
       This allows to easily reject Motion events that report such move,
       as they will have ValidArea(new position)=true but ValidArea(old position)=false.

       In case the Motion event has a summed value of "moving to center by SetMousePosition
       + movement from user", we will ignore too much, but this should
       not have noticeable effect on speed (as it will happen seldom).

    3. Actually, 2 was also invalid.

       The new fix is to just use motion delta always,
       and use explicit SettingMousePositionCausesMotion knowwledge
       to eventually subtract FMotionToSubtract.

    4. Another problem is when player switches to another window, moves the mouse,
       than goes Alt+Tab back to our window.
       Next mouse move would cause huge change,
       because it's really *not* from the middle of the screen.

       Solution to this is to use FIgnoreNextMotion
       for the next event.

    5. Note that earlier I tried to reposition mouse for mouse look at various
       moments. I tried calling MakeMousePositionedForMouseLook from Press, Motion,
       Update... all these approaches had some drawbacks, and eventually
       they have one big drawback: this could result in Motion that isn't rejected
       below, causing weird first movement.
  }

  { Position is roughly within the center of the window. }
  function ValidArea(const P, Middle: TVector2; const Margin: Single): Boolean;
  begin
    Result := (Abs(P.X - Middle.X) < Margin * ControlRect.Width) and
              (Abs(P.Y - Middle.Y) < Margin * ControlRect.Height);
  end;

var
  Middle: TVector2;
begin
  Result := TVector2.Zero;
  if not Active then Exit;

  Middle := ControlRect.Center;
  { Round to integers, because if CastleWindow backend doesn't support
    sup-pixel accuracy for SetMousePosition, then it ignores the fractional
    part of SetMousePosition argument.
    Such fractional part would only cause later "error by 0.5 (or other epsilon)"
    when using FMotionToSubtract to modify Result. }
  Middle.X := Trunc(Middle.X);
  Middle.Y := Trunc(Middle.Y);

  if FIgnoreNextMotion then
  begin
    FIgnoreNextMotion := false;
    FWaitingForMiddle := false;
    FMotionToSubtract := TVector2.Zero;
    Exit;
  end;

  Result := Event.Position - Event.OldPosition;

  { If FWaitingForMiddle then we have set MousePosition := Middle,
    and now we wait for MousePosition to be reported near Middle.

    Until this happens, we don't apply FMotionToSubtract,
    (as it would cause unexpected shift before the "MousePosition := Middle"
    is actually realized).

    We still set MousePosition := Middle again
    (although we already wait for it to happen),
    in case waiting on "ValidArea(Event.Position, Middle, 1 / 8)" is not reliable.
  }
  if FWaitingForMiddle and ValidArea(Event.Position, Middle, 1 / 8) then
    FWaitingForMiddle := false;

  if (not FWaitingForMiddle) and Container.SettingMousePositionCausesMotion then
  begin
    Result := Result - FMotionToSubtract;
    FMotionToSubtract := TVector2.Zero;
  end;

  // When mouse gets too far away from Middle, move it back to Middle.
  if not ValidArea(Event.Position, Middle, 1 / 4) then
  begin
    if not FWaitingForMiddle then
      FMotionToSubtract := FMotionToSubtract + Middle - Event.Position;
    Container.MousePosition := Middle;
    FWaitingForMiddle := true;
  end;
end;

procedure TCastleDesktopPointerLock.InternalIgnoreNextMotion;
begin
  FIgnoreNextMotion := true;
end;

{$endif read_implementation}
