{
  Copyright 2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ User interface (2D) basic classes. }
unit UIControls;

interface

{$define read_interface}

uses KeysMouse, SysUtils, KambiUtils, KambiClassUtils;

type
  { Basic user interface control class. All controls derive from this class,
    overriding chosen methods to react to some events.
    Various user interface "windows" (things that directly receive messages
    from something outside, like operating system, windowing library etc.)
    implement support for such controls.

    Control may handle mouse/keyboard input, see KeyDown, MouseDown etc.
    methods.

    Various methods return boolean saying if input event is handled.
    The idea is that not handled events are passed to the next
    control suitable. Handled events are generally not processed more
    --- otherwise the same event could be handled by more than one listener,
    which is bad. Generally, return ExclusiveEvents if anything (possibly)
    was done (you changed any field value etc.) as a result of this,
    and only return @false when you're absolutely sure that nothing was done
    by this control.

    All screen (mouse etc.) coordinates passed here should be in the usual
    window system coordinates, that is (0, 0) is left-top window corner.
    (Note that this is contrary to usual OpenGL 2D system,
    where (0, 0) is left-bottom window corner.) }
  TUIControl = class
  private
    FExclusiveEvents: boolean;
  public
    constructor Create;

    (*Handle key press event.
      Returns @true if the key was somehow handled.

      In this class this always returns @false, when implementing
      in descendants you should override it like

      @longCode(#
  Result := inherited;
  if Result then Exit;
  { ... And do the job here.
    In other words, the handling of keys in inherited
    class should have a priority. }
#)

      @param(KeysDown You can pass here a boolean table indicating
        which keys are pressed. You can pass @nil if you don't know this.

        (Contents of table passed here will never be modified anyway.
        This is a pointer only so that you can simply pass @nil here.)) *)
    function KeyDown(Key: TKey; C: char; KeysDown: PKeysBooleans): boolean; virtual;

    { Called when user moves the mouse.

      Like for KeyDown and Idle, you can pass KeysDown as
      @nil if you don't know this. }
    function MouseMove(const OldX, OldY, NewX, NewY: Single;
      const MousePressed: TMouseButtons; KeysDown: PKeysBooleans): boolean; virtual;

    (*Handle mouse down event.

      Just like KeyDown, this returns whether the event was handled.
      Descendants should always override this like
      @longCode(#
  Result := inherited;
  if Result then Exit;
  { ... do the job here ... }
#) *)
    function MouseDown(const MouseX, MouseY: Single; Button: TMouseButton;
      const MousePressed: TMouseButtons): boolean; virtual;

    function MouseUp(const MouseX, MouseY: Single; Button: TMouseButton;
      const MousePressed: TMouseButtons): boolean; virtual;

    { Call this often, to respond to user actions and to perform
      other tasks. E.g. for navigator: falling down due to gravity
      in Walker mode, rotating model in Examine mode, and many more.

      @param(CompSpeed Should be calculated like TFramesPerSecond.IdleSpeed,
        and usually it's in fact just taken from TGLWindow.Fps.IdleSpeed.)

      @param(KeysDown What keys are pressed currently ?
        You pass here a pointer to a boolean table indicating
        which keys are currently pressed. Or you can pass @nil
        here if you don't know it. Just like for @link(KeyDown) method.)

      @param(CharactersDown What character codes are pressed currently ?
        Analogous to KeysDown, you can pass here a pointer or @nil.)

      @param(MousePressed Which mouse buttons are currently pressed ?) }
    procedure Idle(const CompSpeed: Single;
      KeysDown: PKeysBooleans;
      CharactersDown: PCharactersBooleans;
      const MousePressed: TMouseButtons); virtual;

    { Is given position inside this control.
      Returns always @false in this class. }
    function PositionInside(const X, Y: Single): boolean; virtual;

    { Should we disable further mouse / keys handling for events that
      we already handled in this control. If @true, then our events will
      return @true for mouse and key events handled.

      This means that events will not be simultaneously handled by both this
      control and some other (or navigator or normal window callbacks),
      which is usually more sensible, but sometimes less functional. }
    property ExclusiveEvents: boolean
      read FExclusiveEvents write FExclusiveEvents default true;
  end;

  TObjectsListItem_1 = TUIControl;
  {$I objectslist_1.inc}
  TUIControlsList = TObjectsList_1;

{$undef read_interface}

implementation

{$define read_implementation}
{$I objectslist_1.inc}

constructor TUIControl.Create;
begin
  inherited;
  FExclusiveEvents := true;
end;

function TUIControl.KeyDown(Key: TKey; C: char; KeysDown: PKeysBooleans): boolean;
begin
  Result := false;
end;

function TUIControl.MouseMove(const OldX, OldY, NewX, NewY: Single;
  const MousePressed: TMouseButtons; KeysDown: PKeysBooleans): boolean;
begin
  Result := false;
end;

function TUIControl.MouseDown(const MouseX, MouseY: Single; Button: TMouseButton;
  const MousePressed: TMouseButtons): boolean;
begin
  Result := false;
end;

function TUIControl.MouseUp(const MouseX, MouseY: Single; Button: TMouseButton;
  const MousePressed: TMouseButtons): boolean;
begin
  Result := false;
end;

procedure TUIControl.Idle(const CompSpeed: Single;
  KeysDown: PKeysBooleans;
  CharactersDown: PCharactersBooleans;
  const MousePressed: TMouseButtons);
begin
end;

function TUIControl.PositionInside(const X, Y: Single): boolean;
begin
  Result := false;
end;

end.
