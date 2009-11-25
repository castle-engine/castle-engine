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

{ TInputListener class, a generic handler of mouse/keyboard input. }
unit InputListener;

interface

{$define read_interface}

uses Keys, SysUtils, KambiUtils, KambiClassUtils, Areas;

type
  { An abstract handler of mouse/keyboard input.
    Various UI "controls" descend from this, and override
    it's KeyDown / MouseDown etc. methods. For example TGLMenu.
    Various UI "windows" (things that directly receive messages
    from something outside, like operating system, windowing library etc.)
    accept TInputListener classes, and pass to them inputs.

    Various methods return boolean saying if input event is handled.
    The idea is that not handled events are passed to the next
    input listener suitable. Handled events are generally not processed more
    --- otherwise the same event could be handled by more than one listener,
    which is bad. Generally, return @true if anything (possibly) was done
    (you changed any field value etc.) as a result of this, and only return
    @false when you're absolutely sure that nothing was done by this control.

    All mouse coordinates passed here should be in usual window system
    coordinates, that is (0, 0) is left-top window corner.
    (Note that this is contrary to usual OpenGL 2D system,
    where (0, 0) is left-bottom window corner.) }
  TInputListener = class
  public
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

    { Area occupied on the window.
      Returns empty area in this class. }
    function Area: TArea; virtual;
  end;

  TObjectsListItem_1 = TInputListener;
  {$I objectslist_1.inc}
  TInputListenersList = TObjectsList_1;

{$undef read_interface}

implementation

{$define read_implementation}
{$I objectslist_1.inc}

function TInputListener.KeyDown(Key: TKey; C: char; KeysDown: PKeysBooleans): boolean;
begin
  Result := false;
end;

function TInputListener.MouseMove(const OldX, OldY, NewX, NewY: Single;
  const MousePressed: TMouseButtons; KeysDown: PKeysBooleans): boolean;
begin
  Result := false;
end;

function TInputListener.MouseDown(const MouseX, MouseY: Single; Button: TMouseButton;
  const MousePressed: TMouseButtons): boolean;
begin
  Result := false;
end;

function TInputListener.MouseUp(const MouseX, MouseY: Single; Button: TMouseButton;
  const MousePressed: TMouseButtons): boolean;
begin
  Result := false;
end;

procedure TInputListener.Idle(const CompSpeed: Single;
  KeysDown: PKeysBooleans;
  CharactersDown: PCharactersBooleans;
  const MousePressed: TMouseButtons);
begin
end;

function TInputListener.Area: TArea;
begin
  Result := EmptyArea;
end;

end.
