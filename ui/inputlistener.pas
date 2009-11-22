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
    accept TInputListener classes, and pass to them inputs. }
  TInputListener = class
  public
    procedure KeyDown(Key: TKey; C: char); virtual;

    { Called when user moves the mouse.
      NewX, NewY is in OpenGL 2d screen coordinates, so usually
      (when you call this from TGLWindow.OnMouseMove) you will
      have to flip the NewY like @code(Glwin.Height - NewY).

      TODO: add ParentWindow, with abstract Width / Height,
      and then you can pass here normal NewX / NewY in traditional window coords. }
    procedure MouseMove(const NewX, NewY: Single;
      const MousePressed: TMouseButtons); virtual;

    procedure MouseDown(const MouseX, MouseY: Single; Button: TMouseButton;
      const MousePressed: TMouseButtons); virtual;
    procedure MouseUp(const MouseX, MouseY: Single; Button: TMouseButton;
      const MousePressed: TMouseButtons); virtual;

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

procedure TInputListener.KeyDown(Key: TKey; C: char);
begin
end;

procedure TInputListener.MouseMove(const NewX, NewY: Single;
  const MousePressed: TMouseButtons);
begin
end;

procedure TInputListener.MouseDown(const MouseX, MouseY: Single; Button: TMouseButton;
  const MousePressed: TMouseButtons);
begin
end;

procedure TInputListener.MouseUp(const MouseX, MouseY: Single; Button: TMouseButton;
  const MousePressed: TMouseButtons);
begin
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
