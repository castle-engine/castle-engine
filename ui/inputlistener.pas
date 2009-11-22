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

uses Keys;

type
  { An abstract handler of mouse/keyboard input.
    Various UI "controls" descend from this, and override
    it's KeyDown / MouseDown etc. methods. For example TGLMenu.
    Various UI "windows" (things that directly receive messages
    from something outside, like operating system, windowing library etc.)
    accept TInputListener classes, and pass to them inputs. }
  TInputListener = class
  protected
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

    procedure Idle(const CompSpeed: Single); virtual;
  end;

implementation

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

procedure TInputListener.Idle(const CompSpeed: Single);
begin
end;

end.
