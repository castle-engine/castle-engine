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

uses SysUtils, Classes, KeysMouse, KambiUtils, KambiClassUtils, Contnrs;

type
  { Basic user interface container. This may be a window
    (like TGLWindowNavigated) or some Lazarus control (like TKamOpenGLControl
    component). }
  IUIContainer = interface
    procedure SetMousePosition(const NewMouseX, NewMouseY: Integer);
  end;

  { Basic user interface control class. All controls derive from this class,
    overriding chosen methods to react to some events.
    Various user interface containers (things that directly receive messages
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
  TUIControl = class(TComponent)
  private
    FExclusiveEvents: boolean;
    FOnVisibleChange: TNotifyEvent;
    FMouseLook: boolean;
    FContainerWidth, FContainerHeight: Cardinal;
    FContainerSizeKnown: boolean;
    FContainer: IUIContainer;
  protected
    { Container (window containing the control) size, as known by this control,
      undefined when ContainerSizeKnown = @false. This is simply collected at
      ContainerResize calls here.
      @groupBegin }
    property ContainerWidth: Cardinal read FContainerWidth;
    property ContainerHeight: Cardinal read FContainerHeight;
    property ContainerSizeKnown: boolean read FContainerSizeKnown;
    { @groupEnd }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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

      @param(Pressed You can pass here information indicating
        which keys are pressed. You can pass @nil if you don't know this.) *)
    function KeyDown(Key: TKey; C: char; Pressed: TKeysPressed): boolean; virtual;

    { Called when user moves the mouse.

      Like for KeyDown and Idle, you can pass Pressed as
      @nil if you don't know this. }
    function MouseMove(const OldX, OldY, NewX, NewY: Integer;
      const MousePressed: TMouseButtons; Pressed: TKeysPressed): boolean; virtual;

    (*Handle mouse down event.

      Just like KeyDown, this returns whether the event was handled.
      Descendants should always override this like
      @longCode(#
  Result := inherited;
  if Result then Exit;
  { ... do the job here ... }
#) *)
    function MouseDown(const MouseX, MouseY: Integer; Button: TMouseButton;
      const MousePressed: TMouseButtons): boolean; virtual;

    function MouseUp(const MouseX, MouseY: Integer; Button: TMouseButton;
      const MousePressed: TMouseButtons): boolean; virtual;

    { Call this often, to respond to user actions and to perform
      other tasks. E.g. for navigator: falling down due to gravity
      in Walker mode, rotating model in Examine mode, and many more.

      @param(CompSpeed Should be calculated like TFramesPerSecond.IdleSpeed,
        and usually it's in fact just taken from TGLWindow.Fps.IdleSpeed.)

      @param(Pressed What keys are pressed currently ?
        You pass here an instance of TKeysPressed (for example
        from TGLWindow.Pressed, or TKamOpenGLControl.Pressed).
        Or you can pass @nil
        here if you don't know it. Just like for @link(KeyDown) method.)

      @param(MousePressed Which mouse buttons are currently pressed ?) }
    procedure Idle(const CompSpeed: Single;
      Pressed: TKeysPressed;
      const MousePressed: TMouseButtons); virtual;

    { Is given position inside this control.
      Returns always @false in this class. }
    function PositionInside(const X, Y: Integer): boolean; virtual;

    { Should we disable further mouse / keys handling for events that
      we already handled in this control. If @true, then our events will
      return @true for mouse and key events handled.

      This means that events will not be simultaneously handled by both this
      control and some other (or navigator or normal window callbacks),
      which is usually more sensible, but sometimes less functional. }
    property ExclusiveEvents: boolean
      read FExclusiveEvents write FExclusiveEvents default true;

    { Called always when some visible part of this control
      changes. In the simplest case, this is used by the controls manager to
      know when we need to redraw the control.

      In this class this simply calls OnVisibleChange (if assigned). }
    procedure VisibleChange; virtual;

    { Called always when some visible part of this control
      changes. In the simplest case, this is used by the controls manager to
      know when we need to redraw the control.

      Be careful when handling this event, various changes may cause this,
      so be prepared to handle OnVisibleChange at every time.

      @seealso VisibleChange }
    property OnVisibleChange: TNotifyEvent
      read FOnVisibleChange write FOnVisibleChange;

    { Allow window containing this control to suspend waiting for user input.
      Typically you want to override this to return @false when you do
      something in the overridden @link(Idle) method.

      In this class, this simply returns always @true.

      @seeAlso TGLWindow.AllowSuspendForInput }
    function AllowSuspendForInput: boolean; virtual;

    { This indicates that control wants to use mouse look mode.
      The window containing such control should then hide the
      mouse cursor, and force mouse position to the middle of the window
      (to avoid the situation when mouse movement is blocked by screen borders).
      The idea is that your MouseMove is not interested in mouse
      positions anymore (in fact, user cannot choose mouse position
      since we hide the mouse from him), but we're interested in relative
      mouse movements (whether user drags mouse up, down, left, right etc.).

      Note that actually making the muse look useful requires some support
      from the descendant. Although this property is available for all
      TUIControl instances, in fact it makes sense to set it @true only for
      specific classes. For now, only TWalkNavigator actually handles this
      sensibly, doing usual "mouse look" navigation mode popular in FPS games. }
    property MouseLook: boolean read FMouseLook write FMouseLook default false;

    { Draw 2D control. If you want your Draw2D called automatically by the
      window, return @true from IsDraw2D and draw your control inside Draw2D.

      Do's and don't's when implementing Draw2D:

      @unorderedList(
        @item(OpenGL projection is guaranteed to be set to standard 2D
          projection, like by @code(gluOrtho2D(0, Glwin.Width, 0, Glwin.Height)).)

        @item(The only OpenGL state you can change carelessly is:
          @unorderedList(
            @itemSpacing Compact
            @item The modelview matrix value.
            @item The raster position.
            @item The color (glColor), material (glMaterial) values.
          )
          Every other change should be wrapped in appropriate glPushAttrib / glPopAttrib.)

        @item(Things that are guaranteed about OpenGL state when Draw2D is called:
          @unorderedList(
            @itemSpacing Compact
            @item The current matrix is modelview, and it's value is identity.
            @item The raster position is at (0, 0).
            @item Texturing, depth test, lighting are turned off.
          )
          If you require anything else, set this yourself.)
      )

      @groupBegin }
    function IsDraw2D: boolean; virtual;
    procedure Draw2D(const Focused: boolean); virtual;
    { @groupEnd }

    { Called always when containing window size changes.
      Also, when the control is first inserted into the window controls list
      (like @link(TGLWindowNavigated.Controls)), it will also receive
      initial ContainerResize event. So every member of of Controls list
      knows window width / height.

      In this class, this sets values of ContainerWidth, ContainerHeight, ContainerSizeKnown
      properties. }
    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); virtual;

    { Container of this control. When adding control to container's Controls
      list (like TGLWindowNavigated.Controls) container will automatically
      set itself here, an when removing from container this will be changed
      back to @nil.

      May be @nil if this control is not yet inserted into any container.
      May also be @nil since not all containers have to implement
      right now IUIContainer interface, it's not crucial for most controls
      to work. }
    property Container: IUIContainer read FContainer write FContainer;

    { Called when OpenGL context of the window is destroyed.
      Control should clear here any resources that are tied to the GL context.
      This will be also automatically called from destructor. }
    procedure GLContextClose; virtual;
  end;

  TUIControlList = class(TKamObjectList)
  private
    function GetItem(const I: Integer): TUIControl;
    procedure SetItem(const I: Integer; const Item: TUIControl);
  public
    property Items[I: Integer]: TUIControl read GetItem write SetItem; default;
  end;

implementation

constructor TUIControl.Create(AOwner: TComponent);
begin
  inherited;
  FExclusiveEvents := true;
end;

destructor TUIControl.Destroy;
begin
  GLContextClose;
  inherited;
end;

function TUIControl.KeyDown(Key: TKey; C: char; Pressed: TKeysPressed): boolean;
begin
  Result := false;
end;

function TUIControl.MouseMove(const OldX, OldY, NewX, NewY: Integer;
  const MousePressed: TMouseButtons; Pressed: TKeysPressed): boolean;
begin
  Result := false;
end;

function TUIControl.MouseDown(const MouseX, MouseY: Integer; Button: TMouseButton;
  const MousePressed: TMouseButtons): boolean;
begin
  Result := false;
end;

function TUIControl.MouseUp(const MouseX, MouseY: Integer; Button: TMouseButton;
  const MousePressed: TMouseButtons): boolean;
begin
  Result := false;
end;

procedure TUIControl.Idle(const CompSpeed: Single;
  Pressed: TKeysPressed; const MousePressed: TMouseButtons);
begin
end;

function TUIControl.PositionInside(const X, Y: Integer): boolean;
begin
  Result := false;
end;

procedure TUIControl.VisibleChange;
begin
  if Assigned(OnVisibleChange) then
    OnVisibleChange(Self);
end;

function TUIControl.AllowSuspendForInput: boolean;
begin
  Result := true;
end;

function TUIControl.IsDraw2D: boolean;
begin
  Result := false;
end;

procedure TUIControl.Draw2D(const Focused: boolean);
begin
end;

procedure TUIControl.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
  FContainerWidth := AContainerWidth;
  FContainerHeight := AContainerHeight;
  FContainerSizeKnown := true;
end;

procedure TUIControl.GLContextClose;
begin
end;

{ TUIControlList ------------------------------------------------------------- }

function TUIControlList.GetItem(const I: Integer): TUIControl;
begin
  Result := TUIControl(inherited Items[I]);
end;

procedure TUIControlList.SetItem(const I: Integer; const Item: TUIControl);
begin
  (inherited Items[I]) := Item;
end;

end.
