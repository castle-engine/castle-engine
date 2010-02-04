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

uses SysUtils, Classes, KeysMouse, KambiUtils, KambiClassUtils;

type
  { Basic user interface container. This may be a window
    (like TGLUIWindow) or some Lazarus control (like TKamOpenGLControl
    component). }
  IUIContainer = interface
  ['{0F0BA87D-95C3-4520-B9F9-CDF30015FDB3}']
    procedure SetMousePosition(const NewMouseX, NewMouseY: Integer);

    function GetMouseX: Integer;
    function GetMouseY: Integer;

    property MouseX: Integer read GetMouseX;
    property MouseY: Integer read GetMouseY;

    function GetWidth: Integer;
    function GetHeight: Integer;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;

    function GetMousePressed: TMouseButtons;
    function GetPressed: TKeysPressed;

    { Mouse buttons currently pressed. }
    property MousePressed: TMouseButtons read GetMousePressed;

    { Keys currently pressed. }
    property Pressed: TKeysPressed read GetPressed;

    { Called by controls within this container when some TUIControl.Cursor
      changes. This is called by a IUIContainer interface, that's why
      it can remain as private method of actual container class.

      This recalculates the final cursor of container, looking at
      Container's UseControls and Cursor property of focused control.

      When UseControls change, or when you add / remove some control
      from the Controls list, or when you move mouse (focused changes)
      this will also be automatically called
      (since final container cursor may also change then). }
    procedure UpdateMouseCursor;
  end;

  { In what projection TUIControl.Draw will be called.
    See TUIControl.Draw, TUIControl.DrawStyle. }
  TUIControlDrawStyle = (dsNone, ds2D, ds3D);

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
    (Note that this is contrary to the usual OpenGL 2D system,
    where (0, 0) is left-bottom window corner.) }
  TUIControl = class(TComponent)
  private
    FExclusiveEvents: boolean;
    FOnVisibleChange: TNotifyEvent;
    FContainerWidth, FContainerHeight: Cardinal;
    FContainerSizeKnown: boolean;
    FContainer: IUIContainer;
    FCursor: TMouseCursor;
    FOnCursorChange: TNotifyEvent;
    FDisableContextInitClose: Cardinal;
    procedure SetCursor(const Value: TMouseCursor);
  protected
    { Container (window containing the control) size, as known by this control,
      undefined when ContainerSizeKnown = @false. This is simply collected at
      ContainerResize calls here.
      @groupBegin }
    property ContainerWidth: Cardinal read FContainerWidth;
    property ContainerHeight: Cardinal read FContainerHeight;
    property ContainerSizeKnown: boolean read FContainerSizeKnown;
    { @groupEnd }
    procedure SetContainer(const Value: IUIContainer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    (*Handle key events.
      Return @true if the key event was somehow handled.

      In this class this always returns @false, when implementing
      in descendants you should override it like

      @longCode(#
  Result := inherited;
  if Result then Exit;
  { ... And do the job here.
    In other words, the handling of keys in inherited
    class should have a priority. }
#)

      @groupBegin *)
    function KeyDown(Key: TKey; C: char): boolean; virtual;
    function KeyUp(Key: TKey; C: char): boolean; virtual;
    { @groupEnd }

    { Handle mouse events.
      @groupBegin }
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; virtual;
    function MouseDown(const Button: TMouseButton): boolean; virtual;
    function MouseUp(const Button: TMouseButton): boolean; virtual;
    { @groupEnd }

    { Control may do here anything that must be continously repeated.
      This is called often by the container.
      E.g. camera handles here falling down due to gravity
      in Walker mode, rotating model in Examine mode, and many more.

      @param(CompSpeed Should be calculated like TFramesPerSecond.IdleSpeed,
        and usually it's in fact just taken from TGLWindow.Fps.IdleSpeed.)

      HandleMouseAndKeys says if this control can
      handle currently pressed keys and mouse buttons.
      Only if it is @true, the control can look at Container.Pressed
      and Container.MousePressed. HandleMouseAndKeys will be passed as @true
      only to the controls under the mouse, and only if previous
      controls under the mouse did not set LetOthersHandleMouseAndKeys = @false.

      Also (only when HandleMouseAndKeys = @true) the control can
      set LetOthersHandleMouseAndKeys. In fact, it's by default
      set to @code(not ExclusiveEvents), which (since ExclusiveEvents by default
      is @true) usually means @false. This reflects the fact that "normal"
      UI controls, that actually take screen space implied by PositionInside,
      want to block controls underneath from handling keys/mouse.
      For example, when pressing key "left" over TGLMenu, you do not
      want to let the Camera to also capture this left key down.

      @italic(More reasoning behind HandleMouseAndKeys:)

      Note that the "Idle" events are called
      differently that other mouse and key events.

      Mouse and key events
      return whether the event was somehow "handled", and the container
      passes them only to the controls under the mouse (decided by
      PositionInside). And as soon as some control says it "handled"
      the event, other controls (even if under the mouse) will not
      receive the event.

      This approach is not suitable for idle events. Some controls
      need to do the idle job all the time,
      regardless of whether the control is under the mouse and regardless
      of what other controls already did. So all controls receive
      Idle calls.

      So the "handled" status is passed through HandleMouseAndKeys
      and controlled by LetOthersHandleMouseAndKeys.
      If a control is not under the mouse, it will receive HandleMouseAndKeys
      = @false, and LetOthersHandleMouseAndKeys return value is ignored.
      If a control is under the mouse, it will receive HandleMouseAndKeys
      = @true and has the power to disallow mouse/key handling for all other
      controls by LetOthersHandleMouseAndKeys. }
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); virtual;

    { Is given position inside this control.
      Returns always @false in this class. }
    function PositionInside(const X, Y: Integer): boolean; virtual;

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

    { Prepare your resources, right before drawing. }
    procedure BeforeDraw; virtual;

    { Draw a control. If you want your Draw called automatically by the
      window, return something <> dsNone from DrawStyle,
      and draw your control inside Draw.

      Do's and don't's when implementing Draw:

      @unorderedList(
        @item(All controls with DrawStyle = ds3D are drawn first,
          with projection that you set yourself. Usually you should
          use TKamSceneManager, which sets projection automatically for you
          to something suitable, see TKamSceneManager.ApplyProjection and
          TVRMLGLScene.GLProjection.

          Then al controls with DrawStyle = ds2D are drawn.
          For them, OpenGL projection is guaranteed to be set to standard 2D
          projection, like by @code(gluOrtho2D(0, Container.Width, 0, Container.Height)).)

        @item(The only OpenGL state you can change carelessly is:
          @unorderedList(
            @itemSpacing Compact
            @item The modelview matrix value.
            @item The raster position.
            @item The color (glColor), material (glMaterial) values.
          )
          Every other change should be wrapped in appropriate glPushAttrib / glPopAttrib.)

        @item(Things that are guaranteed about OpenGL state when Draw is called:
          @unorderedList(
            @itemSpacing Compact
            @item The current matrix is modelview, and it's value is identity.
            @item Only for DrawStyle = ds2D: the raster position is at (0, 0).
            @item Only for DrawStyle = ds2D: Texturing, depth test, lighting are turned off.
          )
          If you require anything else, set this yourself.)
      )

      @groupBegin }
    function DrawStyle: TUIControlDrawStyle; virtual;
    procedure Draw(const Focused: boolean); virtual;
    { @groupEnd }

    { Called always when containing window size changes.
      Also, when the control is first inserted into the window controls list
      (like @link(TGLUIWindow.Controls)), it will also receive
      initial ContainerResize event. So every member of of Controls list
      knows window width / height.

      In this class, this sets values of ContainerWidth, ContainerHeight, ContainerSizeKnown
      properties. }
    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); virtual;

    { Container of this control. When adding control to container's Controls
      list (like TGLUIWindow.Controls) container will automatically
      set itself here, an when removing from container this will be changed
      back to @nil.

      May be @nil if this control is not yet inserted into any container.
      May also be @nil since not all containers have to implement
      right now IUIContainer interface, it's not crucial for most controls
      to work. }
    property Container: IUIContainer read FContainer write SetContainer;

    { Initialize your OpenGL resources.

      This is called when OpenGL context of the container is created.
      Also called when the control is added to the already existing context.
      In other words, this is the moment when you can initialize
      OpenGL resources, like display lists, VBOs, OpenGL texture names, etc. }
    procedure GLContextInit; virtual;

    { Destroy your OpenGL resources.

      Called when OpenGL context of the container is destroyed.
      Also called when controls is removed from the container
      @code(Controls) list. Also called from the destructor.

      You should release here any resources that are tied to the
      OpenGL context. In particular, the ones created in GLContextInit. }
    procedure GLContextClose; virtual;

    { When non-zero, container will not call GLContextInit and
      GLContextClose (when control is added/removed to/from the
      @code(Controls) list).

      This is useful, although should be used with much caution:
      you're expected to call controls GLContextInit /
      GLContextClose on your own when this is non-zero. Example usage is
      when the same control is often added/removed to/from the @code(Controls)
      list, and the window (with it's OpenGL context) stays open for a longer
      time. In such case, default (when DisableContextInitClose = 0) behavior
      will often release (only to be forced to reinitialize again) OpenGL
      resources of the control. Some controls have large OpenGL
      resources (for example TVRMLGLScene keeps display lists, textures etc.,
      and TVRMLGLAnimation keeps all the scenes) --- so constantly
      reinitializing them may eat a noticeable time.

      By using non-zero DisableContextInitClose you can disable this behavior.

      In particular, TGLMode uses this trick, to avoid releasing and
      reinitializing OpenGL resources for controls only temporarily
      removed from the @link(TGLUIWindow.Controls) list. }
    property DisableContextInitClose: Cardinal
      read FDisableContextInitClose write FDisableContextInitClose;

    { Design note: ExclusiveEvents is not published now, as it's too "obscure"
      (for normal usage you don't want to deal with it). Also, it's confusing
      on TVRMLScene, the name suggests it relates to ProcessEvents (VRML events,
      totally not related to this property that is concerned with handling
      TUIControl events.) }

    { Should we disable further mouse / keys handling for events that
      we already handled in this control. If @true, then our events will
      return @true for mouse and key events handled.

      This means that events will not be simultaneously handled by both this
      control and some other (or camera or normal window callbacks),
      which is usually more sensible, but sometimes less functional. }
    property ExclusiveEvents: boolean
      read FExclusiveEvents write FExclusiveEvents default true;

    { Mouse cursor over this control.
      When user moves mouse over the Container, the currently focused
      (topmost under the cursor) control determines the mouse cursor look. }
    property Cursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { Event called when the @link(Cursor) property changes.
      This event is, in normal circumstances, used by the Container,
      so you should not use it in your own programs. }
    property OnCursorChange: TNotifyEvent
      read FOnCursorChange write FOnCursorChange;
  end;

  TUIControlList = class(TKamObjectList)
  private
    function GetItem(const I: Integer): TUIControl;
    procedure SetItem(const I: Integer; const Item: TUIControl);
  public
    property Items[I: Integer]: TUIControl read GetItem write SetItem; default;
    procedure Add(Item: TUIControl);
    procedure Insert(Index: Integer; Item: TUIControl);
  end;

implementation

constructor TUIControl.Create(AOwner: TComponent);
begin
  inherited;
  FExclusiveEvents := true;
  FCursor := mcDefault;
end;

destructor TUIControl.Destroy;
begin
  GLContextClose;
  inherited;
end;

function TUIControl.KeyDown(Key: TKey; C: char): boolean;
begin
  Result := false;
end;

function TUIControl.KeyUp(Key: TKey; C: char): boolean;
begin
  Result := false;
end;

function TUIControl.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
begin
  Result := false;
end;

function TUIControl.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := false;
end;

function TUIControl.MouseUp(const Button: TMouseButton): boolean;
begin
  Result := false;
end;

procedure TUIControl.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
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

function TUIControl.DrawStyle: TUIControlDrawStyle;
begin
  Result := dsNone;
end;

procedure TUIControl.BeforeDraw;
begin
end;

procedure TUIControl.Draw(const Focused: boolean);
begin
end;

procedure TUIControl.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
  FContainerWidth := AContainerWidth;
  FContainerHeight := AContainerHeight;
  FContainerSizeKnown := true;
end;

procedure TUIControl.GLContextInit;
begin
end;

procedure TUIControl.GLContextClose;
begin
end;

procedure TUIControl.SetCursor(const Value: TMouseCursor);
begin
  if Value <> FCursor then
  begin
    FCursor := Value;
    if Container <> nil then Container.UpdateMouseCursor;
    if Assigned(OnCursorChange) then OnCursorChange(Self);
  end;
end;

procedure TUIControl.SetContainer(const Value: IUIContainer);
begin
  FContainer := Value;
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

procedure TUIControlList.Add(Item: TUIControl);
begin
  inherited Add(Item);
end;

procedure TUIControlList.Insert(Index: Integer; Item: TUIControl);
begin
  inherited Insert(Index, Item);
end;

end.
