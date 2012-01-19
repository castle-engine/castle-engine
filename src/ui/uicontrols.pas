{
  Copyright 2009-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ User interface (2D) basic classes. }
unit UIControls;

interface

uses SysUtils, Classes, KeysMouse, CastleUtils, CastleClassUtils;

type
  { Basic user interface container. This may be a window
    (like TCastleWindowCustom) or some Lazarus control (like TCastleControlCustom
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

    function GetTooltipX: Integer;
    function GetTooltipY: Integer;

    property TooltipX: Integer read GetTooltipX;
    property TooltipY: Integer read GetTooltipY;

    { Called by controls within this container when something could
      change the container focused control (or it's cursor).
      In practice, called when TUIControl.Cursor or TUIControl.PositionInside
      results change. This is called by a IUIContainer interface, that's why
      it can remain as private method of actual container class.

      This recalculates the focused control and the final cursor of
      the container, looking at Container's UseControls,
      testing PositionInside with current mouse position,
      and looking at Cursor property of the focused control.

      When UseControls change, or when you add / remove some control
      from the Controls list, or when you move mouse (focused changes)
      this will also be automatically called
      (since focused control or final container cursor may also change then). }
    procedure UpdateFocusAndMouseCursor;
  end;

  { In what projection TUIControl.Draw will be called.
    See TUIControl.Draw, TUIControl.DrawStyle. }
  TUIControlDrawStyle = (dsNone, ds2D, ds3D);

  { Base class for things that listen to user input: cameras and 2D controls. }
  TInputListener = class(TComponent)
  private
    FOnVisibleChange: TNotifyEvent;
    FContainerWidth, FContainerHeight: Cardinal;
    FContainerSizeKnown: boolean;
    FContainer: IUIContainer;
    FCursor: TMouseCursor;
    FOnCursorChange: TNotifyEvent;
    FExclusiveEvents: boolean;
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
    { Called when @link(Cursor) changed.
      In TUIControl class, just calls OnCursorChange. }
    procedure DoCursorChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    (*Handle key events.
      Return @true if the key event was somehow handled.

      In this class this always returns @false, when implementing
      in descendants you should override it like

      @longCode(#
  Result := inherited;
  if Result or (not GetExists) then Exit;
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
    function MouseWheel(const Scroll: Single; const Vertical: boolean): boolean; virtual;
    { @groupEnd }

    { Control may do here anything that must be continously repeated.
      This is called often by the container.
      E.g. camera handles here falling down due to gravity
      in Walker mode, rotating model in Examine mode, and many more.

      @param(CompSpeed Should be calculated like TFramesPerSecond.IdleSpeed,
        and usually it's in fact just taken from TCastleWindowBase.Fps.IdleSpeed.)

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
      For example, when pressing key "left" over TCastleOnScreenMenu, you do not
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

      @seeAlso TCastleWindowBase.AllowSuspendForInput }
    function AllowSuspendForInput: boolean; virtual;

    { Called always when containing window size changes.
      Also, when the control is first inserted into the window controls list
      (like @link(TCastleWindowCustom.Controls)), it will also receive
      initial ContainerResize event. So every member of of Controls list
      knows window width / height.

      In this class, this sets values of ContainerWidth, ContainerHeight, ContainerSizeKnown
      properties. }
    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); virtual;

    { Container of this control. When adding control to container's Controls
      list (like TCastleWindowCustom.Controls) container will automatically
      set itself here, an when removing from container this will be changed
      back to @nil.

      May be @nil if this control is not yet inserted into any container.
      May also be @nil since not all containers have to implement
      right now IUIContainer interface, it's not crucial for most controls
      to work. }
    property Container: IUIContainer read FContainer write SetContainer;

    { Mouse cursor over this control.
      When user moves mouse over the Container, the currently focused
      (topmost under the cursor) control determines the mouse cursor look. }
    property Cursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { Event called when the @link(Cursor) property changes.
      This event is, in normal circumstances, used by the Container,
      so you should not use it in your own programs. }
    property OnCursorChange: TNotifyEvent
      read FOnCursorChange write FOnCursorChange;

    { Design note: ExclusiveEvents is not published now, as it's too "obscure"
      (for normal usage you don't want to deal with it). Also, it's confusing
      on TCastleSceneCore, the name suggests it relates to ProcessEvents (VRML events,
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
    (Note that this is contrary to the usual OpenGL 2D system,
    where (0, 0) is left-bottom window corner.) }
  TUIControl = class(TInputListener)
  private
    FDisableContextOpenClose: Cardinal;
    FFocused: boolean;
    FGLInitialized: boolean;
    FExists: boolean;
    procedure SetExists(const Value: boolean);
  protected
    { Return whether item really exists, see @link(Exists).
      It TUIControl class, returns @link(Exists) value.
      May be modified in subclasses, to return something more complicated. }
    function GetExists: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Is given position inside this control.
      Returns always @false in this class. }
    function PositionInside(const X, Y: Integer): boolean; virtual;

    { Prepare your resources, right before drawing. }
    procedure BeforeDraw; virtual;

    { Draw a control. If you want your Draw called automatically by the
      window, return something <> dsNone from DrawStyle,
      and draw your control inside Draw.

      Do's and don't's when implementing Draw:

      @unorderedList(
        @item(All controls with DrawStyle = ds3D are drawn first,
          with projection that you set yourself. Usually you should
          use TCastleSceneManager, which sets projection automatically for you
          to something suitable, see TCastleSceneManager.ApplyProjection and
          TCastleScene.GLProjection.

          Then all the controls with DrawStyle = ds2D are drawn.
          For them, OpenGL projection is guaranteed to be set to standard 2D
          projection, like by @code(gluOrtho2D(0, Container.Width, 0, Container.Height)).
          And OpenGL viewport is guaranteed to contain whole container.)

        @item(The only OpenGL state you can change carelessly is:
          @unorderedList(
            @itemSpacing Compact
            @item The modelview matrix value.
            @item The raster position.
            @item The color (glColor), material (glMaterial) values.
            @item The line width, point size.
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

      When @link(GetExists) is @false, remember to do nothing in Draw,
      and return dsNone in DrawStyle.

      @groupBegin }
    function DrawStyle: TUIControlDrawStyle; virtual;
    procedure Draw; virtual;
    { @groupEnd }

    { Draw a tooltip of this control. If you want to have tooltip for
      this control detected, you have to override TooltipStyle
      to return something <> dsNone.
      Then the TCastleWindowBase.TooltipVisible will be detected,
      and your DrawTooltip will be called.

      So you can draw your tooltip either in overridden DrawTooltip,
      and/or somewhere else when you see that TCastleWindowBase.TooltipVisible is @true.
      (Tooltip is always drawn for TCastleWindowBase.Focus control.)
      But in both cases, make sure to override TooltipStyle to return
      something <> dsNone.

      The values of ds2D and ds3D are interpreted in the same way
      as DrawStyle. And DrawTooltip is called in the same way as @link(Draw).
      DrawTooltip is always called as a last (front-most) 2D or 3D control.

      @groupBegin }
    function TooltipStyle: TUIControlDrawStyle; virtual;
    procedure DrawTooltip; virtual;
    { @groupEnd }

    { Initialize your OpenGL resources.

      This is called when OpenGL context of the container is created.
      Also called when the control is added to the already existing context.
      In other words, this is the moment when you can initialize
      OpenGL resources, like display lists, VBOs, OpenGL texture names, etc. }
    procedure GLContextOpen; virtual;

    { Destroy your OpenGL resources.

      Called when OpenGL context of the container is destroyed.
      Also called when controls is removed from the container
      @code(Controls) list. Also called from the destructor.

      You should release here any resources that are tied to the
      OpenGL context. In particular, the ones created in GLContextOpen. }
    procedure GLContextClose; virtual;

    property GLInitialized: boolean read FGLInitialized default false;

    { When non-zero, container will not call GLContextOpen and
      GLContextClose (when control is added/removed to/from the
      @code(Controls) list).

      This is useful, although should be used with much caution:
      you're expected to call controls GLContextOpen /
      GLContextClose on your own when this is non-zero. Example usage is
      when the same control is often added/removed to/from the @code(Controls)
      list, and the window (with it's OpenGL context) stays open for a longer
      time. In such case, default (when DisableContextOpenClose = 0) behavior
      will often release (only to be forced to reinitialize again) OpenGL
      resources of the control. Some controls have large OpenGL
      resources (for example TCastleScene keeps display lists, textures etc.,
      and TCastlePrecalculatedAnimation keeps all the scenes) --- so constantly
      reinitializing them may eat a noticeable time.

      By using non-zero DisableContextOpenClose you can disable this behavior.

      In particular, TGLMode uses this trick, to avoid releasing and
      reinitializing OpenGL resources for controls only temporarily
      removed from the @link(TCastleWindowCustom.Controls) list. }
    property DisableContextOpenClose: Cardinal
      read FDisableContextOpenClose write FDisableContextOpenClose;

    { Called when this control becomes or stops being focused.
      In this class, they simply update Focused property. }
    procedure SetFocused(const Value: boolean); virtual;

    property Focused: boolean read FFocused write SetFocused;

  published
    { Not existing control is not visible, it doesn't receive input
      and generally doesn't exist from the point of view of user.
      You can also remove this from controls list (like
      @link(TCastleWindowCustom.Controls)), but often it's more comfortable
      to set this property to false. }
    property Exists: boolean read FExists write SetExists default true;
  end;

  { TUIControl with position (in Left, Bottom fields).

    This takes care of some internal quirks with saving Left property
    correctly. (Because TComponent doesn't declare, but saves/loads a "magic"
    property named Left during streaming. This is used to place non-visual
    components on the form. Our Left is completely independent from this.) }
  TUIControlPos = class(TUIControl)
  private
    FLeft: Integer;
    FBottom: Integer;
    procedure ReadRealLeft(Reader: TReader);
    procedure WriteRealLeft(Writer: TWriter);

    Procedure ReadLeft(Reader: TReader);
    Procedure ReadTop(Reader: TReader);
    Procedure WriteLeft(Writer: TWriter);
    Procedure WriteTop(Writer: TWriter);

    procedure SetLeft(const Value: Integer);
    procedure SetBottom(const Value: Integer);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  published
    property Left: Integer read FLeft write SetLeft stored false default 0;
    property Bottom: Integer read FBottom write SetBottom default 0;
  end;

  TUIControlList = class(TCastleObjectList)
  private
    function GetItem(const I: Integer): TUIControl;
    procedure SetItem(const I: Integer; const Item: TUIControl);
  public
    property Items[I: Integer]: TUIControl read GetItem write SetItem; default;
    procedure Add(Item: TUIControl);
    procedure Insert(Index: Integer; Item: TUIControl);

    { BeginDisableContextOpenClose disables sending
      TUIControl.GLContextOpen and TUIControl.GLContextClose to all the controls
      on the list. EndDisableContextOpenClose ends this.
      They work by increasing / decreasing the TUIControl.DisableContextOpenClose
      for all the items on the list.

      @groupBegin }
    procedure BeginDisableContextOpenClose;
    procedure EndDisableContextOpenClose;
    { @groupEnd }
  end;

implementation

{ TInputListener ------------------------------------------------------------- }

constructor TInputListener.Create(AOwner: TComponent);
begin
  inherited;
  FExclusiveEvents := true;
  FCursor := mcDefault;
end;

function TInputListener.KeyDown(Key: TKey; C: char): boolean;
begin
  Result := false;
end;

function TInputListener.KeyUp(Key: TKey; C: char): boolean;
begin
  Result := false;
end;

function TInputListener.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
begin
  Result := false;
end;

function TInputListener.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := false;
end;

function TInputListener.MouseWheel(const Scroll: Single; const Vertical: boolean): boolean;
begin
  Result := false;
end;

function TInputListener.MouseUp(const Button: TMouseButton): boolean;
begin
  Result := false;
end;

procedure TInputListener.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
end;

procedure TInputListener.VisibleChange;
begin
  if Assigned(OnVisibleChange) then
    OnVisibleChange(Self);
end;

function TInputListener.AllowSuspendForInput: boolean;
begin
  Result := true;
end;

procedure TInputListener.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
  FContainerWidth := AContainerWidth;
  FContainerHeight := AContainerHeight;
  FContainerSizeKnown := true;
end;

procedure TInputListener.SetCursor(const Value: TMouseCursor);
begin
  if Value <> FCursor then
  begin
    FCursor := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
    DoCursorChange;
  end;
end;

procedure TInputListener.DoCursorChange;
begin
  if Assigned(OnCursorChange) then OnCursorChange(Self);
end;

procedure TInputListener.SetContainer(const Value: IUIContainer);
begin
  FContainer := Value;
end;

{ TUIControl ----------------------------------------------------------------- }

constructor TUIControl.Create(AOwner: TComponent);
begin
  inherited;
  FExists := true;
end;

destructor TUIControl.Destroy;
begin
  GLContextClose;
  inherited;
end;

function TUIControl.PositionInside(const X, Y: Integer): boolean;
begin
  Result := false;
end;

function TUIControl.DrawStyle: TUIControlDrawStyle;
begin
  Result := dsNone;
end;

procedure TUIControl.BeforeDraw;
begin
end;

procedure TUIControl.Draw;
begin
end;

function TUIControl.TooltipStyle: TUIControlDrawStyle;
begin
  Result := dsNone;
end;

procedure TUIControl.DrawTooltip;
begin
end;

procedure TUIControl.GLContextOpen;
begin
  FGLInitialized := true;
end;

procedure TUIControl.GLContextClose;
begin
  FGLInitialized := false;
end;

function TUIControl.GetExists: boolean;
begin
  Result := FExists;
end;

procedure TUIControl.SetFocused(const Value: boolean);
begin
  FFocused := Value;
end;

procedure TUIControl.SetExists(const Value: boolean);
begin
  { Exists is typically used in PositionInside implementations,
    so changing it must case UpdateFocusAndMouseCursor. }
  if FExists <> Value then
  begin
    FExists := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
  end;
end;

{ TUIControlPos -------------------------------------------------------------- }

{ We store Left property value in file under "tuicontrolpos_real_left" name,
  to avoid clashing with TComponent magic "left" property name.
  The idea how to do this is taken from TComponent's own implementation
  of it's "left" magic property (rtl/objpas/classes/compon.inc). }

procedure TUIControlPos.ReadRealLeft(Reader: TReader);
begin
  FLeft := Reader.ReadInteger;
end;

procedure TUIControlPos.WriteRealLeft(Writer: TWriter);
begin
  Writer.WriteInteger(FLeft);
end;

Procedure TUIControlPos.ReadLeft(Reader: TReader);
var
  D: LongInt;
begin
  D := DesignInfo;
  LongRec(D).Lo:=Reader.ReadInteger;
  DesignInfo := D;
end;

Procedure TUIControlPos.ReadTop(Reader: TReader);
var
  D: LongInt;
begin
  D := DesignInfo;
  LongRec(D).Hi:=Reader.ReadInteger;
  DesignInfo := D;
end;

Procedure TUIControlPos.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

Procedure TUIControlPos.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

procedure TUIControlPos.DefineProperties(Filer: TFiler);
Var Ancestor : TComponent;
    Temp : longint;
begin
  { Don't call inherited that defines magic left/top.
    This would make reading design-time "left" broken, it seems that our
    declaration of Left with "stored false" would then prevent the design-time
    Left from ever loading.

    Instead, we'll save design-time "Left" below, under a special name. }

  Filer.DefineProperty('TUIControlPos_RealLeft', @ReadRealLeft, @WriteRealLeft,
    FLeft <> 0);

  { Code from fpc/trunk/rtl/objpas/classes/compon.inc }
  Temp:=0;
  Ancestor:=TComponent(Filer.Ancestor);
  If Assigned(Ancestor) then Temp:=Ancestor.DesignInfo;
  Filer.Defineproperty('TUIControlPos_Design_Left',@readleft,@writeleft,
                       (longrec(DesignInfo).Lo<>Longrec(temp).Lo));
  Filer.Defineproperty('TUIControlPos_Design_Top',@readtop,@writetop,
                       (longrec(DesignInfo).Hi<>Longrec(temp).Hi));
end;

procedure TUIControlPos.SetLeft(const Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
  end;
end;

procedure TUIControlPos.SetBottom(const Value: Integer);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
  end;
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

procedure TUIControlList.BeginDisableContextOpenClose;
var
  I: Integer;
begin
 for I := 0 to Count - 1 do
   with Items[I] do
     DisableContextOpenClose := DisableContextOpenClose + 1;
end;

procedure TUIControlList.EndDisableContextOpenClose;
var
  I: Integer;
begin
 for I := 0 to Count - 1 do
   with Items[I] do
     DisableContextOpenClose := DisableContextOpenClose - 1;
end;

end.
