{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ On-screen menu displayed in OpenGL (TCastleOnScreenMenu). }
unit CastleOnScreenMenu;

interface

uses Classes, CastleVectors, CastleGLBitmapFonts, CastleControls,
  GL, CastleGLUtils, CastleUIControls, CastleKeysMouse, CastleColors,
  CastleGenericLists;

type
  TCastleOnScreenMenu = class;

  { 2D rectangle. Useful to detect in which rectangle a point
    (like a mouse position) is.

    We don't use standard Types.TRect for this, as TRect behavior is sometimes
    (see PtInRect) suited for rectangles anchored in top-left corner
    (while we are anchored in bottom-left corner). }
  TRectangle = object
    X0, Y0, Width, Height: Integer;
    function PointInside(const X, Y: Integer): boolean;
    procedure DrawBorder;
  end;
  PRectangle = ^TRectangle;

  TRectangleList = class(specialize TGenericStructList<TRectangle>)
  public
    { FindRectangle returns index of the rectangle that contains point (X, Y).

      @italic(It returns the index of the @bold(last) rectangle, that is:
      it searches from the end of the list.) This way the rectangles added
      later by Add method are treated as being on top of previous
      rectangles, which is more intuitive.

      Returns -1 if not found. }
    function FindRectangle(const X, Y: Integer): integer;
  end;

  { Attachment to a specific menu item of TCastleOnScreenMenu,
    for example may store a value associated with given menu option,
    and allow to change it by a slider. }
  TMenuAccessory = class
  private
    FOwnedByParent: boolean;
  public
    constructor Create;

    { Return the width you will need to display yourself.

      Note that this will be asked only from FixItemsRectangles
      from TCastleOnScreenMenu. So for example TMenuArgument
      is *not* supposed to return here something based on
      current TMenuArgument.Value,
      because we will not query GetWidth after every change of
      TMenuArgument.Value. Instead, TMenuArgument
      should return here the width of widest possible Value. }
    function GetWidth: Integer; virtual; abstract;

    { Draw yourself. Note that Rectangle.Width is for sure the same
      as you returned in GetWidth. }
    procedure Draw(const Rectangle: TRectangle); virtual; abstract;

    { This will be called if user will press a key when currently
      selected item has this TMenuAccessory.

      You can use ParentMenu to call ParentMenu.AccessoryValueChanged. }
    function KeyDown(Key: TKey; C: char;
      ParentMenu: TCastleOnScreenMenu): boolean; virtual;

    { This will be called if user will click mouse when currently
      selected item has this TMenuAccessory.

      MouseX, Y passed here are in coords where MouseY goes up from the bottom
      to the top. (This is different than usual window system coords.)

      This will be called only if MouseX and MouseY will be within
      appropriate Rectangle of this accessory. This Rectangle is also
      passed here, so you can e.g. calculate mouse position
      relative to this accessory as (MouseX - Rectangle.X0, MouseY - Rectangle.Y0).

      Note that while the user holds the mouse clicked (MousePressed <> []),
      the mouse is "grabbed" by this accessory, and even when the user
      will move the mouse over other items, they will not receive their
      MouseDown/MouseMove messages until user will let the mouse go.
      This prevents the bad situation when user does MouseDown e.g.
      on "Sound Volume" slider, slides it to the right and then accidentaly
      moves the mouse also a little down, and suddenly he's over "Music Volume"
      slider and he changed the position of "Music Volume" slider.

      You can use ParentMenu to call ParentMenu.AccessoryValueChanged. }
    function MouseDown(const MouseX, MouseY: Integer; Button: TMouseButton;
      const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu): boolean; virtual;

    { This will be called if user will move mouse over the currently selected
      menu item and menu item will have this accessory.

      Just like with MouseDown: This will be called only if NewX and NewY
      will be within appropriate Rectangle of accessory.
      You can use ParentMenu to call ParentMenu.AccessoryValueChanged. }
    procedure MouseMove(const NewX, NewY: Integer;
      const MousePressed: TMouseButtons;
      const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu); virtual;

    { Should this accessory be freed when TCastleOnScreenMenu using it is freed.
      Useful to set this to @false when you want to share one TMenuAccessory
      across more than one TCastleOnScreenMenu. }
    property OwnedByParent: boolean
      read FOwnedByParent write FOwnedByParent default true;
  end;

  { This is TMenuAccessory that will just display
    additional text (using some different color than Menu.CurrentItemColor)
    after the menu item. The intention is that the Value will be changeable
    by the user (while the basic item text remains constant).
    For example Value may describe "on" / "off" state of something,
    the name of some key currently assigned to some function etc. }
  TMenuArgument = class(TMenuAccessory)
  private
    FMaximumValueWidth: Integer;
    FValue: string;
  public
    constructor Create(const AMaximumValueWidth: Integer);

    property Value: string read FValue write FValue;

    property MaximumValueWidth: Integer
      read FMaximumValueWidth write FMaximumValueWidth;

    { Calculate text width using font used by TMenuArgument. }
    class function TextWidth(const Text: string): Integer;

    function GetWidth: Integer; override;
    procedure Draw(const Rectangle: TRectangle); override;
  end;

  { This is like TMenuArgument that displays boolean value
    (as "Yes" or "No").

    Don't access MaximumValueWidth or inherited Value (as string)
    when using this class --- this class should handle this by itself. }
  TMenuBooleanArgument = class(TMenuArgument)
  private
    FBooleanValue: boolean;
    procedure SetValue(const AValue: boolean);
  public
    constructor Create(const AValue: boolean);
    property Value: boolean read FBooleanValue write SetValue;
  end;

  TMenuSlider = class(TMenuAccessory)
  private
    FDisplayValue: boolean;
  protected
    { Draw a slider at given Position. If Position is outside 0..1, it is clamped
      to 0..1 (this way we do not show slider at some wild position if it's
      outside the expected range; but DrawSliderText will still show the true,
      unclamped, value). }
    procedure DrawSliderPosition(const Rectangle: TRectangle; const Position: Single);

    { Returns a value of Position, always in 0..1 range,
      that would result in slider being drawn at XCoord screen position
      by DrawSliderPosition.
      Takes Rectangle as the rectangle currently occupied by the whole slider. }
    function XCoordToSliderPosition(const XCoord: Single;
      const Rectangle: TRectangle): Single;

    procedure DrawSliderText(const Rectangle: TRectangle; const Text: string);
  public
    constructor Create;

    function GetWidth: Integer; override;
    procedure Draw(const Rectangle: TRectangle); override;

    { Should the Value be displayed as text ?
      Usually useful --- but only if the Value has some meaning for the user.
      If @true, then ValueToStr is used. }
    property DisplayValue: boolean
      read FDisplayValue write FDisplayValue default true;
  end;

  TMenuFloatSlider = class(TMenuSlider)
  private
    FBeginRange: Single;
    FEndRange: Single;
    FValue: Single;
  public
    constructor Create(const ABeginRange, AEndRange, AValue: Single);

    property BeginRange: Single read FBeginRange;
    property EndRange: Single read FEndRange;

    { Current value. When setting this property, always make sure
      that it's within the allowed range. }
    property Value: Single read FValue write FValue;

    procedure Draw(const Rectangle: TRectangle); override;

    function KeyDown(Key: TKey; C: char;
      ParentMenu: TCastleOnScreenMenu): boolean; override;

    function MouseDown(const MouseX, MouseY: Integer; Button: TMouseButton;
      const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu): boolean; override;

    procedure MouseMove(const NewX, NewY: Integer;
      const MousePressed: TMouseButtons;
      const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu); override;

    function ValueToStr(const AValue: Single): string; virtual;
  end;

  TMenuIntegerSlider = class(TMenuSlider)
  private
    FBeginRange: Integer;
    FEndRange: Integer;
    FValue: Integer;

    function XCoordToValue(
      const XCoord: Single; const Rectangle: TRectangle): Integer;
  public
    constructor Create(const ABeginRange, AEndRange, AValue: Integer);

    property BeginRange: Integer read FBeginRange;
    property EndRange: Integer read FEndRange;

    { Current value. When setting this property, always make sure
      that it's within the allowed range. }
    property Value: Integer read FValue write FValue;

    procedure Draw(const Rectangle: TRectangle); override;

    function KeyDown(Key: TKey; C: char;
      ParentMenu: TCastleOnScreenMenu): boolean; override;

    function MouseDown(const MouseX, MouseY: Integer; Button: TMouseButton;
      const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu): boolean; override;

    procedure MouseMove(const NewX, NewY: Integer;
      const MousePressed: TMouseButtons;
      const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu); override;

    function ValueToStr(const AValue: Integer): string; virtual;
  end;

  { How TCastleOnScreenMenu.Position will be interpreted.

    This type is used for two cases:
    @orderedList(

      @item(PositionRelativeMenu: specifies (for X or Y)
        what point of menu rectangle is affected by Position value.
        In this case,
        @unorderedList(
          @itemSpacing Compact
          @item(prLowerBorder means that we want to
            align left (or bottom) border of the menu rectangle,)
          @item(prMiddle means that we want to align middle of the menu rectangle,)
          @item(prHigherBorder means that we want to align right
            (or top) border of the menu rectangle.))
      )

      @item(PositionRelativeScreen: somewhat analogous.
        But specifies relative to which @italic(screen edge) we align.
        So
        @unorderedList(
          @itemSpacing Compact
          @item(prLowerBorder means that we want to
            align relative to left (or bottom) border of the screen,)
          @item(prMiddle means that we want to align relative to the middle
            of the screen,)
          @item(prHigherBorder means that we want to align relative to the
            right (or top) border of the screen.))
      )
    )

    This may sound complicated, but it gives you complete
    control over the menu position, so that it will look good on all
    window sizes. In most common examples, both PositionRelativeMenu
    and PositionRelativeScreen are equal, so

    @unorderedList(
      @item(If both are prLowerBorder, then Position specifies position
        of left/lower menu border relative to left/lower screen border.
        Position should always be >= 0 is such cases,
        otherwise there is no way for the menu to be completely visible.)
      @item(If both are prMiddle, then the Position (most often just 0, 0
        in this case) specifies the shift between screen middle to
        menu rectangle middle. If Position is zero, then menu is just in the
        middle of the screen.)
      @item(If both are prHigherBorder, then Position specifies position
        of right/top menu border relative to right/top screen border.
        Position should always be <= 0 is such cases,
        otherwise there is no way for the menu to be completely visible.)
    )

    In TCastleOnScreenMenu.DesignerMode you can see a line connecting the appropriate
    screen position (from PositionRelativeScreen) to the appropriate
    menu position (from PositionRelativeMenu) and you can experiment
    with these settings.
  }
  TPositionRelative = (
    prLowerBorder,
    prMiddle,
    prHigherBorder);

  { On-screen menu displayed in OpenGL. All the menu items are simply
    displayed on the screen, one after the other. Typical for game menus.
    Normal user programs may prefer to use the menu bar instead of this
    (for example TCastleWindowBase.Menu, or normal Lazarus menu).
    Although this still may be useful for displaying things like sliders. }
  TCastleOnScreenMenu = class(TUIControl)
  private
    FFullSize: boolean;
    FOnClick: TNotifyEvent;
    FOnAccessoryValueChanged: TNotifyEvent;
    FItems: TStringList;
    FCurrentItem: Integer;
    FPositionRelativeMenuX: TPositionRelative;
    FPositionRelativeMenuY: TPositionRelative;
    FPositionRelativeScreenX: TPositionRelative;
    FPositionRelativeScreenY: TPositionRelative;
    FRectangles: TRectangleList;
    FAccessoryRectangles: TRectangleList;
    FAllItemsRectangle: TRectangle;
    FKeyNextItem: TKey;
    FKeyPreviousItem: TKey;
    FKeySelectItem: TKey;
    FKeySliderDecrease: TKey;
    FKeySliderIncrease: TKey;
    MenuAnimation: Single;
    FCurrentItemBorderColor1: TVector3Single;
    FCurrentItemBorderColor2: TVector3Single;
    FCurrentItemColor: TVector3Single;
    FNonCurrentItemColor: TVector3Single;
    MaxItemWidth: Integer;
    FRegularSpaceBetweenItems: Cardinal;
    FDrawBackgroundRectangle: boolean;
    { Item accessory that currently has "grabbed" the mouse.
      -1 if none. }
    ItemAccessoryGrabbed: Integer;
    FDrawFocusedBorder: boolean;
    FDesignerMode: boolean;
    FPositionAbsolute,
      PositionScreenRelativeMove, PositionMenuRelativeMove: TVector2Integer;
    FBackgroundOpacityFocused, FBackgroundOpacityNotFocused: Single;
    function GetCurrentItem: Integer;
    procedure SetCurrentItem(const Value: Integer);
    procedure SetItems(const Value: TStringList);
    procedure SetDesignerMode(const Value: boolean);
  public
    const
      DefaultMenuKeyNextItem = K_Down;
      DefaultMenuKeyPreviousItem = K_Up;
      DefaultMenuKeySelectItem = K_Enter;
      DefaultMenuKeySliderIncrease = K_Right;
      DefaultMenuKeySliderDecrease = K_Left;

      DefaultCurrentItemBorderColor1: TVector3Single = (   1,    1,    1) { White3Single  }; { }
      DefaultCurrentItemBorderColor2: TVector3Single = ( 0.5,  0.5,  0.5) { Gray3Single   }; { }
      DefaultCurrentItemColor       : TVector3Single = (   1,    1,  0.3) { Yellow3Single }; { }
      DefaultNonCurrentItemColor    : TVector3Single = (   1,    1,    1) { White3Single  }; { }

      DefaultRegularSpaceBetweenItems = 10;
      DefaultBackgroundOpacityNotFocused = 0.4;
      DefaultBackgroundOpacityFocused = 0.7;

    var
      { Position of the menu. Expressed as position of some corner of the menu
        (see PositionRelativeMenuX/Y), relative to some corner of the
        screen (see PositionRelativeScreenX/Y).

        See TPositionRelative documentation for more information.

        You may be interested in DesignerMode for a possibility to set
        this property at run-time.

        Expressed as a public field (instead of a read-write property)
        because assigning a field of record property is a risk in ObjectPascal
        (you may be modifying only a temporary copy of the record returned
        by property getter). }
      Position: TVector2Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { PositionAbsolute expresses the position of the menu rectangle
      independently from all PositionRelative* properties.
      You can think of it as "What value would Position have
      if all PositionRelative* were equal prLowerBorder".

      An easy exercise for the reader is to check implementation that when
      all PositionRelative* are prLowerBorder, PositionAbsolute is indeed
      always equal to Position :)

      This is read-only, is calculated by FixItemsRectangles.
      It's calculated anyway because our drawing code needs this.
      You may find it useful if you want to draw something relative to menu
      position. }
    property PositionAbsolute: TVector2Integer read FPositionAbsolute;

    { When Items.Count <> 0, this is always some number
      between 0 and Items.Count - 1.
      Otherwise (when Items.Count <> 0) this is always -1.

      If you assign it to wrong value (breaking conditions above),
      or if you change Items such that conditions are broken,
      it will be arbitrarily fixed.

      Changing this calls CurrentItemChanged automatically when needed. }
    property CurrentItem: Integer read GetCurrentItem write SetCurrentItem;

    { These change CurrentItem as appropriate.
      Usually you will just let this class call it internally
      (from MouseMove, KeyDown etc.) and will not need to call it yourself.

      @groupBegin }
    procedure NextItem;
    procedure PreviousItem;
    { @groupEnd }

    procedure GLContextClose; override;

    { Calculate final positions, sizes of menu items on the screen.
      You must call FixItemsRectangles between last modification of
      @unorderedList(
        @itemSpacing Compact
        @item Items
        @item Position
        @item(RegularSpaceBetweenItems (and eventually everything else that
          affects your custom SpaceBetweenItems implementation))
      )
      and calling one of the procedures
      @unorderedList(
        @itemSpacing Compact
        @item Draw
        @item MouseMove
        @item MouseDown
        @item MouseUp
        @item KeyDown
        @item Idle
      )
      You can call this only while OpenGL context is initialized.

      ContainerResize already calls FixItemsRectangles, and window resize is already
      called automatically by window (at the addition to Controls list,
      or whenever window size changes). So in simplest cases (when you
      fill @link(Items) etc. properties before adding TCastleOnScreenMenu to Controls)
      you, in practice, do not have to call this explicitly. }
    procedure FixItemsRectangles;

    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); override;

    { Calculates menu items positions, sizes.
      These are initialized by FixItemsRectangles.
      They are absolutely read-only for the user of this class.
      You can use them to do some graphic effects, when you e.g.
      want to draw something on the screen that is somehow positioned
      relative to some menu item or to whole menu rectangle.
      Note that AllItemsRectangle includes also some outside margin.
      @groupBegin }
    property Rectangles: TRectangleList read FRectangles;
    property AllItemsRectangle: TRectangle read FAllItemsRectangle;
    property AccessoryRectangles: TRectangleList read FAccessoryRectangles;
    { @groupEnd }

    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;

    property KeyNextItem: TKey read FKeyNextItem write FKeyNextItem
      default DefaultMenuKeyNextItem;
    property KeyPreviousItem: TKey read FKeyPreviousItem write FKeyPreviousItem
      default DefaultMenuKeyPreviousItem;
    property KeySelectItem: TKey read FKeySelectItem write FKeySelectItem
      default DefaultMenuKeySelectItem;
    property KeySliderIncrease: TKey
      read FKeySliderIncrease write FKeySliderIncrease
      default DefaultMenuKeySliderIncrease;
    property KeySliderDecrease: TKey
      read FKeySliderDecrease write FKeySliderDecrease
      default DefaultMenuKeySliderDecrease;

    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; override;
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
    function PositionInside(const X, Y: Integer): boolean; override;
    function AllowSuspendForInput: boolean; override;

    { Called when user will select CurrentItem, either with mouse
      or with keyboard. }
    procedure Click; virtual;

    { @deprecated Deprecated name for Click. }
    procedure CurrentItemSelected; virtual; deprecated;

    { Called when the value of current accessory (TMenuAccessory assigned
      to CurrentItem) will change value.
      (Which may happen due to user clicking, or pressing some keys etc.)

      Note that this will not be called when you just set
      Value of some property.

      In the TCastleOnScreenMenu class this just calls VisibleChange,
      and OnAccessoryValueChanged. }
    procedure AccessoryValueChanged; virtual;

    { @deprecated Deprecated name for AccessoryValueChanged. }
    procedure CurrentItemAccessoryValueChanged; virtual; deprecated;

    { Called when CurrentItem changed.
      But *not* when CurrentItem changed because of Items.Count changes.
      In this class this just calls VisibleChange and
      plays sound stMenuCurrentItemChanged. }
    procedure CurrentItemChanged; virtual;

    { Default value is DefaultCurrentItemBorderColor1 }
    property CurrentItemBorderColor1: TVector3Single
      read FCurrentItemBorderColor1
      write FCurrentItemBorderColor1;
    { Default value is DefaultCurrentItemBorderColor2 }
    property CurrentItemBorderColor2: TVector3Single
      read FCurrentItemBorderColor2
      write FCurrentItemBorderColor2;
    { Default value is DefaultCurrentItemColor }
    property CurrentItemColor       : TVector3Single
      read FCurrentItemColor write FCurrentItemColor;
    { Default value is DefaultNonCurrentItemColor }
    property NonCurrentItemColor    : TVector3Single
      read FNonCurrentItemColor write FNonCurrentItemColor;

    { Return the space needed before NextItemIndex.
      This will be a space between NextItemIndex - 1 and NextItemIndex
      (this method will not be called for NextItemIndex = 0).

      Default implementation in this class simply returns
      RegularSpaceBetweenItems always.

      Note that this is used only at FixItemsRectangles call.
      So when some variable affecting the implementation of this changes,
      you should call FixItemsRectangles again. }
    function SpaceBetweenItems(const NextItemIndex: Cardinal): Cardinal; virtual;

    { "Designer mode" is useful for a developer to visually design
      some properties of TCastleOnScreenMenu.

      @link(Container) of this control will be aumatically used,
      we will set mouse position when entering DesignerMode
      to match current menu position. This is usually desirable (otherwise
      slight mouse move will immediately change menu position).
      To make it work, make sure @link(Container) is assigned
      before setting DesignerMode to @true --- in other words,
      make sure you add this control to something like TCastleWindowCustom.Controls
      first, and only then set DesignedMode := @true.
      This works assuming that you always call our Draw with identity
      transform matrix (otherwise, this unit is not able to know how to
      calculate mouse position corresponding to given menu PositionAbsolute).

      By default, we're not in designer mode,
      and user has @italic(no way to enter into designer mode).
      You have to actually add some code to your program to activate
      designer mode. E.g. in "The Rift" game I required that user
      passes @--debug-menu-designer command-line option and then
      DesignerMode could be toggled by F12 key press.

      Right now, features of designer mode:
      @unorderedList(
        @item(Mouse move change Position to current mouse position.)
        @item(PositionRelative changing:
          @unorderedList(
            @itemSpacing Compact
            @item Key X     changes PositionRelativeScreenX value,
            @item key Y     changes PositionRelativeScreenY value,
            @item Key CtrlX changes PositionRelativeMenuX values,
            @item Key CtrlY changes PositionRelativeMenuY values.
          )
          Also, a white line is drawn in designer mode, to indicate
          the referenced screen and menu positions.)
        @item(CtrlB toggles DrawBackgroundRectangle.)
        @item(Key CtrlD dumps current properties to StdOut.
          Basically, every property that can be changed from designer mode
          is dumped here. This is crucial function if you decide that
          you want to actually use the designed properties in your program,
          so you want to paste code setting such properties.)
      ) }
    property DesignerMode: boolean
      read FDesignerMode write SetDesignerMode default false;

  published
    { Opacity of the background rectangle (displayed when DrawBackgroundRectangle).
      @groupBegin }
    property         BackgroundOpacityFocused: Single
      read          FBackgroundOpacityFocused
      write         FBackgroundOpacityFocused
      default DefaultBackgroundOpacityFocused;
    property         BackgroundOpacityNotFocused: Single
      read          FBackgroundOpacityNotFocused
      write         FBackgroundOpacityNotFocused
      default DefaultBackgroundOpacityNotFocused;
    { @groupEnd }

    { See TPositionRelative documentation for meaning of these four
      PositionRelativeXxx properties.
      @groupBegin }
    property PositionRelativeMenuX: TPositionRelative
      read FPositionRelativeMenuX write FPositionRelativeMenuX
      default prMiddle;

    property PositionRelativeMenuY: TPositionRelative
      read FPositionRelativeMenuY write FPositionRelativeMenuY
      default prMiddle;

    property PositionRelativeScreenX: TPositionRelative
      read FPositionRelativeScreenX write FPositionRelativeScreenX
      default prMiddle;

    property PositionRelativeScreenY: TPositionRelative
      read FPositionRelativeScreenY write FPositionRelativeScreenY
      default prMiddle;
    { @groupEnd }

    property DrawBackgroundRectangle: boolean
      read FDrawBackgroundRectangle write FDrawBackgroundRectangle
      default true;

    { Additional vertical space, in pixels, between menu items.

      If you want more control over it (if you want to add more/less
      space between some menu items), override SpaceBetweenItems method. }
    property RegularSpaceBetweenItems: Cardinal
      read FRegularSpaceBetweenItems write FRegularSpaceBetweenItems
      default DefaultRegularSpaceBetweenItems;

    { Draw a flashing border around the menu when we are focused. }
    property DrawFocusedBorder: boolean read FDrawFocusedBorder write FDrawFocusedBorder
      default true;

    { Items of this menu.

      Note that Objects of this class have special meaning: they must
      be either nil or some TMenuAccessory instance
      (different TMenuAccessory instance for each item).
      When freeing this TCastleOnScreenMenu instance, note that we will also
      free all Items.Objects. }
    property Items: TStringList read FItems write SetItems;

    { Called when user will select CurrentItem.
      @seealso Click }
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    { Called when the value of current accessory (TMenuAccessory assigned
      to CurrentItem) will change value.
      @seealso AccessoryValueChanged }
    property OnAccessoryValueChanged: TNotifyEvent
      read FOnAccessoryValueChanged
      write FOnAccessoryValueChanged;

    { Should menu intercept all key/mouse input, that is behave like
      it was filling full container (window or lazarus component).
      This affects key/mouse processing (menu processes input
      before all controls underneath), but not drawing (controls underneath
      are still visible as usual). }
    property FullSize: boolean read FFullSize write FFullSize default false;
  end;

procedure Register;

{ @deprecated Deprecated names for UIFont and UIFontSmall in CastleControls unit.
  @groupBegin }
property MenuFont: TGLBitmapFontAbstract read GetUIFont write SetUIFont;
property SliderFont: TGLBitmapFontAbstract read GetUIFontSmall write SetUIFontSmall;
{ @groupEnd }

function Rectangle(const X0, Y0, Width, Height: Integer): TRectangle;

implementation

uses SysUtils, CastleUtils, CastleImages, CastleFilesUtils, CastleClassUtils,
  CastleStringUtils, CastleGLImages, CastleOnScreenMenuImages, CastleSoundEngine;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleOnScreenMenu]);
end;

var
  ImageSlider: TCastleImage;
  ImageSliderPosition: TCastleImage;
  GLImageSlider: TGLImage;
  GLImageSliderPosition: TGLImage;

procedure ImageSliderInit;

  { Compose RGB image of desired width (or very slightly larger),
    by horizontally stretching base image.

    When stretching, we take MiddleWidth pixels from the image as a pattern
    that may be infinitely repeated in the middle, as needed to get
    to DesiredWidth.

    Remaining pixels (to the left / right of MiddleWidth pixels) are placed
    at the left / right of resulting image. }
  function ComposeSliderImage(Base: TRGBImage; const MiddleWidth: Cardinal;
    const DesiredWidth: Cardinal): TRGBImage;
  var
    LeftWidth, RightWidth, MiddleCount, I: Cardinal;
  begin
    Assert(MiddleWidth <= Base.Width);
    LeftWidth := (Base.Width - MiddleWidth) div 2;
    RightWidth := Base.Width - MiddleWidth - LeftWidth;
    MiddleCount := DivRoundUp(DesiredWidth - LeftWidth - RightWidth, MiddleWidth);
    Result := TRGBImage.Create(LeftWidth + MiddleWidth * MiddleCount + RightWidth,
      Base.Height);
    Result.CopyFrom(Base, 0, 0, 0, 0, LeftWidth, Base.Height);
    if MiddleCount <> 0 then
      for I := 0 to MiddleCount do
        Result.CopyFrom(Base, I * MiddleWidth + LeftWidth, 0,
          LeftWidth, 0, MiddleWidth, Base.Height);
    Result.CopyFrom(Base, Result.Width - RightWidth, 0,
      Base.Width - RightWidth, 0, RightWidth, Base.Height);
  end;

begin
  if ImageSlider = nil then
    ImageSlider := ComposeSliderImage(Slider_Base, 1, 250);

  ImageSliderPosition := Slider_Position;

  if GLImageSlider = nil then
    GLImageSlider := TGLImage.Create(ImageSlider);

  if GLImageSliderPosition = nil then
    GLImageSliderPosition := TGLImage.Create(ImageSliderPosition);
end;

procedure WindowClose(const Container: IUIContainer);
begin
  FreeAndNil(GLImageSlider);
  FreeAndNil(GLImageSliderPosition);
  FreeAndNil(ImageSlider);

  { Do not free, this is a reference to Slider_Position (that will be freed at
    unit OnScreenMenuImages finalization.)

    Note: I once tried to make here
      if ImageSliderPosition <> Slider_Position then
        FreeAndNil(ImageSliderPosition);
    but this isn't so smart because WindowClose may be called at various
    moments, and then Slider_Position may be already freed and nil.
    Then "ImageSliderPosition <> Slider_Position" = true,
    but ImageSliderPosition is an invalid pointer.
    Smarter solutions (like ImageSliderPositionOwned: boolean)
    are possible. But for now, this is just always equal to Slider_Position,
    so no point in complicating this. }
  ImageSliderPosition := nil;
end;

{ TRectangleList -------------------------------------------------------------- }

function TRectangleList.FindRectangle(const X, Y: Integer): integer;
begin
  for Result := Count - 1 downto 0 do
    if L[Result].PointInside(X, Y) then
      Exit;
  Result := -1;
end;

{ TRectangle ----------------------------------------------------------------- }

function Rectangle(const X0, Y0, Width, Height: Integer): TRectangle;
begin
  Result.X0 := X0;
  Result.Y0 := Y0;
  Result.Width := Width;
  Result.Height := Height;
end;

function TRectangle.PointInside(const X, Y: Integer): boolean;
begin
  Result := (X >= X0) and (X <= X0 + Width) and
            (Y >= Y0) and (Y <= Y0 + Height);
end;

procedure TRectangle.DrawBorder;
begin
  GLRectangleBorder(X0, Y0, X0 + Width, Y0 + Height);
end;

{ TMenuAccessory ------------------------------------------------------ }

constructor TMenuAccessory.Create;
begin
  inherited;
  FOwnedByParent := true;
end;

function TMenuAccessory.KeyDown(Key: TKey; C: char;
  ParentMenu: TCastleOnScreenMenu): boolean;
begin
  { Nothing to do in this class. }
  Result := false;
end;

function TMenuAccessory.MouseDown(
  const MouseX, MouseY: Integer; Button: TMouseButton;
  const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu): boolean;
begin
  { Nothing to do in this class. }
  Result := false;
end;

procedure TMenuAccessory.MouseMove(const NewX, NewY: Integer;
  const MousePressed: TMouseButtons;
  const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu);
begin
  { Nothing to do in this class. }
end;

{ TMenuArgument -------------------------------------------------------- }

constructor TMenuArgument.Create(const AMaximumValueWidth: Integer);
begin
  inherited Create;
  FMaximumValueWidth := AMaximumValueWidth;
end;

class function TMenuArgument.TextWidth(const Text: string): Integer;
begin
  Result := UIFont.TextWidth(Text);
end;

function TMenuArgument.GetWidth: Integer;
begin
  Result := MaximumValueWidth;
end;

procedure TMenuArgument.Draw(const Rectangle: TRectangle);
begin
  glColorv(LightGreen3Single);
  SetWindowPos(Rectangle.X0, Rectangle.Y0 + UIFont.Descend);
  UIFont.Print(Value);
end;

{ TMenuBooleanArgument ----------------------------------------------------- }

constructor TMenuBooleanArgument.Create(const AValue: boolean);
begin
  inherited Create(
    Max(TMenuArgument.TextWidth(BoolToStrYesNo[true]),
        TMenuArgument.TextWidth(BoolToStrYesNo[false])));
  FBooleanValue := AValue;
  inherited Value := BoolToStrYesNo[Value];
end;

procedure TMenuBooleanArgument.SetValue(const AValue: boolean);
begin
  if FBooleanValue <> AValue then
  begin
    FBooleanValue := AValue;
    inherited Value := BoolToStrYesNo[Value];
  end;
end;

{ TMenuSlider -------------------------------------------------------------- }

constructor TMenuSlider.Create;
begin
  inherited;
  FDisplayValue := true;
end;

function TMenuSlider.GetWidth: Integer;
begin
  ImageSliderInit;
  Result := ImageSlider.Width;
end;

procedure TMenuSlider.Draw(const Rectangle: TRectangle);
begin
  ImageSliderInit;

  SetWindowPos(Rectangle.X0, Rectangle.Y0 + (Rectangle.Height - ImageSlider.Height) / 2);
  GLImageSlider.Draw;
end;

const
  ImageSliderPositionMargin = 2;

procedure TMenuSlider.DrawSliderPosition(const Rectangle: TRectangle;
  const Position: Single);
begin
  ImageSliderInit;

  SetWindowPos(Rectangle.X0 + ImageSliderPositionMargin +
    MapRange(Clamped(Position, 0, 1), 0, 1, 0,
      ImageSlider.Width - 2 * ImageSliderPositionMargin -
      ImageSliderPosition.Width),
    Rectangle.Y0 + (Rectangle.Height - ImageSliderPosition.Height) / 2);
  GLImageSliderPosition.Draw;
end;

function TMenuSlider.XCoordToSliderPosition(
  const XCoord: Single; const Rectangle: TRectangle): Single;
begin
  { I subtract below ImageSliderPosition.Width div 2
    because we want XCoord to be in the middle
    of ImageSliderPosition, not on the left. }
  Result := MapRange(XCoord - ImageSliderPosition.Width div 2,
    Rectangle.X0 + ImageSliderPositionMargin,
    Rectangle.X0 + ImageSlider.Width - 2 * ImageSliderPositionMargin -
    ImageSliderPosition.Width, 0, 1);

  Clamp(Result, 0, 1);
end;

procedure TMenuSlider.DrawSliderText(
  const Rectangle: TRectangle; const Text: string);
begin
  glColorv(Black3Single);
  SetWindowPos(
    Rectangle.X0 + (Rectangle.Width - UIFontSmall.TextWidth(Text)) / 2,
    Rectangle.Y0 + (Rectangle.Height - UIFontSmall.RowHeight) / 2);
  UIFontSmall.Print(Text);
end;

{ TMenuFloatSlider --------------------------------------------------------- }

constructor TMenuFloatSlider.Create(
  const ABeginRange, AEndRange, AValue: Single);
begin
  inherited Create;
  FBeginRange := ABeginRange;
  FEndRange := AEndRange;
  FValue := AValue;
end;

procedure TMenuFloatSlider.Draw(const Rectangle: TRectangle);
begin
  inherited;

  DrawSliderPosition(Rectangle, MapRange(Value, BeginRange, EndRange, 0, 1));

  if DisplayValue then
    DrawSliderText(Rectangle, ValueToStr(Value));
end;

function TMenuFloatSlider.KeyDown(Key: TKey; C: char;
  ParentMenu: TCastleOnScreenMenu): boolean;
var
  ValueChange: Single;
begin
  Result := inherited;
  if Result then Exit;

  { TODO: TMenuFloatSlider should rather get "smooth" changing of Value ? }
  if Key <> K_None then
  begin
    ValueChange := (EndRange - BeginRange) / 100;

    { KeySelectItem works just like KeySliderIncrease.
      Why ? Because KeySelectItem does something with most menu items,
      so user would be surprised if it doesn't work at all with slider
      menu items. Increasing slider value seems like some sensible operation
      to do on slider menu item. }

    if (Key = ParentMenu.KeySelectItem) or
       (Key = ParentMenu.KeySliderIncrease) then
    begin
      FValue := Min(EndRange, Value + ValueChange);
      ParentMenu.AccessoryValueChanged;
      Result := ParentMenu.ExclusiveEvents;
    end else
    if Key = ParentMenu.KeySliderDecrease then
    begin
      FValue := Max(BeginRange, Value - ValueChange);
      ParentMenu.AccessoryValueChanged;
      Result := ParentMenu.ExclusiveEvents
    end;
  end;
end;

function TMenuFloatSlider.MouseDown(
  const MouseX, MouseY: Integer; Button: TMouseButton;
  const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Button = mbLeft then
  begin
    FValue := MapRange(XCoordToSliderPosition(MouseX, Rectangle), 0, 1,
      BeginRange, EndRange);
    ParentMenu.AccessoryValueChanged;
    Result := ParentMenu.ExclusiveEvents;
  end;
end;

procedure TMenuFloatSlider.MouseMove(const NewX, NewY: Integer;
  const MousePressed: TMouseButtons;
  const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu);
begin
  if mbLeft in MousePressed then
  begin
    FValue := MapRange(XCoordToSliderPosition(NewX, Rectangle), 0, 1,
      BeginRange, EndRange);
    ParentMenu.AccessoryValueChanged;
  end;
end;

function TMenuFloatSlider.ValueToStr(const AValue: Single): string;
begin
  Result := Format('%f', [AValue]);
end;

{ TMenuIntegerSlider ------------------------------------------------------- }

constructor TMenuIntegerSlider.Create(
  const ABeginRange, AEndRange, AValue: Integer);
begin
  inherited Create;
  FBeginRange := ABeginRange;
  FEndRange := AEndRange;
  FValue := AValue;
end;

procedure TMenuIntegerSlider.Draw(const Rectangle: TRectangle);
begin
  inherited;

  DrawSliderPosition(Rectangle, MapRange(Value, BeginRange, EndRange, 0, 1));

  if DisplayValue then
    DrawSliderText(Rectangle, ValueToStr(Value));
end;

function TMenuIntegerSlider.KeyDown(Key: TKey; C: char;
  ParentMenu: TCastleOnScreenMenu): boolean;
var
  ValueChange: Integer;
begin
  Result := inherited;
  if Result then Exit;

  if Key <> K_None then
  begin
    ValueChange := 1;

    { KeySelectItem works just like KeySliderIncrease.
      Reasoning: see TMenuFloatSlider. }

    if (Key = ParentMenu.KeySelectItem) or
       (Key = ParentMenu.KeySliderIncrease) then
    begin
      FValue := Min(EndRange, Value + ValueChange);
      ParentMenu.AccessoryValueChanged;
      Result := ParentMenu.ExclusiveEvents;
    end else
    if Key = ParentMenu.KeySliderDecrease then
    begin
      FValue := Max(BeginRange, Value - ValueChange);
      ParentMenu.AccessoryValueChanged;
      Result := ParentMenu.ExclusiveEvents;
    end;
  end;
end;

function TMenuIntegerSlider.XCoordToValue(
  const XCoord: Single; const Rectangle: TRectangle): Integer;
begin
  { We do additional Clamped over Round result to avoid any
    chance of floating-point errors due to lack of precision. }
  Result := Clamped(Round(
    MapRange(XCoordToSliderPosition(XCoord, Rectangle), 0, 1,
      BeginRange, EndRange)), BeginRange, EndRange);
end;

function TMenuIntegerSlider.MouseDown(
  const MouseX, MouseY: Integer; Button: TMouseButton;
  const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Button = mbLeft then
  begin
    FValue := XCoordToValue(MouseX, Rectangle);
    ParentMenu.AccessoryValueChanged;
    Result := ParentMenu.ExclusiveEvents;
  end;
end;

procedure TMenuIntegerSlider.MouseMove(const NewX, NewY: Integer;
  const MousePressed: TMouseButtons;
  const Rectangle: TRectangle; ParentMenu: TCastleOnScreenMenu);
begin
  if mbLeft in MousePressed then
  begin
    FValue := XCoordToValue(NewX, Rectangle);
    ParentMenu.AccessoryValueChanged;
  end;
end;

function TMenuIntegerSlider.ValueToStr(const AValue: Integer): string;
begin
  Result := IntToStr(AValue);
end;

{ TCastleOnScreenMenu -------------------------------------------------------------------- }

constructor TCastleOnScreenMenu.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
  FCurrentItem := 0;
  FRectangles := TRectangleList.Create;
  FAccessoryRectangles := TRectangleList.Create;
  BackgroundOpacityNotFocused := DefaultBackgroundOpacityNotFocused;
  BackgroundOpacityFocused    := DefaultBackgroundOpacityFocused;

  FPositionRelativeMenuX := prMiddle;
  FPositionRelativeMenuY := prMiddle;
  FPositionRelativeScreenX := prMiddle;
  FPositionRelativeScreenY := prMiddle;

  KeyNextItem := DefaultMenuKeyNextItem;
  KeyPreviousItem := DefaultMenuKeyPreviousItem;
  KeySelectItem := DefaultMenuKeySelectItem;
  KeySliderIncrease := DefaultMenuKeySliderIncrease;
  KeySliderDecrease := DefaultMenuKeySliderDecrease;

  FCurrentItemBorderColor1 := DefaultCurrentItemBorderColor1;
  FCurrentItemBorderColor2 := DefaultCurrentItemBorderColor2;
  FCurrentItemColor := DefaultCurrentItemColor;
  FNonCurrentItemColor := DefaultNonCurrentItemColor;

  FRegularSpaceBetweenItems := DefaultRegularSpaceBetweenItems;
  FDrawBackgroundRectangle := true;
  FDrawFocusedBorder := true;
end;

destructor TCastleOnScreenMenu.Destroy;
var
  I: Integer;
begin
  if FItems <> nil then
  begin
    for I := 0 to FItems.Count - 1 do
      if FItems.Objects[I] <> nil then
      begin
        if TMenuAccessory(FItems.Objects[I]).OwnedByParent then
          FItems.Objects[I].Free;
        FItems.Objects[I] := nil;
      end;
    FreeAndNil(FItems);
  end;

  FreeAndNil(FAccessoryRectangles);
  FreeAndNil(FRectangles);
  inherited;
end;

function TCastleOnScreenMenu.GetCurrentItem: Integer;
begin
  Result := FCurrentItem;

  { Make sure that CurrentItem conditions are OK.

    Alternatively we could watch for this in SetCurrentItem, but then
    changing Items by user of this class could invalidate it.
    So it's safest to just check the conditions here. }

  if Items.Count <> 0 then
  begin
    Clamp(Result, 0, Items.Count - 1);
  end else
    Result := -1;
end;

procedure TCastleOnScreenMenu.SetCurrentItem(const Value: Integer);
var
  OldCurrentItem, NewCurrentItem: Integer;
begin
  OldCurrentItem := CurrentItem;
  FCurrentItem := Value;
  NewCurrentItem := CurrentItem;
  if OldCurrentItem <> NewCurrentItem then
    CurrentItemChanged;
end;

procedure TCastleOnScreenMenu.NextItem;
begin
  if Items.Count <> 0 then
  begin
    if CurrentItem = Items.Count - 1 then
      CurrentItem := 0 else
      CurrentItem := CurrentItem + 1;
  end;
end;

procedure TCastleOnScreenMenu.PreviousItem;
begin
  if Items.Count <> 0 then
  begin
    if CurrentItem = 0 then
      CurrentItem := Items.Count - 1 else
      CurrentItem := CurrentItem - 1;
  end;
end;

procedure TCastleOnScreenMenu.GLContextClose;
begin
end;

function TCastleOnScreenMenu.SpaceBetweenItems(const NextItemIndex: Cardinal): Cardinal;
begin
  Result := RegularSpaceBetweenItems;
end;

const
  MarginBeforeAccessory = 20;

procedure TCastleOnScreenMenu.FixItemsRectangles;
const
  AllItemsRectangleMargin = 30;
var
  I: Integer;
  WholeItemWidth, MaxAccessoryWidth: Integer;
  ItemsBelowHeight: Cardinal;
begin
  { If ContainerResize not called yet, wait for FixItemsRectangles call
    from the first ContainerResize. }
  if not ContainerSizeKnown then
    Exit;

  ItemAccessoryGrabbed := -1;

  FAccessoryRectangles.Count := Items.Count;

  { calculate FAccessoryRectangles[].Width, MaxItemWidth, MaxAccessoryWidth }

  MaxItemWidth := 0;
  MaxAccessoryWidth := 0;
  for I := 0 to Items.Count - 1 do
  begin
    MaxTo1st(MaxItemWidth, UIFont.TextWidth(Items[I]));

    if Items.Objects[I] <> nil then
      FAccessoryRectangles.L[I].Width :=
        TMenuAccessory(Items.Objects[I]).GetWidth else
      FAccessoryRectangles.L[I].Width := 0;

    MaxTo1st(MaxAccessoryWidth, FAccessoryRectangles.L[I].Width);
  end;

  { calculate FAllItemsRectangle Width and Height }

  FAllItemsRectangle.Width := MaxItemWidth;
  if MaxAccessoryWidth <> 0 then
    FAllItemsRectangle.Width += MarginBeforeAccessory + MaxAccessoryWidth;

  FAllItemsRectangle.Height := 0;
  for I := 0 to Items.Count - 1 do
  begin
    FAllItemsRectangle.Height += UIFont.RowHeight;
    if I > 0 then
      FAllItemsRectangle.Height += Integer(SpaceBetweenItems(I));
  end;

  FAllItemsRectangle.Width += 2 * AllItemsRectangleMargin;
  FAllItemsRectangle.Height += 2 * AllItemsRectangleMargin;

  { calculate Rectangles Widths and Heights }

  Rectangles.Count := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if MaxAccessoryWidth <> 0 then
      WholeItemWidth := MaxItemWidth + MarginBeforeAccessory + MaxAccessoryWidth else
      WholeItemWidth := UIFont.TextWidth(Items[I]);
    Rectangles.Add(Rectangle(0, 0, WholeItemWidth,
      UIFont.Descend + UIFont.RowHeight));
  end;

  { Now take into account Position, PositionRelative*
    and calculate PositionAbsolute.

    By the way, we also calculate PositionScreenRelativeMove
    and PositionMenuRelativeMove, but you don't have to worry about them
    too much, they are only for DesignerMode to visualize current
    PositionRelative* meaning. }

  case PositionRelativeScreenX of
    prLowerBorder : PositionScreenRelativeMove[0] := 0;
    prMiddle      : PositionScreenRelativeMove[0] := ContainerWidth div 2;
    prHigherBorder: PositionScreenRelativeMove[0] := ContainerWidth;
    else raise EInternalError.Create('PositionRelative* = ?');
  end;

  case PositionRelativeScreenY of
    prLowerBorder : PositionScreenRelativeMove[1] := 0;
    prMiddle      : PositionScreenRelativeMove[1] := ContainerHeight div 2;
    prHigherBorder: PositionScreenRelativeMove[1] := ContainerHeight;
    else raise EInternalError.Create('PositionRelative* = ?');
  end;

  case PositionRelativeMenuX of
    prLowerBorder : PositionMenuRelativeMove[0] := 0;
    prMiddle      : PositionMenuRelativeMove[0] := FAllItemsRectangle.Width div 2;
    prHigherBorder: PositionMenuRelativeMove[0] := FAllItemsRectangle.Width;
    else raise EInternalError.Create('PositionRelative* = ?');
  end;

  case PositionRelativeMenuY of
    prLowerBorder : PositionMenuRelativeMove[1] := 0;
    prMiddle      : PositionMenuRelativeMove[1] := FAllItemsRectangle.Height div 2;
    prHigherBorder: PositionMenuRelativeMove[1] := FAllItemsRectangle.Height;
    else raise EInternalError.Create('PositionRelative* = ?');
  end;

  FPositionAbsolute := Position + PositionScreenRelativeMove - PositionMenuRelativeMove;

  { Calculate positions of all rectangles. }

  { we iterate downwards from Rectangles.Count - 1 to 0, updating ItemsBelowHeight.
    That's OpenGL (and so, Rectangles.L[I].Y0) coordinates grow up, while
    our menu items are specified from highest to lowest. }
  ItemsBelowHeight := 0;

  for I := Rectangles.Count - 1 downto 0 do
  begin
    Rectangles.L[I].X0 := PositionAbsolute[0] + AllItemsRectangleMargin;
    Rectangles.L[I].Y0 := PositionAbsolute[1] + AllItemsRectangleMargin + ItemsBelowHeight;

    if I > 0 then
      ItemsBelowHeight += Cardinal(UIFont.RowHeight + Integer(SpaceBetweenItems(I)));
  end;
  FAllItemsRectangle.X0 := PositionAbsolute[0];
  FAllItemsRectangle.Y0 := PositionAbsolute[1];

  { Calculate FAccessoryRectangles[].X0, Y0, Height }
  for I := 0 to Rectangles.Count - 1 do
  begin
    FAccessoryRectangles.L[I].X0 := Rectangles.L[I].X0 +
      MaxItemWidth + MarginBeforeAccessory;
    FAccessoryRectangles.L[I].Y0 := Rectangles.L[I].Y0;
    FAccessoryRectangles.L[I].Height := Rectangles.L[I].Height;
  end;
end;

procedure TCastleOnScreenMenu.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
  inherited;
  FixItemsRectangles;
end;

function TCastleOnScreenMenu.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists then
    Result := ds2D else
    Result := dsNone;
end;

procedure TCastleOnScreenMenu.Draw;

  procedure DrawPositionRelativeLine;
  begin
    glColorv(White3Single);
    glLineWidth(1.0);
    glBegin(GL_LINES);
      glVertexv(PositionScreenRelativeMove);
      glVertexv(PositionAbsolute + PositionMenuRelativeMove);
    glEnd();
  end;

const
  CurrentItemBorderMargin = 5;
var
  I: Integer;
  CurrentItemBorderColor: TVector3Single;
begin
  if not GetExists then Exit;

  if DrawBackgroundRectangle then
  begin
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
      if Focused then
        glColor4f(0, 0, 0, BackgroundOpacityFocused) else
        glColor4f(0, 0, 0, BackgroundOpacityNotFocused);
      glRectf(FAllItemsRectangle.X0, FAllItemsRectangle.Y0,
        FAllItemsRectangle.X0 + FAllItemsRectangle.Width,
        FAllItemsRectangle.Y0 + FAllItemsRectangle.Height);
    glDisable(GL_BLEND);
  end;

  { Calculate CurrentItemBorderColor }
  if MenuAnimation <= 0.5 then
    CurrentItemBorderColor := Lerp(
      MapRange(MenuAnimation, 0, 0.5, 0, 1),
      CurrentItemBorderColor1, CurrentItemBorderColor2) else
    CurrentItemBorderColor := Lerp(
      MapRange(MenuAnimation, 0.5, 1, 0, 1),
      CurrentItemBorderColor2, CurrentItemBorderColor1);

  if Focused and DrawFocusedBorder then
  begin
    glColorv(CurrentItemBorderColor);
    glLineWidth(1.0);
    FAllItemsRectangle.DrawBorder;
  end;

  for I := 0 to Items.Count - 1 do
  begin
    if I = CurrentItem then
    begin
      glColorv(CurrentItemBorderColor);
      glLineWidth(1.0);
      GLRectangleBorder(
        Rectangles.L[I].X0 - CurrentItemBorderMargin,
        Rectangles.L[I].Y0,
        Rectangles.L[I].X0 + Rectangles.L[I].Width + CurrentItemBorderMargin,
        Rectangles.L[I].Y0 + Rectangles.L[I].Height);

      glColorv(CurrentItemColor);
    end else
      glColorv(NonCurrentItemColor);

    SetWindowPos(Rectangles.L[I].X0, Rectangles.L[I].Y0 + UIFont.Descend);
    UIFont.Print(Items[I]);

    if Items.Objects[I] <> nil then
      TMenuAccessory(Items.Objects[I]).Draw(FAccessoryRectangles.L[I]);
  end;

  if DesignerMode then
    DrawPositionRelativeLine;
end;

function TCastleOnScreenMenu.Press(const Event: TInputPressRelease): boolean;

  function KeyDown(const Key: TKey; const C: char): boolean;

    function CurrentItemAccessoryKeyDown: boolean;
    begin
      Result := false;
      if Items.Objects[CurrentItem] <> nil then
      begin
        Result := TMenuAccessory(Items.Objects[CurrentItem]).KeyDown(
          Key, C, Self);
      end;
    end;

    procedure IncPositionRelative(var P: TPositionRelative);
    var
      OldChange, NewChange: TVector2Integer;
    begin
      { We want to change P, but preserve PositionAbsolute.
        I.e. we want to change P, but also adjust Position such that
        resulting PositionAbsolute will stay the same. This is very comfortable
        for user is DesignerMode that wants often to change some
        PositionRelative, but wants to preserve current menu position
        (as visible on the screen currently) the same.

        Key is the equation
          PositionAbsolute = Position + PositionScreenRelativeMove - PositionMenuRelativeMove;
        The part that changes when P changes is
          (PositionScreenRelativeMove - PositionMenuRelativeMove)
        Currently it's equal OldChange. So
          PositionAbsolute = Position + OldChange
        After P changes and FixItemsRectangles does it's work, it's NewChange. So it's
          PositionAbsolute = Position + NewChange;
        But I want PositionAbsolute to stay the same. So I add (OldChange - NewChange)
        to the equation after:
          PositionAbsolute = Position + (OldChange - NewChange) + NewChange;
        This way PositionAbsolute will stay the same. So
          NewPosition := Position + (OldChange - NewChange); }
      OldChange := PositionScreenRelativeMove - PositionMenuRelativeMove;

      if P = High(P) then
        P := Low(P) else
        P := Succ(P);

      { Call FixItemsRectangles only to set new
        PositionScreenRelativeMove - PositionMenuRelativeMove. }
      FixItemsRectangles;

      NewChange := PositionScreenRelativeMove - PositionMenuRelativeMove;
      Position := Position + OldChange - NewChange;

      { Call FixItemsRectangles once again, since Position changed. }
      FixItemsRectangles;
    end;

  const
    PositionRelativeName: array [TPositionRelative] of string =
    ( 'prLowerBorder',
      'prMiddle',
      'prHigherBorder' );
    BooleanToStr: array [boolean] of string=('false','true');

  begin
    Result := false;

    if Key = KeyPreviousItem then
    begin
      PreviousItem;
      Result := ExclusiveEvents;
    end else
    if Key = KeyNextItem then
    begin
      NextItem;
      Result := ExclusiveEvents;
    end else
    if Key = KeySelectItem then
    begin
      CurrentItemAccessoryKeyDown;
      Click;
      Result := ExclusiveEvents;
    end else
      Result := CurrentItemAccessoryKeyDown;

    if DesignerMode then
    begin
      case C of
        CtrlB:
          begin
            DrawBackgroundRectangle := not DrawBackgroundRectangle;
            Result := ExclusiveEvents;
          end;
        'x': begin IncPositionRelative(FPositionRelativeScreenX); Result := ExclusiveEvents; end;
        'y': begin IncPositionRelative(FPositionRelativeScreenY); Result := ExclusiveEvents; end;
        CtrlX: begin IncPositionRelative(FPositionRelativeMenuX); Result := ExclusiveEvents; end;
        CtrlY: begin IncPositionRelative(FPositionRelativeMenuY); Result := ExclusiveEvents; end;
        CtrlD:
          begin
            InfoWrite(Format(
              'Position.Init(%f, %f);' +nl+
              'PositionRelativeScreenX := %s;' +nl+
              'PositionRelativeScreenY := %s;' +nl+
              'PositionRelativeMenuX := %s;' +nl+
              'PositionRelativeMenuY := %s;' +nl+
              'DrawBackgroundRectangle := %s;',
              [ Position[0],
                Position[1],
                PositionRelativeName[PositionRelativeScreenX],
                PositionRelativeName[PositionRelativeScreenY],
                PositionRelativeName[PositionRelativeMenuX],
                PositionRelativeName[PositionRelativeMenuY],
                BooleanToStr[DrawBackgroundRectangle] ]));
            Result := ExclusiveEvents;
          end;
      end;
    end;
  end;

  function MouseDown(const Button: TMouseButton): boolean;
  var
    NewItemIndex: Integer;
    MX, MY: Integer;
  begin
    Result := false;

    { For TCastleOnScreenMenu, we like MouseY going higher from the bottom to the top. }
    MX := Container.MouseX;
    MY := ContainerHeight - Container.MouseY;

    if (CurrentItem <> -1) and
       (Items.Objects[CurrentItem] <> nil) and
       FAccessoryRectangles.L[CurrentItem].PointInside(MX, MY) and
       (Container.MousePressed - [Button] = []) then
    begin
      ItemAccessoryGrabbed := CurrentItem;
      TMenuAccessory(Items.Objects[CurrentItem]).MouseDown(
        MX, MY, Button, FAccessoryRectangles.L[CurrentItem], Self);
      Result := ExclusiveEvents;
    end;

    if Button = mbLeft then
    begin
      NewItemIndex := Rectangles.FindRectangle(MX, MY);
      if NewItemIndex <> -1 then
      begin
        CurrentItem := NewItemIndex;
        Click;
        Result := ExclusiveEvents;
      end;
    end;
  end;

begin
  Result := inherited;
  if Result or (not GetExists) then Exit;
  case Event.EventType of
    itKey        : Result := KeyDown(Event.Key, Event.KeyCharacter);
    itMouseButton: Result := MouseDown(Event.MouseButton);
  end;
end;

function TCastleOnScreenMenu.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
var
  MX, MY: Integer;

  procedure ChangePosition;
  var
    NewPositionAbsolute: TVector2Integer;
  begin
    NewPositionAbsolute := Vector2Integer(MX, MY);
    { I want Position set such that (MX, MY) are lower/left corner
      of menu rectangle. I know that
        PositionAbsolute = Position + PositionScreenRelativeMove - PositionMenuRelativeMove;
      (MX, MY) are new PositionAbsolute, so I can calculate from
      this new desired Position value. }
    Position := NewPositionAbsolute - PositionScreenRelativeMove + PositionMenuRelativeMove;
    FixItemsRectangles;
  end;

var
  NewItemIndex: Integer;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  { For TCastleOnScreenMenu, we like MouseY going higher from the bottom to the top. }
  MX := NewX;
  MY := ContainerHeight - NewY;

  NewItemIndex := Rectangles.FindRectangle(MX, MY);
  if NewItemIndex <> -1 then
  begin
    if NewItemIndex <> CurrentItem then
      CurrentItem := NewItemIndex else
    { If NewItemIndex = CurrentItem and NewItemIndex <> -1,
      then user just moves mouse within current item.
      So maybe we should call TMenuAccessory.MouseMove. }
    if (Items.Objects[CurrentItem] <> nil) and
       FAccessoryRectangles.L[CurrentItem].PointInside(MX, MY) and
       (ItemAccessoryGrabbed = CurrentItem) then
      TMenuAccessory(Items.Objects[CurrentItem]).MouseMove(
        MX, MY, Container.MousePressed,
        FAccessoryRectangles.L[CurrentItem], Self);
  end;

  if DesignerMode then
    ChangePosition;

  Result := ExclusiveEvents;
end;

function TCastleOnScreenMenu.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) or (Event.EventType <> itMouseButton) then Exit;

  { This is actually not needed, smart check for
    (MousePressed - [Button] = []) inside MouseDown handles everything,
    so we don't have to depend on MouseUp for ungrabbing.
    But I do it here, just "to keep my state as current as possible". }
  if Container.MousePressed = [] then
    ItemAccessoryGrabbed := -1;

  Result := ExclusiveEvents;
end;

procedure TCastleOnScreenMenu.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;

  MenuAnimation += 0.5 * CompSpeed;
  MenuAnimation := Frac(MenuAnimation);
  VisibleChange;
end;

function TCastleOnScreenMenu.AllowSuspendForInput: boolean;
begin
  Result := false;
end;

procedure TCastleOnScreenMenu.Click;
begin
  if Assigned(OnClick) then OnClick(Self);
  SoundEngine.Sound(stMenuClick);
end;

procedure TCastleOnScreenMenu.CurrentItemChanged;
begin
  VisibleChange;
  SoundEngine.Sound(stMenuCurrentItemChanged);
end;

procedure TCastleOnScreenMenu.AccessoryValueChanged;
begin
  VisibleChange;
  if Assigned(OnAccessoryValueChanged) then OnAccessoryValueChanged(Self);
end;

procedure TCastleOnScreenMenu.CurrentItemSelected;
begin
  Click; { call non-deprecated equivalent }
end;

procedure TCastleOnScreenMenu.CurrentItemAccessoryValueChanged;
begin
  AccessoryValueChanged; { call non-deprecated equivalent }
end;

procedure TCastleOnScreenMenu.SetDesignerMode(const Value: boolean);
begin
  if (not FDesignerMode) and Value and (Container <> nil) then
  begin
    Container.SetMousePosition(
      Round(PositionAbsolute[0]),
      ContainerHeight - Round(PositionAbsolute[1]));
  end;

  FDesignerMode := Value;
end;

function TCastleOnScreenMenu.PositionInside(const X, Y: Integer): boolean;
begin
  Result := FullSize or
    FAllItemsRectangle.PointInside(X, ContainerHeight - Y);
end;

procedure TCastleOnScreenMenu.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);
end;

initialization
  OnGLContextClose.Add(@WindowClose);
end.
