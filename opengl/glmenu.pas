{
  Copyright 2006,2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Menu displayed in OpenGL.

  This unit draws a menu in OpenGL,
  which should be suitable for games etc. "Normal" user programs
  may prefer to use the native menu bar, provided in each
  TGLWindow instance.

  Two "quirks" that you should be aware of are:
  @orderedList(
    @item(Make sure you call GLMenuCloseGL when you ended using any menus
      (otherwise you'll get memory leak).)
    @item(Make sure GetMenuImagesPath is set properly and you have
      appropriate menu images copied to appropriate directory,
      if you want to use menu features that use these images.)
  ) }
unit GLMenu;

interface

uses Classes, OpenGLBmpFonts, BFNT_BitstreamVeraSans_Unit, VectorMath, Areas,
  GLWindow, GL, GLU, KambiGLUtils, Matrix;

const
  DefaultGLMenuKeyNextItem = K_Down;
  DefaultGLMenuKeyPreviousItem = K_Up;
  DefaultGLMenuKeySelectItem = K_Enter;
  DefaultGLMenuKeySliderIncrease = K_Right;
  DefaultGLMenuKeySliderDecrease = K_Left;

  DefaultCurrentItemBorderColor1: TVector3Single = (   1,    1,    1) { White3Single };
  DefaultCurrentItemBorderColor2: TVector3Single = ( 0.5,  0.5,  0.5) { Gray3Single };
  DefaultCurrentItemColor       : TVector3Single = (   1,    1,  0.3) { Yellow3Single };
  DefaultNonCurrentItemColor    : TVector3Single = (   1,    1,    1) { White3Single };

  DefaultRegularSpaceBetweenItems = 10;

type
  TGLMenu = class;

  { This is something that can be attached to some menu items of TGLMenu.
    For example, a slider --- see TGLMenuSlider. }
  TGLMenuItemAccessory = class
  public
    { Return the width you will need to display yourself.

      Note that this will be asked only from FixItemsAreas
      from TGLMenu. So for example TGLMenuItemArgument
      is *not* supposed to return here something based on
      current TGLMenuItemArgument.Value,
      because we will not query GetWidth after every change of
      TGLMenuItemArgument.Value. Instead, TGLMenuItemArgument
      should return here the width of widest possible Value. }
    function GetWidth(MenuFont: TGLBitmapFont): Single; virtual; abstract;

    { Draw yourself. Note that Area.Width is for sure the same
      as you returned in GetWidth. }
    procedure Draw(const Area: TArea); virtual; abstract;

    { This will be called if user will press a key when currently
      selected item has this TGLMenuItemAccessory.

      You can use ParentMenu to call
      ParentMenu.CurrentItemAccessoryValueChanged. }
    procedure KeyDown(Key: TKey; C: char;
      ParentMenu: TGLMenu); virtual;

    { This will be called if user will click mouse when currently
      selected item has this TGLMenuItemAccessory.

      This will be called only if MouseX and MouseY will be within
      appropriate Area of this accessory. This Area is also
      passed here, so you can e.g. calculate mouse position
      relative to this accessory as (MouseX - Area.X0, MouseY - Area.Y0).

      Note that while the user holds the mouse clicked (MousePressed <> []),
      the mouse is "grabbed" by this accessory, and even when the user
      will move the mouse over other items, they will not receive their
      MouseDown/MouseMove messages until user will let the mouse go.
      This prevents the bad situation when user does MouseDown e.g.
      on "Sound Volume" slider, slides it to the right and then accidentaly
      moves the mouse also a little down, and suddenly he's over "Music Volume"
      slider and he changed the position of "Music Volume" slider.

      You can use ParentMenu to call
      ParentMenu.CurrentItemAccessoryValueChanged. }
    procedure MouseDown(const MouseX, MouseY: Single; Button: TMouseButton;
      const Area: TArea; ParentMenu: TGLMenu); virtual;

    { This will be called if user will move mouse over the currently selected
      menu item and menu item will have this accessory.

      Just like with MouseDown: This will be called only if NewX and NewY
      will be within appropriate Area of accessory.
      You can use ParentMenu to call
      ParentMenu.CurrentItemAccessoryValueChanged. }
    procedure MouseMove(const NewX, NewY: Single;
      const MousePressed: TMouseButtons;
      const Area: TArea; ParentMenu: TGLMenu); virtual;
  end;

  { This is TGLMenuItemAccessory that will just display
    additional text (using some different color than Menu.CurrentItemColor)
    after the menu item. The intention is that the Value will be changeable
    by the user (while the basic item text remains constant).
    For example Value may describe "on" / "off" state of something,
    the name of some key currently assigned to some function etc. }
  TGLMenuItemArgument = class(TGLMenuItemAccessory)
  private
    FMaximumValueWidth: Single;
    FValue: string;
  public
    constructor Create(const AMaximumValueWidth: Single);

    property Value: string read FValue write FValue;

    property MaximumValueWidth: Single
      read FMaximumValueWidth write FMaximumValueWidth;

    { Calculate text width using font used by TGLMenuItemArgument. }
    class function TextWidth(const Text: string): Single;

    function GetWidth(MenuFont: TGLBitmapFont): Single; override;
    procedure Draw(const Area: TArea); override;
  end;

  { This is like TGLMenuItemArgument that displays boolean value
    (as "Yes" or "No").

    Don't access MaximumValueWidth or inherited Value (as string)
    when using this class --- this class should handle this by itself. }
  TGLMenuBooleanArgument = class(TGLMenuItemArgument)
  private
    FBooleanValue: boolean;
    procedure SetValue(const AValue: boolean);
  public
    constructor Create(const AValue: boolean);
    property Value: boolean read FBooleanValue write SetValue;
  end;

  TGLMenuSlider = class(TGLMenuItemAccessory)
  private
    FDisplayValue: boolean;
  protected
    procedure DrawSliderPosition(const Area: TArea; const Position: Single);

    { This returns a value of Position (for DrawSliderPosition, so in range 0..1)
      that would result in slider being drawn at XCoord screen position.
      Takes Area as the area currently occupied by the whole slider. }
    function XCoordToSliderPosition(const XCoord: Single;
      const Area: TArea): Single;

    procedure DrawSliderText(const Area: TArea; const Text: string);
  public
    constructor Create;

    function GetWidth(MenuFont: TGLBitmapFont): Single; override;
    procedure Draw(const Area: TArea); override;

    { Should the Value be displayed as text ?
      Usually useful --- but only if the Value has some meaning for the user.
      If @true, then ValueToStr is used. }
    property DisplayValue: boolean
      read FDisplayValue write FDisplayValue default true;
  end;

  TGLMenuFloatSlider = class(TGLMenuSlider)
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

    procedure Draw(const Area: TArea); override;

    procedure KeyDown(Key: TKey; C: char;
      ParentMenu: TGLMenu); override;

    procedure MouseDown(const MouseX, MouseY: Single; Button: TMouseButton;
      const Area: TArea; ParentMenu: TGLMenu); override;

    procedure MouseMove(const NewX, NewY: Single;
      const MousePressed: TMouseButtons;
      const Area: TArea; ParentMenu: TGLMenu); override;

    function ValueToStr(const AValue: Single): string; virtual;
  end;

  TGLMenuIntegerSlider = class(TGLMenuSlider)
  private
    FBeginRange: Integer;
    FEndRange: Integer;
    FValue: Integer;

    function XCoordToValue(
      const XCoord: Single; const Area: TArea): Integer;
  public
    constructor Create(const ABeginRange, AEndRange, AValue: Integer);

    property BeginRange: Integer read FBeginRange;
    property EndRange: Integer read FEndRange;

    { Current value. When setting this property, always make sure
      that it's within the allowed range. }
    property Value: Integer read FValue write FValue;

    procedure Draw(const Area: TArea); override;

    procedure KeyDown(Key: TKey; C: char;
      ParentMenu: TGLMenu); override;

    procedure MouseDown(const MouseX, MouseY: Single; Button: TMouseButton;
      const Area: TArea; ParentMenu: TGLMenu); override;

    procedure MouseMove(const NewX, NewY: Single;
      const MousePressed: TMouseButtons;
      const Area: TArea; ParentMenu: TGLMenu); override;

    function ValueToStr(const AValue: Integer): string; virtual;
  end;

  { How TGLMenu.Position will be interpreted.

    This type is used for two cases:
    @orderedList(

      @item(PositionRelativeMenu: specifies (for X or Y)
        what point of menu area is affected by Position value.
        In this case,
        @unorderedList(
          @itemSpacing Compact
          @item(prLowerBorder means that we want to
            align left (or bottom) border of the menu area,)
          @item(prMiddle means that we want to align middle of the menu area,)
          @item(prHigherBorder means that we want to align right
            (or top) border of the menu area.))
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
        menu area middle. If Position is zero, then menu is just in the
        middle of the screen.)
      @item(If both are prHigherBorder, then Position specifies position
        of right/top menu border relative to right/top screen border.
        Position should always be <= 0 is such cases,
        otherwise there is no way for the menu to be completely visible.)
    )

    In TGLMenu.DesignerMode you can see a line connecting the appropriate
    screen position (from PositionRelativeScreen) to the appropriate
    menu position (from PositionRelativeMenu) and you can experiment
    with these settings.
  }
  TPositionRelative = (
    prLowerBorder,
    prMiddle,
    prHigherBorder);

  { A menu displayed in OpenGL.

    Note that all 2d positions and sizes for this class are interpreted
    as pixel positions on your 2d screen (for glRaster, glBitmap etc.)
    and also as normal positions (for glTranslatef etc.) on your 2d screen.
    Smaller x positions are considered more to the left,
    smaller y positions are considered lower.
    Stating it simpler: just make sure that your OpenGL projection is
    @code(ProjectionGLOrtho(0, Glwin.Width, 0, Glwin.Height);) }
  TGLMenu = class
  private
    FItems: TStringList;
    FCurrentItem: Integer;
    FPositionRelativeMenuX: TPositionRelative;
    FPositionRelativeMenuY: TPositionRelative;
    FPositionRelativeScreenX: TPositionRelative;
    FPositionRelativeScreenY: TPositionRelative;
    FAreas: TDynAreaArray;
    FAccessoryAreas: TDynAreaArray;
    FAllItemsArea: TArea;
    FKeyNextItem: TKey;
    FKeyPreviousItem: TKey;
    FKeySelectItem: TKey;
    FKeySliderDecrease: TKey;
    FKeySliderIncrease: TKey;
    GLList_DrawFadeRect: TGLuint;
    MenuAnimation: Single;
    FCurrentItemBorderColor1: TVector3Single;
    FCurrentItemBorderColor2: TVector3Single;
    FCurrentItemColor: TVector3Single;
    FNonCurrentItemColor: TVector3Single;
    MaxItemWidth: Single;
    FRegularSpaceBetweenItems: Cardinal;
    FDrawBackgroundRectangle: boolean;
    { Item accessory that currently has "grabbed" the mouse.
      -1 if none. }
    ItemAccessoryGrabbed: Integer;
    function GetCurrentItem: Integer;
    procedure SetCurrentItem(const Value: Integer);

    FDesignerModeWindow: TGLWindow;
    FDesignerMode: boolean;
    procedure SetDesignerMode(const Value: boolean);

    LastWindowWidth, LastWindowHeight: Cardinal;
    FPositionAbsolute,
      PositionScreenRelativeMove, PositionMenuRelativeMove: TVector2_Single;
  public
    constructor Create;
    destructor Destroy; override;

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
    Position: TVector2_Single;

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

    { PositionAbsolute expresses the position of the menu area
      independently from all PositionRelative* properties.
      You can think of it as "What value would Position have
      if all PositionRelative* were equal prLowerBorder".

      An easy exercise for the reader is to check implementation that when
      all PositionRelative* are prLowerBorder, PositionAbsolute is indeed
      always equal to Position :)

      This is read-only, is calculated by FixItemsAreas.
      It's calculated anyway because out drawing code needs this.
      You may find it useful if you want to draw something relative to menu
      position. }
    property PositionAbsolute: TVector2_Single
      read FPositionAbsolute;

    { Items of this menu.

      Note that Objects of this class have special meaning: they must
      be either nil or some TGLMenuItemAccessory instance
      (different TGLMenuItemAccessory instance for each item).
      When freeing this TGLMenu instance, note that we will also
      free all Items.Objects. }
    property Items: TStringList read FItems;

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

    { Release things associated with OpenGL context.
      This will be also automatically called from destructor. }
    procedure CloseGL;

    { You must call FixItemsAreas between last modification of
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
      You can call this only while OpenGL context is initialized. }
    procedure FixItemsAreas(const WindowWidth, WindowHeight: Cardinal);

    { These are initialized by FixItemsAreas.
      They are absolutely read-only for the user of this class.
      You can use them to do some graphic effects, when you e.g.
      want to draw something on the screen that is somehow positioned
      relative to some menu item or to whole menu area.
      Note that AllItemsArea includes also some outside margin. }
    property Areas: TDynAreaArray read FAreas;
    property AllItemsArea: TArea read FAllItemsArea;
    property AccessoryAreas: TDynAreaArray read FAccessoryAreas;

    procedure Draw; virtual;

    property KeyNextItem: TKey read FKeyNextItem write FKeyNextItem
      default DefaultGLMenuKeyNextItem;
    property KeyPreviousItem: TKey read FKeyPreviousItem write FKeyPreviousItem
      default DefaultGLMenuKeyPreviousItem;
    property KeySelectItem: TKey read FKeySelectItem write FKeySelectItem
      default DefaultGLMenuKeySelectItem;
    property KeySliderIncrease: TKey
      read FKeySliderIncrease write FKeySliderIncrease
      default DefaultGLMenuKeySliderIncrease;
    property KeySliderDecrease: TKey
      read FKeySliderDecrease write FKeySliderDecrease
      default DefaultGLMenuKeySliderDecrease;

    procedure KeyDown(Key: TKey; C: char);

    { Call this when user moves the mouse.
      NewX, NewY is in OpenGL 2d screen coordinates, so usually
      (when you call this from TGLWindow.OnMouseMove) you will
      have to flip the NewY like @code(Glwin.Height - NewY). }
    procedure MouseMove(const NewX, NewY: Single;
      const MousePressed: TMouseButtons);

    procedure MouseDown(const MouseX, MouseY: Single; Button: TMouseButton;
      const MousePressed: TMouseButtons);
    procedure MouseUp(const MouseX, MouseY: Single; Button: TMouseButton;
      const MousePressed: TMouseButtons);

    procedure Idle(const CompSpeed: Single);

    { Called when user will select CurrentItem, either with mouse
      or with keyboard. }
    procedure CurrentItemSelected; virtual;

    { This will be called when the TGLMenuItemAccessory assigned
      to CurrentItem will signal that it's value changed
      because of user interface actions (KeyDown, MouseDown etc.).

      Note that this will not be called when you just set
      Value of some property.

      In this class this just calls SomethingChanged. }
    procedure CurrentItemAccessoryValueChanged; virtual;

    { Called when CurrentItem changed.
      But *not* when CurrentItem changed because of Items.Count changes.
      In this class this just calls SomethingChanged. }
    procedure CurrentItemChanged; virtual;

    { Called when various things changed.
      E.g. color of current item changed.
      CurrentItemChanged also calls this.
      Or some TGLMenuItemAccessory may call this when some value changed. }
    procedure SomethingChanged; virtual;

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

    property DrawBackgroundRectangle: boolean
      read FDrawBackgroundRectangle write FDrawBackgroundRectangle
      default true;

    { Additional vertical space, in pixels, between menu items.

      If you want more control over it (if you want to add more/less
      space between some menu items), override SpaceBetweenItems method. }
    property RegularSpaceBetweenItems: Cardinal
      read FRegularSpaceBetweenItems write FRegularSpaceBetweenItems
      default DefaultRegularSpaceBetweenItems;

    { Return the space needed before NextItemIndex.
      This will be a space between NextItemIndex - 1 and NextItemIndex
      (this method will not be called for NextItemIndex = 0).

      Default implementation in this class simply returns
      RegularSpaceBetweenItems always.

      Note that this is used only at FixItemsAreas call.
      So when some variable affecting the implementation of this changes,
      you should call FixItemsAreas again. }
    function SpaceBetweenItems(const NextItemIndex: Cardinal): Cardinal; virtual;

    { "Designer mode" is useful for a developer to visually design
      some properties of TGLMenu.

      Note that you can set DesignerModeWindow before setting this
      to @true, then we will set mouse position when entering DesignerMode
      to match current menu position. This is usually desirable (otherwise
      slight mouse move will immediately change menu position).
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

    property DesignerModeWindow: TGLWindow
      read FDesignerModeWindow write FDesignerModeWindow;
  end;

var
  { These fonts will be automatically initialized by any TGLMenu operation
    that require them. You can set them yourself or just let TGLMenu
    to set it.

    YOU MUST RELEASE THEM BY GLMenuCloseGL. Don't forget about it.

    @groupBegin }
  MenuFont: TGLBitmapFont;
  SliderFont: TGLBitmapFont;
  { @groupEnd }

{ This releases some fonts, images, display lists that were created
  during GLMenu lifetime when necessary. You must call this
  when you ended using GLMenu things. }
procedure GLMenuCloseGL;

type
  TFilePathFunction = function: string;

var
  { This function will be used when loading any external files needed
    by menu. It indicates the path from where to load certain image
    files. It should return a path, terminated with final PathDelim.
    Currently, it's used to search for files
    @unorderedList(
      @itemSpacing Compact
      @item menu_slider.png
      @item menu_slider_position.png
    )

    If it's not assigned, we'll use the path
    @code(ProgramDataPath + 'data' + PathDelim + 'menu_bg' + PathDelim). }
  GetMenuImagesPath: TFilePathFunction;

implementation

uses SysUtils, KambiUtils, Images, KambiFilesUtils,
  BFNT_BitstreamVeraSans_m10_Unit, KambiStringUtils, GLImages;

procedure SliderFontInit;
begin
  if SliderFont = nil then
    SliderFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans_m10);
end;

procedure MenuFontInit;
begin
  if MenuFont = nil then
    MenuFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

var
  ImageSlider: TImage;
  ImageSliderPosition: TImage;
  GLList_ImageSlider: TGLuint;
  GLList_ImageSliderPosition: TGLuint;

procedure ImageSliderInit;

  function DoGetMenuImagesPath: string;
  begin
    if Assigned(GetMenuImagesPath) then
      Result := GetMenuImagesPath() else
      Result := ProgramDataPath + 'data' + PathDelim + 'menu_bg' + PathDelim;
  end;

begin
  if ImageSlider = nil then
    ImageSlider := LoadImage(DoGetMenuImagesPath + 'menu_slider.png',
      [TRGBImage], []);

  if ImageSliderPosition = nil then
    ImageSliderPosition :=
      LoadImage(DoGetMenuImagesPath + 'menu_slider_position.png',
      [TRGBImage], []);

  if GLList_ImageSlider = 0 then
    GLList_ImageSlider := ImageDrawToDisplayList(ImageSlider);

  if GLList_ImageSliderPosition = 0 then
    GLList_ImageSliderPosition := ImageDrawToDisplayList(ImageSliderPosition);
end;

procedure GLMenuCloseGL;
begin
  FreeAndNil(MenuFont);
  FreeAndNil(SliderFont);
  glFreeDisplayList(GLList_ImageSlider);
  glFreeDisplayList(GLList_ImageSliderPosition);
  FreeAndNil(ImageSlider);
  FreeAndNil(ImageSliderPosition);
end;

{ TGLMenuItemAccessory ------------------------------------------------------ }

procedure TGLMenuItemAccessory.KeyDown(Key: TKey; C: char;
  ParentMenu: TGLMenu);
begin
  { Nothing to do in this class. }
end;

procedure TGLMenuItemAccessory.MouseDown(
  const MouseX, MouseY: Single; Button: TMouseButton;
  const Area: TArea; ParentMenu: TGLMenu);
begin
  { Nothing to do in this class. }
end;

procedure TGLMenuItemAccessory.MouseMove(const NewX, NewY: Single;
  const MousePressed: TMouseButtons;
  const Area: TArea; ParentMenu: TGLMenu);
begin
  { Nothing to do in this class. }
end;

{ TGLMenuItemArgument -------------------------------------------------------- }

constructor TGLMenuItemArgument.Create(const AMaximumValueWidth: Single);
begin
  inherited Create;
  FMaximumValueWidth := AMaximumValueWidth;
end;

class function TGLMenuItemArgument.TextWidth(const Text: string): Single;
begin
  MenuFontInit;
  Result := MenuFont.TextWidth(Text);
end;

function TGLMenuItemArgument.GetWidth(MenuFont: TGLBitmapFont): Single;
begin
  Result := MaximumValueWidth;
end;

procedure TGLMenuItemArgument.Draw(const Area: TArea);
begin
  MenuFontInit;

  glPushMatrix;
    glTranslatef(Area.X0, Area.Y0 + MenuFont.Descend, 0);
    glColorv(LightGreen3Single);
    glRasterPos2i(0, 0);
    MenuFont.Print(Value);
  glPopMatrix;
end;

{ TGLMenuBooleanArgument ----------------------------------------------------- }

constructor TGLMenuBooleanArgument.Create(const AValue: boolean);
begin
  inherited Create(
    Max(TGLMenuItemArgument.TextWidth(BoolToStrYesNo[true]),
        TGLMenuItemArgument.TextWidth(BoolToStrYesNo[false])));
  FBooleanValue := AValue;
  inherited Value := BoolToStrYesNo[Value];
end;

procedure TGLMenuBooleanArgument.SetValue(const AValue: boolean);
begin
  if FBooleanValue <> AValue then
  begin
    FBooleanValue := AValue;
    inherited Value := BoolToStrYesNo[Value];
  end;
end;

{ TGLMenuSlider -------------------------------------------------------------- }

constructor TGLMenuSlider.Create;
begin
  inherited;
  FDisplayValue := true;
end;

function TGLMenuSlider.GetWidth(MenuFont: TGLBitmapFont): Single;
begin
  ImageSliderInit;
  Result := ImageSlider.Width;
end;

procedure TGLMenuSlider.Draw(const Area: TArea);
begin
  ImageSliderInit;

  glPushMatrix;
    glTranslatef(Area.X0, Area.Y0 + (Area.Height - ImageSlider.Height) / 2, 0);
    glRasterPos2i(0, 0);
    glCallList(GLList_ImageSlider);
  glPopMatrix;
end;

const
  ImageSliderPositionMargin = 2;

procedure TGLMenuSlider.DrawSliderPosition(const Area: TArea;
  const Position: Single);
begin
  ImageSliderInit;

  glPushMatrix;
    glTranslatef(Area.X0 + ImageSliderPositionMargin +
      MapRange(Position, 0, 1, 0,
        ImageSlider.Width - 2 * ImageSliderPositionMargin -
        ImageSliderPosition.Width),
      Area.Y0 + (Area.Height - ImageSliderPosition.Height) / 2, 0);
    glRasterPos2i(0, 0);
    glCallList(GLList_ImageSliderPosition);
  glPopMatrix;
end;

function TGLMenuSlider.XCoordToSliderPosition(
  const XCoord: Single; const Area: TArea): Single;
begin
  { I subtract below ImageSliderPosition.Width div 2
    because we want XCoord to be in the middle
    of ImageSliderPosition, not on the left. }
  Result := MapRange(XCoord - ImageSliderPosition.Width div 2,
    Area.X0 + ImageSliderPositionMargin,
    Area.X0 + ImageSlider.Width - 2 * ImageSliderPositionMargin -
    ImageSliderPosition.Width, 0, 1);

  Clamp(Result, 0, 1);
end;

procedure TGLMenuSlider.DrawSliderText(
  const Area: TArea; const Text: string);
begin
  SliderFontInit;

  glPushMatrix;
    glTranslatef(
      Area.X0 + (Area.Width - SliderFont.TextWidth(Text)) / 2,
      Area.Y0 + (Area.Height - SliderFont.RowHeight) / 2, 0);
    glColorv(Black3Single);
    glRasterPos2i(0, 0);
    SliderFont.Print(Text);
  glPopMatrix;
end;

{ TGLMenuFloatSlider --------------------------------------------------------- }

constructor TGLMenuFloatSlider.Create(
  const ABeginRange, AEndRange, AValue: Single);
begin
  inherited Create;
  FBeginRange := ABeginRange;
  FEndRange := AEndRange;
  FValue := AValue;
end;

procedure TGLMenuFloatSlider.Draw(const Area: TArea);
begin
  inherited;

  DrawSliderPosition(Area, MapRange(Value, BeginRange, EndRange, 0, 1));

  if DisplayValue then
    DrawSliderText(Area, ValueToStr(Value));
end;

procedure TGLMenuFloatSlider.KeyDown(Key: TKey; C: char;
  ParentMenu: TGLMenu);
var
  ValueChange: Single;
begin
  { TODO: TGLMenuFloatSlider should rather get "smooth" changing of Value ? }
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
      ParentMenu.CurrentItemAccessoryValueChanged;
    end else
    if Key = ParentMenu.KeySliderDecrease then
    begin
      FValue := Max(BeginRange, Value - ValueChange);
      ParentMenu.CurrentItemAccessoryValueChanged;
    end;
  end;
end;

procedure TGLMenuFloatSlider.MouseDown(
  const MouseX, MouseY: Single; Button: TMouseButton;
  const Area: TArea; ParentMenu: TGLMenu);
begin
  if Button = mbLeft then
  begin
    FValue := MapRange(XCoordToSliderPosition(MouseX, Area), 0, 1,
      BeginRange, EndRange);
    ParentMenu.CurrentItemAccessoryValueChanged;
  end;
end;

procedure TGLMenuFloatSlider.MouseMove(const NewX, NewY: Single;
  const MousePressed: TMouseButtons;
  const Area: TArea; ParentMenu: TGLMenu);
begin
  if mbLeft in MousePressed then
  begin
    FValue := MapRange(XCoordToSliderPosition(NewX, Area), 0, 1,
      BeginRange, EndRange);
    ParentMenu.CurrentItemAccessoryValueChanged;
  end;
end;

function TGLMenuFloatSlider.ValueToStr(const AValue: Single): string;
begin
  Result := Format('%f', [AValue]);
end;

{ TGLMenuIntegerSlider ------------------------------------------------------- }

constructor TGLMenuIntegerSlider.Create(
  const ABeginRange, AEndRange, AValue: Integer);
begin
  inherited Create;
  FBeginRange := ABeginRange;
  FEndRange := AEndRange;
  FValue := AValue;
end;

procedure TGLMenuIntegerSlider.Draw(const Area: TArea);
begin
  inherited;

  DrawSliderPosition(Area, MapRange(Value, BeginRange, EndRange, 0, 1));

  if DisplayValue then
    DrawSliderText(Area, ValueToStr(Value));
end;

procedure TGLMenuIntegerSlider.KeyDown(Key: TKey; C: char;
  ParentMenu: TGLMenu);
var
  ValueChange: Integer;
begin
  if Key <> K_None then
  begin
    ValueChange := 1;

    { KeySelectItem works just like KeySliderIncrease.
      Reasoning: see TGLMenuFloatSlider. }

    if (Key = ParentMenu.KeySelectItem) or
       (Key = ParentMenu.KeySliderIncrease) then
    begin
      FValue := Min(EndRange, Value + ValueChange);
      ParentMenu.CurrentItemAccessoryValueChanged;
    end else
    if Key = ParentMenu.KeySliderDecrease then
    begin
      FValue := Max(BeginRange, Value - ValueChange);
      ParentMenu.CurrentItemAccessoryValueChanged;
    end;
  end;
end;

function TGLMenuIntegerSlider.XCoordToValue(
  const XCoord: Single; const Area: TArea): Integer;
begin
  { We do additional Clamped over Round result to avoid any
    chance of floating-point errors due to lack of precision. }
  Result := Clamped(Round(
    MapRange(XCoordToSliderPosition(XCoord, Area), 0, 1,
      BeginRange, EndRange)), BeginRange, EndRange);
end;

procedure TGLMenuIntegerSlider.MouseDown(
  const MouseX, MouseY: Single; Button: TMouseButton;
  const Area: TArea; ParentMenu: TGLMenu);
begin
  if Button = mbLeft then
  begin
    FValue := XCoordToValue(MouseX, Area);
    ParentMenu.CurrentItemAccessoryValueChanged;
  end;
end;

procedure TGLMenuIntegerSlider.MouseMove(const NewX, NewY: Single;
  const MousePressed: TMouseButtons;
  const Area: TArea; ParentMenu: TGLMenu);
begin
  if mbLeft in MousePressed then
  begin
    FValue := XCoordToValue(NewX, Area);
    ParentMenu.CurrentItemAccessoryValueChanged;
  end;
end;

function TGLMenuIntegerSlider.ValueToStr(const AValue: Integer): string;
begin
  Result := IntToStr(AValue);
end;

{ TGLMenu -------------------------------------------------------------------- }

constructor TGLMenu.Create;
begin
  inherited;
  FItems := TStringList.Create;
  FCurrentItem := 0;
  FAreas := TDynAreaArray.Create;
  FAccessoryAreas := TDynAreaArray.Create;

  FPositionRelativeMenuX := prMiddle;
  FPositionRelativeMenuY := prMiddle;
  FPositionRelativeScreenX := prMiddle;
  FPositionRelativeScreenY := prMiddle;

  KeyNextItem := DefaultGLMenuKeyNextItem;
  KeyPreviousItem := DefaultGLMenuKeyPreviousItem;
  KeySelectItem := DefaultGLMenuKeySelectItem;
  KeySliderIncrease := DefaultGLMenuKeySliderIncrease;
  KeySliderDecrease := DefaultGLMenuKeySliderDecrease;

  FCurrentItemBorderColor1 := DefaultCurrentItemBorderColor1;
  FCurrentItemBorderColor2 := DefaultCurrentItemBorderColor2;
  FCurrentItemColor := DefaultCurrentItemColor;
  FNonCurrentItemColor := DefaultNonCurrentItemColor;

  FRegularSpaceBetweenItems := DefaultRegularSpaceBetweenItems;
  FDrawBackgroundRectangle := true;
end;

destructor TGLMenu.Destroy;
var
  I: Integer;
begin
  CloseGL;

  for I := 0 to Items.Count - 1 do
    Items.Objects[I].Free;
  FreeAndNil(FItems);

  FreeAndNil(FAccessoryAreas);
  FreeAndNil(FAreas);
  inherited;
end;

function TGLMenu.GetCurrentItem: Integer;
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

procedure TGLMenu.SetCurrentItem(const Value: Integer);
var
  OldCurrentItem, NewCurrentItem: Integer;
begin
  OldCurrentItem := CurrentItem;
  FCurrentItem := Value;
  NewCurrentItem := CurrentItem;
  if OldCurrentItem <> NewCurrentItem then
    CurrentItemChanged;
end;

procedure TGLMenu.NextItem;
begin
  if Items.Count <> 0 then
  begin
    if CurrentItem = Items.Count - 1 then
      CurrentItem := 0 else
      CurrentItem := CurrentItem + 1;
  end;
end;

procedure TGLMenu.PreviousItem;
begin
  if Items.Count <> 0 then
  begin
    if CurrentItem = 0 then
      CurrentItem := Items.Count - 1 else
      CurrentItem := CurrentItem - 1;
  end;
end;

procedure TGLMenu.CloseGL;
begin
  glFreeDisplayList(GLList_DrawFadeRect);
end;

function TGLMenu.SpaceBetweenItems(const NextItemIndex: Cardinal): Cardinal;
begin
  Result := RegularSpaceBetweenItems;
end;

const
  MarginBeforeAccessory = 20;

procedure TGLMenu.FixItemsAreas(const WindowWidth, WindowHeight: Cardinal);
const
  AllItemsAreaMargin = 30;
var
  I: Integer;
  WholeItemWidth, MaxAccessoryWidth: Single;
  ItemsBelowHeight: Cardinal;
begin
  LastWindowWidth := WindowWidth;
  LastWindowHeight := WindowHeight;

  MenuFontInit;

  ItemAccessoryGrabbed := -1;

  FAccessoryAreas.Count := Items.Count;

  { calculate FAccessoryAreas[].Width, MaxItemWidth, MaxAccessoryWidth }

  MaxItemWidth := 0.0;
  MaxAccessoryWidth := 0.0;
  for I := 0 to Items.Count - 1 do
  begin
    MaxTo1st(MaxItemWidth, MenuFont.TextWidth(Items[I]));

    if Items.Objects[I] <> nil then
      FAccessoryAreas.Items[I].Width :=
        TGLMenuItemAccessory(Items.Objects[I]).GetWidth(MenuFont) else
      FAccessoryAreas.Items[I].Width := 0.0;

    MaxTo1st(MaxAccessoryWidth, FAccessoryAreas.Items[I].Width);
  end;

  { calculate FAllItemsArea Width and Height }

  FAllItemsArea.Width := MaxItemWidth;
  if MaxAccessoryWidth <> 0.0 then
    FAllItemsArea.Width += MarginBeforeAccessory + MaxAccessoryWidth;

  FAllItemsArea.Height := 0;
  for I := 0 to Items.Count - 1 do
  begin
    FAllItemsArea.Height += MenuFont.RowHeight;
    if I > 0 then
      FAllItemsArea.Height += Integer(SpaceBetweenItems(I));
  end;

  FAllItemsArea.Width += 2 * AllItemsAreaMargin;
  FAllItemsArea.Height += 2 * AllItemsAreaMargin;

  { calculate Areas Widths and Heights }

  Areas.Count := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if MaxAccessoryWidth <> 0.0 then
      WholeItemWidth := MaxItemWidth + MarginBeforeAccessory + MaxAccessoryWidth else
      WholeItemWidth := MenuFont.TextWidth(Items[I]);
    Areas.AppendItem(Area(0, 0, WholeItemWidth,
      MenuFont.Descend + MenuFont.RowHeight));
  end;

  { Now take into account Position, PositionRelative*
    and calculate PositionAbsolute.

    By the way, we also calculate PositionScreenRelativeMove
    and PositionMenuRelativeMove, but you don't have to worry about them
    too much, they are only for DesignerMode to visualize current
    PositionRelative* meaning. }

  case PositionRelativeScreenX of
    prLowerBorder : PositionScreenRelativeMove.Data[0] := 0;
    prMiddle      : PositionScreenRelativeMove.Data[0] := WindowWidth div 2;
    prHigherBorder: PositionScreenRelativeMove.Data[0] := WindowWidth;
    else raise EInternalError.Create('PositionRelative* = ?');
  end;

  case PositionRelativeScreenY of
    prLowerBorder : PositionScreenRelativeMove.Data[1] := 0;
    prMiddle      : PositionScreenRelativeMove.Data[1] := WindowHeight div 2;
    prHigherBorder: PositionScreenRelativeMove.Data[1] := WindowHeight;
    else raise EInternalError.Create('PositionRelative* = ?');
  end;

  case PositionRelativeMenuX of
    prLowerBorder : PositionMenuRelativeMove.Data[0] := 0;
    prMiddle      : PositionMenuRelativeMove.Data[0] := FAllItemsArea.Width / 2;
    prHigherBorder: PositionMenuRelativeMove.Data[0] := FAllItemsArea.Width;
    else raise EInternalError.Create('PositionRelative* = ?');
  end;

  case PositionRelativeMenuY of
    prLowerBorder : PositionMenuRelativeMove.Data[1] := 0;
    prMiddle      : PositionMenuRelativeMove.Data[1] := FAllItemsArea.Height / 2;
    prHigherBorder: PositionMenuRelativeMove.Data[1] := FAllItemsArea.Height;
    else raise EInternalError.Create('PositionRelative* = ?');
  end;

  FPositionAbsolute := Position + PositionScreenRelativeMove - PositionMenuRelativeMove;

  { Calculate positions of all areas. }

  { we iterate downwards from Areas.High to 0, updating ItemsBelowHeight.
    That's OpenGL (and so, Areas.Items[I].Y0) coordinates grow up, while
    our menu items are specified from highest to lowest. }
  ItemsBelowHeight := 0;

  for I := Areas.High downto 0 do
  begin
    Areas.Items[I].X0 := PositionAbsolute.Data[0] + AllItemsAreaMargin;
    Areas.Items[I].Y0 := PositionAbsolute.Data[1] + AllItemsAreaMargin + ItemsBelowHeight;

    if I > 0 then
      ItemsBelowHeight += Cardinal(MenuFont.RowHeight + Integer(SpaceBetweenItems(I)));
  end;
  FAllItemsArea.X0 := PositionAbsolute.Data[0];
  FAllItemsArea.Y0 := PositionAbsolute.Data[1];

  { Calculate FAccessoryAreas[].X0, Y0, Height }
  for I := 0 to Areas.High do
  begin
    FAccessoryAreas.Items[I].X0 := Areas.Items[I].X0 +
      MaxItemWidth + MarginBeforeAccessory;
    FAccessoryAreas.Items[I].Y0 := Areas.Items[I].Y0;
    FAccessoryAreas.Items[I].Height := Areas.Items[I].Height;
  end;

  { Calculate GLList_DrawFadeRect }

  if GLList_DrawFadeRect = 0 then
    GLList_DrawFadeRect := glGenListsCheck(1, 'TGLMenu.FixItemsAreas');
  glNewList(GLList_DrawFadeRect, GL_COMPILE);
  try
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
      glColor4f(0, 0, 0, 0.4);
      glRectf(FAllItemsArea.X0, FAllItemsArea.Y0,
        FAllItemsArea.X0 + FAllItemsArea.Width,
        FAllItemsArea.Y0 + FAllItemsArea.Height);
    glDisable(GL_BLEND);
  finally glEndList end;
end;

procedure TGLMenu.Draw;

  procedure DrawPositionRelativeLine;
  begin
    glColorv(White3Single);
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
  if DrawBackgroundRectangle then
    glCallList(GLList_DrawFadeRect);

  for I := 0 to Items.Count - 1 do
  begin
    if I = CurrentItem then
    begin
      { Calculate CurrentItemBorderColor }
      if MenuAnimation <= 0.5 then
        CurrentItemBorderColor := VLerp(
          MapRange(MenuAnimation, 0, 0.5, 0, 1),
          CurrentItemBorderColor1, CurrentItemBorderColor2) else
        CurrentItemBorderColor := VLerp(
          MapRange(MenuAnimation, 0.5, 1, 0, 1),
          CurrentItemBorderColor2, CurrentItemBorderColor1);

      glColorv(CurrentItemBorderColor);
      DrawGLRectBorder(
        Areas.Items[I].X0 - CurrentItemBorderMargin,
        Areas.Items[I].Y0,
        Areas.Items[I].X0 + Areas.Items[I].Width + CurrentItemBorderMargin,
        Areas.Items[I].Y0 + Areas.Items[I].Height);

      glColorv(CurrentItemColor);
    end else
      glColorv(NonCurrentItemColor);

    glPushMatrix;
      glTranslatef(Areas.Items[I].X0, Areas.Items[I].Y0 + MenuFont.Descend, 0);
      glRasterPos2i(0, 0);
      MenuFont.Print(Items[I]);
    glPopMatrix;

    if Items.Objects[I] <> nil then
      TGLMenuItemAccessory(Items.Objects[I]).Draw(FAccessoryAreas.Items[I]);
  end;

  if DesignerMode then
    DrawPositionRelativeLine;
end;

procedure TGLMenu.KeyDown(Key: TKey; C: char);

  procedure CurrentItemAccessoryKeyDown;
  begin
    if Items.Objects[CurrentItem] <> nil then
    begin
      TGLMenuItemAccessory(Items.Objects[CurrentItem]).KeyDown(
        Key, C, Self);
    end;
  end;

  procedure IncPositionRelative(var P: TPositionRelative);
  var
    OldChange, NewChange: TVector2_Single;
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
      After P changes and FixItemsAreas does it's work, it's NewChange. So it's
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

    { Call FixItemsAreas only to set new
      PositionScreenRelativeMove - PositionMenuRelativeMove. }
    FixItemsAreas(LastWindowWidth, LastWindowHeight);

    NewChange := PositionScreenRelativeMove - PositionMenuRelativeMove;
    Position := Position + OldChange - NewChange;

    { Call FixItemsAreas once again, since Position changed. }
    FixItemsAreas(LastWindowWidth, LastWindowHeight);
  end;

const
  PositionRelativeName: array [TPositionRelative] of string =
  ( 'prLowerBorder',
    'prMiddle',
    'prHigherBorder' );
  BooleanToStr: array [boolean] of string=('false','true');

begin
  if Key = KeyPreviousItem then
    PreviousItem else
  if Key = KeyNextItem then
    NextItem else
  if Key = KeySelectItem then
  begin
    CurrentItemAccessoryKeyDown;
    CurrentItemSelected;
  end else
    CurrentItemAccessoryKeyDown;

  if DesignerMode then
  begin
    case C of
      CtrlB:
        DrawBackgroundRectangle := not DrawBackgroundRectangle;
      'x': IncPositionRelative(FPositionRelativeScreenX);
      'y': IncPositionRelative(FPositionRelativeScreenY);
      CtrlX: IncPositionRelative(FPositionRelativeMenuX);
      CtrlY: IncPositionRelative(FPositionRelativeMenuY);
      CtrlD:
        InfoWrite(Format(
          'Position.Init(%f, %f);' +nl+
          'PositionRelativeScreenX := %s;' +nl+
          'PositionRelativeScreenY := %s;' +nl+
          'PositionRelativeMenuX := %s;' +nl+
          'PositionRelativeMenuY := %s;' +nl+
          'DrawBackgroundRectangle := %s;',
          [ Position.Data[0],
            Position.Data[1],
            PositionRelativeName[PositionRelativeScreenX],
            PositionRelativeName[PositionRelativeScreenY],
            PositionRelativeName[PositionRelativeMenuX],
            PositionRelativeName[PositionRelativeMenuY],
            BooleanToStr[DrawBackgroundRectangle] ]));
    end;
  end;
end;

procedure TGLMenu.MouseMove(const NewX, NewY: Single;
  const MousePressed: TMouseButtons);

  procedure ChangePosition;
  var
    NewPositionAbsolute: TVector2_Single;
  begin
    NewPositionAbsolute.Init(NewX, NewY);
    { I want Position set such that (NewX, NewY) are lower/left corner
      of menu area. I know that
        PositionAbsolute = Position + PositionScreenRelativeMove - PositionMenuRelativeMove;
      (NewX, NewY) are new PositionAbsolute, so I can calculate from
      this new desired Position value. }
    Position := NewPositionAbsolute - PositionScreenRelativeMove + PositionMenuRelativeMove;
    FixItemsAreas(LastWindowWidth, LastWindowHeight);
  end;

var
  NewItemIndex: Integer;
begin
  NewItemIndex := Areas.FindArea(NewX, NewY);
  if NewItemIndex <> -1 then
  begin
    if NewItemIndex <> CurrentItem then
      CurrentItem := NewItemIndex else
    { If NewItemIndex = CurrentItem and NewItemIndex <> -1,
      then user just moves mouse within current item.
      So maybe we should call TGLMenuItemAccessory.MouseMove. }
    if (Items.Objects[CurrentItem] <> nil) and
       (PointInArea(NewX, NewY, FAccessoryAreas.Items[CurrentItem])) and
       (ItemAccessoryGrabbed = CurrentItem) then
      TGLMenuItemAccessory(Items.Objects[CurrentItem]).MouseMove(
        NewX, NewY, MousePressed,
        FAccessoryAreas.Items[CurrentItem], Self);
  end;

  if DesignerMode then
    ChangePosition;
end;

procedure TGLMenu.MouseDown(const MouseX, MouseY: Single; Button: TMouseButton;
  const MousePressed: TMouseButtons);
var
  NewItemIndex: Integer;
begin
  if (CurrentItem <> -1) and
     (Items.Objects[CurrentItem] <> nil) and
     (PointInArea(MouseX, MouseY, FAccessoryAreas.Items[CurrentItem])) and
     (MousePressed - [Button] = []) then
  begin
    ItemAccessoryGrabbed := CurrentItem;
    TGLMenuItemAccessory(Items.Objects[CurrentItem]).MouseDown(
      MouseX, MouseY, Button, FAccessoryAreas.Items[CurrentItem], Self);
  end;

  if Button = mbLeft then
  begin
    NewItemIndex := Areas.FindArea(MouseX, MouseY);
    if NewItemIndex <> -1 then
    begin
      CurrentItem := NewItemIndex;
      CurrentItemSelected;
    end;
  end;
end;

procedure TGLMenu.MouseUp(const MouseX, MouseY: Single; Button: TMouseButton;
  const MousePressed: TMouseButtons);
begin
  { This is actually not needed, smart check for
    (MousePressed - [Button] = []) inside MouseDown handles everything,
    so we don't have to depend on MouseUp for ungrabbing.
    But I do it here, just "to keep my state as current as possible". }
  if MousePressed = [] then
    ItemAccessoryGrabbed := -1;

  { Nothing to do here for now. }
end;

procedure TGLMenu.Idle(const CompSpeed: Single);
begin
  MenuAnimation += 0.01 * CompSpeed;
  MenuAnimation := Frac(MenuAnimation);
  SomethingChanged;
end;

procedure TGLMenu.CurrentItemSelected;
begin
  { Nothing to do in this class. }
end;

procedure TGLMenu.CurrentItemChanged;
begin
  SomethingChanged;
end;

procedure TGLMenu.CurrentItemAccessoryValueChanged;
begin
  SomethingChanged;
end;

procedure TGLMenu.SomethingChanged;
begin
  { Nothing to do in this class. }
end;

procedure TGLMenu.SetDesignerMode(const Value: boolean);
begin
  if (not FDesignerMode) and Value and (DesignerModeWindow <> nil) then
  begin
    if not DesignerModeWindow.Closed then
      DesignerModeWindow.SetMousePosition(
        Round(PositionAbsolute.Data[0]),
        DesignerModeWindow.Height - Round(PositionAbsolute.Data[1]));
  end;

  FDesignerMode := Value;
end;

end.
