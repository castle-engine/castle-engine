{
  Copyright 2006-2017 Michalis Kamburelis.

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

{$I castleconf.inc}

interface

uses Classes, CastleVectors, CastleFonts, CastleControls,
  CastleGLUtils, CastleUIControls, CastleKeysMouse, CastleColors,
  CastleRectangles;

type
  TCastleOnScreenMenu = class;

  { Button that looks nice as an "accessory"
    attached to the TCastleOnScreenMenu item. }
  TCastleMenuButton = class(TCastleButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CustomBackground default true;
    property CustomTextColorUse default true;
    property PaddingHorizontal default 0;
    property PaddingVertical default 0;
  end;

  { Button that looks nice as an "accessory" that can be toggled
    (shows "yes" / "no" depending on @link(TCastleButton.Pressed)),
    attached to the TCastleOnScreenMenu item. }
  TCastleMenuToggle = class(TCastleMenuButton)
  private
    { TODO: if would be nice if Width of this component was appropriate
      for the longest possible string ("Yes" in this case).
      This way the menu could be larger, in preparation of it. }
    FAutoToggle: boolean;
  protected
    procedure SetPressed(const Value: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoClick; override;
  published
    property Toggle default true;
    { Automatically negate @link(TCastleButton.Pressed Pressed) on each click.
      This makes it easy to handle this component, often it means that you
      don't need to handle it's @link(TCastleButton.OnClick OnClick) event,
      and you only need to observe it's @link(TCastleButton.Pressed Pressed)
      value. }
    property AutoToggle: boolean read FAutoToggle write FAutoToggle default false;
  end;

  { On-screen menu, with all menu items displayed on the screen,
    one under the other. Typical for game menus.
    Normal tools may prefer to use the menu bar instead of this
    (for example TCastleWindowCustom.Menu, or normal Lazarus menu).

    Each menu item can have an "accessory", for example an associated
    slider (from TCastleFloatSlider, any TUIControl is OK).
    This allows to use this menu also for settings. }
  TCastleOnScreenMenu = class(TUIControlFont)
  private
    FCaptureAllEvents: boolean;
    FOnClick: TNotifyEvent;
    FCurrentItem: Integer;
    { Calculated menu items positions and sizes, by RecalculateSize. }
    FRectangles: TRectangleList;
    FWidth, FHeight: Cardinal; //< calculated necessary size
    FKeyNextItem: TKey;
    FKeyPreviousItem: TKey;
    FKeySelectItem: TKey;
    MenuAnimation: Single;
    FCurrentItemBorderColor1: TCastleColor;
    FCurrentItemBorderColor2: TCastleColor;
    FCurrentItemColor: TCastleColor;
    FNonCurrentItemColor: TCastleColor;
    FNonFocusableItemColor: TCastleColor;
    MaxItemWidth: Integer;
    FRegularSpaceBetweenItems: Cardinal;
    FDrawBackgroundRectangle: boolean;
    FDrawFocusedBorder: boolean;
    FBackgroundOpacityFocused, FBackgroundOpacityNotFocused: Single;
    function GetCurrentItem: Integer;
    procedure SetCurrentItem(const Value: Integer);
    procedure SetRegularSpaceBetweenItems(const Value: Cardinal);
    function FindChildIndex(const ScreenPosition: TVector2Single): Integer;
  protected
    procedure UIScaleChanged; override;
    { Decide whether a children control is focusable or not.
      This is used to decide should we show an "active frame" around this item
      in the menu. It @italic(does not) determine whether the mouse events
      actually reach the child control.

      By default, a single TCastleLabel (without children) is considered
      "not focusable". Everything else is focusable. }
    function ControlFocusable(const C: TUIControl): boolean; virtual;
  public
    const
      DefaultMenuKeyNextItem = K_Down;
      DefaultMenuKeyPreviousItem = K_Up;
      DefaultMenuKeySelectItem = K_Enter;

      DefaultCurrentItemBorderColor1: TCastleColor = (1.0, 1.0, 1.0, 1.0) { White  }; { }
      DefaultCurrentItemBorderColor2: TCastleColor = (0.5, 0.5, 0.5, 1.0) { Gray   }; { }
      DefaultCurrentItemColor       : TCastleColor = (1.0, 1.0, 0.0, 1.0) { Yellow }; { }
      DefaultNonCurrentItemColor    : TCastleColor = (1.0, 1.0, 1.0, 1.0) { White  }; { }
      DefaultNonFocusableItemColor  : TCastleColor = (0.75, 0.75, 0.75, 1.0) { Light Gray }; { }

      DefaultRegularSpaceBetweenItems = 10;
      DefaultBackgroundOpacityNotFocused = 0.4;
      DefaultBackgroundOpacityFocused = 0.7;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(const S: string);
    procedure Add(const S: string; const Accessory: TUIControl);
    procedure Add(const S: string; const ItemOnClick: TNotifyEvent);
    procedure Add(const NewItem: TUIControl);

    { Currently selected child index.
      This is always some number between @code(0) and @code(ControlsCount - 1).
      It can also be @code(-1) to indicate "no child is selected".
      When there are no controls (@code(ControlsCount = 0)) it is always @code(-1).

      You can change it by code.
      Assigning here an invalid value (e.g. larger then @code(ControlsCount - 1))
      will be automatically fixed. Also when the children change
      (and so ControlsCount changes), this is automatically fixed if necessary.

      Changing this calls CurrentItemChanged automatically,
      but not CurrentItemChangedByUser. }
    property CurrentItem: Integer read GetCurrentItem write SetCurrentItem;

    { Change CurrentItem to next or previous.
      Usually you will just let this class call it internally
      (from Motion, KeyDown etc.) and will not need to call it yourself.

      @param(UserAction Determines should we play a sound when user changes
        menu items.)

      @groupBegin }
    procedure NextItem;
    procedure PreviousItem;
    { @groupEnd }

    { Calculate final positions, sizes of menu items on the screen.
      Usually this is called automatically when necessary. }
    procedure RecalculateSize;

    procedure Resize; override;

    function Rect: TRectangle; override;
    function CapturesEventsAtPosition(const Position: TVector2Single): boolean; override;
    procedure Render; override;

    property KeyNextItem: TKey read FKeyNextItem write FKeyNextItem
      default DefaultMenuKeyNextItem;
    property KeyPreviousItem: TKey read FKeyPreviousItem write FKeyPreviousItem
      default DefaultMenuKeyPreviousItem;
    property KeySelectItem: TKey read FKeySelectItem write FKeySelectItem
      default DefaultMenuKeySelectItem;

    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function AllowSuspendForInput: boolean; override;

    { Called when user will select CurrentItem, either with mouse
      or with keyboard. }
    procedure Click; virtual; deprecated 'use TCastleMenuButton and it''s OnClick event';

    { @deprecated Deprecated name for Click. }
    procedure CurrentItemSelected; virtual; deprecated 'use TCastleMenuButton and it''s OnClick event';

    { Called when CurrentItem changed.
      But *not* when CurrentItem changed because of ControlsCount changes.
      If you want to only get notified when the CurrentItem changed because
      of user actions, better override CurrentItemChangedByUser. }
    procedure CurrentItemChanged; virtual;

    { Called when CurrentItem changed by a user action (keyboard press,
      mouse move or click or something like that). }
    procedure CurrentItemChangedByUser; virtual;

    { 1st color of the border to display around focused child.
      Default value is DefaultCurrentItemBorderColor1 }
    property CurrentItemBorderColor1: TCastleColor
      read FCurrentItemBorderColor1 write FCurrentItemBorderColor1;
    { 2nd color of the border to display around focused child.
      Default value is DefaultCurrentItemBorderColor2 }
    property CurrentItemBorderColor2: TCastleColor
      read FCurrentItemBorderColor2 write FCurrentItemBorderColor2;
    { Label color for the focused child.
      Default value is DefaultCurrentItemColor }
    property CurrentItemColor: TCastleColor
      read FCurrentItemColor write FCurrentItemColor;
    { Label color for the non-focused but focusable child.
      Default value is DefaultNonCurrentItemColor }
    property NonCurrentItemColor: TCastleColor
      read FNonCurrentItemColor write FNonCurrentItemColor;
    { Label color for the non-focusable child.
      Default value is DefaultNonFocusableItemColor }
    property NonFocusableItemColor: TCastleColor
      read FNonFocusableItemColor write FNonFocusableItemColor;

    { Return the space needed before NextItemIndex.
      This will be a space between NextItemIndex - 1 and NextItemIndex
      (this method will not be called for NextItemIndex = 0).

      Default implementation in this class simply returns
      RegularSpaceBetweenItems always.

      Note that this is used only at RecalculateSize call.
      So when some variable affecting the implementation of this changes,
      you should call RecalculateSize again. }
    function SpaceBetweenItems(const NextItemIndex: Cardinal): Cardinal; virtual;
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

    property DrawBackgroundRectangle: boolean
      read FDrawBackgroundRectangle write FDrawBackgroundRectangle
      default true;

    { Additional vertical space, in pixels, between menu items.

      If you want more control over it (if you want to add more/less
      space between some menu items), override SpaceBetweenItems method. }
    property RegularSpaceBetweenItems: Cardinal
      read FRegularSpaceBetweenItems write SetRegularSpaceBetweenItems
      default DefaultRegularSpaceBetweenItems;

    { Draw a flashing border around the menu when we are focused. }
    property DrawFocusedBorder: boolean read FDrawFocusedBorder write FDrawFocusedBorder
      default true;

    { Called when user will select CurrentItem.
      @seealso Click }
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
      deprecated 'use TCastleMenuButton and it''s OnClick event';

    { Should menu intercept all key/mouse input, regardless if mouse position
      is over our rectangle.
      This affects key/mouse processing (menu processes input
      before all controls underneath), but not drawing (controls underneath
      are still visible as usual). }
    property CaptureAllEvents: boolean
      read FCaptureAllEvents write FCaptureAllEvents default false;
  end;

procedure Register;

implementation

uses SysUtils, CastleUtils, CastleImages, CastleFilesUtils, CastleClassUtils,
  CastleStringUtils, CastleGLImages, CastleSoundEngine;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleOnScreenMenu]);
end;

{ TCastleMenuButton ---------------------------------------------------------- }

constructor TCastleMenuButton.Create(AOwner: TComponent);
begin
  inherited;
  CustomBackground := true; // leave CustomBackgroundImage for transparent background
  CustomTextColorUse := true;
  CustomTextColor := LightGreen;
  PaddingHorizontal := 0;
  PaddingVertical := 0;
end;

{ TCastleMenuToggle ---------------------------------------------------------- }

constructor TCastleMenuToggle.Create(AOwner: TComponent);
begin
  inherited;
  Toggle := true;
  Caption := BoolToStr(Pressed, 'Yes', 'No');
end;

procedure TCastleMenuToggle.SetPressed(const Value: boolean);
begin
  inherited;
  Caption := BoolToStr(Pressed, 'Yes', 'No');
end;

procedure TCastleMenuToggle.DoClick;
begin
  inherited;
  if AutoToggle then
    Pressed := not Pressed;
end;

{ TCastleOnScreenMenu -------------------------------------------------------------------- }

constructor TCastleOnScreenMenu.Create(AOwner: TComponent);
begin
  inherited;
  FCurrentItem := 0;
  FRectangles := TRectangleList.Create;
  BackgroundOpacityNotFocused := DefaultBackgroundOpacityNotFocused;
  BackgroundOpacityFocused    := DefaultBackgroundOpacityFocused;

  KeyNextItem := DefaultMenuKeyNextItem;
  KeyPreviousItem := DefaultMenuKeyPreviousItem;
  KeySelectItem := DefaultMenuKeySelectItem;

  FCurrentItemBorderColor1 := DefaultCurrentItemBorderColor1;
  FCurrentItemBorderColor2 := DefaultCurrentItemBorderColor2;
  FCurrentItemColor := DefaultCurrentItemColor;
  FNonCurrentItemColor := DefaultNonCurrentItemColor;
  FNonFocusableItemColor := DefaultNonFocusableItemColor;

  FRegularSpaceBetweenItems := DefaultRegularSpaceBetweenItems;
  FDrawBackgroundRectangle := true;
  FDrawFocusedBorder := true;
end;

destructor TCastleOnScreenMenu.Destroy;
begin
  FreeAndNil(FRectangles);
  inherited;
end;

function TCastleOnScreenMenu.GetCurrentItem: Integer;
begin
  Result := FCurrentItem;

  { Make sure that CurrentItem conditions are OK.
    User can change the Controls list at any moment, so you cannot depend that
    FCurrentItem is Ok here. }

  if ControlsCount <> 0 then
  begin
    ClampVar(Result, 0, ControlsCount - 1);
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
var
  OldCurrentItem, NewCurrentItem: Integer;
begin
  if ControlsCount <> 0 then
  begin
    OldCurrentItem := CurrentItem;
    repeat
      if CurrentItem = ControlsCount - 1 then
        CurrentItem := 0
      else
        CurrentItem := CurrentItem + 1;
      { Loop until you find the next focusable control.
        Also, stop when you reach the original control (to avoid an infinite loop). }
    until ControlFocusable(Controls[CurrentItem]) or (CurrentItem = OldCurrentItem);

    NewCurrentItem := CurrentItem;
    if OldCurrentItem <> NewCurrentItem then
      CurrentItemChangedByUser;
  end;
end;

procedure TCastleOnScreenMenu.PreviousItem;
var
  OldCurrentItem, NewCurrentItem: Integer;
begin
  if ControlsCount <> 0 then
  begin
    OldCurrentItem := CurrentItem;
    repeat
      if CurrentItem = 0 then
        CurrentItem := ControlsCount - 1
      else
        CurrentItem := CurrentItem - 1;
      { Loop until you find the next focusable control.
        Also, stop when you reach the original control (to avoid an infinite loop). }
    until ControlFocusable(Controls[CurrentItem]) or (CurrentItem = OldCurrentItem);

    NewCurrentItem := CurrentItem;
    if OldCurrentItem <> NewCurrentItem then
      CurrentItemChangedByUser;
  end;
end;

function TCastleOnScreenMenu.SpaceBetweenItems(const NextItemIndex: Cardinal): Cardinal;
begin
  Result := RegularSpaceBetweenItems;
end;

const
  MarginBeforeAccessory = 20;

procedure TCastleOnScreenMenu.RecalculateSize;

  function ChildHeight(const Index: Integer): Integer;
  var
    C: TUIControl;
  begin
    C := Controls[Index];
    // ChildHeight assumes that TCastleLabel(C).AutoSize = true
    Assert((not (C is TCastleLabel)) or (TCastleLabel(C).AutoSize = true));
    Result := C.Rect.Height;
    { add accessory (slider etc.) height inside the menu item }
    if C.ControlsCount <> 0 then
      MaxVar(Result, C.Controls[0].Rect.Height);
  end;

const
  Padding = 30;
  ItemPaddingHorizontal = 5;
var
  I: Integer;
  WholeItemWidth, MaxAccessoryWidth: Integer;
  ItemsBelowHeight: Cardinal;
  MarginBeforeAccessoryScaled, PaddingScaled: Integer;
  C: TUIControl;
  R: TRectangle;
begin
  MarginBeforeAccessoryScaled := Round(UIScale * MarginBeforeAccessory);
  PaddingScaled := Round(UIScale * Padding);

  { calculate MaxItemWidth, MaxAccessoryWidth }

  MaxItemWidth := 0;
  MaxAccessoryWidth := 0;
  for I := 0 to ControlsCount - 1 do
  begin
    C := Controls[I];
    if C is TCastleLabel then
    begin
      TCastleLabel(C).AutoSize := true; // later we'll turn it back to false
      TCastleLabel(C).PaddingHorizontal := 0; // later we'll turn it back to nonzero
    end;
    MaxVar(MaxItemWidth, C.Rect.Width);
    { add accessory (slider etc.) width inside the menu item }
    if C.ControlsCount <> 0 then
      MaxVar(MaxAccessoryWidth, C.Controls[0].Rect.Width);
  end;

  { calculate FWidth and FHeight }

  FWidth := MaxItemWidth;
  if MaxAccessoryWidth <> 0 then
    FWidth += MarginBeforeAccessoryScaled + MaxAccessoryWidth;

  FHeight := 0;
  for I := 0 to ControlsCount - 1 do
  begin
    FHeight += ChildHeight(I);
    if I > 0 then
      FHeight += Round(UIScale * SpaceBetweenItems(I));
  end;

  FWidth += 2 * PaddingScaled + 2 * Round(UIScale * ItemPaddingHorizontal);
  FHeight += 2 * PaddingScaled;

  { calculate children Widths and Heights }

  FRectangles.Count := 0;
  for I := 0 to ControlsCount - 1 do
  begin
    if MaxAccessoryWidth <> 0 then
      WholeItemWidth := MaxItemWidth + MarginBeforeAccessoryScaled + MaxAccessoryWidth else
      WholeItemWidth := Controls[I].Rect.Width;
    FRectangles.Add(Rectangle(0, 0, WholeItemWidth, ChildHeight(I)));
  end;

  { Calculate positions of all rectangles. }

  { we iterate downwards from Rectangles.Count - 1 to 0, updating ItemsBelowHeight.
    That because our coordinates grow up,
    but our menu items are specified from highest to lowest. }
  ItemsBelowHeight := 0;

  for I := FRectangles.Count - 1 downto 0 do
  begin
    C := Controls[I];
    FRectangles.L[I].Left := PaddingScaled;
    FRectangles.L[I].Bottom := PaddingScaled + ItemsBelowHeight;
    R := FRectangles.L[I];

    // divide by UIScale, because TCastleLabel.Rect will multiply by it...
    C.Left   := Round(R.Left / UIScale);
    C.Bottom := Round(R.Bottom / UIScale);
    if C is TCastleLabel then
    begin
      TCastleLabel(C).AutoSize := false;
      TCastleLabel(C).PaddingHorizontal := ItemPaddingHorizontal;
      TCastleLabel(C).Width  := Round(R.Width / UIScale) + ItemPaddingHorizontal * 2;
      TCastleLabel(C).Height := Round(R.Height / UIScale);
    end;
    if I > 0 then
      ItemsBelowHeight += Cardinal(R.Height + Round(UIScale * SpaceBetweenItems(I)));

    if C.ControlsCount <> 0 then
      // divide by UIScale, because TCastleLabel.Rect will multiply by it...
      C.Controls[0].Left := Round((MaxItemWidth + MarginBeforeAccessoryScaled) / UIScale);
  end;
end;

procedure TCastleOnScreenMenu.Resize;
begin
  inherited;
  RecalculateSize;
end;

procedure TCastleOnScreenMenu.Render;
var
  I: Integer;
  ItemColor, BgColor, CurrentItemBorderColor: TCastleColor;
  SR: TRectangle;
  Focusable: boolean;
begin
  inherited;

  SR := ScreenRect;

  if DrawBackgroundRectangle then
  begin
    if Focused then
      BgColor := Vector4Single(0, 0, 0, BackgroundOpacityFocused) else
      BgColor := Vector4Single(0, 0, 0, BackgroundOpacityNotFocused);
    DrawRectangle(SR, BgColor);
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
    Theme.Draw(SR, tiActiveFrame, UIScale, CurrentItemBorderColor);

  for I := 0 to ControlsCount - 1 do
  begin
    Focusable := ControlFocusable(Controls[I]);
    if (I = CurrentItem) and Focusable then
    begin
      Theme.Draw(Controls[I].ScreenRect, tiActiveFrame, UIScale, CurrentItemBorderColor);
      ItemColor := CurrentItemColor;
    end else
    if Focusable then
      ItemColor := NonCurrentItemColor
    else
      ItemColor := NonFocusableItemColor;
    if Controls[I] is TCastleLabel then
      TCastleLabel(Controls[I]).Color := ItemColor;
  end;
end;

function TCastleOnScreenMenu.FindChildIndex(
  const ScreenPosition: TVector2Single): Integer;
var
  I: Integer;
begin
  for I := 0 to ControlsCount - 1 do
    if Controls[I].ScreenRect.Contains(ScreenPosition) then
      Exit(I);
  Result := -1;
end;

function TCastleOnScreenMenu.Press(const Event: TInputPressRelease): boolean;

  function KeyDown(const Key: TKey; const C: char): boolean;
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
    if (Key = KeySelectItem) and (CurrentItem <> -1) then
    begin
      {$warnings off}
      Click;
      {$warnings on}
      Result := ExclusiveEvents;
    end;
  end;

  function MouseDown(const Button: TMouseButton): boolean;
  var
    NewItemIndex: Integer;
  begin
    Result := false;
    if Event.MouseButton = mbLeft then
    begin
      NewItemIndex := FindChildIndex(Container.MousePosition);
      if NewItemIndex <> -1 then
      begin
        if CurrentItem <> NewItemIndex then
        begin
          CurrentItem := NewItemIndex;
          CurrentItemChangedByUser;
        end;
        {$warnings off}
        Click;
        {$warnings on}
        Result := ExclusiveEvents;
      end;
    end;
  end;

begin
  Result := inherited;
  if Result then Exit;

  case Event.EventType of
    itKey        : Result := KeyDown(Event.Key, Event.KeyCharacter);
    itMouseButton: Result := MouseDown(Event.MouseButton);
  end;
end;

function TCastleOnScreenMenu.Motion(const Event: TInputMotion): boolean;
var
  NewItemIndex: Integer;
begin
  Result := inherited;
  if Result then Exit;

  NewItemIndex := FindChildIndex(Event.Position);
  if NewItemIndex <> -1 then
  begin
    if NewItemIndex <> CurrentItem then
    begin
      CurrentItem := NewItemIndex;
      CurrentItemChangedByUser;
      Result := ExclusiveEvents;
    end;
  end;
end;

function TCastleOnScreenMenu.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (Event.EventType <> itMouseButton) then Exit;
end;

procedure TCastleOnScreenMenu.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;

  { some keys pressed are already handled by slider }
  if HandleInput and ExclusiveEvents and
     (Container.Pressed[KeyPreviousItem] or
      Container.Pressed[KeyNextItem] or
      Container.Pressed[KeySelectItem]) then
    HandleInput := false;

  MenuAnimation += 0.5 * SecondsPassed;
  MenuAnimation := Frac(MenuAnimation);
  VisibleChange;
end;

function TCastleOnScreenMenu.AllowSuspendForInput: boolean;
begin
  Result := false;
end;

procedure TCastleOnScreenMenu.Click;
begin
  {$warnings off}
  if Assigned(OnClick) then OnClick(Self);
  {$warnings on}
  SoundEngine.Sound(stMenuClick);

  { TODO: dirty special handling for button attached to menu item.
    Also, this means that button can be activated by simple "mouse down",
    while usually it should be activated by "mouse down + mouse up". }
  if (Controls[CurrentItem].ControlsCount <> 0) and
     (Controls[CurrentItem].Controls[0] is TCastleButton) then
    TCastleButton(Controls[CurrentItem].Controls[0]).DoClick;
end;

procedure TCastleOnScreenMenu.CurrentItemChanged;
begin
  VisibleChange;
end;

procedure TCastleOnScreenMenu.CurrentItemChangedByUser;
begin
  SoundEngine.Sound(stMenuCurrentItemChanged);
end;

procedure TCastleOnScreenMenu.CurrentItemSelected;
begin
  {$warnings off}
  Click;
  {$warnings on}
end;

function TCastleOnScreenMenu.Rect: TRectangle;
begin
  Result := Rectangle(LeftBottomScaled, FWidth, FHeight);
end;

function TCastleOnScreenMenu.CapturesEventsAtPosition(const Position: TVector2Single): boolean;
begin
  Result := CaptureAllEvents or (inherited CapturesEventsAtPosition(Position));
end;

procedure TCastleOnScreenMenu.SetRegularSpaceBetweenItems(const Value: Cardinal);
begin
  if FRegularSpaceBetweenItems <> Value then
  begin
    FRegularSpaceBetweenItems := Value;
    RecalculateSize;
  end;
end;

function TCastleOnScreenMenu.ControlFocusable(const C: TUIControl): boolean;
begin
  Result := (C.ControlsCount <> 0) or not (C is TCastleLabel);
end;

procedure TCastleOnScreenMenu.Add(const NewItem: TUIControl);
begin
  if NewItem is TUIControlFont then
  begin
    TUIControlFont(NewItem).CustomFont := CustomFont;
    // TODO: dirty:
    // do not inherit SmallFont to avoid resetting slider SmallFont=true
    //TUIControlFont(NewItem).SmallFont := SmallFont;
    TUIControlFont(NewItem).FontSize := FontSize;
  end;
  InsertFront(NewItem);

  if NewItem.ControlsCount <> 0 then
  begin
    { Pass our CustomFont / FontSize to NewItem.Controls[0].
      TODO: this is a poor way, it's not updated later when we change
      CustomFont/FontSize, it's not recursive.... }
    if NewItem.Controls[0] is TUIControlFont then
    begin
      TUIControlFont(NewItem.Controls[0]).CustomFont := CustomFont;
      // TODO: dirty:
      // do not inherit SmallFont to avoid resetting slider SmallFont=true
      //TUIControlFont(NewItem.Controls[0]).SmallFont := SmallFont;
      TUIControlFont(NewItem.Controls[0]).FontSize := FontSize;
    end;
  end;

  RecalculateSize;
end;

procedure TCastleOnScreenMenu.Add(const S: string; const Accessory: TUIControl);
var
  L: TCastleLabel;
begin
  L := TCastleLabel.Create(Self);
  L.Caption := S;
  if Accessory <> nil then
  begin
    L.InsertFront(Accessory);
    L.Color := NonCurrentItemColor;
  end else
    L.Color := NonFocusableItemColor;

  Add(L);
end;

procedure TCastleOnScreenMenu.Add(const S: string);
begin
  Add(S, TUIControl(nil));
end;

procedure TCastleOnScreenMenu.Add(const S: string; const ItemOnClick: TNotifyEvent);
var
  Button: TCastleMenuButton;
begin
  { This method depends on the fact that leaving Caption='' on TCastleMenuButton
    makes it's size (width, height) exactly 0. Otherwise our menu items
    would needlessly grow to accomodate invisible button.
    Try setting Button.Caption := ' ' to see this problem,
    e.g. on "The Castle" start menu). }

  Button := TCastleMenuButton.Create(Self);
  Button.OnClick := ItemOnClick;
  Add(S, Button);
end;

procedure TCastleOnScreenMenu.UIScaleChanged;
begin
  inherited;
  RecalculateSize;
end;

end.
