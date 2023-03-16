{
  Copyright 2006-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ On-screen menu displayed in OpenGL (TCastleOnScreenMenu). }
unit CastleOnScreenMenu
  deprecated 'use TCastleVerticalGroup with buttons, sliders inside to desing on-screen menus';

{$I castleconf.inc}

interface

uses Classes, CastleVectors, CastleFonts, CastleControls,
  CastleGLUtils, CastleUIControls, CastleKeysMouse, CastleColors,
  CastleRectangles, CastleClassUtils, CastleSoundEngine;

type
  TCastleOnScreenMenu = class;

  TCastleMenuButton = class(TCastleButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CustomBackground default true;
    property CustomTextColorUse default true;
    property PaddingHorizontal {$ifdef FPC}default 0{$endif};
    property PaddingVertical {$ifdef FPC}default 0{$endif};
  end deprecated 'use TCastleOnScreenMenuItem';

  { Button that looks nice as an "accessory" that can be toggled
    (shows "yes" / "no" depending on @link(TCastleButton.Pressed)),
    attached to the TCastleOnScreenMenu item. }
  TCastleMenuToggle = class(TCastleMenuButton)
  private
    { Old TO DO (no longer valid, as this is deprecated):
      it would be nice if Width of this component was appropriate
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
  end deprecated 'use TCastleOnScreenMenuItemToggle';

  { Clickable menu item of @link(TCastleOnScreenMenu). }
  TCastleOnScreenMenuItem = class(TCastleUserInterface)
  strict private
    const
      Padding = 5;
    var
      FCaptionLabel, FRightCaptionLabel: TCastleLabel;
      FCaption: String;
      FOnClick: TNotifyEvent;
      FRightCaption: String;
      MenuAnimation: Single;
      ClickStarted: boolean;
      ClickStartedPosition: TVector2;
      FCaptionTranslate: Boolean;
      FEnabled: Boolean;
    procedure SetCaption(const Value: String);
    procedure SetRightCaption(const Value: String);
    { Update Menu.CurrentItem to point to Self. }
    procedure MakeCurrent;
    { Containing TCastleOnScreenMenu. }
    function Menu: TCastleOnScreenMenu;
    procedure SetEnabled(const Value: Boolean);
  private
    function LeftColumnWidth: Single;
    procedure PositionChildren(const MaxLeftColumnWidth: Single);
  protected
    procedure DoClick; virtual;
    procedure BeforeSizing; override;
    procedure TranslateProperties(const TranslatePropertyEvent: TTranslatePropertyEvent); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
  published
    property AutoSizeToChildren default true;

    { Text displayed by this on-screen menu item. }
    property Caption: String read FCaption write SetCaption;

    { Should the @link(Caption) be localized (translated into other languages).
      Determines if the property is enumerated by @link(TCastleComponent.TranslateProperties),
      which affects the rest of localization routines. }
    property CaptionTranslate: Boolean read FCaptionTranslate write FCaptionTranslate default true;

    { Event fired when user chooses this menu item in any way. }
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    { Whether menu item is enabled. }
    property Enabled: Boolean read FEnabled write SetEnabled default true;

    { Additional text displayed on the right side. }
    property RightCaption: String read FRightCaption write SetRightCaption;
  end;

  { Menu item of TCastleOnScreenMenuItem that can be toggled.
    Shows "yes" or "no" depending on a Boolean @link(Checked). }
  TCastleOnScreenMenuItemToggle = class(TCastleOnScreenMenuItem)
  strict private
    FChecked, FAutoToggle: Boolean;
    procedure SetChecked(const Value: Boolean);
  protected
    procedure DoClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    { The Boolean value of this menu item. }
    property Checked: Boolean read FChecked write SetChecked default false;

    { Automatically negate @link(Checked) on each click.
      This makes it easy to handle this component, often it means that you
      don't need to handle it's @link(OnClick) event. }
    property AutoToggle: boolean read FAutoToggle write FAutoToggle default false;
  end;

  { On-screen menu, with all menu items displayed on the screen,
    one under another. Often used for game "main menu" screen.
    Normal tools may prefer to use the normal menu bar
    (@link(TCastleWindow.MainMenu), or normal Lazarus main menu)
    instead of this.

    Each clickable menu item should be a TCastleOnScreenMenuItem descendant,
    and should be added to the MenuItems list.
    The TCastleOnScreenMenuItem.Menu should be set to point back to the menu instance,
    this way it can look at menu's properties like desired color and font.
    You can also add other controls to MenuItems -- it's a simple
    TCastleVerticalGroup, all the controls are just displayed one under
    another. E.g. you can add TCastleLabel to have non-clickable text,
    or you can add TCastleUserInterface with explicit TCastleUserInterface.Height
    to add some vertical space. }
  TCastleOnScreenMenu = class(TCastleUserInterfaceFont)
  private
    type
      TMenuItems = class(TCastleVerticalGroup)
      public
        Menu: TCastleOnScreenMenu;
      end;
    var
      FCaptureAllEvents: boolean;
      FOnClick: TNotifyEvent;
      FCurrentItem: Integer;
      FKeyNextItem: TKey;
      FKeyPreviousItem: TKey;
      FKeySelectItem: TKey;
      MenuAnimation: Single;
      FCurrentItemBorderColor1: TCastleColor;
      FCurrentItemBorderColor2: TCastleColor;
      FCurrentItemColor: TCastleColor;
      FNonCurrentItemColor: TCastleColor;
      FNonFocusableItemColor: TCastleColor;
      FRegularSpaceBetweenItems: Single;
      FDrawBackgroundRectangle: boolean;
      FDrawFocusedBorder: boolean;
      FBackgroundOpacityFocused, FBackgroundOpacityNotFocused: Single;
      FMenuItems: TCastleVerticalGroup;
      FSoundClick: TCastleSound;
      FSoundClickObserver: TFreeNotificationObserver;
      FSoundCurrentItemChanged: TCastleSound;
      FSoundCurrentItemChangedObserver: TFreeNotificationObserver;

    function GetCurrentItem: Integer;
    procedure SetCurrentItem(const Value: Integer);
    procedure SetRegularSpaceBetweenItems(const Value: Single);
    procedure SetSoundClick(const Value: TCastleSound);
    procedure SoundClickFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetSoundCurrentItemChanged(const Value: TCastleSound);
    procedure SoundCurrentItemChangedFreeNotification(const Sender: TFreeNotificationObserver);
  protected
    procedure BeforeSizing; override;
  public
    const
      DefaultMenuKeyNextItem = keyArrowDown;
      DefaultMenuKeyPreviousItem = keyArrowUp;
      DefaultMenuKeySelectItem = keyEnter;

      DefaultCurrentItemBorderColor1: TCastleColor = (X: 1.0; Y:  1.0; Z:  1.0; W: 1.0) { White  }; { }
      DefaultCurrentItemBorderColor2: TCastleColor = (X: 0.5; Y:  0.5; Z:  0.5; W: 1.0) { Gray   }; { }
      DefaultCurrentItemColor       : TCastleColor = (X: 1.0; Y:  1.0; Z:  0.0; W: 1.0) { Yellow }; { }
      DefaultNonCurrentItemColor    : TCastleColor = (X: 1.0; Y:  1.0; Z:  1.0; W: 1.0) { White  }; { }
      DefaultNonFocusableItemColor  : TCastleColor = (X:0.75; Y: 0.75; Z: 0.75; W: 1.0) { Light Gray }; { }

      DefaultRegularSpaceBetweenItems = 10;
      DefaultBackgroundOpacityNotFocused = 0.4;
      DefaultBackgroundOpacityFocused = 0.7;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(const S: string); overload;
    procedure Add(const S: string; const Accessory: TCastleUserInterface); overload;
    procedure Add(const S: string; const ItemOnClick: TNotifyEvent); overload;
    procedure Add(const NewItem: TCastleUserInterface); overload;

    { Currently selected child index.
      This is always some number between @code(0) and @code(MenuItems.ControlsCount - 1).
      It can also be @code(-1) to indicate "no child is selected".
      When MenuItems is empty (@code(MenuItems.ControlsCount = 0)) it is always @code(-1).

      You can change it by code.
      Assigning here an invalid value (e.g. larger then @code(MenuItems.ControlsCount - 1))
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

    function CapturesEventsAtPosition(const Position: TVector2): boolean; override;
    procedure Render; override;

    property KeyNextItem: TKey read FKeyNextItem write FKeyNextItem
      default DefaultMenuKeyNextItem;
    property KeyPreviousItem: TKey read FKeyPreviousItem write FKeyPreviousItem
      default DefaultMenuKeyPreviousItem;
    property KeySelectItem: TKey read FKeySelectItem write FKeySelectItem
      default DefaultMenuKeySelectItem;

    function Press(const Event: TInputPressRelease): boolean; override;
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
    { Label color for the non-focusable child (or TCastleOnScreenMenuItem with TCastleOnScreenMenuItem.Enabled = @false).
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
    function SpaceBetweenItems(const NextItemIndex: Cardinal): Single; virtual;
      deprecated 'ignored now; if you want to add extra space, add TCastleUserInterface with explicit Height to MenuItems';

    {$ifdef FPC}
    { Called when user will select CurrentItem.
      @seealso Click }
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
      deprecated 'use Add method to add a particular menu item with it''s own click callback; or just add TCastleMenuButton and handle it''s OnClick event';
    {$endif}
  published
    property AutoSizeToChildren default true;

    { Menu items, and things between menu items, should be added here. }
    property MenuItems: TCastleVerticalGroup read FMenuItems;

    { Opacity of the background rectangle (displayed when DrawBackgroundRectangle).
      @groupBegin }
    property         BackgroundOpacityFocused: Single
      read          FBackgroundOpacityFocused
      write         FBackgroundOpacityFocused
      {$ifdef FPC}default DefaultBackgroundOpacityFocused{$endif};
    property         BackgroundOpacityNotFocused: Single
      read          FBackgroundOpacityNotFocused
      write         FBackgroundOpacityNotFocused
      {$ifdef FPC}default DefaultBackgroundOpacityNotFocused{$endif};
    { @groupEnd }

    property DrawBackgroundRectangle: boolean
      read FDrawBackgroundRectangle write FDrawBackgroundRectangle
      default true;

    { Additional vertical space, in pixels, between menu items.

      If you want more control over it (if you want to add more/less
      space between some menu items), override SpaceBetweenItems method. }
    property RegularSpaceBetweenItems: Single
      read FRegularSpaceBetweenItems write SetRegularSpaceBetweenItems
      {$ifdef FPC}default DefaultRegularSpaceBetweenItems{$endif};

    { Draw a flashing border around the menu when we are focused. }
    property DrawFocusedBorder: boolean read FDrawFocusedBorder write FDrawFocusedBorder
      default true;

    { Should menu intercept all key/mouse input, regardless if mouse position
      is over our rectangle.
      This affects key/mouse processing (menu processes input
      before all controls underneath), but not drawing (controls underneath
      are still visible as usual). }
    property CaptureAllEvents: boolean
      read FCaptureAllEvents write FCaptureAllEvents default false;

    property SoundClick: TCastleSound
      read FSoundClick write SetSoundClick;
    property SoundCurrentItemChanged: TCastleSound
      read FSoundCurrentItemChanged write SetSoundCurrentItemChanged;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleonscreenmenu_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

implementation

uses SysUtils, CastleUtils, CastleImages, CastleFilesUtils,
  CastleStringUtils, CastleGLImages, CastleComponentSerialize;

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
  Caption := Iff(Pressed, 'Yes', 'No');
end;

procedure TCastleMenuToggle.SetPressed(const Value: boolean);
begin
  inherited;
  Caption := Iff(Pressed, 'Yes', 'No');
end;

procedure TCastleMenuToggle.DoClick;
begin
  // change Boolean value before calling OnClick in inherited
  if AutoToggle then
    Pressed := not Pressed;
  inherited;
end;

{ TCastleOnScreenMenuItem ---------------------------------------------------- }

constructor TCastleOnScreenMenuItem.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := true;
  FCaptionTranslate := true;

  FCaptionLabel := TCastleLabel.Create(Self);
  FCaptionLabel.SetTransient;
  FCaptionLabel.Anchor(vpMiddle);
  FCaptionLabel.Anchor(hpLeft, Padding);
  //FCaptionLabel.PaddingVertical := Padding;
  InsertFront(FCaptionLabel);

  FRightCaptionLabel := TCastleLabel.Create(Self);
  FRightCaptionLabel.SetTransient;
  // the anchors for FRightCaptionLabel will be set in TCastleOnScreenMenuItem.PositionChildren
  FRightCaptionLabel.Exists := false;
  FRightCaptionLabel.Color := LightGreen;
  InsertFront(FRightCaptionLabel);

  AutoSizeToChildren := true;
  AutoSizeToChildrenPaddingRight := Padding;
end;

function TCastleOnScreenMenuItem.LeftColumnWidth: Single;
begin
  Result := FCaptionLabel.EffectiveWidth + Padding { padding on the left };
end;

procedure TCastleOnScreenMenuItem.PositionChildren(const MaxLeftColumnWidth: Single);
var
  I: Integer;
begin
  for I := 0 to ControlsCount - 1 do
    if Controls[I] <> FCaptionLabel then
    begin
      Controls[I].Anchor(hpLeft, MaxLeftColumnWidth);
      Controls[I].Anchor(vpMiddle);
    end;
end;

procedure TCastleOnScreenMenuItem.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    FCaptionLabel.Caption := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleOnScreenMenuItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleOnScreenMenuItem.SetRightCaption(const Value: String);
begin
  if FRightCaption <> Value then
  begin
    FRightCaption := Value;
    FRightCaptionLabel.Caption := Value;
    FRightCaptionLabel.Exists := Value <> '';
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleOnScreenMenuItem.BeforeSizing;
var
  M: TCastleOnScreenMenu;
begin
  inherited;

  M := Menu;
  if M <> nil then
  begin
    FCaptionLabel.AssignFont(M);
    FRightCaptionLabel.AssignFont(M);
  end;
end;

procedure TCastleOnScreenMenuItem.Render;
var
  US: Single;

  function Current: Boolean;
  begin
    Result :=
      (Menu <> nil) and
      Between(Menu.CurrentItem, 0, Menu.MenuItems.ControlsCount - 1) and
      (Menu.MenuItems.Controls[Menu.CurrentItem] = Self);
  end;

var
  RR: TFloatRectangle;
  ItemColor, CurrentItemBorderColor: TCastleColor;
begin
  inherited;

  US := UIScale;
  RR := RenderRect;

  if Menu <> nil then
  begin
    if not Enabled then
      ItemColor := Menu.NonFocusableItemColor
    else
    if Current then
    begin
      { Calculate CurrentItemBorderColor }
      if MenuAnimation <= 0.5 then
        CurrentItemBorderColor := Lerp(
          MapRange(MenuAnimation, 0, 0.5, 0, 1),
          Menu.CurrentItemBorderColor1, Menu.CurrentItemBorderColor2)
      else
        CurrentItemBorderColor := Lerp(
          MapRange(MenuAnimation, 0.5, 1, 0, 1),
          Menu.CurrentItemBorderColor2, Menu.CurrentItemBorderColor1);
      Theme.Draw(RR, tiActiveFrame, US, CurrentItemBorderColor);

      ItemColor := Menu.CurrentItemColor;
    end else
      ItemColor := Menu.NonCurrentItemColor;
  end else
  begin
    if Enabled then
      ItemColor := TCastleOnScreenMenu.DefaultCurrentItemColor
    else
      ItemColor := TCastleOnScreenMenu.DefaultNonFocusableItemColor;
  end;

  FCaptionLabel.Color := ItemColor;
end;

function TCastleOnScreenMenuItem.Menu: TCastleOnScreenMenu;
begin
  if Parent is TCastleOnScreenMenu.TMenuItems then
    Result := TCastleOnScreenMenu.TMenuItems(Parent).Menu
  else
    Result := nil;
end;

procedure TCastleOnScreenMenuItem.MakeCurrent;
var
  Index: Integer;
begin
  if Menu <> nil then
  begin
    Index := Menu.MenuItems.IndexOfControl(Self);
    if (Index <> -1) and
       (Menu.CurrentItem <> Index) then
    begin
      Menu.CurrentItemChangedByUser;
      Menu.CurrentItem := Index;
    end;
  end;
end;

function TCastleOnScreenMenuItem.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (Event.EventType <> itMouseButton) then Exit;

  Result := true;
  MakeCurrent;
  ClickStarted := true;
  ClickStartedPosition := Event.Position;
end;

function TCastleOnScreenMenuItem.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (Event.EventType <> itMouseButton) then Exit;

  if ClickStarted then
  begin
    Result := true;
    ClickStarted := false;
    if CapturesEventsAtPosition(Event.Position) then
      DoClick;
  end;
end;

function TCastleOnScreenMenuItem.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.Pressed = [] then
  begin
    MakeCurrent;
    Result := true;
  end;
end;

procedure TCastleOnScreenMenuItem.DoClick;
begin
  if not Enabled then
    Exit;
  {$warnings off}
  if Menu <> nil then
    Menu.Click; // keep deprecated Menu.Click working
  {$warnings on}
  if Menu.SoundClick <> nil then
    SoundEngine.Play(Menu.SoundClick)
  else
    {$warnings off} // keep deprecated working
    SoundEngine.Play(stMenuClick);
    {$warnings on}
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TCastleOnScreenMenuItem.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  MenuAnimation := MenuAnimation + (0.5 * SecondsPassed);
  MenuAnimation := Frac(MenuAnimation);
  VisibleChange([chRender]);
end;

procedure TCastleOnScreenMenuItem.TranslateProperties(
  const TranslatePropertyEvent: TTranslatePropertyEvent);
var
  S: String;
begin
  if CaptionTranslate and (Caption <> '') then
  begin
    S := Caption;
    TranslatePropertyEvent(Self, 'Caption', S);
    Caption := S;
  end;
end;

{ TCastleOnScreenMenuItemToggle ---------------------------------------------------------- }

constructor TCastleOnScreenMenuItemToggle.Create(AOwner: TComponent);
begin
  inherited;
  RightCaption := Iff(Checked, 'Yes', 'No');
end;

procedure TCastleOnScreenMenuItemToggle.SetChecked(const Value: boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    RightCaption := Iff(Checked, 'Yes', 'No');
  end;
end;

procedure TCastleOnScreenMenuItemToggle.DoClick;
begin
  // change Boolean value before calling OnClick in inherited
  if AutoToggle then
    Checked := not Checked;
  inherited;
end;

{ TCastleOnScreenMenu -------------------------------------------------------------------- }

constructor TCastleOnScreenMenu.Create(AOwner: TComponent);
begin
  inherited;

  FMenuItems := TMenuItems.Create(Self);
  TMenuItems(FMenuItems).Menu := Self;
  FMenuItems.SetSubComponent(true);
  FMenuItems.Name := 'MenuItems';
  FMenuItems.Padding := 30;
  InsertFront(FMenuItems);

  FCurrentItem := 0;
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
  FMenuItems.Spacing := RegularSpaceBetweenItems;

  FDrawBackgroundRectangle := true;
  FDrawFocusedBorder := true;
  AutoSizeToChildren := true;

  FSoundClickObserver := TFreeNotificationObserver.Create(Self);
  FSoundClickObserver.OnFreeNotification := {$ifdef FPC}@{$endif}SoundClickFreeNotification;
  FSoundCurrentItemChangedObserver := TFreeNotificationObserver.Create(Self);
  FSoundCurrentItemChangedObserver.OnFreeNotification := {$ifdef FPC}@{$endif}SoundCurrentItemChangedFreeNotification;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleonscreenmenu_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleOnScreenMenu.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleonscreenmenu_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleOnScreenMenu.GetCurrentItem: Integer;
begin
  Result := FCurrentItem;

  { Make sure that CurrentItem conditions are OK.
    User can change the Controls list at any moment, so you cannot depend that
    FCurrentItem is Ok here. }

  if MenuItems.ControlsCount <> 0 then
  begin
    ClampVar(Result, 0, MenuItems.ControlsCount - 1);
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
  if MenuItems.ControlsCount <> 0 then
  begin
    OldCurrentItem := CurrentItem;
    repeat
      if CurrentItem = MenuItems.ControlsCount - 1 then
        CurrentItem := 0
      else
        CurrentItem := CurrentItem + 1;
      { Loop until you find the next focusable control.
        Also, stop when you reach the original control (to avoid an infinite loop). }
    until
      (MenuItems.Controls[CurrentItem] is TCastleOnScreenMenuItem) or
      (CurrentItem = OldCurrentItem);

    NewCurrentItem := CurrentItem;
    if OldCurrentItem <> NewCurrentItem then
      CurrentItemChangedByUser;
  end;
end;

procedure TCastleOnScreenMenu.PreviousItem;
var
  OldCurrentItem, NewCurrentItem: Integer;
begin
  if MenuItems.ControlsCount <> 0 then
  begin
    OldCurrentItem := CurrentItem;
    repeat
      if CurrentItem = 0 then
        CurrentItem := MenuItems.ControlsCount - 1
      else
        CurrentItem := CurrentItem - 1;
      { Loop until you find the next focusable control.
        Also, stop when you reach the original control (to avoid an infinite loop). }
    until
      (MenuItems.Controls[CurrentItem] is TCastleOnScreenMenuItem) or
      (CurrentItem = OldCurrentItem);

    NewCurrentItem := CurrentItem;
    if OldCurrentItem <> NewCurrentItem then
      CurrentItemChangedByUser;
  end;
end;

function TCastleOnScreenMenu.SpaceBetweenItems(const NextItemIndex: Cardinal): Single;
begin
  Result := RegularSpaceBetweenItems;
end;

procedure TCastleOnScreenMenu.BeforeSizing;
const
  MarginBeforeAccessory = 20;
var
  MaxLeftColumnWidth: Single;
  I: Integer;
begin
  inherited;

  { recalculate MaxLeftColumnWidth and update children position based on it }
  MaxLeftColumnWidth := 0;
  for I := 0 to MenuItems.ControlsCount - 1 do
    if MenuItems.Controls[I] is TCastleOnScreenMenuItem then
      MaxVar(MaxLeftColumnWidth, TCastleOnScreenMenuItem(MenuItems.Controls[I]).LeftColumnWidth);
  if MaxLeftColumnWidth <> 0 then
    MaxLeftColumnWidth := MaxLeftColumnWidth + MarginBeforeAccessory;
  for I := 0 to MenuItems.ControlsCount - 1 do
    if MenuItems.Controls[I] is TCastleOnScreenMenuItem then
      TCastleOnScreenMenuItem(MenuItems.Controls[I]).PositionChildren(MaxLeftColumnWidth);
end;

procedure TCastleOnScreenMenu.Render;
var
  BgColor, CurrentItemBorderColor: TCastleColor;
  SR: TFloatRectangle;
begin
  inherited;

  SR := RenderRect;

  if DrawBackgroundRectangle then
  begin
    if Focused then
      BgColor := Vector4(0, 0, 0, BackgroundOpacityFocused) else
      BgColor := Vector4(0, 0, 0, BackgroundOpacityNotFocused);
    DrawRectangle(SR, BgColor);
  end;

  { Calculate CurrentItemBorderColor }
  if MenuAnimation <= 0.5 then
    CurrentItemBorderColor := Lerp(
      MapRange(MenuAnimation, 0, 0.5, 0, 1),
      CurrentItemBorderColor1, CurrentItemBorderColor2)
  else
    CurrentItemBorderColor := Lerp(
      MapRange(MenuAnimation, 0.5, 1, 0, 1),
      CurrentItemBorderColor2, CurrentItemBorderColor1);

  if Focused and DrawFocusedBorder then
    Theme.Draw(SR, tiActiveFrame, UIScale, CurrentItemBorderColor);
end;

function TCastleOnScreenMenu.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(KeyPreviousItem) then
  begin
    PreviousItem;
    Result := true;
  end else
  if Event.IsKey(KeyNextItem) then
  begin
    NextItem;
    Result := true;
  end else
  if Event.IsKey(KeySelectItem) and
     Between(CurrentItem, 0, MenuItems.ControlsCount - 1) and
     (MenuItems.Controls[CurrentItem] is TCastleOnScreenMenuItem) then
  begin
    TCastleOnScreenMenuItem(MenuItems.Controls[CurrentItem]).DoClick;
    Result := true;
  end;
end;

procedure TCastleOnScreenMenu.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;

  { some keys pressed are already handled by Press }
  if HandleInput and
     (Container.Pressed[KeyPreviousItem] or
      Container.Pressed[KeyNextItem] or
      Container.Pressed[KeySelectItem]) then
    HandleInput := false;

  MenuAnimation := MenuAnimation + (0.5 * SecondsPassed);
  MenuAnimation := Frac(MenuAnimation);
  VisibleChange([chRender]);
end;

function TCastleOnScreenMenu.AllowSuspendForInput: boolean;
begin
  Result := false;
end;

procedure TCastleOnScreenMenu.Click;
begin
  // TODO: check this in Delphi
  {$ifdef FPC}
  {$warnings off}
  if Assigned(OnClick) then OnClick(Self);
  {$warnings on}
  {$endif}
end;

procedure TCastleOnScreenMenu.CurrentItemChanged;
begin
  VisibleChange([chRender]);
end;

procedure TCastleOnScreenMenu.CurrentItemChangedByUser;
begin
  if SoundCurrentItemChanged <> nil then
    SoundEngine.Play(SoundCurrentItemChanged)
  else
    {$warnings off} // keep deprecated working
    SoundEngine.Play(stMenuCurrentItemChanged);
    {$warnings on}
end;

procedure TCastleOnScreenMenu.CurrentItemSelected;
begin
  {$warnings off}
  Click;
  {$warnings on}
end;

function TCastleOnScreenMenu.CapturesEventsAtPosition(const Position: TVector2): boolean;
begin
  Result := CaptureAllEvents or (inherited CapturesEventsAtPosition(Position));
end;

procedure TCastleOnScreenMenu.SetRegularSpaceBetweenItems(const Value: Single);
begin
  if FRegularSpaceBetweenItems <> Value then
  begin
    FRegularSpaceBetweenItems := Value;
    FMenuItems.Spacing := RegularSpaceBetweenItems;
  end;
end;

procedure TCastleOnScreenMenu.Add(const NewItem: TCastleUserInterface);
begin
  MenuItems.InsertFront(NewItem);
end;

procedure TCastleOnScreenMenu.Add(const S: string; const Accessory: TCastleUserInterface);
var
  Item: TCastleOnScreenMenuItem;
begin
  if Accessory <> nil then
  begin
    Item := TCastleOnScreenMenuItem.Create(Self);
    Item.Caption := S;
    Item.InsertFront(Accessory);
    Add(Item);
  end else
  begin
    Add(S); // add only TCastleLabel
  end;
end;

procedure TCastleOnScreenMenu.Add(const S: string);
var
  ItemLabel: TCastleLabel;
begin
  ItemLabel := TCastleLabel.Create(Self);
  ItemLabel.Caption := S;
  ItemLabel.Color := NonFocusableItemColor;
  ItemLabel.Padding := 5; // like TCastleOnScreenMenuItem.Padding
  Add(ItemLabel);
end;

procedure TCastleOnScreenMenu.Add(const S: string; const ItemOnClick: TNotifyEvent);
var
  Item: TCastleOnScreenMenuItem;
begin
  Item := TCastleOnScreenMenuItem.Create(Self);
  Item.Caption := S;
  Item.OnClick := ItemOnClick;
  Add(Item);
end;

procedure TCastleOnScreenMenu.SetSoundClick(const Value: TCastleSound);
begin
  if FSoundClick <> Value then
  begin
    FSoundClick := Value;
    FSoundClickObserver.Observed := Value;
  end;
end;

procedure TCastleOnScreenMenu.SoundClickFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FSoundClick := nil;
end;

procedure TCastleOnScreenMenu.SetSoundCurrentItemChanged(const Value: TCastleSound);
begin
  if FSoundCurrentItemChanged <> Value then
  begin
    FSoundCurrentItemChanged := Value;
    FSoundCurrentItemChangedObserver.Observed := Value;
  end;
end;

procedure TCastleOnScreenMenu.SoundCurrentItemChangedFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FSoundCurrentItemChanged := nil;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleonscreenmenu_persistent_vectors.inc}
{$undef read_implementation_methods}

initialization
  RegisterSerializableComponent(TCastleOnScreenMenu, 'On-screen Menu');
  RegisterSerializableComponent(TCastleOnScreenMenuItem, 'On-screen Menu Item');
end.
