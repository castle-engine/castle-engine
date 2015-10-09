{
  Copyright 2006-2014 Michalis Kamburelis.

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

  { Label that shows the boolean value, as "Yes" or "No".
    Particularly useful as a control inside TCastleOnScreenMenu,
    to show a boolean toggle. }
  TCastleBooleanLabel = class(TCastleLabel)
  private
    FValue: boolean;
    procedure SetValue(const AValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value: boolean read FValue write SetValue;
  end;

  { On-screen menu, with all menu items displayed on the screen,
    one under the other. Typical for game menus.
    Normal tools may prefer to use the menu bar instead of this
    (for example TCastleWindowCustom.Menu, or normal Lazarus menu).

    Each menu item can have an "accessory", for example an associated
    slider (from TCastleFloatSlider, any TUIControl is OK).
    This allows to use this menu also for settings. }
  TCastleOnScreenMenu = class(TUIControl)
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
    FAccessoryLabelColor: TCastleColor;
    MaxItemWidth: Integer;
    FRegularSpaceBetweenItems: Cardinal;
    FDrawBackgroundRectangle: boolean;
    FDrawFocusedBorder: boolean;
    FBackgroundOpacityFocused, FBackgroundOpacityNotFocused: Single;
    function GetCurrentItem: Integer;
    procedure SetCurrentItem(const Value: Integer);
    procedure SetRegularSpaceBetweenItems(const Value: Cardinal);
    function FindChildIndex(const ScreenPosition: TVector2Single): Integer;
  public
    const
      DefaultMenuKeyNextItem = K_Down;
      DefaultMenuKeyPreviousItem = K_Up;
      DefaultMenuKeySelectItem = K_Enter;

      DefaultCurrentItemBorderColor1: TCastleColor = (1.0, 1.0, 1.0, 1.0) { White  }; { }
      DefaultCurrentItemBorderColor2: TCastleColor = (0.5, 0.5, 0.5, 1.0) { Gray   }; { }
      DefaultCurrentItemColor       : TCastleColor = (1.0, 1.0, 0.0, 1.0) { Yellow }; { }
      DefaultNonCurrentItemColor    : TCastleColor = (1.0, 1.0, 1.0, 1.0) { White  }; { }
      DefaultAccessoryLabelColor    : TCastleColor = (0.33, 1.0 , 0.33, 1.0); { LightGreen } { }

      DefaultRegularSpaceBetweenItems = 10;
      DefaultBackgroundOpacityNotFocused = 0.4;
      DefaultBackgroundOpacityFocused = 0.7;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(const S: string);
    procedure Add(const S: string; const Accessory: TUIControl);

    { When ControlsCount <> 0, this is always some number
      between 0 and ControlsCount - 1.
      Otherwise (when ControlsCount <> 0) this is always -1.

      If you assign it to wrong value (breaking conditions above),
      or if you change Items such that conditions are broken,
      it will be arbitrarily fixed.

      Changing this calls CurrentItemChanged automatically when needed. }
    property CurrentItem: Integer read GetCurrentItem write SetCurrentItem;

    { These change CurrentItem as appropriate.
      Usually you will just let this class call it internally
      (from Motion, KeyDown etc.) and will not need to call it yourself.

      @groupBegin }
    procedure NextItem;
    procedure PreviousItem;
    { @groupEnd }

    procedure GLContextClose; override;

    { Calculate final positions, sizes of menu items on the screen.
      Usually this is called automatically when necessary. }
    procedure RecalculateSize;

    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); override;

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
    procedure Click; virtual;

    { @deprecated Deprecated name for Click. }
    procedure CurrentItemSelected; virtual; deprecated;

    { Called when CurrentItem changed.
      But *not* when CurrentItem changed because of ControlsCount changes.
      In this class this just calls VisibleChange and
      plays sound stMenuCurrentItemChanged. }
    procedure CurrentItemChanged; virtual;

    { Default value is DefaultCurrentItemBorderColor1 }
    property CurrentItemBorderColor1: TCastleColor
      read FCurrentItemBorderColor1 write FCurrentItemBorderColor1;
    { Default value is DefaultCurrentItemBorderColor2 }
    property CurrentItemBorderColor2: TCastleColor
      read FCurrentItemBorderColor2 write FCurrentItemBorderColor2;
    { Default value is DefaultCurrentItemColor }
    property CurrentItemColor: TCastleColor
      read FCurrentItemColor write FCurrentItemColor;
    { Default value is DefaultNonCurrentItemColor }
    property NonCurrentItemColor: TCastleColor
      read FNonCurrentItemColor write FNonCurrentItemColor;
    { Default value is DefaultAccessoryLabelColor }
    property AccessoryLabelColor: TCastleColor
      read FAccessoryLabelColor write FAccessoryLabelColor;

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
  CastleStringUtils, CastleGLImages, CastleSoundEngine, CastleGL;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleOnScreenMenu]);
end;

{ TCastleBooleanLabel ----------------------------------------------------- }

constructor TCastleBooleanLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text.Text := BoolToStrYesNo[Value];
end;

procedure TCastleBooleanLabel.SetValue(const AValue: boolean);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    Text.Text := BoolToStrYesNo[Value];
  end;
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
  FAccessoryLabelColor := DefaultAccessoryLabelColor;

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
begin
  if ControlsCount <> 0 then
  begin
    if CurrentItem = ControlsCount - 1 then
      CurrentItem := 0 else
      CurrentItem := CurrentItem + 1;
  end;
end;

procedure TCastleOnScreenMenu.PreviousItem;
begin
  if ControlsCount <> 0 then
  begin
    if CurrentItem = 0 then
      CurrentItem := ControlsCount - 1 else
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

procedure TCastleOnScreenMenu.RecalculateSize;
const
  Padding = 30;
var
  I: Integer;
  WholeItemWidth, MaxAccessoryWidth: Integer;
  ItemsBelowHeight: Cardinal;
begin
  { calculate MaxItemWidth, MaxAccessoryWidth }

  MaxItemWidth := 0;
  MaxAccessoryWidth := 0;
  for I := 0 to ControlsCount - 1 do
  begin
    MaxVar(MaxItemWidth, Controls[I].Rect.Width);
    { add accessory (slider etc.) width inside the menu item }
    if Controls[I].ControlsCount <> 0 then
      MaxVar(MaxAccessoryWidth, Controls[I].Controls[0].Rect.Width);
  end;

  { calculate FWidth and FHeight }

  FWidth := MaxItemWidth;
  if MaxAccessoryWidth <> 0 then
    FWidth += MarginBeforeAccessory + MaxAccessoryWidth;

  FHeight := 0;
  for I := 0 to ControlsCount - 1 do
  begin
    FHeight += Controls[I].Rect.Height;
    if I > 0 then
      FHeight += Integer(SpaceBetweenItems(I));
  end;

  FWidth += 2 * Padding;
  FHeight += 2 * Padding;

  { calculate children Widths and Heights }

  FRectangles.Count := 0;
  for I := 0 to ControlsCount - 1 do
  begin
    if MaxAccessoryWidth <> 0 then
      WholeItemWidth := MaxItemWidth + MarginBeforeAccessory + MaxAccessoryWidth else
      WholeItemWidth := Controls[I].Rect.Width;
    FRectangles.Add(Rectangle(0, 0, WholeItemWidth,
      Controls[I].Rect.Height));
  end;

  { Calculate positions of all rectangles. }

  { we iterate downwards from Rectangles.Count - 1 to 0, updating ItemsBelowHeight.
    That because our coordinates grow up,
    but our menu items are specified from highest to lowest. }
  ItemsBelowHeight := 0;

  for I := FRectangles.Count - 1 downto 0 do
  begin
    FRectangles.L[I].Left := Padding;
    FRectangles.L[I].Bottom := Padding + ItemsBelowHeight;
    Controls[I].Left := FRectangles.L[I].Left;
    Controls[I].Bottom := FRectangles.L[I].Bottom;
    if I > 0 then
      ItemsBelowHeight += Cardinal(Controls[I].Rect.Height + Integer(SpaceBetweenItems(I)));
  end;

  for I := 0 to FRectangles.Count - 1 do
    if Controls[I].ControlsCount <> 0 then
      Controls[I].Controls[0].Left := MaxItemWidth + MarginBeforeAccessory;
end;

procedure TCastleOnScreenMenu.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
  inherited;
  RecalculateSize;
end;

const
  ItemBorderMargin = 5;

procedure TCastleOnScreenMenu.Render;
var
  I: Integer;
  ItemColor, BgColor, CurrentItemBorderColor: TCastleColor;
  SR, R: TRectangle;
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
    Theme.Draw(SR, tiActiveFrame, CurrentItemBorderColor);

  for I := 0 to ControlsCount - 1 do
  begin
    if I = CurrentItem then
    begin
      R := FRectangles.L[I].Grow(ItemBorderMargin, 0);
      R.Left := R.Left + SR.Left;
      R.Bottom := R.Bottom + SR.Bottom;
      Theme.Draw(R, tiActiveFrame, CurrentItemBorderColor);
      ItemColor := CurrentItemColor;
    end else
      ItemColor := NonCurrentItemColor;
    if Controls[I] is TCastleLabel then
      TCastleLabel(Controls[I]).Color := ItemColor;
  end;
end;

function TCastleOnScreenMenu.FindChildIndex(
  const ScreenPosition: TVector2Single): Integer;
var
  I: Integer;
  SR, R: TRectangle;
begin
  SR := ScreenRect;
  for I := 0 to ControlsCount - 1 do
  begin
    R := FRectangles.L[I].Grow(ItemBorderMargin, 0);
    R.Left := R.Left + SR.Left;
    R.Bottom := R.Bottom + SR.Bottom;
    if R.Contains(ScreenPosition) then Exit(I);
  end;

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
    if Key = KeySelectItem then
    begin
      Click;
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

function TCastleOnScreenMenu.Motion(const Event: TInputMotion): boolean;
var
  NewItemIndex: Integer;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  NewItemIndex := FindChildIndex(Event.Position);
  if NewItemIndex <> -1 then
  begin
    if NewItemIndex <> CurrentItem then
    begin
      CurrentItem := NewItemIndex;
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
  if Assigned(OnClick) then OnClick(Self);
  SoundEngine.Sound(stMenuClick);
end;

procedure TCastleOnScreenMenu.CurrentItemChanged;
begin
  VisibleChange;
  SoundEngine.Sound(stMenuCurrentItemChanged);
end;

procedure TCastleOnScreenMenu.CurrentItemSelected;
begin
  Click; { call non-deprecated equivalent }
end;

function TCastleOnScreenMenu.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, FWidth, FHeight);
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

procedure TCastleOnScreenMenu.Add(const S: string);
begin
  Add(S, nil);
end;

procedure TCastleOnScreenMenu.Add(const S: string; const Accessory: TUIControl);
var
  L: TCastleLabel;
begin
  L := TCastleLabel.Create(Self);
  L.Text.Text := S;
  InsertFront(L);
  if Accessory <> nil then
  begin
    L.InsertFront(Accessory);
    if (Accessory is TCastleLabel) then
      TCastleLabel(Accessory).Color := AccessoryLabelColor;
  end;
  RecalculateSize;
end;

end.
