{%MainUnit castlecontrols.pas}
{
  Copyright 2017-2023 Michalis Kamburelis and Jan Adamec.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Touch-friendly checkbox control representing on/off state. }
  TCastleSwitchControl = class(TCastleUserInterface)
  strict private
    FChecked: boolean;
    FEnabled: boolean;
    FPressed: boolean;
    FOnChange: TNotifyEvent;

    procedure SetChecked(const Value: boolean);
    procedure SetEnabled(const Value: boolean);
  public
    const
      DefaultSwitchControlWidth = 50;
      DefaultSwitchControlHeight = 28;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    {$ifdef FPC}
    property IsOn: boolean read FChecked write SetChecked default false;
      deprecated 'use Checked instead, consistent with LCL TCheckBox.Checked';
    {$endif}
  published
    { Switch state. }
    property Checked: boolean read FChecked write SetChecked default false;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    { Event sent when @link(Checked) value was changed by a user click.
      Note that this is @italic(not) called when you change @link(Checked)
      property programmatically by setting the property. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Width {$ifdef FPC}default DefaultSwitchControlWidth{$endif};
    property Height {$ifdef FPC}default DefaultSwitchControlHeight{$endif};
  end deprecated 'use TCastleCheckbox or TCastleButton with TCastleButton.Toggle=true and custom backgrounds';

{$endif read_interface}

{$ifdef read_implementation}

{
  This may be brought back from deprecation, but we need to address some things:

  TODO:

  - Default UI design of this has to change to look more modern.
    The current look of it was done by reusing some images for other UI controls.

  - UI design has to be 100% adjustable using TCastleImagePersistent subcomponents.
    See e.g. how TCastleEdit, TCastleAbstractSlider do it.

  - Name maybe should change -- "Control" suffix is likely unnecessary,
    we generally try to avoid such suffix for UI controls.
    See what other UI toolkits call it.

  - To be really more useful than "TCastleButton with Toggle=true and custom backgrounds"
    the thumb should animate when toggling.
}

{ TCastleSwitchControl ---------------------------------------------------------- }

constructor TCastleSwitchControl.Create(AOwner: TComponent);
begin
  inherited;
  FChecked := false;
  FEnabled := true;
  FPressed := false;
  FOnChange := nil;
  Width := DefaultSwitchControlWidth;
  Height := DefaultSwitchControlHeight;
end;

destructor TCastleSwitchControl.Destroy;
begin
  inherited;
end;

procedure TCastleSwitchControl.Render;
var
  BaseRect, BackRect, HandleRect: TFloatRectangle;
  HandleImage: TThemeImage;
begin
  inherited;

  BaseRect := RenderRect;

  // background
  BackRect := BaseRect;
  BackRect.Height := Round(BaseRect.Height * 0.5);  //*0.4; //BaseRect.Height / 2;
  BackRect.Bottom := BaseRect.Bottom + ((BaseRect.Height - BackRect.Height) / 2);
  Theme.Draw(BackRect, tiSwitchControl, UIScale);
  if Checked then
    Theme.Draw(BackRect, tiSwitchControlFill, UIScale);

  // handle
  HandleRect := BaseRect;
  HandleRect.Width := BaseRect.Width / 3; //HandleRect.Height;
  if Checked then
    HandleRect.Left := BaseRect.Right - HandleRect.Width;

  if FPressed then
    HandleImage := tiButtonPressed
  else if not Enabled then
    HandleImage := tiButtonDisabled
  else if Focused then
    HandleImage := tiButtonFocused
  else
    HandleImage := tiButtonNormal;
  Theme.Draw(HandleRect, HandleImage, UIScale)
end;

procedure TCastleSwitchControl.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleSwitchControl.SetChecked(const Value: boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleSwitchControl.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) or
     Event.IsKey(keySpace) or
     Event.IsKey(keyEnter) then
  begin
    FPressed := true;
    Exit(true);
  end;
end;

function TCastleSwitchControl.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if FPressed and (
     Event.IsMouseButton(buttonLeft) or
     Event.IsKey(keySpace) or
     Event.IsKey(keyEnter)) then
  begin
    FPressed := false;

    { Because of mouse capture mechanism in TCastleContainer,
      we receive Release notifications about all clicks that started
      within our rect, regardless where they finished.

      Just like TCastleButton, the click only counts
      if it happens over our rect. }
    if CapturesEventsAtPosition(Event.Position) then
    begin
      FChecked := not FChecked; // FPressed, FChecked changed
      VisibleChange([chRender]);

      { Make sure to execute FOnChange at the end,
        to have consistent state when calling the callback that can
        do anything with this TCastleSwitchControl. }
      if Assigned(FOnChange) then
        FOnChange(Self);
    end else
    begin
      VisibleChange([chRender]); // FPressed changed
    end;

    Exit(true);
  end;
end;

function TCastleSwitchControl.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Checked') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
