{
  Copyright 2017-2018 Michalis Kamburelis and Jan Adamec.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Touch-friendly checkbox control with representing on/off state. }
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

    property IsOn: boolean read FChecked write SetChecked default false;
      deprecated 'use Checked instead, consistent with LCL TCheckBox.Checked';
  published
    { Switch state. }
    property Checked: boolean read FChecked write SetChecked default false;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    { Event sent when switch value changed. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Width default DefaultSwitchControlWidth;
    property Height default DefaultSwitchControlHeight;
  end;

{$endif read_interface}

{$ifdef read_implementation}

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
  FPressed := true;
  Result := ExclusiveEvents;
end;

function TCastleSwitchControl.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  FPressed := false;
  FChecked := not FChecked;
  VisibleChange([chRender]);

  if Assigned(FOnChange) then
    FOnChange(Self);
  Result := ExclusiveEvents;
end;

{$endif read_implementation}
