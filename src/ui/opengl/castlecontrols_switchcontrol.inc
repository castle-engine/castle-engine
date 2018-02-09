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
  TCastleSwitchControl = class(TUIControl)
  strict private
    FIsOn: boolean;
    FEnabled: boolean;
    FPressed: boolean;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FOnChange: TNotifyEvent;

    procedure SetIsOn(const Value: boolean);
    procedure SetEnabled(const Value: boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Render; override;
    function Rect: TRectangle; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;

  published
    { Switch state. }
    property IsOn: boolean read FIsOn write SetIsOn default false;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
    { Event sent when switch value changed. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleSwitchControl ---------------------------------------------------------- }

constructor TCastleSwitchControl.Create(AOwner: TComponent);
begin
  inherited;
  FIsOn := false;
  FEnabled := true;
  FPressed := false;
  FOnChange := nil;
  FWidth := 50;
  FHeight := 28;
end;

destructor TCastleSwitchControl.Destroy;
begin
  inherited;
end;

procedure TCastleSwitchControl.Render;
var
  BaseRect, BackRect, HandleRect: TRectangle;
  HandleImage: TThemeImage;
begin
  inherited;

  BaseRect := ScreenRect;

  // background
  BackRect := BaseRect;
  BackRect.Height := Round(BaseRect.Height * 0.5);  //*0.4; //BaseRect.Height div 2;
  BackRect.Bottom := BaseRect.Bottom + ((BaseRect.Height - BackRect.Height) div 2);
  Theme.Draw(BackRect, tiSwitchControl, UIScale);
  if IsOn then
    Theme.Draw(BackRect, tiSwitchControlFill, UIScale);

  // handle
  HandleRect := BaseRect;
  HandleRect.Width := BaseRect.Width div 3; //HandleRect.Height;
  if IsOn then
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

function TCastleSwitchControl.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, Width, Height);
  Result := Result.ScaleAround0(UIScale);
end;

procedure TCastleSwitchControl.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    VisibleChange;
  end;
end;

procedure TCastleSwitchControl.SetIsOn(const Value: boolean);
begin
  if FIsOn <> Value then
  begin
    FIsOn := Value;
    VisibleChange;
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
  FIsOn := not FIsOn;
  VisibleChange;

  if Assigned(FOnChange) then
    FOnChange(Self);
  Result := ExclusiveEvents;
end;

{$endif read_implementation}
