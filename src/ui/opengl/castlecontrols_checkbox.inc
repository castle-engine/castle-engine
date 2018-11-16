{%MainUnit castlecontrols.pas}
{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Checkbox with a caption. }
  TCastleCheckbox = class(TCastleUserInterface)
  strict private
    FChecked, FPressed: Boolean;
    FCaption: String;
    FOnChange: TNotifyEvent;
    FCaptionMargin: Single;
    FImageSquare, FImageCheckmark, FImagePressed: TCastleImageControl;
    FLabelCaption: TCastleLabel;
    FTextColor, FCheckboxColor: TCastleColor;
    FAutoSize: Boolean;
    FFontSize: Single;
    FCheckboxSize: Single;
    procedure SetChecked(const Value: Boolean);
    procedure SetCaption(const Value: String);
    procedure SetTextColor(const Value: TCastleColor);
    procedure SetCheckboxColor(const Value: TCastleColor);
    procedure SetCaptionMargin(const Value: Single);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetFontSize(const Value: Single);
    procedure SetCheckboxSize(const Value: Single);
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    const
      DefaultCaptionMargin = 10;
      DefaultCheckboxSize = 24;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    procedure EditorAllowResize(
      out ResizeWidth, ResizeHeight: Boolean; out Reason: String); override;

    { Color of the label. By default opaque black. }
    property TextColor: TCastleColor read FTextColor write SetTextColor;
    { Color of the checkbox square and checkmark. By default opaque black. }
    property CheckboxColor: TCastleColor read FCheckboxColor write SetCheckboxColor;
  published
    property Checked: Boolean read FChecked write SetChecked default false;
    property Caption: String read FCaption write SetCaption;
    { Margin between checkbox square and a text @link(Caption). }
    property CaptionMargin: Single read FCaptionMargin write SetCaptionMargin
      default DefaultCaptionMargin;
    { Event sent when @link(Checked) value was changed by a user click.
      Note that this is @italic(not) called when you change @link(Checked)
      property programmatically by setting the property. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Should our size be determined by checkbox image size and caption. }
    property AutoSize: Boolean read FAutoSize write SetAutoSize default true;
    { Caption font size. }
    property FontSize: Single read FFontSize write SetFontSize default 0.0;
    { Size of the square checkbox. }
    property CheckboxSize: Single read FCheckboxSize write SetCheckboxSize
      default DefaultCheckboxSize;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlecheckbox_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleCheckbox ---------------------------------------------------------- }

constructor TCastleCheckbox.Create(AOwner: TComponent);
begin
  inherited;
  FCaptionMargin := DefaultCaptionMargin;
  FAutoSize := true;
  FTextColor := Black;
  FCheckboxColor := Black;
  FCheckboxSize := DefaultCheckboxSize;

  FImageSquare := TCastleImageControl.Create(Self);
  FImageSquare.SetTransient;
  FImageSquare.Image := EmptySquare;
  FImageSquare.OwnsImage := false;
  FImageSquare.Color := FCheckboxColor;
  FImageSquare.Anchor(vpMiddle);
  FImageSquare.Stretch := true;
  FImageSquare.Width := FCheckboxSize;
  FImageSquare.Height := FCheckboxSize;
  InsertFront(FImageSquare);

  FImageCheckmark := TCastleImageControl.Create(Self);
  FImageCheckmark.SetTransient;
  FImageCheckmark.Image := Checkmark;
  FImageCheckmark.OwnsImage := false;
  FImageCheckmark.Anchor(hpMiddle);
  FImageCheckmark.Anchor(vpMiddle);
  FImageCheckmark.Color := FCheckboxColor;
  FImageCheckmark.Exists := FChecked;
  FImageCheckmark.Stretch := true;
  FImageCheckmark.Width := FCheckboxSize;
  FImageCheckmark.Height := FCheckboxSize;
  FImageSquare.InsertFront(FImageCheckmark);

  FImagePressed := TCastleImageControl.Create(Self);
  FImagePressed.SetTransient;
  FImagePressed.Image := EmptySquareGlow;
  FImagePressed.OwnsImage := false;
  FImagePressed.Anchor(hpMiddle);
  FImagePressed.Anchor(vpMiddle);
  FImagePressed.Color := FCheckboxColor;
  FImagePressed.Exists := FPressed;
  FImagePressed.Stretch := true;
  FImagePressed.Width := FCheckboxSize;
  FImagePressed.Height := FCheckboxSize;
  FImageSquare.InsertFront(FImagePressed);

  FLabelCaption := TCastleLabel.Create(Self);
  FLabelCaption.SetTransient;
  FLabelCaption.Color := FTextColor;
  FLabelCaption.Anchor(hpLeft, FImageSquare.EffectiveWidth + CaptionMargin);
  FLabelCaption.Anchor(vpMiddle);
  InsertFront(FLabelCaption);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlecheckbox_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleCheckbox.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlecheckbox_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleCheckbox.PreferredSize(var PreferredWidth, PreferredHeight: Single);
begin
  if AutoSize then
  begin
    PreferredWidth := UIScale * (
      FImageSquare.EffectiveWidth + CaptionMargin +
      FLabelCaption.EffectiveWidth);
    PreferredHeight := UIScale *
      Max(FImageSquare.EffectiveHeight, FLabelCaption.EffectiveHeight);
  end;
end;

procedure TCastleCheckbox.EditorAllowResize(
  out ResizeWidth, ResizeHeight: Boolean; out Reason: String);
begin
  inherited;
  if AutoSize then
  begin
    ResizeWidth := false;
    ResizeHeight := false;
    Reason := SAppendPart(Reason, NL, 'Turn off "TCastleCheckbox.AutoSize" to change size.');
  end;
end;

procedure TCastleCheckbox.SetChecked(const Value: boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    FImageCheckmark.Exists := FChecked;
    { Note that we don't call "VisibleChange([chRender])" here,
      because the change to "FImageCheckmark.Exists" should already call it.
      In general, since TCastleCheckbox doesn't override Render method,
      it doesn't need to worry about calling "VisibleChange([chRender])". }
  end;
end;

function TCastleCheckbox.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(mbLeft) or
     Event.IsKey(keySpace) or
     Event.IsKey(keyEnter) then
  begin
    FPressed := true;
    FImagePressed.Exists := FPressed;
    Exit(ExclusiveEvents);
  end;
end;

function TCastleCheckbox.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if FPressed and (
     Event.IsMouseButton(mbLeft) or
     Event.IsKey(keySpace) or
     Event.IsKey(keyEnter)) then
  begin
    FPressed := false;
    FImagePressed.Exists := FPressed;

    { Because of mouse capture mechanism in TUIContainer,
      we receive Release notifications about all clicks that started
      within our rect, regardless where they finished.

      Just like TCastleButton, the click only counts
      if it happens over our rect. }
    if CapturesEventsAtPosition(Event.Position) then
    begin
      // change using property, so FImageCheckmark.Exists is switched
      Checked := not Checked;
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;

    Exit(ExclusiveEvents);
  end;
end;

procedure TCastleCheckbox.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    FLabelCaption.Caption := Value;
  end;
end;

procedure TCastleCheckbox.SetTextColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FTextColor, Value) then
  begin
    FTextColor := Value;
    FLabelCaption.Color := Value;
  end;
end;

procedure TCastleCheckbox.SetCheckboxColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FCheckboxColor, Value) then
  begin
    FCheckboxColor := Value;
    FImageSquare.Color := Value;
    FImageCheckmark.Color := Value;
    FImagePressed.Color := Value;
  end;
end;

procedure TCastleCheckbox.SetCheckboxSize(const Value: Single);
begin
  if FCheckboxSize <> Value then
  begin
    FCheckboxSize := Value;
    FImageSquare.Width := Value;
    FImageSquare.Height := Value;
    FImageCheckmark.Width := Value;
    FImageCheckmark.Height := Value;
    FImagePressed.Width := Value;
    FImagePressed.Height := Value;
    FLabelCaption.Anchor(hpLeft, FImageSquare.EffectiveWidth + CaptionMargin);
  end;
end;

procedure TCastleCheckbox.SetCaptionMargin(const Value: Single);
begin
  if FCaptionMargin <> Value then
  begin
    FCaptionMargin := Value;
    FLabelCaption.Anchor(hpLeft, FImageSquare.EffectiveWidth + CaptionMargin);
  end;
end;

procedure TCastleCheckbox.SetFontSize(const Value: Single);
begin
  if FFontSize <> Value then
  begin
    FFontSize := Value;
    FLabelCaption.FontSize := Value;
  end;
end;

procedure TCastleCheckbox.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    VisibleChange([chRectangle]); // our effective size changed now
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlecheckbox_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
