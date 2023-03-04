{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { An abstract slider user interface. You actually want to use one of its descendants:
    @link(TCastleFloatSlider) or @link(TCastleIntegerSlider). }
  TCastleAbstractSlider = class(TCastleUserInterfaceFont)
  strict private
    FDisplayValue: boolean;
    FOnChange: TNotifyEvent;
    FCaption: string;
    FBackground, FThumb: TCastleImagePersistent;
    FTextColor: TCastleColor;
    function GetThumbSize(const ARenderRect: TFloatRectangle): TVector2;
    procedure SetCaption(const Value: string);
    procedure SetTextColor(const Value: TCastleColor);
    procedure ImageChanged(Sender: TObject);
  private
    { Draw a slider at given Position. If Position is outside 0..1, it is clamped
      to 0..1 (this way we do not show slider at some wild position if it's
      outside the expected range; but DrawSliderText will still show the true,
      unclamped, value).

      @param(ARenderRect Rectangle currently occupied by the whole slider,
        in final device pixels. Should be equal to RenderRect.) }
    procedure DrawSliderPosition(const ARenderRect: TFloatRectangle; const Position: Single);

    { Returns a value of Position, always in 0..1 range,
      that would result in slider being drawn at XCoord screen position
      by DrawSliderPosition.

      @param(ARenderRect Rectangle currently occupied by the whole slider,
        in final device pixels. Should be equal to RenderRect.) }
    function XCoordToSliderPosition(const XCoord: Single;
      const ARenderRect: TFloatRectangle): Single;

    procedure DrawSliderText(const R: TFloatRectangle; Text: string);
  strict protected
    { React to value change. The default implementation simply calls the OnChange
      event, if assigned. This is called when value is changed by user actions
      (key, mouse), not when it's explicitly assigned by code. }
    procedure DoChange; virtual;
  public
    const
      DefaultSliderWidth = 200;
      DefaultSliderHeight = 30;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Color of the label. By default opaque black. }
    property TextColor: TCastleColor read FTextColor write SetTextColor;
  published
    property Width {$ifdef FPC}default DefaultSliderWidth{$endif};
    property Height {$ifdef FPC}default DefaultSliderHeight{$endif};

    { Display the current value as text on the slider,
      right next to the @link(Caption).

      The exact method to display is defined by method
      @link(TCastleFloatSlider.ValueToStr) or
      @link(TCastleIntegerSlider.ValueToStr) (depending on descendant),
      so you can further customize it. }
    property DisplayValue: boolean
      read FDisplayValue write FDisplayValue default true;

    { Displayed on the slider. Right before value, if @link(DisplayValue). }
    property Caption: string read FCaption write SetCaption;

    { Called when value (like @link(TCastleFloatSlider.Value) or @link(TCastleIntegerSlider.Value))
      changed because of user interaction (but not when it was changed by code setting
      the property). }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    { Background image that fills the entire slider area. }
    property Background: TCastleImagePersistent read FBackground;

    { Thumb image indicates the current value,
      like (like @link(TCastleFloatSlider.Value) or @link(TCastleIntegerSlider.Value)). }
    property Thumb: TCastleImagePersistent read FThumb;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleabstractslider_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Slider to change a float value within a given range. }
  TCastleFloatSlider = class(TCastleAbstractSlider)
  strict private
    FMin: Single;
    FMax: Single;
    FValue: Single;
    FMultipleOf: Single;
    FValuePointer: PSingle; //< non-nil only when initiazed with appropriate constructor
    procedure SetMin(const AMin: Single);
    procedure SetMax(const AMax: Single);
    procedure SetValue(const AValue: Single);
  strict protected
    procedure DoChange; override;
  public
    const
      DefaultMin = 0.0;
      DefaultMax = 1.0;
    constructor Create(AOwner: TComponent); overload; override;
    { An easy way to construct float slider. }
    constructor Create(AOwner: TComponent;
      const ValuePointer: PSingle; const AMin, AMax: Single;
      const AOnChange: TNotifyEvent); reintroduce; overload;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    function ValueToStr(const AValue: Single): string; virtual;
    { Round to multiple of @link(MultipleOf), if non-zero, and clamp
      to @link(Min) and @link(Max) range. }
    function RoundAndClamp(const AValue: Single): Single;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Min: Single read FMin write SetMin {$ifdef FPC}default DefaultMin{$endif};
    property Max: Single read FMax write SetMax {$ifdef FPC}default DefaultMax{$endif};

    { Current value. Usually within @link(Min) and @link(Max) range,
      although the general code should be ready for handle any value here
      (to work even during changes to @link(Min) and @link(Max) properties). }
    property Value: Single read FValue write SetValue {$ifdef FPC}default DefaultMin{$endif};

    { If non-zero, we force the value selected by user to be a multiple
      of this value (clamped to @link(Min), @link(Max) range).
      For example, if you set this to 0.25, and slider is between 0..1,
      then when user clicks around 0.3 --- we will pick 0.25. It user clicks
      around 0.4 --- we will pick 0.5.

      This only affects values selected by user interactions (clicking,
      dragging). This does not process the values you set by code to @link(Value)
      property, though you can use RoundAndClamp method on your values
      yourself. }
    property MultipleOf: Single read FMultipleOf write FMultipleOf;
  end;

  { Slider to change an integer value within a given range. }
  TCastleIntegerSlider = class(TCastleAbstractSlider)
  strict private
    FMin: Integer;
    FMax: Integer;
    FValue: Integer;
    FValuePointer: PInteger; //< non-nil only when initiazed with appropriate constructor
    function XCoordToValue(
      const XCoord: Single; const R: TFloatRectangle): Integer;
    procedure SetMin(const AMin: Integer);
    procedure SetMax(const AMax: Integer);
    procedure SetValue(const AValue: Integer);
  strict protected
    procedure DoChange; override;
  public
    const
      DefaultMin = 0;
      DefaultMax = 10;
    constructor Create(AOwner: TComponent); overload; override;
    { An easy way to construct integer slider. }
    constructor Create(AOwner: TComponent;
      const ValuePointer: PInteger; const AMin, AMax: Integer;
      const AOnChange: TNotifyEvent); reintroduce; overload;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    function ValueToStr(const AValue: Integer): string; virtual;
  published
    property Min: Integer read FMin write SetMin default DefaultMin;
    property Max: Integer read FMax write SetMax default DefaultMax;

    { Current value. Usually within @link(Min) and @link(Max) range,
      although the general code should be ready for handle any value here
      (to work even during changes to @link(Min) and @link(Max) properties). }
    property Value: Integer read FValue write SetValue default DefaultMin;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleAbstractSlider ------------------------------------------------------ }

constructor TCastleAbstractSlider.Create(AOwner: TComponent);
begin
  inherited;
  FDisplayValue := true;
  Width := DefaultSliderWidth;
  Height := DefaultSliderHeight;
  FTextColor := Black;

  FBackground := TCastleImagePersistent.Create;
  FBackground.OnChange := {$ifdef FPC}@{$endif} ImageChanged;
  FThumb := TCastleImagePersistent.Create;
  FThumb.OnChange := {$ifdef FPC}@{$endif} ImageChanged;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleabstractslider_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleAbstractSlider.Destroy;
begin
  FreeAndNil(FBackground);
  FreeAndNil(FThumb);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleabstractslider_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleAbstractSlider.ImageChanged(Sender: TObject);
begin
  VisibleChange([chRender]);
end;

procedure TCastleAbstractSlider.Render;
var
  BackgroundFinal: TCastleImagePersistent;
begin
  inherited;

  if FBackground.Empty then
    BackgroundFinal := Theme.ImagesPersistent[tiSliderBackground]
  else
    BackgroundFinal := FBackground;

  BackgroundFinal.DrawUiBegin(UIScale);
  BackgroundFinal.Draw(RenderRect);
  BackgroundFinal.DrawUiEnd;
end;

function TCastleAbstractSlider.GetThumbSize(const ARenderRect: TFloatRectangle): TVector2;
begin
  if FThumb.Empty then
  begin
    { The default thumb image adjusts to slides sizes. }
    Result.X := ARenderRect.Height;
    Result.Y := ARenderRect.Height;
  end else
  begin
    Result.X := UIScale * FThumb.Width;
    Result.Y := UIScale * FThumb.Height;
  end;
end;

procedure TCastleAbstractSlider.DrawSliderPosition(const ARenderRect: TFloatRectangle;
  const Position: Single);
var
  ThumbSize: TVector2;
  ThumbFinal: TCastleImagePersistent;
begin
  ThumbSize := GetThumbSize(ARenderRect);

  if FThumb.Empty then
    ThumbFinal := Theme.ImagesPersistent[tiSliderThumb]
  else
    ThumbFinal := FThumb;

  ThumbFinal.DrawUiBegin(UIScale);
  ThumbFinal.Draw(FloatRectangle(
    ARenderRect.Left
      + Round(MapRangeClamped(Position, 0, 1, 0, ARenderRect.Width - ThumbSize.X)),
    ARenderRect.Bottom
      + ARenderRect.Height / 2
      - ThumbSize.Y / 2,
    ThumbSize.X,
    ThumbSize.Y));
  ThumbFinal.DrawUiEnd;
end;

function TCastleAbstractSlider.XCoordToSliderPosition(
  const XCoord: Single; const ARenderRect: TFloatRectangle): Single;
var
  ThumbSize: TVector2;
begin
  ThumbSize := GetThumbSize(ARenderRect);
  Result := MapRangeClamped(XCoord,
    ARenderRect.Left + 0,
    ARenderRect.Left + ARenderRect.Width - ThumbSize.X, 0, 1);
end;

procedure TCastleAbstractSlider.DrawSliderText(
  const R: TFloatRectangle; Text: string);
begin
  if (Caption <> '') and (Text <> '') then
    Text := Caption + ': ' + Text else
    Text := Caption + Text;
  Font.Print(
    R.Left + (R.Width - Font.TextWidth(Text)) / 2,
    R.Bottom + (R.Height - Font.RowHeight) / 2,
    TextColor, Text);
end;

procedure TCastleAbstractSlider.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCastleAbstractSlider.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleAbstractSlider.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  { left / right arrow keys pressed are already handled by slider }
  if HandleInput and
     (Container.Pressed[keyArrowRight] or Container.Pressed[keyArrowLeft]) then
    HandleInput := false;
end;

function TCastleAbstractSlider.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Caption', 'DisplayValue', 'TextColorPersistent', 'Background', 'Thumb'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleAbstractSlider.SetTextColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FTextColor, Value) then
  begin
    FTextColor := Value;
    VisibleChange([chRender]);
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleabstractslider_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleFloatSlider --------------------------------------------------------- }

constructor TCastleFloatSlider.Create(AOwner: TComponent);
begin
  inherited;
  FMin := DefaultMin;
  FMax := DefaultMax;
  FValue := FMin;
end;

constructor TCastleFloatSlider.Create(AOwner: TComponent;
  const ValuePointer: PSingle; const AMin, AMax: Single;
  const AOnChange: TNotifyEvent);
begin
  Create(AOwner);
  FValuePointer := ValuePointer;
  if Assigned(FValuePointer) then
    Value := FValuePointer^;
  Min := AMin;
  Max := AMax;
  OnChange := AOnChange;
end;

procedure TCastleFloatSlider.DoChange;
begin
  if Assigned(FValuePointer) then
    FValuePointer^ := Value;
  inherited;
end;

procedure TCastleFloatSlider.Render;
var
  RR: TFloatRectangle;
begin
  inherited;
  RR := RenderRect;
  DrawSliderPosition(RR, MapRange(Value, Min, Max, 0, 1));
  if DisplayValue then
    DrawSliderText(RR, ValueToStr(Value));
end;

function TCastleFloatSlider.Press(const Event: TInputPressRelease): boolean;

  function ValueChange: Single;
  begin
    Result := (Max - Min) / 100;
  end;

begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyArrowRight) then
  begin
    if MultipleOf <> 0 then
      Value := Math.Min(Max, Value + MultipleOf) else
      Value := Math.Min(Max, Value + ValueChange);
    DoChange;
    Result := true;
  end else
  if Event.IsKey(keyArrowLeft) then
  begin
    if MultipleOf <> 0 then
      Value := Math.Max(Min, Value - MultipleOf) else
      Value := Math.Max(Min, Value - ValueChange);
    DoChange;
    Result := true;
  end else
  if Event.IsMouseButton(buttonLeft) then
  begin
    Value := RoundAndClamp(MapRange(
      XCoordToSliderPosition(Event.Position[0], RenderRect), 0, 1, Min, Max));
    DoChange;
    Result := true;
  end;
end;

function TCastleFloatSlider.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if buttonLeft in Event.Pressed then
  begin
    Value := RoundAndClamp(MapRange(
      XCoordToSliderPosition(Event.Position[0], RenderRect), 0, 1, Min, Max));
    DoChange;
    Result := true;
  end;
end;

function TCastleFloatSlider.ValueToStr(const AValue: Single): string;
begin
  Result := Format('%f', [AValue]);
end;

procedure TCastleFloatSlider.SetMin(const AMin: Single);
begin
  if FMin <> AMin then
  begin
    FMin := AMin;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleFloatSlider.SetMax(const AMax: Single);
begin
  if FMax <> AMax then
  begin
    FMax := AMax;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleFloatSlider.SetValue(const AValue: Single);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    VisibleChange([chRender]);
  end;
end;

function TCastleFloatSlider.RoundAndClamp(const AValue: Single): Single;
var
  DivResult: Int64;
  M, Remainder: Double;
begin
  if MultipleOf <> 0 then
  begin
    { we use FloatDivMod.
      We have to secure in case AValue or MultipleOf are < 0.
      For MultipleOf it's easy, just always use Abs(MultipleOf). }
    M := Abs(MultipleOf);
    if AValue >= 0 then
    begin
      FloatDivMod(AValue, M, DivResult, Remainder);
      if Remainder < M / 2 then
        Result := M * DivResult else
        Result := M * (DivResult + 1);
    end else
    begin
      FloatDivMod(-AValue, M, DivResult, Remainder);
      if Remainder < M / 2 then
        Result := M * DivResult else
        Result := M * (DivResult + 1);
      Result := -Result;
    end;
  end else
    Result := AValue;

  { Clamp at the end. If Min, Max are not a multiple of MultipleOf - so be it. }
  Result := Clamped(Result, Min, Max);
end;

function TCastleFloatSlider.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Min', 'Max', 'Value', 'MultipleOf'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TCastleIntegerSlider ------------------------------------------------------- }

constructor TCastleIntegerSlider.Create(AOwner: TComponent);
begin
  inherited;
  FMin := DefaultMin;
  FMax := DefaultMax;
  FValue := FMin;
end;

constructor TCastleIntegerSlider.Create(AOwner: TComponent;
  const ValuePointer: PInteger; const AMin, AMax: Integer;
  const AOnChange: TNotifyEvent);
begin
  Create(AOwner);
  FValuePointer := ValuePointer;
  if Assigned(FValuePointer) then
    Value := FValuePointer^;
  Min := AMin;
  Max := AMax;
  OnChange := AOnChange;
end;

procedure TCastleIntegerSlider.DoChange;
begin
  if Assigned(FValuePointer) then
    FValuePointer^ := Value;
  inherited;
end;

procedure TCastleIntegerSlider.Render;
var
  RR: TFloatRectangle;
begin
  inherited;
  RR := RenderRect;
  DrawSliderPosition(RR, MapRange(Value, Min, Max, 0, 1));
  if DisplayValue then
    DrawSliderText(RR, ValueToStr(Value));
end;

function TCastleIntegerSlider.Press(const Event: TInputPressRelease): boolean;
const
  ValueChange = 1;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyArrowRight) then
  begin
    Value := Math.Min(Max, Value + ValueChange);
    DoChange;
    Result := true;
  end else
  if Event.IsKey(keyArrowLeft) then
  begin
    Value := Math.Max(Min, Value - ValueChange);
    DoChange;
    Result := true;
  end else
  if Event.IsMouseButton(buttonLeft) then
  begin
    Value := XCoordToValue(Event.Position[0], RenderRect);
    DoChange;
    Result := true;
  end;
end;

function TCastleIntegerSlider.XCoordToValue(
  const XCoord: Single; const R: TFloatRectangle): Integer;
begin
  { We do additional Clamped over Round result to avoid any
    chance of floating-point errors due to lack of precision. }
  Result := Clamped(Round(
    MapRange(XCoordToSliderPosition(XCoord, R), 0, 1,
      Min, Max)), Min, Max);
end;

function TCastleIntegerSlider.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if buttonLeft in Event.Pressed then
  begin
    Value := XCoordToValue(Event.Position[0], RenderRect);
    DoChange;
    Result := true;
  end;
end;

function TCastleIntegerSlider.ValueToStr(const AValue: Integer): string;
begin
  Result := IntToStr(AValue);
end;

procedure TCastleIntegerSlider.SetMin(const AMin: Integer);
begin
  if FMin <> AMin then
  begin
    FMin := AMin;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleIntegerSlider.SetMax(const AMax: Integer);
begin
  if FMax <> AMax then
  begin
    FMax := AMax;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleIntegerSlider.SetValue(const AValue: Integer);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    VisibleChange([chRender]);
  end;
end;

function TCastleIntegerSlider.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Min', 'Max', 'Value'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
