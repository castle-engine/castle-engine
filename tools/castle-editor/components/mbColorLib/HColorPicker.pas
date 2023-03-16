unit HColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbColorConv, mbTrackBarPicker;

type
  THColorPicker = class(TmbHSLVTrackBarPicker)
  private
    function ArrowPosFromHue(h: Double): integer;
    function HueFromArrowPos(p: integer): Double;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
    procedure SetMaxHue(H: Integer); override;
    procedure SetRelHue(H: Double); override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Layout default lyHorizontal;
    property Hue default 0;
    property Saturation default 255;
    property Luminance default 127;
    property Value default 255;
    property SelectedColor default clRed;
    property HintFormat;
  end;


implementation

uses
  mbUtils;

{THColorPicker}

constructor THColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := FMaxHue;
  FGradientHeight := 1;
  FSat := 1.0;
  FVal := 1.0;
  FLum := 0.5;
  Hue := 0;
  HintFormat := 'Hue: %value (selected)';
end;

function THColorPicker.ArrowPosFromHue(H: Double): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round((Width - 12) * H);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((Height - 12) * H);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function THColorPicker.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if Layout = lyVertical then WheelDelta := -WheelDelta;
  WheelDelta := WheelDelta * 3;  // use larger steps
  Result := inherited;
end;

procedure THColorPicker.Execute(tbaAction: integer);
var
  dHue: Double;
begin
  if FMaxHue = 0 then dHue := 0 else dHue := Increment / FMaxHue;
  case tbaAction of
    TBA_Resize:
      SetRelHue(FHue);                 // wp: Is this working?
    TBA_MouseMove:
      SetRelHue(HueFromArrowPos(FArrowPos));
    TBA_MouseDown:
      SetRelHue(HueFromArrowPos(FArrowPos));
    TBA_MouseUp:
      SetRelHue(HueFromArrowPos(FArrowPos));
    TBA_WheelUp:
      SetRelHue(FHue + dHue);
    TBA_WheelDown:
      SetRelHue(FHue - dHue);
    TBA_VKLeft:
      SetRelHue(FHue - dHue);
    TBA_VKCtrlLeft:
      SetRelHue(0);
    TBA_VKRight:
      SetRelHue(FHue + dHue);
    TBA_VKCtrlRight:
      SetRelHue(1 - dHue);  // go one step below 360, or the hue will flip back to 0
    TBA_VKUp:
      SetRelHue(FHue - dHue);
    TBA_VKCtrlUp:
      SetRelHue(0);
    TBA_VKDown:
      SetRelHue(FHue + dHue);
    TBA_VKCtrlDown:
      SetRelHue(1 - dHue);
    else
      inherited;
  end;
end;

function THColorPicker.GetArrowPos: integer;
begin
  if FMaxHue = 0 then
    Result := inherited GetArrowPos
  else
    Result := ArrowPosFromHue(FHue);
end;

function THColorPicker.GetGradientColor(AValue: Integer): TColor;
var
  h: Double;
begin
  if Layout = lyVertical then AValue := FMaxHue - 1 - AValue;
    // Width is FMaxHue --> last index is FMaxHue - 1
  h := AValue / FMaxHue;
  Result := HSLVtoColor(h, FSat, FLum, FVal);
end;

function THColorPicker.GetSelectedValue: integer;
begin
  Result := Hue;
end;

function THColorPicker.HueFromArrowPos(p: integer): Double;
var
  h: Double;
begin
  case Layout of
    lyHorizontal : h := p / (Width - 12);
    lyVertical   : h := p / (Height - 12)
  end;
  Clamp(h, 0, 1.0 - 1/FMaxHue);
  Result := h;
end;

procedure THColorPicker.SetMaxHue(h: Integer);
begin
  if h = FMaxHue then
    exit;
  FMaxHue := h;
  FGradientWidth := FMaxHue;   // we don't want to access H=360, i.e. don't use FMaxHue+1
  CreateGradient;
  Invalidate;
end;

procedure THColorPicker.SetRelHue(H: Double);
begin
  if FMaxHue = 0 then
    exit;
  Clamp(H, 0, 1 - 1/FMaxHue);  // don't go up to 360 because this will flip back to the start
  if (FHue <> H) then
  begin
    FHue := H;
    FArrowPos := ArrowPosFromHue(H);
    Invalidate;
    DoChange;
  end;
end;

procedure THColorPicker.SetSelectedColor(c: TColor);
var
  H: Double = 0;
  S: Double = 0;
  L: Double = 0;
  V: Double = 0;
  needNewGradient: Boolean;
begin
  if WebSafe then
    c := GetWebSafe(c);
  if c = GetSelectedColor then
    exit;

  ColorToHSLV(c, H, S, L, V);
  case BrightnessMode of
    bmLuminance:
      begin
        needNewGradient := (S <> FSat) or (L <> FLum);
        FLum := L;
      end;
    bmValue:
      begin
        needNewGradient := (S <> FSat) or (V <> FVal);
        FVal := V;
      end;
  end;
  FHue := H;
  FSat := S;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

end.
