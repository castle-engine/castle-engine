{ A trackbar picker for Luminance or Value parameters from the HSL or HSV
  color models (depending on setting for BrightnessMode) }

unit LVColorPicker;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbColorConv, mbTrackBarPicker;

type
  TLVColorPicker = class(TmbHSLVTrackBarPicker)
  private
    FHint: array[TBrightnessMode] of string;
    function ArrowPosFromLum(L: Double): integer;
    function ArrowPosFromVal(V: Double): integer;
    function LumFromArrowPos(p: integer): Double;
    function ValFromArrowPos(p: Integer): Double;
    function GetHint(AMode: TBrightnessMode): String;
    procedure SetHint(AMode: TBrightnessMode; AText: String);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
    procedure SetBrightnessMode(AMode: TBrightnessMode); override;
    procedure SetMaxLum(L: Integer); override;
    procedure SetMaxVal(V: Integer); override;
    procedure SetRelLum(L: Double); override;
    procedure SetRelVal(V: Double); override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Hue default 0;
    property Saturation default 0;
    property Luminance default 255;
    property Value default 255;
    property SelectedColor default clWhite;
    property LHintFormat: String index bmLuminance read GetHint write SetHint;
    property VHintFormat: String index bmValue read GetHint write SetHint;
  end;

implementation

uses
  mbUtils;

{ TLVColorPicker }

constructor TLVColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  case BrightnessMode of
    bmLuminance : FGradientWidth := FMaxLum + 1;
    bmValue     : FGradientWidth := FMaxVal + 1;
  end;
  FGradientHeight := 1;
  FHue := 0;
  FSat := 0;
  FLum := 1;
  FVal := 1;
  FHint[bmLuminance] := 'Luminance: %lum (selected)';
  FHint[bmValue] := 'Value: %value (selected)';
end;

function TLVColorPicker.ArrowPosFromLum(L: Double): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round((Width - 12) * L);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((Height - 12) * (1.0 - L));
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TLVColorPicker.ArrowPosFromVal(V: Double): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round((Width - 12) * V);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((Height - 12) * (1.0 - V));
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

procedure TLVColorPicker.Execute(tbaAction: integer);
var
  dLum, dVal: Double;
begin
  case BrightnessMode of
    bmLuminance:
      begin
        if FMaxLum = 0 then dLum := 0 else dLum := Increment / FMaxLum;
        case tbaAction of
          TBA_Resize:
            SetRelLum(FLum);
          TBA_MouseMove:
            SetRelLum(LumFromArrowPos(FArrowPos));
          TBA_MouseDown:
            SetRelLum(LumFromArrowPos(FArrowPos));
          TBA_MouseUp:
            SetRelLum(LumFromArrowPos(FArrowPos));
          TBA_WheelUp:
            SetRelLum(FLum + dLum);
          TBA_WheelDown:
            SetRelLum(FLum - dLum);
          TBA_VKRight:
            SetRelLum(FLum + dLum);
          TBA_VKCtrlRight:
            SetRelLum(1.0);
          TBA_VKLeft:
            SetRelLum(FLum - dLum);
          TBA_VKCtrlLeft:
            SetRelLum(0.0);
          TBA_VKUp:
            SetRelLum(FLum + dLum);
          TBA_VKCtrlUp:
            SetRelLum(1.0);
          TBA_VKDown:
            SetRelLum(FLum - dLum);
          TBA_VKCtrlDown:
            SetRelLum(0);
          else
            inherited;
        end;
      end;

    bmValue:
      begin
        if FMaxVal = 0 then dVal := 0 else dVal := Increment / FMaxVal;
        case tbaAction of
          TBA_Resize:
            SetRelVal(FVal);
          TBA_MouseMove:
            SetRelVal(ValFromArrowPos(FArrowPos));
          TBA_MouseDown:
            SetRelVal(ValFromArrowPos(FArrowPos));
          TBA_MouseUp:
            SetRelVal(ValFromArrowPos(FArrowPos));
          TBA_WheelUp:
            SetRelVal(FVal + dVal);
          TBA_WheelDown:
            SetRelVal(FVal - dVal);
          TBA_VKRight:
            SetRelval(FVal + dVal);
          TBA_VKCtrlRight:
            SetRelVal(1.0);
          TBA_VKLeft:
            SetRelval(FVal - dVal);
          TBA_VKCtrlLeft:
            SetRelVal(0.0);
          TBA_VKUp:
            SetRelVal(FVal + dVal);
          TBA_VKCtrlUp:
            SetRelVal(1.0);
          TBA_VKDown:
            SetRelval(FVal - dVal);
          TBA_VKCtrlDown:
            SetRelVal(0.0);
          else
            inherited;
        end;
      end;
  end;
end;

function TLVColorPicker.GetArrowPos: integer;
begin
  case BrightnessMode of
    bmLuminance:
      if FMaxLum = 0 then
        Result := inherited GetArrowPos
      else
        Result := ArrowPosFromLum(FLum);
    bmValue:
      if FMaxVal = 0 then
        Result := inherited GetArrowPos
      else
        Result := ArrowPosFromVal(FVal);
  end;
end;

function TLVColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := HSLVtoColor(FHue, FSat, AValue/FMaxLum, AValue/FMaxVal);
end;

function TLVColorPicker.GetHint(AMode: TBrightnessMode): String;
begin
  Result := FHint[AMode];
end;

function TLVColorPicker.GetSelectedValue: integer;
begin
  case BrightnessMode of
    bmLuminance : Result := Luminance;
    bmValue     : Result := Value;
  end;
end;

function TLVColorPicker.LumFromArrowPos(p: integer): Double;
var
  L: Double;
begin
  case Layout of
    lyHorizontal : L :=       p / (Width - 12);
    lyVertical   : L := 1.0 - p /(Height - 12);
  end;
  Clamp(L, 0, 1.0);
  Result := L;
end;

procedure TLVColorPicker.SetBrightnessMode(AMode: TBrightnessMode);
begin
  inherited;
  HintFormat := FHint[AMode];
end;

procedure TLVColorPicker.SetHint(AMode: TBrightnessMode; AText: String);
begin
  FHint[AMode] := AText;
end;

procedure TLVColorPicker.SetMaxLum(L: Integer);
begin
  if L = FMaxLum then
    exit;
  FMaxLum := L;
  if BrightnessMode = bmLuminance then begin
    FGradientWidth := FMaxLum + 1;
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure TLVColorPicker.SetMaxVal(V: Integer);
begin
  if V = FMaxVal then
    exit;
  FMaxVal := V;
  if BrightnessMode = bmValue then begin
    FGradientWidth := FMaxVal + 1;
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure TLVColorPicker.SetRelLum(L: Double);
begin
  Clamp(L, 0, 1.0);
  if FLum <> L then
  begin
    FLum := L;
    FArrowPos := ArrowPosFromLum(L);
    Invalidate;
    DoChange;
  end;
end;

procedure TLVColorPicker.SetRelVal(V: Double);
begin
  Clamp(V, 0, 1.0);
  if FVal <> V then
  begin
    FVal := V;
    FArrowPos := ArrowPosFromVal(V);
    Invalidate;
    DoChange;
  end;
end;

procedure TLVColorPicker.SetSelectedColor(c: TColor);
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
  needNewGradient := (H <> FHue) or (S <> FSat);
  FHue := H;
  FSat := S;
  case BrightnessMode of
    bmLuminance : FLum := L;
    bmValue     : FVal := V;
  end;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

function TLVColorPicker.ValFromArrowPos(p: integer): Double;
var
  V: Double;
begin
  case Layout of
    lyHorizontal : V :=       p / (Width - 12);
    lyVertical   : V := 1.0 - p /(Height - 12);
  end;
  Clamp(V, 0, 1.0);
  Result := V;
end;

end.
