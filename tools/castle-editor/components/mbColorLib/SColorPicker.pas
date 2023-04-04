unit SColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  mbColorConv, mbTrackBarPicker, HTMLColors;

type
  TSColorPicker = class(TmbHSLVTrackBarPicker)
  private
    function ArrowPosFromSat(s: Double): integer;
    function SatFromArrowPos(p: integer): Double;
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
    procedure SetMaxSat(S: Integer); override;
    procedure SetRelSat(S: Double); override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
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

{ TSColorPicker }

constructor TSColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := FMaxSat + 1;
  FGradientHeight := 1;
  FHue := 0;
  FLum := 0.5;
  FVal := 1.0;
  Saturation := 255;
  HintFormat := 'Saturation: %value (selected)';
end;

function TSColorPicker.ArrowPosFromSat(s: Double): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(s * (Width - 12));
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((1.0 - s) * (Height - 12));
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

procedure TSColorPicker.Execute(tbaAction: integer);
var
  dSat: Double;
begin
  if FMaxSat = 0 then dSat := 0 else dSat := Increment / FMaxSat;
  case tbaAction of
    TBA_Resize:
      SetRelSat(FSat);
    TBA_MouseMove:
      SetRelSat(SatFromArrowPos(FArrowPos));
    TBA_MouseDown:
      SetRelSat(SatFromArrowPos(FArrowPos));
    TBA_MouseUp:
      SetRelSat(SatFromArrowPos(FArrowPos));
    TBA_WheelUp:
      SetRelSat(FSat + dSat);
    TBA_WheelDown:
      SetRelSat(FSat - dSat);
    TBA_VKLeft:
      SetRelSat(FSat - dSat);
    TBA_VKCtrlLeft:
      SetRelSat(0.0);
    TBA_VKRight:
      SetRelSat(FSat + dSat);
    TBA_VKCtrlRight:
      SetRelSat(1.0);
    TBA_VKUp:
      SetRelSat(FSat + dSat);
    TBA_VKCtrlUp:
      SetRelSat(1.0);
    TBA_VKDown:
      SetRelSat(FSat - dSat);
    TBA_VKCtrlDown:
      SetRelSat(0.0);
    else
      inherited;
  end;
end;

function TSColorPicker.GetArrowPos: integer;
begin
  if FMaxSat = 0 then
    Result := inherited GetArrowPos
  else
    Result := ArrowPosFromSat(FSat);
end;

function TSColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := HSLVtoColor(FHue, AValue/FMaxSat, FLum, FVal);
end;

function TSColorPicker.GetSelectedValue: integer;
begin
  Result := Saturation;
end;

function TSColorPicker.SatFromArrowPos(p: integer): Double;
var
  s: Double;
begin
  case Layout of
    lyHorizontal: s :=       p / (Width  - 12);
    lyVertical  : s := 1.0 - p / (Height - 12);
  end;
  Clamp(s, 0, 1.0);
  Result := s;
end;

procedure TSColorPicker.SetMaxSat(S: Integer);
begin
  if S = FMaxSat then
    exit;
  FMaxSat := S;
  FGradientWidth := FMaxSat + 1;
  CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TSColorPicker.SetRelSat(S: Double);
begin
  Clamp(S, 0, 1.0);
  if FSat <> S then
  begin
    FSat := S;
    FArrowPos := ArrowPosFromSat(S);
    Invalidate;
    DoChange;
  end;
end;

procedure TSColorPicker.SetSelectedColor(c: TColor);
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

  ColorToHSLV(c, H,S,L,V);
  case BrightnessMode of
    bmLuminance:
      begin
        needNewGradient := (H <> FHue) or (L <> FLum);
        FLum := L;
      end;
    bmValue:
      begin
        needNewGradient := (H <> FHue) or (V <> FVal);
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
