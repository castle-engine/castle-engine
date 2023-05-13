unit SLColorPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  mbColorConv, mbColorPickerControl;

type
  TSLColorPicker = class(TmbHSLVColorPickerControl)
  private
    FHint: array[TBrightnessMode] of string;
    function GetHint(AMode: TBrightnessMode): String;
    procedure SetHint(AMode: TBrightnessMode; AText: String);
  protected
    procedure CorrectCoords(var x, y: integer);
    procedure CreateWnd; override;
    procedure DrawMarker(x, y: integer);
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure SelectColor(x, y: integer); override;
    procedure SetBrightnessMode(AMode: TBrightnessMode); override;
    procedure SetMaxLum(L: Integer); override;
    procedure SetMaxSat(S: Integer); override;
    procedure SetMaxVal(V: Integer); override;
    procedure SetRelLum(L: Double); override;
    procedure SetRelSat(S: Double); override;
    procedure SetRelVal(V: Double); override;
    procedure SetSelectedColor(c: TColor); override;
    procedure UpdateCoords;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
    property ColorUnderCursor;
  published
    property Hue default 0;
    property Saturation default 0;
    property Luminance default 255;
    property Value default 255;
    property MaxHue default 360;
    property MaxSaturation default 255;
    property MaxLuminance default 255;
    property MaxValue default 255;
    property SelectedColor default clWhite;
    property MarkerStyle default msCircle;
    property SLHintFormat: String index bmLuminance read GetHint write SetHint;
    property SVHintFormat: String index bmValue read GetHint write SetHint;
    property OnChange;
  end;

implementation

uses
  HTMLColors, mbUtils;

{ TSLColorPicker }

constructor TSLColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := FMaxSat + 1;                       // x --> Saturation
  case BrightnessMode of
    bmLuminance : FGradientHeight := FMaxLum + 1;      // y --> Luminance
    bmValue     : FGradientHeight := FMaxVal + 1;      // y --> value
  end;
  SetInitialBounds(0, 0, FGradientWidth, FGradientHeight);
  FHue := 0;
  FSat := 1.0;
  FLum := 1.0;
  FVal := 1.0;
  SLHintFormat := 'S: %hslS L: %l' + LineEnding + 'Hex: %hex';
  SVHintFormat := 'S: %hslS V: %v' + LineEnding + 'Hex: %hex';
  MarkerStyle := msCircle;
end;

procedure TSLColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TSLColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure TSLColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  c := not GetColorAtPoint(x, y);     // "not" --> invert color bits
  InternalDrawMarker(x, y, c);
end;

function TSLColorPicker.GetColorAtPoint(x, y: integer): TColor;
var
  S, LV: Double;
begin
  S := x / (Width - 1);
  LV := 1.0 - y / (Height - 1);
  Result := HSLVtoColor(FHue, S, LV, LV);
end;

{ This picker has Saturation along the X and Luminance or Value on the Y axis. }
function TSLColorPicker.GetGradientColor2D(X, Y: Integer): TColor;
begin
  Result := HSLVtoColor(FHue, x/FMaxSat, 1.0 - y/FMaxLum, 1.0 - y/FMaxVal);
end;

function TSLColorPicker.GetHint(AMode: TBrightnessMode): String;
begin
  Result := FHint[AMode];
end;

procedure TSLColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBMP);
//  UpdateCoords;
  DrawMarker(mx, my);
end;

procedure TSLColorPicker.Resize;
begin
  inherited;
  UpdateCoords;
end;

procedure TSLColorPicker.SelectColor(x, y: integer);
var
  S, LV: Double;
begin
  CorrectCoords(x, y);
  S := x / (Width - 1);
  LV := 1 - y / (Height - 1);

  case BrightnessMode of
    bmLuminance:
      begin
        if (S = FSat) and (LV = FLum) then
          exit;
        FLum := LV;
      end;
    bmValue:
      begin
        if (S = FSat) and (LV = FVal) then
          exit;
        FVal := LV;
      end;
  end;
  FSat := S;
  FSelected := HSLVtoColor(FHue, FSat, FLum, FVal);

  Invalidate;
  UpdateCoords;
  DoChange;
end;

procedure TSLColorPicker.SetBrightnessMode(AMode: TBrightnessMode);
begin
  inherited;
  HintFormat := FHint[AMode];
end;

procedure TSLColorPicker.SetHint(AMode: TBrightnessMode; AText: String);
begin
  FHint[AMode] := AText;
end;

procedure TSLColorPicker.SetMaxLum(L: Integer);
begin
  if L = FMaxLum then
    exit;
  if BrightnessMode = bmLuminance then
    FGradientHeight := L + 1;
  inherited;
end;

procedure TSLColorPicker.SetMaxSat(S: Integer);
begin
  if S = FMaxSat then
    exit;
  FGradientWidth := S + 1;  // inherited will re-create the gradient
  inherited;
end;

procedure TSLColorPicker.SetMaxVal(V: Integer);
begin
  if V = FMaxVal then
    exit;
  if BrightnessMode = bmValue then
    FGradientHeight := V + 1;
  inherited;
end;

procedure TSLColorPicker.SetRelLum(L: Double);
begin
  Clamp(L, 0.0, 1.0);
  if FLum <> L then
  begin
    FLum := L;
    if BrightnessMode = bmLuminance then
    begin
      FSelected := HSLtoColor(FHue, FSat, FLum);
      UpdateCoords;
      Invalidate;
    end;
    DoChange;
  end;
end;

procedure TSLColorPicker.SetRelSat(S: Double);
begin
  Clamp(S, 0.0, 1.0);
  if FSat <> S then
  begin
    FSat := S;
    FSelected := HSLVtoColor(FHue, FSat, FLum, FVal);
    UpdateCoords;
    Invalidate;
    DoChange;
  end;
end;

procedure TSLColorPicker.SetSelectedColor(c: TColor);
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
  needNewGradient := (FHue <> H);
  FHue := H;
  FSat := S;
  case BrightnessMode of
    bmLuminance : FLum := L;
    bmValue     : FVal := V;
  end;
  FSelected := c;
  UpdateCoords;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TSLColorPicker.SetRelVal(V: Double);
begin
  Clamp(V, 0.0, 1.0);
  if FVal <> V then
  begin
    FVal := V;
    if BrightnessMode = bmValue then
    begin
      FSelected := HSVtoColor(FHue, FSat, FVal);
      UpdateCoords;
      Invalidate;
    end;
    DoChange;
  end;
end;

procedure TSLColorPicker.UpdateCoords;
begin
  mx := round(FSat * (Width - 1));
  case BrightnessMode of
    bmLuminance : my := round((1.0 - FLum) * (Height - 1));
    bmValue     : my := round((1.0 - FVal) * (Height - 1));
  end;
end;


end.
