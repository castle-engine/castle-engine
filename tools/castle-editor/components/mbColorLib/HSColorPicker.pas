unit HSColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbColorConv, mbColorPickerControl;

type

  { THSColorPicker }

  THSColorPicker = class(TmbHSLVColorPickerControl)
  private
    FLumDisp, FValDisp: Double;  // Lum and Value used for display
  protected
    procedure CreateWnd; override;
    procedure DrawMarker(x, y: integer);
    function GetGradientColor2D(x, y: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
    procedure Paint; override;
    function PredictColor: TColor;
    procedure Resize; override;
    procedure SelectColor(x, y: Integer); override;
    procedure SetMaxHue(H: Integer); override;
    procedure SetMaxSat(S: Integer); override;
    procedure SetRelHue(H: Double); override;
    procedure SetRelSat(S: Double); override;
    procedure SetSelectedColor(c: TColor); override;
    procedure UpdateCoords;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: Integer): TColor; override;
  published
    property SelectedColor default clRed;
    property Hue default 0;
    property Saturation default 255;
    property Luminance default 127;
    property Value default 255;
    property MaxHue default 360;
    property MaxSaturation default 255;
    property MaxLuminance default 255;
    property MaxValue default 255;
    property MarkerStyle default msCross;
    property OnChange;
  end;


implementation

uses
  Math, mbUtils, PalUtils;

{ THSColorPicker }

constructor THSColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := FMaxHue;  // We want to skip the point at 360° --> no +1
  FGradientHeight := FMaxSat + 1;
  SetInitialBounds(0, 0, FGradientWidth, FGradientHeight);
  FHue := 0;
  FSat := 1.0;
  FLum := 0.5;
  FLumDisp := 0.5;
  FVal := 1.0;
  FValDisp := 1.0;
  FSelected := clRed;
  CreateGradient;
  HintFormat := 'H: %h S: %hslS'#13'Hex: %hex';
  MarkerStyle := msCross;
end;

procedure THSColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

procedure THSColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
  dummy: Double = 0;
begin
  CorrectCoords(x, y);
  ColorToHSLV(FSelected, FHue, FSat, dummy, dummy);
  if Focused or (csDesigning in ComponentState) then
    c := clBlack
  else
    case BrightnessMode of
      bmLuminance: c := clWhite;
      bmValue    : c := clGray;
    end;
  InternalDrawMarker(x, y, c);
end;

function THSColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  H, S: Double;
begin
  if InRange(x, 0, Width - 1) and InRange(y, 0, Height - 1) then
  begin
    H := x / Width;   // Width = FMaxHue
    S := 1 - y / (Height - 1);
    Result := HSLVtoColor(H, S, FLum, FVal);
  end else
    Result := clNone;
end;

function THSColorPicker.GetGradientColor2D(x, y: Integer): TColor;
var
  H, S: Double;
begin
  H := x / FMaxHue;
  S := 1 - y / FMaxSat;
  Result := HSLVtoColor(H, S, FLumDisp, FValDisp);
end;

function THSColorPicker.GetSelectedColor: TColor;
begin
  Result := HSLVtoColor(FHue, FSat, FLum, FVal);
end;

procedure THSColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  DrawMarker(mx, my);
end;

function THSColorPicker.PredictColor: TColor;
begin
  Result := GetColorUnderCursor;
end;

procedure THSColorPicker.Resize;
begin
  SetSelectedColor(FSelected);
  inherited;
end;

procedure THSColorPicker.SelectColor(x, y: Integer);
var
  H: Double = 0;
  S: Double = 0;
  L: Double = 0;
  V: Double = 0;
  c: TColor;
begin
  CorrectCoords(x, y);
  mx := x;
  my := y;
  c := GetColorAtPoint(x, y);
  if WebSafe then c := GetWebSafe(c);

  ColorToHSLV(c, H, S, L, V);
  {
  if (H = FHue) and (S = FSat) then
    exit;
   }
  FHue := H;
  FSat := S;
  FSelected := HSLVtoColor(FHue, FSat, FLum, FVal);

  Invalidate;
  DoChange;
end;

procedure THSColorPicker.SetMaxHue(H: Integer);
begin
  if H = FMaxHue then
    exit;
  FGradientWidth := H + 1;
  inherited;
end;

procedure THSColorPicker.SetMaxSat(S: Integer);
begin
  if S = FMaxSat then
    exit;
  FGradientHeight := S + 1;
  inherited;
end;

procedure THSColorPicker.SetRelHue(H: Double);
begin
  Clamp(H, 0, 1 - 1/FMaxHue);  // Don't use H=360°
  if H = FHue then
    exit;

  FHue := H;
  FSelected := GetSelectedColor;
  UpdateCoords;
  Invalidate;
  DoChange;
end;

procedure THSColorPicker.SetRelSat(S: Double);
begin
  Clamp(S, 0.0, 1.0);
  if S = FSat then
    exit;

  FSat := S;
  FSelected := GetSelectedColor;
  UpdateCoords;
  Invalidate;
  DoChange;
end;

// NOTE: In the picker display only the hue and the saturation of the input
// color are used, the luminance is replaced by the preset value of the picker.
// --> The selected color in the üicker display in general is different from the
//     input color.
procedure THSColorPicker.SetSelectedColor(c: TColor);
var
  H: Double = 0;
  S: Double = 0;
  L: Double = 0;
  V: Double = 0;
begin
  if WebSafe then
    c := GetWebSafe(c);

  ColorToHSLV(c, H, S, L, V);
  if (H = FHue) and (S = FSat) then
    exit;

  FSelected := c;
  FHue := H;
  FSat := S;
  case BrightnessMode of
    bmLuminance : FLum := L;
    bmValue     : FVal := V;
  end;

  UpdateCoords;
  Invalidate;
  DoChange;
end;

procedure THSColorPicker.UpdateCoords;
begin
  mx := Round(FHue * Width);
  my := Round((1.0 - FSat) * Height);
  CorrectCoords(mx, my);
end;

end.
