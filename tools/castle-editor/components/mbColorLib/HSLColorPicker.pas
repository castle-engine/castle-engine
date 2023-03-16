unit HSLColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms, Menus, Themes,
  HTMLColors, mbColorConv, HSColorPicker, LVColorPicker, mbBasicPicker;

type
  THSLColorPicker = class(TmbBasicPicker)
  private
    FHSPicker: THSColorPicker;
    FLVPicker: TLVColorPicker;
    FRed, FGreen, FBlue: integer;
    FHSHint: string;
    FLVMenu, FHSMenu: TPopupMenu;
    FLVIncrement: integer;
    FHSCursor, FLVCursor: TCursor;
    PBack: TBitmap;
    function GetBrightnessMode: TBrightnessMode;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetLum: Integer;
    function GetVal: Integer;
    function GetMaxHue: Integer;
    function GetMaxSat: Integer;
    function GetMaxLum: Integer;
    function GetMaxVal: Integer;
    function GetRelHue: Double;
    function GetRelSat: Double;
    function GetRelLum: Double;
    function GetRelVal: Double;
    function GetLVHint(AMode: TBrightnessMode): String;

    procedure SetBrightnessMode(AMode: TBrightnessMode);

    procedure SetHue(H: integer);
    procedure SetSat(S: integer);
    procedure SetLum(L: integer);
    procedure SetVal(V: Integer);

    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetMaxVal(V: Integer);

    procedure SetRed(R: integer);
    procedure SetGreen(G: integer);
    procedure SetBlue(B: integer);

    procedure SetRelHue(H: Double);
    procedure SetRelLum(L: Double);
    procedure SetRelSat(S: Double);
    procedure SetRelVal(V: Double);

    procedure SetHSCursor(c: TCursor);
    procedure SetHSHint(h: string);
    procedure SetHSMenu(m: TPopupMenu);

    procedure SetLVCursor(c: TCursor);
    procedure SetLVHint(AMode: TBrightnessMode; AText: string);
    procedure SetLVMenu(m: TPopupMenu);
    procedure SetLVIncrement(i: integer);

  protected
    procedure DoChange; override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetColorUnderCursor: TColor; override;
    function GetSelectedColor: TColor; override;
    procedure HSPickerChange(Sender: TObject);
    procedure LVPickerChange(Sender: TObject);
    procedure Paint; override;
    procedure Resize; override;
    procedure SetSelectedColor(Value: TColor); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHexColorUnderCursor: string; override;
    function GetSelectedHexColor: string;
    procedure SetFocus; override;
    property ColorUnderCursor;
    property Red: integer read FRed write SetRed;
    property Green: integer read FGreen write SetGreen;
    property Blue: integer read FBlue write SetBlue;
    property RelHue: Double read GetRelHue write SetRelHue;
    property RelSaturation: Double read GetRelSat write SetRelSat;
    property RelLuminance: Double read GetRelLum write SetRelLum;
    property RelValue: Double read GetRelVal write SetRelVal;
  published
    property BrightnessMode: TBrightnessMode read GetBrightnessMode
      write SetBrightnessMode default bmLuminance;
    property Hue: integer read GetHue write SetHue default 0;
    property Saturation: integer read GetSat write SetSat default 255;
    property Luminance: integer read GetLum write SetLum default 127;
    property LVIncrement: integer read FLVIncrement write SetLVIncrement default 1;
    property Value: Integer read GetVal write SetVal default 255;
    property SelectedColor default clRed;
    property HSPickerPopupMenu: TPopupMenu read FHSMenu write SetHSMenu;
    property LVPickerPopupMenu: TPopupMenu read FLVMenu write SetLVMenu;
    property HSPickerHintFormat: string read FHSHint write SetHSHint;
    property LPickerHintFormat: string index bmLuminance read GetLVHint write SetLVHint;
    property VPickerHintFormat: string index bmValue read GetLVHint write SetLVHint;
    property HSPickerCursor: TCursor read FHSCursor write SetHSCursor default crDefault;
    property LVPickerCursor: TCursor read FLVCursor write SetLVCursor default crDefault;
    property MaxHue: Integer read GetMaxHue write SetMaxHue default 360;
    property MaxSaturation: Integer read GetMaxSat write SetMaxSat default 255;
    property MaxLuminance: Integer read GetMaxLum write SetMaxLum default 255;
    property MaxValue: Integer read GetMaxVal write SetMaxVal default 255;
    property TabStop default true;
    property ShowHint;
    property ParentShowHint;
    property Anchors;
    property Align;
    property BorderSpacing;
    property Visible;
    property Enabled;
    property TabOrder;
    property Color;
    property ParentColor default true;
    property OnChange;
    property OnMouseMove;
  end;


implementation

{ THSLColorPicker }

uses
  mbTrackbarPicker;

constructor THSLColorPicker.Create(AOwner: TComponent);
begin
  inherited;
//  ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];

  PBack := TBitmap.Create;
//  PBack.PixelFormat := pf32bit;
  SetInitialBounds(0, 0, 206, 146);
  TabStop := true;

  FLVIncrement := 1;
  FHSCursor := crDefault;
  FLVCursor := crDefault;

  FHSPicker := THSColorPicker.Create(Self);
  InsertControl(FHSPicker);
  with FHSPicker do
  begin
    SetInitialBounds(0, 6, 174, 134);
    Anchors := [akLeft, akTop, akRight, akBottom];
 //   Visible := true;
    BrightnessMode := bmLuminance;
    MaxHue := 360;
    MaxSaturation := 255;
    MaxLuminance := 255;
    MaxValue := 255;
    OnChange := HSPickerChange;
    OnMouseMove := DoMouseMove;
  end;

  FLVPicker := TLVColorPicker.Create(Self);
  InsertControl(FLVPicker);
  with FLVPicker do
  begin
    Layout := lyVertical;
    SetInitialBounds(184, 0, 25, 146);
    Anchors := [akRight, akTop, akBottom];
//    Visible := true;
    BrightnessMode := bmLuminance;
    MaxHue := FHSPicker.MaxHue;
    MaxSaturation := FHSPicker.MaxSaturation;
    MaxLuminance := FHSPicker.MaxLuminance;
    MaxValue := FHSPicker.MaxValue;
    Luminance := MaxLuminance div 2;
    Value := MaxValue;
    OnChange := LVPickerChange;
    OnMouseMove := DoMouseMove;
  end;

  Hue := 0;
  Saturation := FHSPicker.MaxLuminance;
  Luminance := FHSPicker.MaxLuminance div 2;
  Value := FHSPicker.MaxValue;

  HSPickerHintFormat := 'H: %h S: %hslS'#13'Hex: %hex';
  {
  FLVHint[bmLuminance] := 'Luminance: %l';
  FLVHint[bmValue] := 'Value: %v';
  }
end;

destructor THSLColorPicker.Destroy;
begin
  PBack.Free;
  inherited Destroy;
end;

procedure THSLColorPicker.DoChange;
var
  c: TColor;
begin
  c := FLVPicker.SelectedColor;
  FRed := GetRValue(c);
  FGreen := GetGValue(c);
  FBlue := GetBValue(c);
  inherited;
end;

procedure THSLColorPicker.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, x, y);
  inherited;
end;

function THSLColorPicker.GetBrightnessMode: TBrightnessMode;
begin
  Result := FHSPicker.BrightnessMode;
end;

function THSLColorPicker.GetColorUnderCursor: TColor;
begin
  Result := FHSPicker.ColorUnderCursor;
end;

function THSLColorPicker.GetHue: Integer;
begin
  Result := FHSPicker.Hue;
end;

function THSLColorPicker.GetHexColorUnderCursor: string;
begin
  Result := FHSPicker.GetHexColorUnderCursor;
end;

function THSLColorPicker.GetSat: Integer;
begin
  Result := FHSPicker.Saturation;
end;

function THSLColorPicker.GetLum: integer;
begin
  Result := FLVPicker.Luminance;
end;

function THSLColorPicker.GetVal: Integer;
begin
  Result := FLVPicker.Value;
end;

function THSLColorPicker.GetMaxHue: Integer;
begin
  Result := FHSPicker.MaxHue;
end;

function THSLColorPicker.GetMaxSat: Integer;
begin
  Result := FHSPicker.MaxSaturation;
end;

function THSLColorPicker.GetMaxLum: Integer;
begin
  Result := FLVPicker.MaxLuminance;
end;

function THSLColorPicker.GetMaxVal: Integer;
begin
  Result := FLVPicker.MaxValue;
end;

function THSLColorPicker.GetRelHue: Double;
begin
  Result := FHSPicker.RelHue;
end;

function THSLColorPicker.GetRelLum: Double;
begin
  Result := FLVPicker.RelLuminance;
end;

function THSLColorPicker.GetRelSat: Double;
begin
  Result := FHSPicker.RelSaturation;
end;

function THSLColorPicker.GetRelVal: Double;
begin
  Result := FLVPicker.RelValue;
end;

function THSLColorPicker.GetLVHint(AMode: TBrightnessMode): String;
begin
  case AMode of
    bmLuminance: Result := FLVPicker.LHintFormat;
    bmValue    : Result := FLVPicker.VHintFormat;
  end;
end;

function THSLColorPicker.GetSelectedColor: TColor;
begin
  Result := FLVPicker.SelectedColor;
end;

function THSLColorPicker.GetSelectedHexColor: string;
begin
  Result := ColorToHex(GetSelectedColor);
end;

procedure THSLColorPicker.HSPickerChange(Sender: TObject);
var
  c: TColor;
begin
  FLVPicker.Lock;  // Lock the LVPicker to generate OnChange events here.
  try
    FLVPicker.Hue := FHSPicker.Hue;
    FLVPicker.Saturation := FHSPicker.Saturation;
  finally
    FLVPicker.Unlock;
    DoChange;
  end;
end;

procedure THSLColorPicker.LVPickerChange(Sender: TObject);
begin
  DoChange;
end;

procedure THSLColorPicker.Resize;
begin
  inherited;

 { if (FHSPicker = nil) or (FLVPicker = nil) then
    exit;
  }
  FHSPicker.Width := Width - FLVPicker.Width - 15;
  FHSPicker.Height := Height - 12;

  FLVPicker.Left := Width - FLVPicker.Width - 2;
  FLVPicker.Height := Height; // - 12;
end;

procedure THSLColorPicker.Paint;
begin
  PaintParentBack(Canvas);
  Canvas.Draw(0, 0, PBack);
end;

procedure THSLColorPicker.SetBlue(B: integer);
begin
  FBlue := B;
  SetSelectedColor(RGBtoColor(FRed, FGreen, FBlue));
end;

procedure THSLColorPicker.SetBrightnessMode(AMode: TBrightnessMode);
begin
  FHSPicker.BrightnessMode := AMode;
  FLVPicker.BrightnessMode := AMode;
end;

procedure THSLColorPicker.SetFocus;
begin
  inherited;
  FHSPicker.SetFocus;
end;

procedure THSLColorPicker.SetGreen(G: integer);
begin
  FGreen := G;
  SetSelectedColor(RGBtoColor(FRed, FGreen, FBlue));
end;

procedure THSLColorPicker.SetHue(H: integer);
begin
  FHSPicker.Hue := H;
  FLVPicker.Hue := H;
end;

procedure THSLColorPicker.SetHSCursor(c: TCursor);
begin
  FHSCursor := c;
  FHSPicker.Cursor := c;
end;

procedure THSLColorPicker.SetHSHint(h: string);
begin
  FHSHint := h;
  FHSPicker.HintFormat := h;
end;

procedure THSLColorPicker.SetHSMenu(m: TPopupMenu);
begin
  FHSMenu := m;
  FHSPicker.PopupMenu := m;
end;

procedure THSLColorPicker.SetLum(L: integer);
begin
  FLVPicker.Luminance := L;
end;

procedure THSLColorPicker.SetLVCursor(c: TCursor);
begin
  FLVCursor := c;
  FLVPicker.Cursor := c;
end;

procedure THSLColorPicker.SetLVHint(AMode: TBrightnessMode; AText: string);
begin
  case AMode of
    bmLuminance: FLVPicker.LHintFormat := AText;
    bmValue    : FLVPicker.VHintFormat := AText;
  end;
end;

procedure THSLColorPicker.SetLVIncrement(i: integer);
begin
  FLVIncrement := i;
  FLVPicker.Increment := i;
end;

procedure THSLColorPicker.SetLVMenu(m: TPopupMenu);
begin
  FLVMenu := m;
  FLVPicker.PopupMenu := m;
end;

procedure THSLColorPicker.SetMaxHue(H: Integer);
begin
  FHSPicker.MaxHue := H;
  FLVPicker.MaxHue := H;
end;

procedure THSLColorPicker.SetMaxLum(L: Integer);
begin
  FHSPicker.MaxLuminance := L;
  FLVPicker.MaxLuminance := L;
end;

procedure THSLColorPicker.SetMaxSat(S: Integer);
begin
  FHSPicker.MaxSaturation := S;
  FLVPicker.MaxSaturation := S;
end;

procedure THSLColorPicker.SetMaxVal(V: Integer);
begin
  FLVPicker.MaxValue := V;
end;

procedure THSLColorPicker.SetRed(R: integer);
begin
  FRed := R;
  SetSelectedColor(RGBtoColor(FRed, FGreen, FBlue));
end;

procedure THSLColorPicker.SetRelHue(H: Double);
begin
  FHSPicker.RelHue := H;
  FLVPicker.RelHue := H;
end;

procedure THSLCOlorPicker.SetRelSat(S: Double);
begin
  FHSPicker.RelSaturation := S;
  FLVPicker.RelSaturation := S;
end;

procedure THSLColorPicker.SetRelLum(L: Double);
begin
  FHSPicker.RelLuminance := L;
  FLVPicker.RelLuminance := L;
end;

procedure THSLColorPicker.SetRelVal(V: Double);
begin
  FHSPicker.RelValue := V;
  FLVPicker.RelValue := V;
end;

procedure THSLColorPicker.SetSat(S: integer);
begin
  if S <> FHSPicker.Saturation then
    FHSPicker.Saturation := S;
  if S <> FLVPicker.Saturation then
    FLVPicker.Saturation := S;
end;

procedure THSLColorPicker.SetSelectedColor(Value: TColor);
var
  c: TColor;
  H: Double = 0;
  S: Double = 0;
  LV: Double = 0;
begin
  c := GetSelectedColor;
  if c <> Value then
  begin
    case GetBrightnessMode of
      bmLuminance: ColorToHSL(Value, H, S, LV);
      bmValue    : ColorToHSV(value, H, S, LV);
    end;
    FHSPicker.RelHue := H;
    FHSPicker.RelSaturation := S;
    FLVPicker.SelectedColor := Value;
    FRed := GetRValue(Value);
    FGreen := GetGValue(Value);
    FBlue := GetBValue(Value);
  end;
end;

procedure THSLColorPicker.SetVal(V: Integer);
begin
  FLVPicker.Value := V;
end;

end.
