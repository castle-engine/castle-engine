unit HSLRingPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics,
  Forms, Menus, Math, Themes,
  mbColorConv, HRingPicker, SLColorPicker, HTMLColors, mbBasicPicker;

type
  THSLRingPicker = class(TmbBasicPicker)
  private
    FRingPicker: THRingPicker;
    FSLPicker: TSLColorPicker;
    FSelectedColor: TColor;
//    FRValue, FGValue, FBValue: integer;
    FRingHint, FSLHint: string;
    FSLMenu, FRingMenu: TPopupMenu;
    FSLCursor, FRingCursor: TCursor;
    PBack: TBitmap;
    function GetBrightnessMode: TBrightnessMode;
    function GetHue: Integer;
    function GetLum: Integer;
    function GetSat: Integer;
    function GetVal: Integer;
    function GetMaxHue: Integer;
    function GetMaxLum: Integer;
    function GetMaxSat: Integer;
    function GetRed: Integer;
    function GetGreen: Integer;
    function GetBlue: Integer;
    function GetLVHint(AMode: TBrightnessMode): String;
    procedure SetBrightnessMode(AMode: TBrightnessMode);
    procedure SetHue(H: integer);
    procedure SetSat(S: integer);
    procedure SetLum(L: integer);
    procedure SetVal(V: Integer);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetRed(R: integer);
    procedure SetGreen(G: integer);
    procedure SetBlue(B: integer);
    procedure SetRingHint(h: string);
    procedure SetSLMenu(m: TPopupMenu);
    procedure SetRingMenu(m: TPopupMenu);
    procedure SetRingCursor(c: TCursor);
    procedure SetSLCursor(c: TCursor);
    procedure SetLVHint(AMode: TBrightnessMode; AText: String);
  protected
//    procedure CreateWnd; override;
    procedure DoChange; override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetColorUnderCursor: TColor; override;
    function GetSelectedColor: TColor; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure RingPickerChange(Sender: TObject);
    procedure SetSelectedColor(c: TColor); override;
    procedure SLPickerChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHexColorUnderCursor: string; override;
    function GetSelectedHexColor: string;
    procedure SetFocus; override;
    property ColorUnderCursor;
    property Red: integer read GetRed write SetRed;
    property Green: integer read GetGreen write SetGreen;
    property Blue: integer read GetBlue write SetBlue;
  published
    property BrightnessMode: TBrightnessMode read GetBrightnessMode
      write SetBrightnessMode default bmValue;
    property Hue: integer read GetHue write SetHue default 0;
    property Saturation: integer read GetSat write SetSat default 255;
    property Luminance: integer read GetLum write SetLum default 127;
    property Value: Integer read GetVal write SetVal default 255;
    property SelectedColor default clRed;
    property RingPickerPopupMenu: TPopupMenu read FRingMenu write SetRingMenu;
    property SLPickerPopupMenu: TPopupMenu read FSLMenu write SetSLMenu;
    property RingPickerHintFormat: string read FRingHint write SetRingHint;
    property SLPickerHintFormat: string index bmLuminance read GetLVHint write SetLVHint;
    property SVPickerHintFormat: String index bmValue read GetLVHint write SetLVHint;
    property RingPickerCursor: TCursor read FRingCursor write SetRingCursor default crDefault;
    property SLPickerCursor: TCursor read FSLCursor write SetSLCursor default crDefault;
    property MaxHue: Integer read GetMaxHue write SetMaxHue default 360;
    property MaxLuminance: Integer read GetMaxLum write SetMaxLum default 240;
    property MaxSaturation: Integer read GetMaxSat write SetMaxSat default 240;
    property TabStop default true;
    property ShowHint;
    property ParentShowHint;
    property Anchors;
    property Align;
    property Visible;
    property Enabled;
    property TabOrder;
    property Color;
    property ParentColor default true;
    property OnChange; //: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseMove;
  end;

implementation

{THSLRingPicker}

constructor THSLRingPicker.Create(AOwner: TComponent);
begin
  inherited;
//  ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque{$IFDEF DELPHI_7_UP}, csParentBackground{$ENDIF}];

  PBack := TBitmap.Create;
//  PBack.PixelFormat := pf32bit;
  SetInitialBounds(0, 0, 245, 245);
  TabStop := true;
  FRingCursor := crDefault;
  FSLCursor := crDefault;

  FRingPicker := THRingPicker.Create(Self);
  InsertControl(FRingPicker);
  with FRingPicker do
  begin
    SetInitialBounds(0, 0, 246, 246);
    BrightnessMode := bmValue;
    Align := alClient;
    OnChange := RingPickerChange;
    OnMouseMove := DoMouseMove;
  end;

  FSLPicker := TSLColorPicker.Create(Self);
  InsertControl(FSLPicker);
  with FSLPicker do
  begin
    SetInitialBounds(63, 63, 120, 120);
    BrightnessMode := bmValue;
    SLHintFormat := 'S: %hslS L: %l'#13'Hex: %hex';
    SVHintFormat := 'S: %hslS V: %v'#13'Hex: %hex';
    OnChange := SLPickerChange;
    OnMouseMove := DoMouseMove;
  end;

  SetSelectedColor(clRed);
end;

destructor THSLRingPicker.Destroy;
begin
  PBack.Free;
  inherited Destroy;
end;
          (*
procedure THSLRingPicker.CreateWnd;
begin
  inherited;
  //PaintParentBack(PBack);
end;        *)

procedure THSLRingPicker.DoChange;
begin
  FSelectedColor := FSLPicker.SelectedColor;
  inherited;
end;

procedure THSLRingPicker.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, x, y);
  inherited;
end;

function THSLRingPicker.GetBlue: Integer;
begin
  Result := GetRValue(FSelectedColor);
end;

function THSLRingPicker.GetBrightnessMode: TBrightnessMode;
begin
  Result := FSLPicker.BrightnessMode;
end;

function THSLRingPicker.GetColorUnderCursor: TColor;
begin
  Result := FSLPicker.ColorUnderCursor;
end;

function THSLRingPicker.GetGreen: Integer;
begin
  Result := GetGValue(FSelectedColor);
end;

function THSLRingPicker.GetHexColorUnderCursor: string;
begin
  Result := FSLPicker.GetHexColorUnderCursor;
end;

function THSLRingPicker.GetHue: Integer;
begin
  Result := FRingPicker.Hue;
end;

function THSLRingPicker.GetLum: Integer;
begin
  Result := FSLPicker.Luminance;
end;

function THSLRingPicker.GetLVHint(AMode: TBrightnessMode): String;
begin
  case BrightnessMode of
    bmLuminance: Result := FSLPicker.SLHintFormat;
    bmValue    : Result := FSLPicker.SVHintFormat;
  end;
end;

function THSLRingPicker.GetMaxHue: Integer;
begin
  Result := FRingPicker.MaxHue;
end;

function THSLRingPicker.GetMaxSat: Integer;
begin
  Result := FSLPicker.MaxSaturation;
end;

function THSLRingPicker.GetMaxLum: Integer;
begin
  Result := FSLPicker.MaxLuminance;
end;

function THSLRingPicker.GetRed: Integer;
begin
  Result := GetRValue(FSelectedColor);
end;

function THSLRingPicker.GetSat: Integer;
begin
  Result := FSLPicker.Saturation;
end;

function THSLRingPicker.GetSelectedColor: TColor;
begin
  Result := FSelectedColor;
end;

function THSLRingPicker.GetSelectedHexColor: string;
begin
  Result := ColorToHex(FSelectedColor);
end;

function THSLRingPicker.GetVal: Integer;
begin
  Result := FSLPicker.Value;
end;

procedure THSLRingPicker.Paint;
begin
  PaintParentBack(PBack);
  Canvas.Draw(0, 0, PBack);
end;

procedure THSLRingPicker.Resize;
var
  circ: TPoint;
  ctr: double;
begin
  inherited;
  if (FRingPicker = nil) or (FSLPicker = nil) then
    exit;

  ctr := Min(Width, Height) / 100;
  circ.x := Min(Width, Height) div 2;
  circ.y := circ.x;

  FRingPicker.Radius := circ.x - round(12*ctr);

  FSLPicker.Left := circ.x - FSLPicker.Width  div 2;
  FSLPicker.Top  := circ.y - FSLPicker.Height div 2;
  FSLPicker.Width := round(50 * ctr);
  FSLPicker.Height := FSLPicker.Width;

  PaintParentBack(PBack);
end;

procedure THSLRingPicker.RingPickerChange(Sender: TObject);
begin
  if FSLPicker.Hue <> FRingPicker.Hue then
  begin
    FSLPicker.Hue := FRingPicker.Hue;
    DoChange;
  end;
end;

procedure THSLRingPicker.SetBlue(B: integer);
begin
  SetSelectedColor(RgbToColor(Red, Green, B));
end;

procedure THSLRingPicker.SetBrightnessMode(AMode: TBrightnessMode);
begin
  FRingPicker.BrightnessMode := AMode;
  FSLPicker.BrightnessMode := AMode;
end;

procedure THSLRingPicker.SetFocus;
begin
  inherited;
  FRingPicker.SetFocus;
end;

procedure THSLRingPicker.SetGreen(G: integer);
begin
  SetSelectedColor(RgbToColor(Red, G, Blue));
end;

procedure THSLRingPicker.SetHue(H: integer);
begin
  FRingPicker.Hue := H;
  FSLPicker.Hue := H;
end;

procedure THSLRingPicker.SetLum(L: integer);
begin
  FSLPicker.Luminance := L;
end;

procedure THSLRingPicker.SetLVHint(AMode: TBrightnessMode; AText: string);
begin
  case AMode of
    bmLuminance: FSLPicker.SLHintFormat := AText;
    bmValue    : FSLPicker.SVHintFormat := AText;
  end;
end;

procedure THSLRingPicker.SetMaxHue(H: Integer);
begin
  FRingPicker.MaxHue := H;
  FSLPicker.MaxHue := H;
end;

procedure THSLRingPicker.SetMaxLum(L: Integer);
begin
  FRingPicker.MaxLuminance := L;
  FSLPicker.MaxLuminance := L;
end;

procedure THSLRingPicker.SetMaxSat(S: Integer);
begin
  FRingPicker.MaxSaturation := S;
  FSLPicker.MaxSaturation := S;
end;

procedure THSLRingPicker.SetRed(R: integer);
begin
  SetSelectedColor(RgbToColor(R, Green, Blue));
end;

procedure THSLRingPicker.SetRingCursor(c: TCursor);
begin
  FRingCursor := c;
  FRingPicker.Cursor := c;
end;

procedure THSLRingPicker.SetRingHint(h: string);
begin
  FRingHint := h;
  FRingPicker.HintFormat := h;
end;

procedure THSLRingPicker.SetRingMenu(m: TPopupMenu);
begin
  FRingMenu := m;
  FRingPicker.PopupMenu := m;
end;

procedure THSLRingPicker.SetSat(S: integer);
begin
  FSLPicker.Saturation := S;
end;

procedure THSLRingPicker.SetSelectedColor(c: TColor);
var
  H, S, LV: Double;
begin
  case BrightnessMode of
    bmLuminance: ColorToHSL(c, H, S, LV);
    bmValue    : ColorToHSV(c, H, S, LV);
  end;
  FRingPicker.RelHue := H;
  FSLPicker.SelectedColor := c;
  FSelectedColor := FSLPicker.SelectedColor;
end;

procedure THSLRingPicker.SetSLCursor(c: TCursor);
begin
  FSLCursor := c;
  FSLPicker.Cursor := c;
end;

procedure THSLRingPicker.SetSLMenu(m: TPopupMenu);
begin
  FSLMenu := m;
  FSLPicker.PopupMenu := m;
end;

procedure THSLRingPicker.SetVal(V: integer);
begin
  FSLPicker.Value := V;
end;

procedure THSLRingPicker.SLPickerChange(Sender: TObject);
begin
  if FSelectedColor = FSLPicker.SelectedColor then
    exit;
  FSelectedColor := FSLPicker.SelectedColor;
  DoChange;
end;

end.
