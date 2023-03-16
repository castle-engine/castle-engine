unit SLHColorPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms, Menus, Themes,
  RGBHSLUtils, mbTrackBarPicker, HTMLColors, SLColorPicker, HColorPicker,
  mbColorConv, mbBasicPicker;

type
  TSLHColorPicker = class(TmbBasicPicker)
  private
    FSLPicker: TSLColorPicker;
    FHPicker: THColorPicker;
    FSelectedColor: TColor;
//    FHValue, FSValue, FLValue: Double;
//    FRed, FGreen, FBlue: integer;
    FSLHint, FHHint: string;
    FSLMenu, FHMenu: TPopupMenu;
    FSLCursor, FHCursor: TCursor;
    PBack: TBitmap;
    function GetBrightnessMode: TBrightnessMode;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetLum: Integer;
    function GetVal: Integer;
    function GetMaxHue: Integer;
    function GetMaxLum: Integer;
    function GetMaxSat: Integer;
    function GetMaxVal: Integer;
    function GetRed: Integer;
    function GetGreen: Integer;
    function GetBlue: Integer;
    procedure SetBlue(B: integer);
    procedure SetBrightnessMode(bm: TBrightnessMode);
    procedure SetGreen(G: integer);
    procedure SetHue(H: integer);
    procedure SetLum(L: integer);
    procedure SetRed(R: integer);
    procedure SetSat(S: integer);
    procedure SetVal(V: Integer);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxVal(V: Integer);
    procedure SetHHint(h: string);
    procedure SetSLHint(h: string);
    procedure SetSLMenu(m: TPopupMenu);
    procedure SetHMenu(m: TPopupMenu);
    procedure SetHCursor(c: TCursor);
    procedure SetSLCursor(c: TCursor);
    procedure HPickerChange(Sender: TObject);
    procedure SLPickerChange(Sender: TObject);
  protected
    procedure DoChange; override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetColorUnderCursor: TColor; override;
    function GetSelectedColor: TColor; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
//    procedure BeginUpdate; override;
//    procedure EndUpdate(DoUpdate: Boolean = true); override;
    function GetHexColorUnderCursor: string; override;
    function GetSelectedHexColor: string;
    procedure SetFocus; override;
    property ColorUnderCursor;
    property Red: integer read GetRed write SetRed default 255;
    property Green: integer read GetGreen write SetGreen default 0;
    property Blue: integer read GetBlue write SetBlue default 0;
  published
    property BrightnessMode: TBrightnessMode read GetBrightnessMode
      write SetBrightnessMode default bmValue;
    property SelectedColor default clRed;
    property Hue: integer read GetHue write SetHue default 0;
    property Saturation: integer read GetSat write SetSat default 255;
    property Value: Integer read GetVal write SetVal default 255;
    property Luminance: integer read GetLum write SetLum default 127;
    property HPickerPopupMenu: TPopupMenu read FHMenu write SetHMenu;
    property SLPickerPopupMenu: TPopupMenu read FSLMenu write SetSLMenu;
    property HPickerHintFormat: string read FHHint write SetHHint;
    property SLPickerHintFormat: string read FSLHint write SetSLHint;
    property HPickerCursor: TCursor read FHCursor write SetHCursor default crDefault;
    property SLPickerCursor: TCursor read FSLCursor write SetSLCursor default crDefault;
    property MaxHue: Integer read GetMaxHue write SetMaxHue default 360;
    property MaxSaturation: Integer read GetMaxSat write SetMaxSat default 240;
    property MaxLuminance: Integer read GetMaxLum write SetMaxLum default 240;
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
    property OnChange;
    property OnMouseMove;
  end;


implementation

const
  WSL = 255;
  HSL = 255;
  WH = 40;
  DIST = 2;
  VDELTA = 8;

{TSLHColorPicker}

constructor TSLHColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  //ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];

  PBack := TBitmap.Create;
//  PBack.PixelFormat := pf32bit;
  ParentColor := true;
  SetInitialBounds(0, 0, WSL + DIST + WH, HSL + 2*VDELTA);
  TabStop := true;
  FHCursor := crDefault;
  FSLCursor := crDefault;
  FHHint := 'Hue: %h';
  FSLHint := 'S: %s L: %l'#13'Hex: %hex';

  // Saturation-Lightness picker
  FSLPicker := TSLColorPicker.Create(Self);
  InsertControl(FSLPicker);
  with FSLPicker do
  begin
    SetInitialBounds(0, VDELTA, WSL, HSL);
    Cursor := FSLCursor;
    BrightnessMode := bmValue;
    OnChange := SLPickerChange;
    OnMouseMove := DoMouseMove;
  end;

  // Hue picker
  FHPicker := THColorPicker.Create(Self);
  InsertControl(FHPicker);
  with FHPicker do
  begin
    Cursor := FHCursor;
    Layout := lyVertical;  // put before setting width and height
    SetInitialBounds(WSL + DIST, 0, WH, HSL + 2*VDELTA);
    BrightnessMode := bmValue;
    ArrowPlacement := spBoth;
    NewArrowStyle := true;
    OnChange := HPickerChange;
    OnMouseMove := DoMouseMove;
  end;

  // red
  SelectedColor := clRed;
end;

destructor TSLHColorPicker.Destroy;
begin
  PBack.Free;
  inherited Destroy;
end;

procedure TSLHColorPicker.DoChange;
begin
  FSelectedColor := FSLPicker.SelectedColor;
  inherited;
end;

procedure TSLHColorPicker.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, x, y);
end;

function TSLHColorPicker.GetBrightnessMode: TBrightnessMode;
begin
  Result := FSLPicker.BrightnessMode;
end;

function TSLHColorPicker.GetColorUnderCursor: TColor;
begin
  Result := FSLPicker.ColorUnderCursor;
end;

function TSLHColorPicker.GetBlue: Integer;
begin
  Result := GetBValue(FSelectedColor);
end;

function TSLHColorPicker.GetGreen: Integer;
begin
  Result := GetGValue(FSelectedColor);
end;

function TSLHColorPicker.GetHue: Integer;
begin
  Result := FHPicker.Hue;
end;

function TSLHColorPicker.GetHexColorUnderCursor: string;
begin
  Result := FSLPicker.GetHexColorUnderCursor;
end;

function TSLHColorPicker.GetLum: Integer;
begin
  Result := FSLPicker.Luminance;
end;

function TSLHColorPicker.GetMaxHue: Integer;
begin
  Result := FSLPicker.MaxHue;
end;

function TSLHColorPicker.GetMaxLum: Integer;
begin
  Result := FSLPicker.MaxLuminance;
end;

function TSLHColorPicker.GetMaxSat: Integer;
begin
  Result := FSLPicker.MaxSaturation;
end;

function TSLHColorPicker.GetMaxVal: Integer;
begin
  Result := FSLPicker.MaxValue;
end;

function TSLHColorPicker.GetRed: Integer;
begin
  Result := GetRValue(FSelectedColor);
end;

function TSLHColorPicker.GetSat: Integer;
begin
  Result := FSLPicker.Saturation;
end;

function TSLHColorPicker.GetSelectedColor: TColor;
begin
  Result := FSelectedColor;
end;

function TSLHColorPicker.GetSelectedHexColor: string;
begin
  Result := ColorToHex(FSelectedColor);
end;

function TSLHColorPicker.GetVal: Integer;
begin
  REsult := FSLPicker.Value;
end;

procedure TSLHColorPicker.HPickerChange(Sender: TObject);
begin
  if FSLPicker.Hue = FHPicker.Hue then
    exit;
  FSLPicker.Hue := FHPicker.Hue;
  DoChange;
end;

procedure TSLHColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  SetFocus;
  inherited;
end;

procedure TSLHColorPicker.Paint;
begin
  PaintParentBack(Canvas);
end;

procedure TSLHColorPicker.Resize;
begin
  inherited;

  if (FSLPicker = nil) or (FHPicker = nil) then
    exit;

  FSLPicker.Width := Width - FHPicker.Width - DIST;
  FSLPicker.Height := Height - 2*VDELTA;

  FHPicker.Left := Width - FHPicker.Width;
  FHPicker.Height := Height;
end;

procedure TSLHColorPicker.SetFocus;
begin
  inherited;
  FSLPicker.SetFocus;
end;

procedure TSLHColorPicker.SetBlue(B: integer);
begin
  SetSelectedColor(RgbToColor(Red, Green, B));
end;

procedure TSLHColorPicker.SetBrightnessMode(bm: TBrightnessMode);
begin
  FSLPicker.BrightnessMode := bm;
  FHPicker.BrightnessMode := bm;
end;

procedure TSLHColorPicker.SetGreen(G: integer);
begin
  SetSelectedColor(RgbToColor(Red, G, Blue));
end;

procedure TSLHColorPicker.SetHCursor(c: TCursor);
begin
  FHCursor := c;
  FHPicker.Cursor := c;
end;

procedure TSLHColorPicker.SetHHint(h: string);
begin
  FHHint := h;
  FHPicker.HintFormat := h;
end;

procedure TSLHColorPicker.SetHMenu(m: TPopupMenu);
begin
  FHMenu := m;
  FHPicker.PopupMenu := m;
end;

procedure TSLHColorPicker.SetHue(H: integer);
begin
  FHPicker.Hue := H;
  FSLPicker.Hue := H;
end;

procedure TSLHColorPicker.SetLum(L: integer);
begin
  FSLPicker.Luminance := L;
end;

procedure TSLHColorPicker.SetMaxHue(H: Integer);
begin
  FSLPicker.MaxHue := H;
  FHPicker.MaxHue := H;
end;

procedure TSLHColorPicker.SetMaxLum(L: Integer);
begin
  FSLPicker.MaxLuminance := L;
  FHPicker.MaxLuminance := L;
end;

procedure TSLHColorPicker.SetMaxSat(S: Integer);
begin
  FSLPicker.MaxSaturation := S;
  FHPicker.MaxSaturation := S;
end;

procedure TSLHColorPicker.SetMaxVal(V: Integer);
begin
  FSLPicker.MaxValue := V;
  FHPicker.MaxValue := V;
end;

procedure TSLHColorPicker.SetRed(R: integer);
begin
  SetSelectedColor(RgbToColor(R, Green, Blue));
end;

procedure TSLHColorPicker.SetSat(S: integer);
begin
  FSLPicker.Saturation := S;
end;

procedure TSLHColorPicker.SetSelectedColor(c: TColor);
begin
  FSelectedColor := c;
  FHPicker.Hue := GetHValue(c);
  FSLPicker.SelectedColor := c;
end;

procedure TSLHColorPicker.SetSLHint(h: string);
begin
  FSLHint := h;
  FSLPicker.HintFormat := h;
end;

procedure TSLHColorPicker.SetSLCursor(c: TCursor);
begin
  FSLCursor := c;
  FSLPicker.Cursor := c;
end;

procedure TSLHColorPicker.SetSLMenu(m: TPopupMenu);
begin
  FSLMenu := m;
  FSLPicker.PopupMenu := m;
end;

procedure TSLHColorPicker.SetVal(V: Integer);
begin
  FSLPicker.Value := V;
end;

procedure TSLHColorPicker.SLPickerChange(Sender: TObject);
begin
  if FSelectedColor = FSLPicker.SelectedColor then
    exit;
  FSelectedColor := FSLPicker.SelectedColor;
  DoChange;
end;


end.
