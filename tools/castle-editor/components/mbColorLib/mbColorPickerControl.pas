unit mbColorPickerControl;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls, Graphics, Forms, Themes,
  HTMLColors, mbColorConv, mbBasicPicker;

type
  TMarkerStyle = (msCircle, msSquare, msCross, msCrossCirc);

  { TmbCustomPicker }

  TmbCustomPicker = class(TmbBasicPicker)
  private
    FHintFormat: string;
    FMarkerStyle: TMarkerStyle;
    FWebSafe: boolean;
    procedure SetMarkerStyle(s: TMarkerStyle);
    procedure SetWebSafe(s: boolean);
  protected
    FSelected: TColor;
    mx, my: integer;
    procedure CreateGradient; override;
    function GetHintStr({%H-}X, {%H-}Y: Integer): String; override;
    function GetSelectedColor: TColor; override;
    procedure InternalDrawMarker(X, Y: Integer; C: TColor);
    procedure SetSelectedColor(C: TColor); override;
    procedure WebSafeChanged; dynamic;
    procedure CMGotFocus(var Message: TLMessage); message CM_ENTER;
    procedure CMLostFocus(var Message: TLMessage); message CM_EXIT;
//    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    property MarkerStyle: TMarkerStyle read FMarkerStyle write SetMarkerStyle;
  public
    constructor Create(AOwner: TComponent); override;
    property ColorUnderCursor;
  published
    property HintFormat: string read FHintFormat write FHintFormat;
    property WebSafe: boolean read FWebSafe write SetWebSafe default false;
  end;

  TmbColorPickerControl = class(TmbCustomPicker)
  published
    property Anchors;
    property Align;
    property BorderSpacing;
    property ShowHint;
    property ParentShowHint;
    property Visible;
    property Enabled;
    property PopupMenu;
    property TabOrder;
    property TabStop default true;
    property Color;
    property ParentColor;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Constraints;
    property OnContextPopup;
    property OnGetHintStr;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;
  end;

  TmbHSLVColorPickerControl = class(TmbColorPickerControl)
  private
    FBrightnessMode: TBrightnessMode;
    function GetHue: Integer;
    function GetLum: Integer;
    function GetSat: Integer;
    function GetVal: Integer;
    function GetRed: Integer;
    function GetGreen: Integer;
    function GetBlue: Integer;
    procedure SetHue(h: integer);
    procedure SetLum(L: Integer);
    procedure SetSat(s: integer);
    procedure SetVal(v: integer);
    procedure SetRed(R: Integer);
    procedure SetGreen(G: Integer);
    procedure SetBlue(B: Integer);
  protected
    FHue, FSat, FLum, FVal: Double;
    FMaxHue, FMaxSat, FMaxLum, FMaxVal: Integer;
    procedure ColorToHSLV(c: TColor; var H, S, L, V: Double);
    procedure CorrectCoords(var x, y: integer);
    function HSLVtoColor(H, S, L, V: Double): TColor;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SelectColor({%H-}x, {%H-}y: Integer); virtual;
    procedure SetBrightnessMode(AMode: TBrightnessMode); virtual;
    procedure SetMaxHue(H: Integer); virtual;
    procedure SetMaxLum(L: Integer); virtual;
    procedure SetMaxSat(S: Integer); virtual;
    procedure SetMaxVal(V: Integer); virtual;
    procedure SetRelHue(H: Double); virtual;
    procedure SetRelLum(L: Double); virtual;
    procedure SetRelSat(S: Double); virtual;
    procedure SetRelVal(V: Double); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property RelHue: Double read FHue write SetRelHue;
    property RelSaturation: Double read FSat write SetRelSat;
    property RelLuminance: Double read FLum write SetRelLum;
    property RelValue: Double read FVal write SetRelVal;
    property Red: Integer read GetRed write SetRed;
    property Green: Integer read GetGreen write SetGreen;
    property Blue: Integer read GetBlue write SetBlue;
  published
    property BrightnessMode: TBrightnessMode
      read FBrightnessMode write SetBrightnessMode default bmLuminance;
    property Hue: integer read GetHue write SetHue;
    property Luminance: Integer read GetLum write SetLum;
    property Saturation: integer read GetSat write SetSat;
    property Value: integer read GetVal write SetVal;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 360;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 255;
    property MaxLuminance: Integer read FMaxLum write SetMaxLum default 255;
    property MaxValue: Integer read FMaxVal write SetMaxVal default 255;
  end;

implementation

uses
  Math, IntfGraphics, fpimage,
  PalUtils, SelPropUtils, mbUtils;

constructor TmbCustomPicker.Create(AOwner: TComponent);
begin
  inherited;
  //ControlStyle := ControlStyle + [csOpaque] - [csAcceptsControls];

  TabStop := true;
  mx := 0;
  my := 0;
  FHintFormat := 'Hex #%hex'#10#13'RGB[%r, %g, %b]'#10#13'HSL[%hslH, %hslS, %hslL]'#10#13'HSV[%hsvH, %hsvS, %hsvV]'#10#13'CMYK[%c, %m, %y, %k]'#10#13'L*a*b*[%cieL, %cieA, %cieB]'#10#13'XYZ[%cieX, %cieY, %cieZ]';
  FWebSafe := false;
end;

procedure TmbCustomPicker.CMGotFocus(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TmbCustomPicker.CMLostFocus(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;
    (*
procedure TmbCustomPicker.CMMouseLeave(var Message: TLMessage);
begin
  mx := 0;
  my := 0;
  inherited;
end;
  *)
procedure TmbCustomPicker.CreateGradient;
var
  x, y: Integer;
  col: TColor;
  fpcol: TFPColor;
  intfimg: TLazIntfImage;
  imgHandle, imgMaskHandle: HBitmap;
begin
  if FBufferBmp = nil then
  begin
    FBufferBmp := TBitmap.Create;
//    FBufferBmp.PixelFormat := pf32bit;
  end;
  FBufferBmp.Width := FGradientWidth;
  FBufferBmp.Height := FGradientHeight;

  intfimg := TLazIntfImage.Create(FBufferBmp.Width, FBufferBmp.Height);
  try
    intfImg.LoadFromBitmap(FBufferBmp.Handle, FBufferBmp.MaskHandle);

    for y := 0 to FBufferBmp.Height - 1 do
    begin
      for x := 0 to FBufferBmp.Width - 1 do
      begin
        col := GetGradientColor2D(x, y);
        if WebSafe then
          col := GetWebSafe(col);
        fpcol := TColorToFPColor(col);
        intfImg.Colors[x, y] := fpcol;
      end;
    end;

    intfimg.CreateBitmaps(imgHandle, imgMaskHandle, false);
    FBufferBmp.Handle := imgHandle;
    FBufferBmp.MaskHandle := imgMaskHandle;
  finally
    intfimg.Free;
  end;
end;

function TmbCustomPicker.GetHintStr(X, Y: Integer): String;
begin
  Result := FormatHint(FHintFormat, GetColorUnderCursor);
end;

function TmbCustomPicker.GetSelectedColor: TColor;
begin
  Result := FSelected;  // valid for most descendents
end;

procedure TmbCustomPicker.InternalDrawMarker(X, Y: Integer; C: TColor);
begin
  case MarkerStyle of
    msCircle    : DrawSelCirc(x, y, Canvas);
    msSquare    : DrawSelSquare(x, y, Canvas);
    msCross     : DrawSelCross(x, y, Canvas, c);
    msCrossCirc : DrawSelCrossCirc(x, y, Canvas, c);
  end;
end;

procedure TmbCustomPicker.SetMarkerStyle(s: TMarkerStyle);
begin
  if FMarkerStyle <> s then
  begin
    FMarkerStyle := s;
    Invalidate;
  end;
end;

procedure TmbCustomPicker.SetSelectedColor(C: TColor);
begin
  FSelected := C;
  //handled in descendents
end;

procedure TmbCustomPicker.SetWebSafe(s: boolean);
begin
  if FWebSafe <> s then
  begin
    FWebSafe := s;
    WebSafeChanged;
  end;
end;

procedure TmbCustomPicker.WebSafeChanged;
begin
  CreateGradient;
  Invalidate;
end;


{ TmbHSLVColorPickerControl }

constructor TmbHSLVColorPickerControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBrightnessMode := bmLuminance;
  FMaxHue := 360;
  FMaxSat := 255;
  FMaxVal := 255;
  FMaxLum := 255;
end;

procedure TmbHSLVColorPickerControl.ColorToHSLV(c: TColor;
  var H, S, L, V: Double);
begin
  case FBrightnessMode of
    bmLuminance : ColorToHSL(c, H, S, L);
    bmValue     : ColorToHSV(c, H, S, V);
  end;
end;

procedure TmbHSLVColorPickerControl.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

function TmbHSLVColorPickerControl.GetBlue: Integer;
begin
  Result := GetBValue(GetSelectedColor);
end;

function TmbHSLVColorPickerControl.GetGreen: Integer;
begin
  Result := GetGValue(GetSelectedColor);
end;

function TmbHSLVColorPickerControl.GetHue: Integer;
begin
  Result := Round(FHue * FMaxHue);
end;

function TmbHSLVColorPickerControl.GetLum: Integer;
begin
  Result := Round(FLum * FMaxLum);
end;

function TmbHSLVColorPickerControl.GetRed: Integer;
begin
  Result := GetRValue(GetSelectedColor);
end;

function TmbHSLVColorPickerControl.GetSat: Integer;
begin
  Result := Round(FSat * FMaxSat);
end;

function TmbHSLVColorPickerControl.GetVal: Integer;
begin
  Result := Round(FVal * FMaxVal);
end;

function TmbHSLVColorPickerControl.HSLVtoColor(H, S, L, V: Double): TColor;
begin
  case FBrightnessMode of
    bmLuminance : Result := HSLToColor(H, S, L);
    bmValue     : Result := HSVtoColor(H, S, V);
  end;
  if WebSafe then
    Result := GetWebSafe(Result);
end;

procedure TmbHSLVColorPickerControl.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
  delta: Integer;
begin
  eraseKey := true;
  delta := IfThen(ssCtrl in Shift, 10, 1);

  case Key of
    VK_LEFT  : SelectColor(mx - delta, my);
    VK_RIGHT : SelectColor(mx + delta, my);
    VK_UP    : SelectColor(mx, my - delta);
    VK_DOWN  : SelectColor(mx, my + delta);
    else       eraseKey := false;
  end;

  if eraseKey then
    Key := 0;

  inherited;
end;

procedure TmbHSLVColorPickerControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  if Button = mbLeft then
    SelectColor(x, y);
  SetFocus;
end;

procedure TmbHSLVColorPickerControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  if ssLeft in Shift then
    SelectColor(x, y);
end;

procedure TmbHSLVColorPickerControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  if Button = mbLeft then
    SelectColor(x, y);
end;

procedure TmbHSLVColorPickerControl.SelectColor(x, y: Integer);
begin
end;

procedure TmbHSLVColorPickerControl.SetBlue(B: Integer);
begin
  Clamp(B, 0, 255);
  SetSelectedColor(RgbToColor(Red, Green, B));
end;

procedure TmbHSLVColorPickerControl.SetBrightnessMode(AMode: TBrightnessMode);
var
  c: TColor;
begin
  c := HSLVtoColor(FHue, FSat, FLum, FVal);
  FBrightnessMode := AMode;
  ColorToHSLV(c, FHue, FSat, FLum, FVal);
  CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TmbHSLVColorPickerControl.SetGreen(G: Integer);
begin
  Clamp(G, 0, 255);
  SetSelectedColor(RgbToColor(Red, G, Blue));
end;

procedure TmbHSLVColorPickerControl.SetHue(H: Integer);
begin
  SetRelHue(H / FMaxHue);
end;

procedure TmbHSLVColorPickerControl.SetLum(L: Integer);
begin
  SetRelLum(L / FMaxLum);
end;

procedure TmbHSLVColorPickerControl.SetMaxHue(h: Integer);
begin
  if h = FMaxHue then
    exit;
  FMaxHue := h;
  CreateGradient;
  Invalidate;
end;

procedure TmbHSLVColorPickerControl.SetMaxLum(L: Integer);
begin
  if L = FMaxLum then
    exit;
  FMaxLum := L;
  if BrightnessMode = bmLuminance then begin
    CreateGradient;
    Invalidate;
  end;
end;

procedure TmbHSLVColorPickerControl.SetMaxSat(S: Integer);
begin
  if S = FMaxSat then
    exit;
  FMaxSat := S;
  CreateGradient;
  Invalidate;
end;

procedure TmbHSLVColorPickerControl.SetMaxVal(V: Integer);
begin
  if V = FMaxVal then
    exit;
  FMaxVal := V;
  if BrightnessMode = bmLuminance then
  begin
    CreateGradient;
    Invalidate;
  end;
end;

procedure TmbHSLVColorPickerControl.SetRed(R: Integer);
begin
  Clamp(R, 0, 255);
  SetSelectedColor(RgbToColor(R, Green, Blue));
end;

procedure TmbHSLVColorPickerControl.SetRelHue(H: Double);
begin
  Clamp(H, 0, 1.0);
  if FHue <> H then
  begin
    FHue := H;
    FSelected := HSLVtoColor(FHue, FSat, FLum, FVal);
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure TmbHSLVColorPickerControl.SetRelLum(L: Double);
begin
  Clamp(L, 0, 1.0);
  if FLum <> L then
  begin
    FLum := L;
    FSelected := HSLVtoColor(FHue, FSat, FLum, FVal);
    if BrightnessMode = bmLuminance then begin
      CreateGradient;
      Invalidate;
    end;
    DoChange;
  end;
end;

procedure TmbHSLVColorPickerControl.SetRelSat(S: Double);
begin
  Clamp(S, 0, 1.0);
  if FSat <> S then
  begin
    FSat := S;
    FSelected := HSLVtoColor(FHue, FSat, FLum, FVal);
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure TmbHSLVColorPickerControl.SetRelVal(V: Double);
begin
  Clamp(v, 0, 1.0);
  if FVal <> V then
  begin
    FVal := V;
    if BrightnessMode = bmValue then
    begin
      FSelected := HSLVtoColor(FHue, FSat, FLum, FVal);
      CreateGradient;
      Invalidate;
    end;
    DoChange;
  end;
end;

procedure TmbHSLVColorPickerControl.SetSat(S: Integer);
begin
  SetRelSat(S / FMaxSat);
end;

procedure TmbHSLVColorPickerControl.SetVal(V: Integer);
begin
  SetRelVal(V / FMaxVal);
end;


end.
