unit MColorPicker;

interface

{$MODE DELPHI}

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Controls, Graphics, Forms,
  RGBCMYKUtils, mbTrackBarPicker, HTMLColors;

type
  TMColorPicker = class(TmbTrackBarPicker)
  private
    FCyan, FMagenta, FYellow, FBlack: integer;
    function ArrowPosFromMagenta(m: integer): integer;
    function MagentaFromArrowPos(p: integer): integer;
    procedure SetBlack(k: integer);
    procedure SetCyan(c: integer);
    procedure SetMagenta(m: integer);
    procedure SetYellow(y: integer);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
    function GetSelectedValue: integer; override;
    procedure SetSelectedColor(clr: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Cyan: integer read FCyan write SetCyan default 0;
    property Magenta: integer read FMagenta write SetMagenta default 255;
    property Yellow: integer read FYellow write SetYellow default 0;
    property Black: integer read FBlack write SetBlack default 0;
    property SelectedColor default clRed;
    property Layout default lyVertical;
    property HintFormat;
  end;


implementation

uses
  mbUtils;

{TMColorPicker}

constructor TMColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 1;
  FCyan := 0;
  FYellow := 0;
  FBlack := 0;
  SetMagenta(255);
  Layout := lyVertical;
  HintFormat := 'Magenta: %value (selected)';
end;

function TMColorPicker.ArrowPosFromMagenta(m: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round((Width - 12) * m / 255);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((Height - 12) * (255 - m) / 255);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

procedure TMColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetMagenta(FMagenta);
    TBA_MouseMove:
      SetMagenta(MagentaFromArrowPos(FArrowPos));
    TBA_MouseDown:
      SetMagenta(MagentaFromArrowPos(FArrowPos));
    TBA_MouseUp:
      SetMagenta(MagentaFromArrowPos(FArrowPos));
    TBA_WheelUp:
      SetMagenta(FMagenta + Increment);
    TBA_WheelDown:
      SetMagenta(FMagenta - Increment);
    TBA_VKRight:
      SetMagenta(FMagenta + Increment);
    TBA_VKCtrlRight:
      SetMagenta(255);
    TBA_VKLeft:
      SetMagenta(FMagenta - Increment);
    TBA_VKCtrlLeft:
      SetMagenta(0);
    TBA_VKUp:
      SetMagenta(FMagenta + Increment);
    TBA_VKCtrlUp:
      SetMagenta(255);
    TBA_VKDown:
      SetMagenta(FMagenta - Increment);
    TBA_VKCtrlDown:
      SetMagenta(0);
    else
      inherited;
  end;
end;

function TMColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromMagenta(FMagenta);
end;

function TMColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := CMYKtoColor(FCyan, AValue, FYellow, FBlack);
end;

function TMColorPicker.GetSelectedColor: TColor;
begin
  Result := CMYKtoColor(FCyan, FMagenta, FYellow, FBlack);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

function TMColorPicker.GetSelectedValue: integer;
begin
  Result := FMagenta;
end;

function TMColorPicker.MagentaFromArrowPos(p: integer): integer;
var
  m: integer;
begin
  case Layout of
    lyHorizontal:
      m := Round(p * 255 / (Width - 12));
    lyVertical:
      m := Round(255 - p * 255 / (Height - 12));
  end;
  Clamp(m, 0, 255);
  Result := m;
end;

procedure TMColorPicker.SetBlack(k: integer);
begin
  Clamp(k, 0, 255);
  if FBlack <> k then
  begin
    FBlack := k;
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure TMColorPicker.SetCyan(c: integer);
begin
  Clamp(c, 0, 255);
  if FCyan <> c then
  begin
    FCyan := c;
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure TMColorPicker.SetMagenta(m: integer);
begin
  Clamp(m, 0, 255);
  if FMagenta <> m then
  begin
    FMagenta := m;
    FArrowPos := ArrowPosFromMagenta(m);
    Invalidate;
    DoChange;
  end;
end;

procedure TMColorPicker.SetSelectedColor(clr: TColor);
var
  c, m, y, k: integer;
  newGradient: Boolean;
begin
  if WebSafe then
    clr := GetWebSafe(clr);
  if clr = GetSelectedColor then
    exit;

  ColorToCMYK(clr, c, m, y, k);
  newGradient := (c <> FCyan) or (y <> FYellow) or (k <> FBlack);
  FCyan := c;
  FMagenta := m;
  FYellow := y;
  FBlack := k;
  if newGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TMColorPicker.SetYellow(y: integer);
begin
  Clamp(y, 0, 255);
  if FYellow <> y then
  begin
    FYellow := y;
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

end.
