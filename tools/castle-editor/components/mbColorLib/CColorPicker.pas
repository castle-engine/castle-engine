unit CColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Controls, Graphics, Forms,
  RGBCMYKUtils, mbTrackBarPicker, HTMLColors;

type
  TCColorPicker = class(TmbTrackBarPicker)
  private
    FCyan, FMagenta, FYellow, FBlack: integer;
    function ArrowPosFromCyan(c: integer): integer;
    function CyanFromArrowPos(p: integer): integer;
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
    property Black: integer read FBlack write SetBlack default 0;
    property Cyan: integer read FCyan write SetCyan default 255;
    property Magenta: integer read FMagenta write SetMagenta default 0;
    property Yellow: integer read FYellow write SetYellow default 0;
    property SelectedColor default clRed;
    property Layout default lyVertical;
    property HintFormat;
  end;


implementation

uses
  mbUtils;

{TCColorPicker}

constructor TCColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 1;
  FMagenta := 0;
  FYellow := 0;
  FBlack := 0;
  SetCyan(255);
  Layout := lyVertical;
  HintFormat := 'Selected cyan value: %value';
end;

function TCColorPicker.ArrowPosFromCyan(c: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round((Width - 12) / 255 * c);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((Height - 12) * (255 - c) / 255);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TCColorPicker.CyanFromArrowPos(p: integer): integer;
var
  c: integer;
begin
  case Layout of
    lyHorizontal:
      c := Round(p * 255 / (Width - 12));
    lyVertical:
      c := Round(255 - p * 255 / (Height - 12));
  end;
  Clamp(c, 0, 255);
  Result := c;
end;

procedure TCColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetCyan(FCyan);
    TBA_MouseMove:
      SetCyan(CyanFromArrowPos(FArrowPos));
    TBA_MouseDown:
      SetCyan(CyanFromArrowPos(FArrowPos));
    TBA_MouseUp:
      SetCyan(CyanFromArrowPos(FArrowPos));
    TBA_WheelUp:
      SetCyan(FCyan + Increment);
    TBA_WheelDown:
      SetCyan(FCyan - Increment);
    TBA_VKRight:
      SetCyan(FCyan + Increment);
    TBA_VKCtrlRight:
      SetCyan(255);
    TBA_VKLeft:
      SetCyan(FCyan - Increment);
    TBA_VKCtrlLeft:
      SetCyan(0);
    TBA_VKUp:
      SetCyan(FCyan + Increment);
    TBA_VKCtrlUp:
      SetCyan(255);
    TBA_VKDown:
      SetCyan(FCyan - Increment);
    TBA_VKCtrlDown:
      SetCyan(0);
    else
      inherited;
  end;
end;

function TCColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromCyan(FCyan);
end;

// Note: AValue is restricted to the range 0..255 by the size of the trackbar.
function TCColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := CMYKtoColor(AValue, FMagenta, FYellow, FBlack);
end;

function TCColorPicker.GetSelectedColor: TColor;
begin
  Result := CMYKtoColor(FCyan, FMagenta, FYellow, FBlack);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

function TCColorPicker.GetSelectedValue: integer;
begin
  Result := FCyan;
end;

procedure TCColorPicker.SetBlack(k: integer);
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

procedure TCColorPicker.SetCyan(C: integer);
begin
  Clamp(c, 0, 255);
  if FCyan <> c then
  begin
    FCyan := c;
    FArrowPos := ArrowPosFromCyan(c);
    Invalidate;
    DoChange;
  end;
end;

procedure TCColorPicker.SetMagenta(m: integer);
begin
  Clamp(m, 0, 255);
  if FMagenta <> m then
  begin
    FMagenta := m;
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure TCColorPicker.SetYellow(y: integer);
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

procedure TCColorPicker.SetSelectedColor(clr: TColor);
var
  c, m, y, k: integer;
  newGradient: Boolean;
begin
  if WebSafe then
    clr := GetWebSafe(clr);
  if clr = GetSelectedColor then
    exit;
  ColorToCMYK(clr, c, m, y, k);
  newGradient := (m <> FMagenta) or (y <> FYellow) or (k <> FBlack);
  FMagenta := m;
  FYellow := y;
  FBlack := k;
  FCyan := c;
  if newGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

end.
