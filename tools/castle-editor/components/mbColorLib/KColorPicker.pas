unit KColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Controls, Graphics, Forms,
  RGBCMYKUtils, mbTrackBarPicker, HTMLColors;

type
  TKColorPicker = class(TmbTrackBarPicker)
  private
    FCyan, FMagenta, FYellow, FBlack: integer;
    function ArrowPosFromBlack(k: integer): integer;
    function BlackFromArrowPos(p: integer): integer;
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
    property Cyan: integer read FCyan write SetCyan default 255;
    property Magenta: integer read FMagenta write SetMagenta default 0;
    property Yellow: integer read FYellow write SetYellow default 0;
    property Black: integer read FBlack write SetBlack default 0;
    property SelectedColor default clRed;
    property Layout default lyVertical;
    property HintFormat;
  end;

implementation

uses
  mbUtils;

{TKColorPicker}

constructor TKColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 1;
  FCyan := 0;
  FMagenta := 0;
  FYellow := 0;
  SetBlack(255);
  Layout := lyVertical;
  HintFormat := 'Black: %value (selected)';
end;

function TKColorPicker.ArrowPosFromBlack(k: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/255)*k);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    k := 255 - k;
    a := Round(((Height - 12)/255)*k);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TKColorPicker.BlackFromArrowPos(p: integer): integer;
var
  k: integer;
begin
  case Layout of
    lyHorizontal:
      k := Round(p * 255 / (Width - 12));
    lyVertical:
      k := Round(255 - p * 255 / (Height - 12));
  end;
  Clamp(k, 0, 255);
  Result := k;
end;

procedure TKColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetBlack(FBlack);
    TBA_MouseMove:
      SetBlack(BlackFromArrowPos(FArrowPos));
    TBA_MouseDown:
      SetBlack(BlackFromArrowPos(FArrowPos));
    TBA_MouseUp:
      SetBlack(BlackFromArrowPos(FArrowPos));
    TBA_WheelUp:
      SetBlack(FBlack + Increment);
    TBA_WheelDown:
      SetBlack(FBlack - Increment);
    TBA_VKRight:
      SetBlack(FBlack + Increment);
    TBA_VKCtrlRight:
      SetBlack(255);
    TBA_VKLeft:
      SetBlack(FBlack - Increment);
    TBA_VKCtrlLeft:
      SetBlack(0);
    TBA_VKUp:
      SetBlack(FBlack + Increment);
    TBA_VKCtrlUp:
      SetBlack(255);
    TBA_VKDown:
      SetBlack(FBlack - Increment);
    TBA_VKCtrlDown:
      SetBlack(0);
    else
      inherited;
  end;
end;

function TKColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromBlack(FBlack);
end;

function TKColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := CMYKtoColor(FCyan, FMagenta, FYellow, AValue);
end;

function TKColorPicker.GetSelectedColor: TColor;
begin
  Result := CMYKtoColor(FCyan, FMagenta, FYellow, FBlack);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

function TKColorPicker.GetSelectedValue: integer;
begin
  Result := FBlack;
end;

procedure TKColorPicker.SetBlack(k: integer);
begin
  Clamp(k, 0, 255);
  if FBlack <> k then
  begin
    FBlack := k;
    FArrowPos := ArrowPosFromBlack(k);
    Invalidate;
    DoChange;
  end;
end;

procedure TKColorPicker.SetCyan(c: integer);
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

procedure TKColorPicker.SetMagenta(m: integer);
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

procedure TKColorPicker.SetSelectedColor(clr: TColor);
var
  c, m, y, k: integer;
  newGradient: Boolean;
begin
  if WebSafe then
    clr := GetWebSafe(clr);
  if clr = GetSelectedColor then
    exit;

  ColorToCMYK(clr, c, m, y, k);
  newGradient := (c <> FCyan) or (m <> FMagenta) or (y <> FYellow);
  FCyan := c;
  FMagenta := m;
  FYellow := y;
  FBlack := k;
  if newGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TKColorPicker.SetYellow(y: integer);
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
