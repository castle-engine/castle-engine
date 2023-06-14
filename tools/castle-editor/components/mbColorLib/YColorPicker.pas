unit YColorPicker;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  RGBCMYKUtils, mbTrackBarPicker, HTMLColors;

type
  TYColorPicker = class(TmbTrackBarPicker)
  private
    FYellow, FMagenta, FCyan, FBlack: integer;
    function ArrowPosFromYellow(y: integer): integer;
    function YellowFromArrowPos(p: integer): integer;
    procedure SetYellow(y: integer);
    procedure SetMagenta(m: integer);
    procedure SetCyan(c: integer);
    procedure SetBlack(k: integer);
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
    property Yellow: integer read FYellow write SetYellow default 255;
    property Magenta: integer read FMagenta write SetMagenta default 0;
    property Cyan: integer read FCyan write SetCyan default 0;
    property Black: integer read FBlack write SetBlack default 0;
    property SelectedColor default clRed;
    property Layout default lyVertical;
    property HintFormat;
  end;

implementation

uses
  mbUtils;

{TYColorPicker}

constructor TYColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 255;
  FGradientHeight := 1;
  FMagenta := 0;
  FCyan := 0;
  FBlack := 0;
  SetYellow(255);
  Layout := lyVertical;
  HintFormat := 'Yellow: %value (selected)';
end;

function TYColorPicker.ArrowPosFromYellow(y: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/255)*y);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    y := 255 - y;
    a := Round(((Height - 12)/255)*y);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

procedure TYColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetYellow(FYellow);
    TBA_MouseMove:
      SetYellow(YellowFromArrowPos(FArrowPos));
    TBA_MouseDown:
      SetYellow(YellowFromArrowPos(FArrowPos));
    TBA_MouseUp:
      SetYellow(YellowFromArrowPos(FArrowPos));
    TBA_WheelUp:
      SetYellow(FYellow + Increment);
    TBA_WheelDown:
      SetYellow(FYellow - Increment);
    TBA_VKRight:
      SetYellow(FYellow + Increment);
    TBA_VKCtrlRight:
      SetYellow(255);
    TBA_VKLeft:
      SetYellow(FYellow - Increment);
    TBA_VKCtrlLeft:
      SetYellow(0);
    TBA_VKUp:
      SetYellow(FYellow + Increment);
    TBA_VKCtrlUp:
      SetYellow(255);
    TBA_VKDown:
      SetYellow(FYellow - Increment);
    TBA_VKCtrlDown:
      SetYellow(0);
    else
      inherited;
  end;
end;

function TYColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromYellow(FYellow);
end;

function TYColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := CMYKtoColor(FCyan, FMagenta, AValue, FBlack);
end;

function TYColorPicker.GetSelectedColor: TColor;
begin
  Result := CMYKtoColor(FCyan, FMagenta, FYellow, FBlack);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

function TYColorPicker.GetSelectedValue: integer;
begin
  Result := FYellow;
end;

procedure TYColorPicker.SetBlack(k: integer);
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

procedure TYColorPicker.SetCyan(c: integer);
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

procedure TYColorPicker.SetMagenta(m: integer);
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

function TYColorPicker.YellowFromArrowPos(p: integer): integer;
var
 r: integer;
begin
  if Layout = lyHorizontal then
    r := Round(p/((Width - 12)/255))
  else
    r := Round(255 - p/((Height - 12)/255));
  Clamp(r, 0, 255);
  Result := r;
end;

procedure TYColorPicker.SetSelectedColor(clr: TColor);
var
  c, m, y, k: integer;
  newGradient: Boolean;
begin
  if WebSafe then
    clr := GetWebSafe(clr);
  if clr = GetSelectedColor then
    exit;

  ColorToCMYK(clr, c, m, y, k);
  newGradient := (c <> FCyan) or (m <> FMagenta) or (k <> FBlack);
  FCyan := c;
  FMagenta := m;
  FYellow := y;
  FBlack := k;
  if newGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TYColorPicker.SetYellow(y: integer);
begin
  Clamp(y, 0, 255);
  if FYellow <> y then
  begin
    FYellow := y;
    FArrowPos := ArrowPosFromYellow(y);
    Invalidate;
    DoChange;
  end;
end;

end.
