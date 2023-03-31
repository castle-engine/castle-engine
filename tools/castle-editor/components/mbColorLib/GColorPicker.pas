unit GColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbTrackBarPicker;

type
  TGColorPicker = class(TmbTrackBarPicker)
  private
    FRed, FGreen, FBlue: integer;
    function ArrowPosFromGreen(g: integer): integer;
    function GreenFromArrowPos(p: integer): integer;
    procedure SetBlue(b: integer);
    procedure SetGreen(g: integer);
    procedure SetRed(r: integer);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
    function GetSelectedValue: integer; override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Red: integer read FRed write SetRed default 128;
    property Green: integer read FGreen write SetGreen default 255;
    property Blue: integer read FBlue write SetBlue default 128;
    property SelectedColor default clRed;
    property Layout default lyVertical;
    property HintFormat;
  end;


implementation

uses
  mbUtils;

{TGColorPicker}

constructor TGColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 1;
  FRed := 128;
  FBlue := 128;
  SetGreen(255);
  Layout := lyVertical;
  HintFormat := 'Green: %value (selected)';
end;

function TGColorPicker.ArrowPosFromGreen(g: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round((Width - 12) / 255 * g);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((Height - 12) * (255 - g) / 255);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

procedure TGColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetGreen(FGreen);
    TBA_MouseMove:
      SetGreen(GreenFromArrowPos(FArrowPos));
    TBA_MouseDown:
      SetGreen(GreenFromArrowPos(FArrowPos));
    TBA_MouseUp:
      SetGreen(GreenFromArrowPos(FArrowPos));
    TBA_WheelUp:
      SetGreen(FGreen + Increment);
    TBA_WheelDown:
      SetGreen(FGreen - Increment);
    TBA_VKRight:
      SetGreen(FGreen + Increment);
    TBA_VKCtrlRight:
      SetGreen(255);
    TBA_VKLeft:
      SetGreen(FGreen - Increment);
    TBA_VKCtrlLeft:
      SetGreen(0);
    TBA_VKUp:
      SetGreen(FGreen + Increment);
    TBA_VKCtrlUp:
      SetGreen(255);
    TBA_VKDown:
      SetGreen(FGreen - Increment);
    TBA_VKCtrlDown:
      SetGreen(0);
    else
      inherited;
  end;
end;

function TGColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromGreen(FGreen);
end;

// Note: AValue is restricted to the range 0..255 by the size of the trackbar.
function TGColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := RGB(FRed, AValue, FBlue);
end;

function TGColorPicker.GetSelectedColor: TColor;
begin
  Result := RGB(FRed, FGreen, FBlue);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

function TGColorPicker.GetSelectedValue: integer;
begin
  Result := FGreen;
end;

function TGColorPicker.GreenFromArrowPos(p: integer): integer;
var
  g: integer;
begin
  case Layout of
    lyHorizontal:
      g := Round(p * 255 / (Width - 12));
    lyVertical:
      g := Round(255 - p * 255 / (Height - 12));
  end;
  Clamp(g, 0, 255);
  Result := g;
end;

procedure TGColorPicker.SetBlue(b: integer);
begin
  Clamp(b, 0, 255);
  if FBlue <> b then
  begin
    FBlue := b;
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure TGColorPicker.SetGreen(g: integer);
begin
  Clamp(g, 0, 255);
  if FGreen <> g then
  begin
    FGreen := g;
    FArrowPos := ArrowPosFromGreen(g);
    Invalidate;
    DoChange;
  end;
end;

procedure TGColorPicker.SetRed(r: integer);
begin
  Clamp(r, 0, 255);
  if FRed <> r then
  begin
    FRed := r;
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure TGColorPicker.SetSelectedColor(c: TColor);
var
  r, g, b: Integer;
  newGradient: Boolean;
begin
  if WebSafe then
    c := GetWebSafe(c);
  if c = GetSelectedColor then
    exit;

  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  newGradient := (r <> FRed) or (b <> FBlue);
  FGreen := g;
  FBlue := b;
  FRed := r;
  if newGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

end.
