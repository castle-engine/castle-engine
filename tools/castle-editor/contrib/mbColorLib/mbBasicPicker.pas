unit mbBasicPicker;

{$mode objfpc}{$H+}

interface

uses
  LMessages, Classes, SysUtils, Graphics, Controls, ExtCtrls, Forms;

type
  THintState = (hsOff, hsWaitingToShow, hsWaitingToHide);
  TGetHintStrEvent = procedure (Sender: TObject; X, Y: Integer; var AText: String) of object;

  { TmbBasicPicker }

  TmbBasicPicker = class(TCustomControl)
  private
    FOnChange: TNotifyEvent;
    FOnGetHintStr: TGetHintStrEvent;
    FLockChange: Integer;
  protected
    FBufferBmp: TBitmap;
    FGradientWidth: Integer;
    FGradientHeight: Integer;
    FHintShown: Boolean;
    procedure CreateGradient; virtual;
    procedure DoChange; virtual;
    function GetColorUnderCursor: TColor; virtual;
    function GetGradientColor({%H-}AValue: Integer): TColor; virtual;
    function GetGradientColor2D({%H-}X, {%H-}Y: Integer): TColor; virtual;
    function GetHintPos(X, Y: Integer): TPoint; virtual;
    function GetHintStr(X, Y: Integer): String; virtual;
    function GetSelectedColor: TColor; virtual; abstract;
    procedure PaintParentBack; virtual; overload;
    procedure PaintParentBack(ACanvas: TCanvas); overload;
    procedure PaintParentBack(ACanvas: TCanvas; ARect: TRect); overload;
    procedure PaintParentBack(ABitmap: TBitmap); overload;
    procedure SetSelectedColor(c: TColor); virtual; abstract;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    property ColorUnderCursor: TColor read GetColorUnderCursor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetHintStr: TGetHintStrEvent read FOnGetHintStr write FOnGetHintStr;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetColorAtPoint(X, Y: Integer): TColor; virtual;
    function GetHexColorAtPoint(X, Y: integer): string;
    function GetHexColorUnderCursor: string; virtual;
    procedure Lock;
    function IsLocked: Boolean;
    procedure Unlock;
  published
    property ParentColor default true;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
  end;

implementation

uses
  LCLIntf,
  HTMLColors, mbUtils;

constructor TmbBasicPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  ControlStyle := ControlStyle - [csOpaque];
  ParentColor := true;
  {$IFDEF WINDOWS}
  DoubleBuffered := true;
  {$ENDIF}
end;

destructor TmbBasicPicker.Destroy;
begin
  FBufferBmp.Free;
  inherited;
end;

procedure TmbBasicPicker.CMHintShow(var Message: TCMHintShow);
var
  cp: TPoint;
  hp: TPoint;
begin
  if GetColorUnderCursor <> clNone then
    with TCMHintShow(Message) do
      if not ShowHint then
        Message.Result := 1
      else
      if Hint <> '' then
        Message.Result := 0
      else
      begin
        cp := HintInfo^.CursorPos;
        hp := GetHintPos(cp.X, cp.Y);
        HintInfo^.ReshowTimeout := 0;  // must be zero!
        HintInfo^.HideTimeout := Application.HintHidePause;
        HintInfo^.HintStr := GetHintStr(cp.X, cp.Y);
        HintInfo^.HintPos := ClientToScreen(Point(hp.X + 16, hp.Y));
        HintInfo^.CursorRect := Rect(cp.X, cp.Y, cp.X+1, cp.Y+1);
        Result := 0;    // 0 means: show hint
      end;

  inherited;
end;

procedure TmbBasicPicker.CMParentColorChanged(var Message: TLMessage);
begin
  {
  if ParentColor then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
    }
  inherited;
end;

procedure TmbBasicPicker.CreateGradient;
begin
  // to be implemented by descendants
end;

procedure TmbBasicPicker.DoChange;
begin
  if (FLockChange = 0) and Assigned(FOnChange) and (ComponentState = []) then
    FOnChange(self);
end;

function TmbBasicPicker.GetColorAtPoint(x, y: integer): TColor;
begin
  Result := Canvas.Pixels[x, y];  // valid for most descendents
end;

function TmbBasicPicker.GetColorUnderCursor: TColor;
var
  P: TPoint;
begin
  P := ScreenToClient(Mouse.CursorPos);
  Result := GetColorAtPoint(P.X, P.Y);
end;

function TmbBasicPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := clNone;
end;

function TmbBasicPicker.GetGradientColor2D(X, Y: Integer): TColor;
begin
  Result := clNone;
end;

function TmbBasicPicker.GetHexColorAtPoint(X, Y: integer): string;
begin
  Result := ColorToHex(GetColorAtPoint(x, y));
end;

function TmbBasicPicker.GetHexColorUnderCursor: string;
begin
  Result := ColorToHex(GetColorUnderCursor);
end;

function TmbBasicPicker.GetHintPos(X, Y: Integer): TPoint;
begin
  Result := Point(X, Y);
end;

function TmbBasicPicker.GetHintStr(X, Y: Integer): String;
begin
  Result := '';
  if Assigned(FOnGetHintStr) then
    FOnGetHintStr(Self, X, Y, Result);
end;

function TmbBasicPicker.IsLocked: Boolean;
begin
  Result := FLockChange > 0;
end;

procedure TmbBasicPicker.Lock;
begin
  inc(FLockChange);
end;

procedure TmbBasicPicker.PaintParentBack;
begin
  PaintParentBack(Canvas);
end;

procedure TmbBasicPicker.PaintParentBack(ABitmap: TBitmap);
begin
  ABitmap.Width := Width;
  ABitmap.Height := Height;
  if Color = clDefault then begin
    ABitmap.Transparent := true;
    ABitmap.TransparentColor := clForm;
    ABitmap.Canvas.Brush.Color := clForm;
  end else
    ABitmap.Canvas.Brush.Color := Color;
  ABitmap.Canvas.FillRect(ABitmap.Canvas.ClipRect);
end;

procedure TmbBasicPicker.PaintParentBack(ACanvas: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Width, Height);
  PaintParentBack(ACanvas, R);
end;

procedure TmbBasicPicker.PaintParentBack(ACanvas: TCanvas; ARect: TRect);
var
  OffScreen: TBitmap;
begin
  Offscreen := TBitmap.Create;
  try
    if Color = clDefault then begin
      Offscreen.Transparent := true;
      Offscreen.TransparentColor := clForm;
    end;
    Offscreen.Width := WidthOfRect(ARect);
    Offscreen.Height := HeightOfRect(ARect);
    PaintParentBack(Offscreen);
    ACanvas.Draw(ARect.Left, ARect.Top, Offscreen);
  finally
    Offscreen.Free;
  end;
end;

procedure TmbBasicPicker.Unlock;
begin
  dec(FLockChange);
end;

end.

