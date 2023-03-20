unit mbColorPreview;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics;

type
  TmbColorPreview = class(TCustomControl)
  private
    FSelColor: TColor;
    FOpacity: integer;
    FOnColorChange: TNotifyEvent;
    FOnOpacityChange: TNotifyEvent;
    FBlockSize: integer;
    FSwatchStyle: boolean;
    function MakeBmp: TBitmap;
    procedure SetSwatchStyle(Value: boolean);
    procedure SetSelColor(c: TColor);
    procedure SetOpacity(o: integer);
    procedure SetBlockSize(s: integer);
  protected
    procedure DoChange;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BlockSize: integer read FBlockSize write SetBlockSize default 6;
    property Color: TColor read FSelColor write SetSelColor default clWhite;
    property Opacity: integer read FOpacity write SetOpacity default 100;
    property SwatchStyle: boolean read FSwatchStyle write SetSwatchStyle default false;
    property Anchors;
    property Align;
    property BorderSpacing;
    property ShowHint;
    property ParentShowHint;
    property Visible;
    property Enabled;
    property PopupMenu;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Constraints;
    property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
    property OnOpacityChange: TNotifyEvent read FOnOpacityChange write FOnOpacityChange;
    property OnContextPopup;
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
    property OnDblClick;
  end;


implementation

uses
  PalUtils;

{ TmbColorPreview }

constructor TmbColorPreview.Create(AOwner: TComponent);
begin
  inherited;
  //ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];

  FSelColor := clWhite;
  SetInitialBounds(0, 0, 68, 32);
  TabStop := false;
  FOpacity := 100;
  FBlockSize := 6;
  FSwatchStyle := false;
  {$IFDEF WINDOWS}
  DoubleBuffered := true;
  {$ENDIF}
end;

procedure TmbColorPreview.DoChange;
begin
  if Assigned(FOnColorChange) then
    FOnColorChange(self);
end;

function TmbColorPreview.MakeBmp: TBitmap;
begin
   Result := TBitmap.Create;
   Result.Width := FBlockSize;
   Result.Height := FBlockSize;
   if (FSelColor = clNone) or (FOpacity = 0) then
     Result.Canvas.Brush.Color := clSilver
   else
     Result.Canvas.Brush.Color := Blend(FSelColor, clSilver, FOpacity);
   Result.Canvas.FillRect(Result.Canvas.ClipRect);
 end;

procedure TmbColorPreview.Paint;
var
  TempBMP, cBMP: TBitmap;
  i, j: integer;
  R: TRect;
  rgn: HRgn;
  c: TColor;
begin
  TempBMP := TBitmap.Create;
  cBMP := nil;
  rgn := 0;
  try
    TempBMP.Width := Width + FBlockSize;
    TempBMP.Height := Height + FBlockSize;
    TempBMP.PixelFormat := pf24bit;
    TempBmp.Canvas.Pen.Color := clBtnShadow;
    TempBmp.Canvas.Brush.Color := FSelColor;
    R := ClientRect;
    with TempBmp.Canvas do
      if (FSelColor <> clNone) and (FOpacity = 100) then
      begin
        if not FSwatchStyle then
          Rectangle(R)
        else
        begin
          Brush.Color := clWindow;
          Rectangle(R);
          InflateRect(R, -1, -1);
          FillRect(R);
          InflateRect(R, 1, 1);
          InflateRect(R, -2, -2);
          Brush.Color := Blend(FSelColor, clBlack, 75);
          FillRect(R);
          InflateRect(R, -1, -1);
          Brush.Color := Blend(FSelColor, clBlack, 87);
          FillRect(R);
          InflateRect(R, -1, -1);
          Brush.Color := FSelColor;
          FillRect(R);
        end;
      end
      else
      begin
        cBMP := MakeBmp;
        if (FSelColor = clNone) or (FOpacity = 0) then
          c := clWhite
        else
          c := Blend(FSelColor, clWhite, FOpacity);
        Brush.Color := c;
        Rectangle(R);
        if FSwatchStyle then
        begin
          InflateRect(R, -1, -1);
          FillRect(R);
          InflateRect(R, 1, 1);
          InflateRect(R, -2, -2);
          Brush.Color := Blend(c, clBlack, 75);
          FillRect(R);
          InflateRect(R, -1, -1);
          Brush.Color := Blend(c, clBlack, 87);
          FillRect(R);
          InflateRect(R, -1, -1);
          Brush.Color := c;
          FillRect(R);
        end;
        InflateRect(R, -1, -1);
        rgn := CreateRectRgnIndirect(R);
        SelectClipRgn(TempBmp.Canvas.Handle, rgn);
        for i := 0 to (Height div FBlockSize) do
          for j := 0 to (Width div FBlockSize) do
          begin
            if i mod 2 = 0 then
            begin
              if j mod 2 > 0 then
                TempBmp.Canvas.Draw(j*FBlockSize, i*FBlockSize, cBMP);
            end
            else
            begin
              if j mod 2 = 0 then
                TempBmp.Canvas.Draw(j*FBlockSize, i*FBlockSize, cBMP);
            end;
          end;
      end;
    Canvas.Draw(0, 0, TempBmp);
  finally
    DeleteObject(rgn);
    cBMP.Free;
    TempBMP.Free;
  end;
end;

procedure TmbColorPreview.SetBlockSize(s: integer);
begin
  if (FBlockSize <> s) and (s > 0) then
  begin
    FBlockSize := s;
    Invalidate;
  end;
end;

procedure TmbColorPreview.SetOpacity(o: integer);
begin
  if FOpacity <> o then
  begin
    FOpacity := o;
    Invalidate;
    if Assigned(FOnOpacityChange) then FOnOpacityChange(Self);
  end;
end;

procedure TmbColorPreview.SetSelColor(c: TColor);
begin
  if c <> FSelColor then
  begin
    FSelColor := c;
    Invalidate;
    DoChange;
  end;
end;

procedure TmbColorPreview.SetSwatchStyle(Value: boolean);
begin
  if FSwatchStyle <> Value then
  begin
    FSwatchStyle := Value;
    Invalidate;
  end;
end;

end.
