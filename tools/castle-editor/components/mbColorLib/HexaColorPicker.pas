unit HexaColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

//{$I mxs.inc}

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls, Graphics, StdCtrls,
  Forms, Themes, Math,
  HTMLColors, mbBasicPicker;

const
  CustomCell = -2;
  NoCell = -1;

type
  TMarker = (smArrow, smRect);

  TCombEntry = record
    Position: TPoint;
    Color: COLORREF;
    TabIndex: integer;
  end;

  TCombArray = array of TCombEntry;

  TFloatPoint = record
    X, Y: Extended;
  end;

  TRGBrec = record
    Red, Green, Blue: Single;
  end;

  TSelectionMode = (smNone, smColor, smBW, smRamp);

  THexaColorPicker = class(TmbBasicPicker)
  private
    FIncrement: integer;
    FSelectedCombIndex: integer;
    mX, mY: integer;
    FHintFormat: string;
    FUnderCursor: TColor;
    //FOnChange,
    FOnIntensityChange: TNotifyEvent;
    FCurrentColor: TColor;
    FSelectedIndex: Integer;
    FColorCombRect, FBWCombRect, FSliderRect, FCustomColorRect: TRect;
    FCombSize, FLevels: Integer;
    FBWCombs, FColorCombs: TCombArray;
    FCombCorners: array[0..5] of TFloatPoint;
    FCenterColor: TRGBrec;
    FCenterIntensity: Single;
    FSliderWidth: integer;
    FCustomIndex: Integer;  // If FSelectedIndex contains CustomCell then this index shows
                            // which index in the custom area has been selected.
                            // Positive values indicate the color comb and negative values
                            // indicate the B&W combs (complement). This value is offset with
                            // 1 to use index 0 to show no selection.
    FRadius: Integer;
    FSelectionMode: TSelectionMode;
    FSliderVisible: boolean;
    FMarker: TMarker;
    FNewArrowStyle: boolean;
    FIntensityText: string;
    procedure CalculateCombLayout;
    procedure ChangeIntensity(increase: boolean);
    procedure DrawAll;
    procedure DrawComb(ACanvas: TCanvas; X, Y, Size: Integer);
    procedure DrawCombControls(ACanvas: TCanvas);
    procedure EndSelection;
    procedure EnumerateCombs;
    function FindBWArea(X, Y: Integer): Integer;
    function FindColorArea(X, Y: Integer): Integer;
    function GetIntensity: integer;
    function GetNextCombIndex(i: integer): integer;
    function GetPreviousCombIndex(i: integer): integer;
    procedure HandleCustomColors(var Message: TLMMouse);
    function HandleBWArea(const Message: TLMMouse): Boolean;
    function HandleColorComb(const Message: TLMMouse): Boolean;
    function HandleSlider(const Message: TLMMouse): Boolean;
    procedure Initialize;
    function PtInComb(Comb: TCombEntry; P: TPoint; Scale: Integer): Boolean;
    procedure SetIntensity(v: integer);
    procedure SetNewArrowStyle(Value: boolean);
    procedure SetMarker(Value: TMarker);
    procedure SetRadius(r: integer);
    procedure SetSliderVisible(Value: boolean);
    procedure SetSliderWidth(w: integer);
    function SelectAvailableColor(Color: TColor): boolean;
    procedure SelectColor(Color: TColor);
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetSelectedColor(Value: TColor); override;
    procedure CMHintShow(var Message: TLMessage); message CM_HINTSHOW;
    procedure WMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
    procedure WMMouseMove(var Message: TLMMouseMove); message LM_MOUSEMOVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetColorAtPoint(X, Y: integer): TColor; override;
    function GetColorUnderCursor: TColor; override;
    function GetHexColorUnderCursor: string; override;
    function GetHexColorAtPoint(X, Y: integer): string;
    function GetSelectedCombIndex: integer;
    procedure SelectCombIndex(i: integer);
    property ColorUnderCursor: TColor read GetColorUnderCursor;
  published
    property Align;
    property Anchors;
    property HintFormat: string read FHintFormat write FHintFormat;
    property Intensity: integer read GetIntensity write SetIntensity default 100;
    property IntensityIncrement: integer read FIncrement write FIncrement default 1;
    property IntensityText: string read FIntensityText write FIntensityText;
    property NewArrowStyle: boolean read FNewArrowStyle write SetNewArrowStyle default false;
    property SelectedColor: TColor read FCurrentColor write SetSelectedColor default clBlack;
    property SliderVisible: boolean read FSliderVisible write SetSliderVisible default true;
    property SliderWidth: integer read FSliderWidth write SetSliderWidth default 12;
    property SliderMarker: TMarker read FMarker write SetMarker default smArrow;
    property ShowHint default true;
    property TabStop default true;
    property Visible;
    property Enabled;
    property PopupMenu;
    property TabOrder;
    property Color;
    property ParentColor;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Constraints;
    property OnChange; //: TNotifyEvent read FOnChange write FOnChange;
    property OnIntensityChange: TNotifyEvent read FOnIntensityChange write FOnIntensityChange;
    property OnDblClick;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
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

  const
    DefCenterColor: TRGBrec = (Red: 1; Green: 1; Blue: 1);  // White
    DefColors: array[0..5] of TRGBrec = (
      (Red: 1; Green: 0; Blue: 1),     // Magenta
      (Red: 1; Green: 0; Blue: 0),     // Red
      (Red: 1; Green: 1; Blue: 0),     // Yellow
      (Red: 0; Green: 1; Blue: 0),     // Green
      (Red: 0; Green: 1; Blue: 1),     // Cyan
      (Red: 0; Green: 0; Blue: 1)      // Blue
    );
    DefCenter: TFloatPoint = (X: 0; Y: 0);


implementation

uses
  PalUtils, mbUtils;

{ THexaColorPicker }

constructor THexaColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  //ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
  FRadius := 90;
  FSliderWidth := 12;
  DoubleBuffered := true;
  SetInitialBounds(0, 0, 204, 204);
  Constraints.MinHeight := 85;
  Constraints.MinWidth := 93;
  TabStop := true;
  FSelectedCombIndex := 0;
  FHintFormat := 'RGB(%r, %g, %b)'#13'Hex: #%hex';
  ShowHint := True;
  FSliderVisible := true;
  FMarker := smArrow;
  FNewArrowStyle := false;
  Initialize;
  DrawAll;
  FIntensityText := 'Intensity';
  {
  MaxHue := 360;
  MaxLum := 255;
  MaxSat := 255;
  }
end;

destructor THexaColorPicker.Destroy;
begin
  FBWCombs := nil;
  FColorCombs := nil;
  // FBufferBmp.Free;  is already destroyed by ancestor TmbBasicPicker
  inherited;
end;

procedure THexaColorPicker.ChangeIntensity(increase: boolean);
var
  i: integer;
begin
  i := round(FCenterIntensity * 100);
  if increase then
  begin
    Inc(i, FIncrement);
    if i > 100 then i := 100;
    SetIntensity(i);
  end
  else
  begin
    Dec(i, FIncrement);
    if i < 0 then i := 0;
    SetIntensity(i);
  end;
end;

function THexaColorPicker.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    Result := True;
    ChangeIntensity(WheelDelta > 0);
  end;
end;

procedure THexaColorPicker.DrawComb(ACanvas: TCanvas; X, Y: Integer; Size: Integer);
var
  I: Integer;
  P: array[0..5] of TPoint;
begin
  for I := 0 to 5 do
  begin
    P[I].X := Round(FCombCorners[I].X * Size + X);
    P[I].Y := Round(FCombCorners[I].Y * Size + Y);
  end;
  ACanvas.Polygon(P);
end;

procedure THexaColorPicker.DrawCombControls(ACanvas: TCanvas);
var
  I, Index: Integer;
  XOffs, YOffs, Count: Integer;
  OffScreen: TBitmap;
  R: TRect;
begin
  OffScreen := TBitmap.Create;
  try
    OffScreen.Width := Width;
    OffScreen.Height := HeightOfRect(FColorCombRect) + HeightOfRect(FBWCombRect);

    //Parent background
    if Color = clDefault then
    begin
      Offscreen.Transparent := true;
      Offscreen.TransparentColor := clForm;
      Offscreen.Canvas.Brush.Color := clForm
    end else
      OffScreen.Canvas.Brush.Color := Color;
    OffScreen.Canvas.FillRect(OffScreen.Canvas.ClipRect);

    with OffScreen.Canvas do
    begin
      Pen.Style := psClear;

      // draw color combs from FColorCombs array
      XOffs := FRadius + FColorCombRect.Left;
      YOffs := FRadius + FColorCombRect.Top;

      // draw the combs
      for I := 0 to High(FColorCombs) do
      begin
        Brush.Color := FColorCombs[I].Color;
        Pen.Mode := pmCopy; // the pen is set here so there are no gaps between the combs
        Pen.Style := psSolid;
        Pen.Color := FColorCombs[I].Color;
        DrawComb(OffScreen.Canvas, FColorCombs[I].Position.X + XOffs, FColorCombs[I].Position.Y + YOffs, FCombSize);
      end;

      // mark selected comb
      if FCustomIndex > 0 then
      begin
        Index := FCustomIndex - 1;
        FSelectedCombIndex := index;
        Pen.Style := psSolid;
        {
        Pen.Mode := pmXOR;
        Pen.Color := clWhite;
        }
        Pen.Color := HighContrastColor(FColorCombs[Index].Color);
        Pen.Width := 2;
        Brush.Style := bsClear;
        DrawComb(OffScreen.Canvas, FColorCombs[Index].Position.X + XOffs, FColorCombs[Index].Position.Y + YOffs, FCombSize);
        Pen.Style := psClear;
        Pen.Mode := pmCopy;
        Pen.Width := 1;
      end;

      // draw white-to-black combs
      XOffs := FColorCombRect.Left;
      YOffs := FColorCombRect.Bottom - 4;
      // brush is automatically reset to bsSolid
      for I := 0 to High(FBWCombs) do
      begin
        Pen.Mode := pmCopy; // the pen is set here so there are no gaps between the combs
        Pen.Style := psSolid;
        Pen.Color := FBWCombs[I].Color;
        Brush.Color := FBWCombs[I].Color;
        if I in [0, High(FBWCombs)] then
        begin
          if Pen.Color = clWhite then   // "white" needs a border if background is white as well
            Pen.Color := clGray;
          DrawComb(OffScreen.Canvas, FBWCombs[I].Position.X + XOffs, FBWCombs[I].Position.Y + YOffs, 2 * FCombSize)
        end else
          DrawComb(OffScreen.Canvas, FBWCombs[I].Position.X + XOffs, FBWCombs[I].Position.Y + YOffs, FCombSize);
      end;

      // mark selected comb
      if FCustomIndex < 0 then
      begin
        Index := -(FCustomIndex + 1);
        if index < 0 then
          FSelectedCombIndex := Index
        else
          FSelectedCombIndex := -index;
        Pen.Style := psSolid;
        {
        Pen.Mode := pmXOR;
        Pen.Color := clWhite;
        }
        Pen.Mode := pmCopy;
        Pen.Color := HighContrastColor(FBWCombs[Index].Color);
        Pen.Width := 2;
        Brush.Style := bsClear;
        if Index in [0, High(FBWCombs)] then
        begin
          if Index = High(FBWCombs) then begin
            Pen.Color := rgb(254, 254, 254); //clWhite;
            Pen.Mode := pmXOR;
          end;
          if ((FColorCombs[0].Color = Cardinal(clWhite)) and (Index = 0)) or
             ((FColorCombs[0].Color = Cardinal(clBlack)) and (Index = High(FBWCombs)))
          then
            DrawComb(OffScreen.Canvas, FRadius + FColorCombRect.Left, FRadius + FColorCombRect.Top, FCombSize); // mark white or black center
          DrawComb(OffScreen.Canvas, FBWCombs[Index].Position.X + XOffs, FBWCombs[Index].Position.Y + YOffs, 2 * FCombSize);
        end
        else
          DrawComb(OffScreen.Canvas, FBWCombs[Index].Position.X + XOffs, FBWCombs[Index].Position.Y + YOffs, FCombSize);
        Pen.Style := psClear;
        Pen.Mode := pmCopy;
        Pen.Width := 1;
      end;

      // Slider
      if FSliderVisible then
      begin
        // center-color trackbar
        R := FSliderRect;
        R.Right := R.Left + FSliderWidth;
        Pen.Style := psSolid;
        GradientFill(R, clWhite, clBlack, gdVertical);

        // draw marker
        Count := FSliderRect.Bottom - FSliderRect.Top - 1;
        XOffs := FSliderRect.Left + FSliderWidth + 1;
        YOffs := FSliderRect.Top + Round(Count * (1 - FCenterIntensity));;
        Pen.Color := clBlack;
        case FMarker of
          smArrow:
            begin
              if not FNewArrowStyle then
              begin
                Brush.Color := clBlack;
                Polygon([
                  Point(XOffs, YOffs),
                  Point(XOffs + 6, YOffs - 4),
                  Point(XOffs + 6, YOffs + 4)
                ])
              end
              else
              begin
                Brush.Color := clWhite;
                Pen.Color := clBtnShadow;
                Polygon([
                  Point(XOffs, YOffs),
                  Point(XOffs + 4, YOffs - 4),
                  Point(XOffs + 6, YOffs - 4),
                  Point(XOffs + 7, YOffs - 3),
                  Point(XOffs + 7, YOffs + 3),
                  Point(XOffs + 6, YOffs + 4),
                  Point(XOffs + 4, YOffs + 4)]);
              end;
            end;
          smRect:
            begin
              Brush.Style := bsClear;
              Pen.Mode := pmNot;
              Rectangle(XOffs - FSliderWidth - 4, YOffs - 3, XOffs + 2, YOffs + 3);
              Pen.Mode := pmCopy;
              Brush.Style := bsSolid;
            end;
        end;  // case
        Pen.Style := psClear;
      end;
    end;
    ACanvas.Draw(0, 0, OffScreen);
  finally
    Offscreen.Free;
  end;
  EnumerateCombs;
end;

// Looks for a comb at position (X, Y) in the black&white area.
// Result is -1 if nothing could be found else the index of the particular comb
// into FBWCombs.
function THexaColorPicker.FindBWArea(X, Y: Integer): Integer;
var
  I, Scale: Integer;
  Pt: TPoint;
begin
  Result := -1;
  Pt := Point(X - FBWCombRect.Left, Y - FBWCombRect.Top);
  for I := 0 to High(FBWCombs) do
  begin
    if I in [0, High(FBWCombs)] then
      Scale := FCombSize
    else
      Scale := FCombSize div 2;
    if PtInComb(FBWCombs[I], Pt, Scale) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

// Looks for a comb at position (X, Y) in the custom color area.
// Result is -1 if nothing could be found else the index of the particular comb
// into FColorCombs.
function THexaColorPicker.FindColorArea(X, Y: Integer): Integer;
var
  I: Integer;
  Pt: TPoint;
begin
  Result := -1;
  Pt := Point(X - (FRadius + FColorCombRect.Left), Y - (FRadius + FColorCombRect.Top));
  for I := 0 to High(FColorCombs) do
  begin
    if PtInComb(FColorCombs[I], Pt, FCombSize div 2) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function THexaColorPicker.GetIntensity: integer;
begin
  Result := round(FCenterIntensity * 100);
end;

function THexaColorPicker.GetNextCombIndex(i: integer): integer;
begin
  if i = 127 then
    Result := -1
  else
  if i = -15 then
    Result := 1
  else
  if i > 0 then
    Result := i + 1
  else
    Result := i - 1;
end;

function THexaColorPicker.GetPreviousCombIndex(i: integer): integer;
begin
  if i = 1 then
    Result := -15
  else
  if i = -1 then
    Result := 127
  else
  if i > 0 then
    Result := i - 1
  else
    Result := i + 1;
end;

function THexaColorPicker.GetSelectedCombIndex: integer;
begin
  if FSelectedCombIndex < 0 then
    Result := FBWCombs[-FSelectedCombIndex].TabIndex
  else
    Result := FColorCombs[FSelectedCombIndex].TabIndex;
end;

// determines whether the mouse position is within the B&W comb area and acts accordingly
function THexaColorPicker.HandleBWArea(const Message: TLMMouse): Boolean;
var
  Index: Integer;
  Shift: TShiftState;
begin
  Result := PtInRect(FBWCombRect, Point(Message.XPos, Message.YPos)) and (FSelectionMode in [smNone, smBW]);
  if Result then
  begin
    Shift := KeysToShiftState(Message.Keys);
    if ssLeft in Shift then
    begin
      FSelectionMode := smBW;
      Index := FindBWArea(Message.XPos, Message.YPos);
      if Index > -1 then
      begin
        // remove selection comb if it was previously in color comb
        if FCustomIndex > 0 then InvalidateRect(Handle, @FColorCombRect, False);
        if FCustomIndex <> -(Index + 1) then
        begin
          FCustomIndex := -(Index + 1);
          InvalidateRect(Handle, @FBWCombRect, False);
          InvalidateRect(Handle, @FCustomColorRect, False);
          EndSelection;
        end;
      end
      else
        Result := False;
    end;
  end;
end;

// determines whether the mouse position is within the color comb area and acts accordingly
function THexaColorPicker.HandleColorComb(const Message: TLMMouse): Boolean;
var
  Index: Integer;
  Shift: TShiftState;
begin
  Result := PtInRect(FColorCombRect, Point(Message.XPos, Message.YPos)) and (FSelectionMode in [smNone, smColor]);
  if Result then
  begin
    Shift := KeysToShiftState(Message.Keys);
    if ssLeft in Shift then
    begin
      FSelectionMode := smColor;
      Index := FindColorArea(Message.XPos, Message.YPos);
      if Index > -1 then
      begin
        // remove selection comb if it was previously in b&w comb
        if FCustomIndex < 0 then InvalidateRect(Handle, @FBWCombRect, False);
        if FCustomIndex <> (Index + 1) then
        begin
          FCustomIndex := Index + 1;
          InvalidateRect(Handle, @FColorCombRect, False);
          InvalidateRect(Handle, @FCustomColorRect, False);
          EndSelection;
        end;
      end
      else
        Result := False;
    end;
  end;
end;

procedure THexaColorPicker.HandleCustomColors(
  var Message: {$IFDEF FPC}TLMMouse{$ELSE}TWMMouse{$ENDIF});
begin
  if not HandleSlider(Message) then
    if not HandleBWArea(Message) then
      HandleColorComb(Message);
end;

// determines whether the mouse position is within the slider area and acts accordingly
function THexaColorPicker.HandleSlider(const Message: TLMMouse): Boolean;
var
  Shift: TShiftState;
  dY: Integer;
  R: TRect;
begin
  if not FSliderVisible then
  begin
    Result := false;
    Exit;
  end;

  Result :=
    (PtInRect(FSliderRect, Point(Message.XPos, Message.YPos))
      and (FSelectionMode = smNone))
    or
    ((Message.XPos >= FSliderRect.Left) and (Message.XPos <= FSliderRect.Right)
      and (FSelectionMode = smRamp));

  if Result then
  begin
    Shift := KeysToShiftState(Message.Keys);
    if ssLeft in Shift then
    begin
      FSelectionMode := smRamp;
      dY := FSliderRect.Bottom - FSliderRect.Top;
      FCenterIntensity := 1 - (Message.YPos - FSliderRect.Top) / dY;
      if FCenterIntensity < 0 then FCenterIntensity := 0;
      if FCenterIntensity > 1 then FCenterIntensity := 1;
      FCenterColor.Red := DefCenterColor.Red * FCenterIntensity;
      FCenterColor.Green := DefCenterColor.Green * FCenterIntensity;
      FCenterColor.Blue := DefCenterColor.Blue * FCenterIntensity;
      R := FSliderRect;
      Dec(R.Top, 3);
      Inc(R.Bottom, 3);
      Inc(R.Left, 10);
      InvalidateRect(Handle, @R, False);
      FColorCombs := nil;
      InvalidateRect(Handle, @FColorCombRect, False);
      InvalidateRect(Handle, @FCustomColorRect, False);
      CalculateCombLayout;
      EndSelection;
      if Assigned(FOnIntensityChange) then
        FOnIntensityChange(Self);
    end;
  end;
end;

procedure THexaColorPicker.Initialize;
var
  I: Integer;
begin
  FSelectedIndex := NoCell;
  for I := 0 to 5 do
  begin
    FCombCorners[I].X := 0.5 * cos(Pi * (90 - I * 60) / 180);
    FCombCorners[I].Y := 0.5 * sin(Pi * (90 - I * 60) / 180);
  end;
  FLevels := 7;
  FCombSize := Round(FRadius / (FLevels - 1));
  FCenterColor := DefCenterColor;
  FIncrement := 1;
  FCenterIntensity := 1;
end;

procedure THexaColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
begin
  eraseKey := true;
  if ssCtrl in Shift then
    case Key of
      VK_LEFT: SetSelectedColor(clWhite);
      VK_RIGHT: SetSelectedColor(clBlack);
      VK_UP: if FSliderVisible then SetIntensity(100);
      VK_DOWN: if FSliderVisible then SetIntensity(0);
    else
      eraseKey := false;
    end
  else
    case Key of
      VK_LEFT: SelectCombIndex(GetPreviousCombIndex(GetSelectedCombIndex));
      VK_RIGHT: SelectCombIndex(GetNextCombIndex(GetSelectedCombIndex));
      VK_UP: if FSliderVisible then ChangeIntensity(true);
      VK_DOWN: if FSliderVisible then ChangeIntensity(false);
     else
       eraseKey := false;
    end;
  if eraseKey then
    Key := 0;
  inherited;
end;

procedure THexaColorPicker.Paint;
begin
  PaintParentBack(Canvas);
  if FColorCombs = nil then
    CalculateCombLayout;
  DrawCombControls(Canvas);
end;

function THexaColorPicker.PtInComb(Comb: TCombEntry; P: TPoint; Scale: Integer): Boolean;
begin
  Result := (Sqr(Comb.Position.X - P.X) + Sqr(Comb.Position.Y - P.Y)) <= Scale * Scale;
end;

procedure THexaColorPicker.DrawAll;
var
  WinTop: integer;
begin
  WinTop := - FRadius div 8; // use 10 instead of 8 if the top has been cut
  FCombSize := Round(1 + FRadius / (FLevels - 1));
  FColorCombRect := Rect(0, WinTop, 2 * FRadius, 2 * FRadius + WinTop);
  FBWCombRect := Rect(
    FColorCombRect.Left,
    FColorCombRect.Bottom - 4,
    Round(17 * FCombSize * cos(Pi / 6) / 2) {%H-}+ 6 * FCombSize,
    FColorCombRect.Bottom + 2 * FCombSize
  );
  if FSliderVisible then
    FSliderRect := Rect(FColorCombRect.Right, FCombSize, FColorCombRect.Right + 10 + FSliderWidth, FColorCombRect.Bottom - FCombSize)
//    FSliderRect := Rect(FColorCombRect.Right, FColorCombRect.Top, FColorCombRect.Right + 10 + FSliderWidth, FColorCombRect.Bottom)
  else
    FSliderRect := Rect(-1, -1, -1, -1);
end;

// fills arrays with centers and colors for the custom color and black & white combs,
// these arrays are used to quickly draw the combx and do hit tests

function RGBFromFloat(Color: TRGBrec): COLORREF;
begin
  Result := RGB(Round(255 * Color.Red), Round(255 * Color.Green), Round(255 * Color.Blue));
end;

{function TRGBrecFromTColor(Color: TColor): TRGBrec;
begin
 Result.Red := GetRValue(Color)/255;
 Result.Green := GetGValue(Color)/255;
 Result.Blue := GetBValue(Color)/255;
end;}

procedure THexaColorPicker.CalculateCombLayout;

  function GrayFromIntensity(Intensity: Byte): COLORREF;
  begin
    Result := RGB(Intensity, Intensity, Intensity);
  end;

var
  I, J, Level, CurrentIndex, CombCount: Cardinal;
  CurrentColor: TRGBrec;
  CurrentPos: TFloatPoint;
  Scale: Extended;
  // triangle vars
  Pos1, Pos2, dPos1, dPos2, dPos: TFloatPoint;
  Color1, Color2, dColor1, dColor2, dColor: TRGBrec;
begin
  // this ensures the radius and comb size is set correctly
//  HandleNeeded;
  if FLevels < 1 then FLevels := 1;
  // To draw perfectly aligned combs we split the final comb into six triangles (sextants)
  // and calculate each separately. The center comb is stored as first entry in the array
  // and will not considered twice (as with the other shared combs too).
  //
  // The way used here for calculation of the layout seems a bit complicated, but works
  // correctly for all cases (even if the comb corners are rotated).
  // initialization
  CurrentIndex := 0;
  CurrentColor := FCenterColor;
  // number of combs can be calculated by:
  // 1 level: 1 comb (the center)
  // 2 levels: 1 comb + 6 combs
  // 3 levels: 1 comb + 1 * 6 combs + 2 * 6 combs
  // n levels: 1 combs + 1 * 6 combs + 2 * 6 combs + .. + (n-1) * 6 combs
  // this equals to 1 + 6 * (1 + 2 + 3 + .. + (n-1)), by using Gauss' famous formula we get:
  // Count = 1 + 6 * (((n-1) * n) / 2)
  // Because there's always an even number involved (either n or n-1) we can use an integer div
  // instead of a float div here...
  CombCount := 1 + 6 * (((FLevels - 1) * FLevels) div 2);
  SetLength(FColorCombs, CombCount);
  // store center values
  FColorCombs[CurrentIndex].Position := Point(0, 0);
  FColorCombs[CurrentIndex].Color := RGBFromFloat(CurrentColor);
  Inc(CurrentIndex);
  // go out off here if there are not further levels to draw
  if FLevels < 2 then Exit;
  // now go for each sextant, the generic corners have been calculated already at creation
  // time for a comb with diameter 1
  //              ------
  //             /\  1 /\
  //            /  \  /  \
  //           / 2  \/  0 \
  //           -----------
  //           \ 3  /\  5 /
  //            \  /  \  /
  //             \/  4 \/
  //              ------
  for I := 0 to 5 do
  begin
    // initialize triangle corner values
    //
    //                center (always at 0,0)
    //                 /\
    //     dPos1      /  \    dPos2
    //     dColor1   /    \   dColor2
    //              / dPos \
    //             /--------\ (span)
    //            /  dColor  \
    //           /____________\
    //    comb corner 1     comb corner 2
    //
    // Pos1, Pos2, Color1, Color2 are running terms for both sides of the triangle
    // incremented by dPos1/2 and dColor1/2.
    // dPos and dColor are used to interpolate a span between the values just mentioned.
    //
    // The small combs are actually oriented with corner 0 at top (i.e. mirrored at y = x,
    // compared with the values in FCombCorners), we can achieve that by simply exchanging
    // X and Y values.
    Scale := 2 * FRadius * cos(Pi / 6);
    Pos1.X := FCombCorners[I].Y * Scale;
    Pos1.Y := FCombCorners[I].X * Scale;
    Color1 := DefColors[I];
    if I = 5 then
    begin
      Pos2.X := FCombCorners[0].Y * Scale;
      Pos2.Y := FCombCorners[0].X * Scale;
      Color2 := DefColors[0];
    end
    else
    begin
      Pos2.X := FCombCorners[I + 1].Y * Scale;
      Pos2.Y := FCombCorners[I + 1].X * Scale;
      Color2 := DefColors[I + 1];
    end;
    dPos1.X := Pos1.X / (FLevels - 1);
    dPos1.Y := Pos1.Y / (FLevels - 1);
    dPos2.X := Pos2.X / (FLevels - 1);
    dPos2.Y := Pos2.Y / (FLevels - 1);
    dColor1.Red := (Color1.Red - FCenterColor.Red) / (FLevels - 1);
    dColor1.Green := (Color1.Green - FCenterColor.Green) / (FLevels - 1);
    dColor1.Blue := (Color1.Blue - FCenterColor.Blue) / (FLevels - 1);

    dColor2.Red := (Color2.Red - FCenterColor.Red) / (FLevels - 1);
    dColor2.Green := (Color2.Green - FCenterColor.Green) / (FLevels - 1);
    dColor2.Blue := (Color2.Blue - FCenterColor.Blue) / (FLevels - 1);

    Pos1 := DefCenter;
    Pos2 := DefCenter;
    Color1 := FCenterColor;
    Color2 := FCenterColor;

    // Now that we have finished the initialization for this step we'll go
    // through a loop for each level to calculate the spans.
    // We can ignore level 0 (as this is the center we already have determined) as well
    // as the last step of each span (as this is the start value in the next triangle and will
    // be calculated there). We have, though, take them into the calculation of the running terms.
    for Level := 0 to FLevels - 1 do
    begin
      if Level > 0 then
      begin
        // initialize span values
        dPos.X := (Pos2.X - Pos1.X) / Level;
        dPos.Y := (Pos2.Y - Pos1.Y) / Level;
        dColor.Red := (Color2.Red - Color1.Red) / Level;
        dColor.Green := (Color2.Green - Color1.Green) / Level;
        dColor.Blue := (Color2.Blue - Color1.Blue) / Level;
        CurrentPos := Pos1;
        CurrentColor := Color1;
        for J := 0 to Level - 1 do
        begin
          // store current values in the array
          FColorCombs[CurrentIndex].Position.X := Round(CurrentPos.X);
          FColorCombs[CurrentIndex].Position.Y := Round(CurrentPos.Y);
          FColorCombs[CurrentIndex].Color := RGBFromFloat(CurrentColor);
          Inc(CurrentIndex);

          // advance in span
          CurrentPos.X := CurrentPos.X + dPos.X;
          CurrentPos.Y := CurrentPos.Y + dPos.Y;

          CurrentColor.Red := CurrentColor.Red + dColor.Red;
          CurrentColor.Green := CurrentColor.Green + dColor.Green;
          CurrentColor.Blue := CurrentColor.Blue + dColor.Blue;
        end;
      end;
      // advance running terms
      Pos1.X := Pos1.X + dPos1.X;
      Pos1.Y := Pos1.Y + dPos1.Y;
      Pos2.X := Pos2.X + dPos2.X;
      Pos2.Y := Pos2.Y + dPos2.Y;

      Color1.Red := Color1.Red + dColor1.Red;
      Color1.Green := Color1.Green + dColor1.Green;
      Color1.Blue := Color1.Blue + dColor1.Blue;

      Color2.Red := Color2.Red + dColor2.Red;
      Color2.Green := Color2.Green + dColor2.Green;
      Color2.Blue := Color2.Blue + dColor2.Blue;
    end;
  end;

  // second step is to build a list for the black & white area
  // 17 entries from pure white to pure black
  // the first and last are implicitely of double comb size
  SetLength(FBWCombs, 17);
  CurrentIndex := 0;
  FBWCombs[CurrentIndex].Color := GrayFromIntensity(255);
  FBWCombs[CurrentIndex].Position := Point(FCombSize, FCombSize);
  Inc(CurrentIndex);

  CurrentPos.X := 3 * FCombSize;
  CurrentPos.Y := 3 * (FCombSize div 4);
  dPos.X := Round(FCombSize * cos(Pi / 6) / 2);
  dPos.Y := Round(FCombSize * (1 + sin(Pi / 6)) / 2);
  for I := 0 to 14 do
  begin
    FBWCombs[CurrentIndex].Color := GrayFromIntensity((16 - CurrentIndex) * 15);
    if Odd(I) then
      FBWCombs[CurrentIndex].Position := Point(Round(CurrentPos.X + I * dPos.X), Round(CurrentPos.Y + dPos.Y))
    else
      FBWCombs[CurrentIndex].Position := Point(Round(CurrentPos.X + I * dPos.X), Round(CurrentPos.Y));
    Inc(CurrentIndex);
  end;
  FBWCombs[CurrentIndex].Color := 0;
  FBWCombs[CurrentIndex].Position := Point(Round(CurrentPos.X + 16 * dPos.X + FCombSize), FCombSize);
  EnumerateCombs;
end;

// determine hint message and out-of-hint rect
procedure THexaColorPicker.CMHintShow(
  var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF} );
var
  Index: Integer;
  Colors: TCombArray;
  cp: TPoint;
begin
  Colors := nil;
  if (GetColorUnderCursor <> clNone) or PtInRect(FSliderRect, Point(mX, mY)) then
  with TCMHintShow(Message) do
  begin
    if not ShowHint then
      Message.Result := 1
    else
    begin
      with HintInfo^ do
      begin
        // show that we want a hint
        Result := 0;
        cp := CursorPos;
        ReshowTimeout := 0; //1;
        HideTimeout := 5000;
        HintInfo^.CursorRect := Rect(cp.X, cp.Y, cp.X+1, cp.Y+1);
        if PtInRect(FSliderRect, cp) and FSliderVisible then
        begin
          // in case of the intensity slider we show the current intensity
          HintStr := FIntensityText + Format(': %d%%', [Round(100 * FCenterIntensity)]);
          HintPos := ClientToScreen(Point(FSliderRect.Right, CursorPos.Y - 8));
        end
        else
        begin
          Index := -1;
          if PtInRect(FBWCombRect, Point(CursorPos.X, CursorPos.Y)) then
          begin
            // considering black&white area...
            if csLButtonDown in ControlState then
              Index := -(FCustomIndex + 1)
            else
              Index := FindBWArea(CursorPos.X, CursorPos.Y);
            Colors := FBWCombs;
          end
          else
          if PtInRect(FColorCombRect, Point(CursorPos.X, CursorPos.Y)) then
          begin
            // considering color comb area...
            if csLButtonDown in ControlState then
              Index := FCustomIndex - 1
            else
              Index := FindColorArea(CursorPos.X, CursorPos.Y);
            Colors := FColorCombs;
          end;
          if (Index > -1) and (Colors <> nil) then
            HintStr := FormatHint(FHintFormat, Colors[Index].Color);
        end;
      end;
    end;
  end;
end;

procedure THexaColorPicker.SetSelectedColor(Value: TColor);
begin
  FCurrentColor := Value;
  SelectColor(Value);
  Invalidate;
end;

procedure THexaColorPicker.EndSelection;
begin
  if FCustomIndex < 0 then
    SetSelectedColor(FBWCombs[-(FCustomIndex + 1)].Color)
  else
  if FCustomIndex > 0 then
    SetSelectedColor(FColorCombs[FCustomIndex - 1].Color)
  else
    SetSelectedColor(clNone);
end;

function THexaColorPicker.GetColorUnderCursor: TColor;
begin
  Result := FUnderCursor;
end;

function THexaColorPicker.GetColorAtPoint(X, Y: integer): TColor;
var
  Index: Integer;
  Colors: TCombArray;
begin
  Colors := nil;
  Index := -1;
  if PtInRect(FBWCombRect, Point(X, Y)) then
  begin
    Index := FindBWArea(X, Y);
    Colors := FBWCombs;
  end
  else
  if PtInRect(FColorCombRect, Point(X, Y)) then
  begin
    Index := FindColorArea(X, Y);
    Colors := FColorCombs;
  end;
  if (Index > -1) and (Colors <> nil) then
    Result := Colors[Index].Color
  else
    Result := clNone;
end;

function THexaColorPicker.GetHexColorUnderCursor: string;
begin
  Result := ColorToHex(GetColorUnderCursor);
end;

function THexaColorPicker.GetHexColorAtPoint(X, Y: integer): string;
begin
  Result := ColorToHex(GetColorAtPoint(X, Y));
end;

procedure THexaColorPicker.EnumerateCombs;
var
  i, k: integer;
begin
  k := 0;
  if FBWCombs <> nil then
    for i := 1 to High(FBWCombs) do
    begin
      case i of
      // b & w comb indices
        1: k := -1;
        2: k := -9;
        3: k := -2;
        4: k := -10;
        5: k := -3;
        6: k := -11;
        7: k := -4;
        8: k := -12;
        9: k := -5;
       10: k := -13;
       11: k := -6;
       12: k := -14;
       13: k := -7;
       14: k := -15;
       15: k := -8;
       // big black comb index (match center comb)
       16: K := 64;
      end;
      FBWCombs[i].TabIndex := k;
    end;
  if FColorCombs <> nil then
    for i := 0 to High(FColorCombs) do
    begin
      case i of
        // center comb index
        0: k := 64;
        // color comb indices
        1: k := 65;
        2: k := 66;
        3: k := 78;
        4: k := 67;
        5: k := 79;
        6: k := 90;
        7: k := 68;
        8: k := 80;
        9: k := 91;
       10: k := 101;
       11: k := 69;
       12: k := 81;
       13: k := 92;
       14: k := 102;
       15: k := 111;
       16: k := 70;
       17: k := 82;
       18: k := 93;
       19: k := 103;
       20: k := 112;
       21: k := 120;
       22: k := 77;
       23: k := 89;
       24: k := 88;
       25: k := 100;
       26: k := 99;
       27: k := 98;
       28: k := 110;
       29: k := 109;
       30: k := 108;
       31: k := 107;
       32: k := 119;
       33: k := 118;
       34: k := 117;
       35: k := 116;
       36: k := 115;
       37: k := 127;
       38: k := 126;
       39: k := 125;
       40: k := 124;
       41: k := 123;
       42: k := 122;
       43: k := 76;
       44: k := 87;
       45: k := 75;
       46: k := 97;
       47: k := 86;
       48: k := 74;
       49: k := 106;
       50: k := 96;
       51: k := 85;
       52: k := 73;
       53: k := 114;
       54: k := 105;
       55: k := 95;
       56: k := 84;
       57: k := 72;
       58: k := 121;
       59: k := 113;
       60: k := 104;
       61: k := 94;
       62: k := 83;
       63: k := 71;
       64: k := 63;
       65: k := 62;
       66: k := 50;
       67: k := 61;
       68: k := 49;
       69: k := 38;
       70: k := 60;
       71: k := 48;
       72: k := 37;
       73: k := 27;
       74: k := 59;
       75: k := 47;
       76: k := 36;
       77: k := 26;
       78: k := 17;
       79: k := 58;
       80: k := 46;
       81: k := 35;
       82: k := 25;
       83: k := 16;
       84: k := 8;
       85: k := 51;
       86: k := 39;
       87: k := 40;
       88: k := 28;
       89: k := 29;
       90: k := 30;
       91: k := 18;
       92: k := 19;
       93: k := 20;
       94: k := 21;
       95: k := 9;
       96: k := 10;
       97: k := 11;
       98: k := 12;
       99: k := 13;
      100: k := 1;
      101: k := 2;
      102: k := 3;
      103: k := 4;
      104: k := 5;
      105: k := 6;
      106: k := 52;
      107: k := 41;
      108: k := 53;
      109: k := 31;
      110: k := 42;
      111: k := 54;
      112: k := 22;
      113: k := 32;
      114: k := 43;
      115: k := 55;
      116: k := 14;
      117: k := 23;
      118: k := 33;
      119: k := 44;
      120: k := 56;
      121: k := 7;
      122: k := 15;
      123: k := 24;
      124: k := 34;
      125: k := 45;
      126: k := 57;
    end;
    FColorCombs[i].TabIndex := k;
  end;
end;

procedure THexaColorPicker.SelectCombIndex(i: integer);
var
  j: integer;
begin
  if i > 0 then
  begin
    if FColorCombs <> nil then
    for j := 0 to High(FColorCombs) do
    begin
      if FColorCombs[j].TabIndex = i then
      begin
        SetSelectedColor(FColorCombs[j].Color);
        Break;
      end;
    end;
  end
  else
  if FBWCombs <> nil then
    for j := 1 to High(FBWCombs) - 1 do
    begin
      if FBWCombs[j].TabIndex = i then
      begin
        SetSelectedColor(FBWCombs[j].Color);
        Break;
      end;
    end;
end;

procedure THexaColorPicker.Resize;
var
  rw, rh: integer;
begin
  if (Width >= 93) and (Height >= 85) then
  begin
    if FSliderVisible then
      rw := Round((Width - 10 - FSliderWidth)/2)
    else
      rw := Round(Width/2 - 5);
    rh := Round((24/53)*(Height - 6));
    SetRadius(Min(rw, rh));
  end;
  inherited;
end;

function THexaColorPicker.SelectAvailableColor(Color: TColor): boolean;
var
  I: integer;
  C: COLORREF;
  found: Boolean;
begin
  found := False;
  Result := false;
  C := ColorToRGB(Color);
  if FColorCombs = nil then CalculateCombLayout;
  FCustomIndex := 0;
  FSelectedIndex := NoCell;
  for I := 0 to High(FBWCombs) do
    if FBWCombs[I].Color = C then
    begin
      FSelectedIndex := CustomCell;
      FCustomIndex := -(I + 1);
      found := True;
      Result := true;
      Break;
    end;
  if not found then
    for I := 0 to High(FColorCombs) do
      if FColorCombs[I].Color = C then
      begin
        FSelectedIndex := CustomCell;
        FCustomIndex := I + 1;
        Result := true;
        Break;
      end;
end;

procedure THexaColorPicker.SelectColor(Color: TColor);
begin
  SelectAvailableColor(Color);
  Invalidate;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure THexaColorPicker.SetIntensity(v: integer);
var
  R: TRect;
begin
  FCenterIntensity := EnsureRange(v/100, 0, 1);
  FCenterColor.Red := DefCenterColor.Red * FCenterIntensity;
  FCenterColor.Green := DefCenterColor.Green * FCenterIntensity;
  FCenterColor.Blue := DefCenterColor.Blue * FCenterIntensity;
  R := FSliderRect;
  Dec(R.Top, 3);
  Inc(R.Bottom, 3);
  Inc(R.Left, 10);
  InvalidateRect(Handle, @R, False);
  FColorCombs := nil;
  InvalidateRect(Handle, @FColorCombRect, False);
  InvalidateRect(Handle, @FCustomColorRect, False);
  CalculateCombLayout;
  EndSelection;
  if Assigned(FOnIntensityChange) then
    FOnIntensityChange(Self);
end;

procedure THexaColorPicker.SetMarker(Value: TMarker);
begin
  if FMarker <> Value then
  begin
    FMarker := Value;
    DrawAll;
    CalculateCombLayout;
    Invalidate;
  end;
end;

procedure THexaColorPicker.SetNewArrowStyle(Value: boolean);
begin
  if FNewArrowStyle <> Value then
  begin
    FNewArrowStyle := Value;
    DrawAll;
    CalculateCombLayout;
    Invalidate;
  end;
end;

procedure THexaColorPicker.SetRadius(r: integer);
begin
  if Parent = nil then
    exit;
  FRadius := r;
  DrawAll;
  CalculateCombLayout;
  Invalidate;
end;

procedure THexaColorPicker.SetSliderVisible(Value: boolean);
begin
  if FSliderVisible <> Value then
  begin
    FSliderVisible := Value;
    DrawAll;
    CalculateCombLayout;
    Invalidate;
  end;
end;

procedure THexaColorPicker.SetSliderWidth(w: integer);
begin
  if (FSliderWidth <> w) and FSliderVisible then
  begin
    FSliderWidth := w;
    DrawAll;
    Width := FSliderRect.Right + 2;
    CalculateCombLayout;
    Invalidate;
  end;
end;

procedure THexaColorPicker.WMLButtonDown(
  var Message: {$IFDEF FPC}TLMLButtonDown{$ELSE}TWMLButtonDown{$ENDIF} );
begin
  inherited;
  SetFocus; // needed so the key events work
  if PtInRect(ClientRect, Point(Message.XPos, Message.YPos)) then
    HandleCustomColors(Message);
end;

procedure THexaColorPicker.WMLButtonUp(
  var Message: {$IFDEF FPC}TLMLButtonUp{$ELSE}TWMLButtonUp{$ENDIF} );
var
  LastMode: TSelectionMode;
begin
  inherited;
  LastMode := FSelectionMode;
  FSelectionMode := smNone;
  if (FSelectedIndex = CustomCell) and (FCustomIndex <> 0) then
  begin
    if ((FSelectedIndex = CustomCell) and (LastMode in [smColor, smBW])) or
       (FSelectedIndex <> NoCell) and (FSelectedIndex <> CustomCell)
    then
      EndSelection
  end;
end;

procedure THexaColorPicker.WMMouseMove(
  var Message: {$IFDEF FPC}TLMMouseMove{$ELSE}TWMMouseMove{$ENDIF} );
var
  Shift: TShiftState;
  Index: Integer;
  Colors: TCombArray;
begin
  inherited;
  mX := Message.XPos;
  mY := Message.YPos;
  //get color under cursor
  Colors := nil;
  FUnderCursor := clNone;
  if PtInRect(FBWCombRect, Point(Message.XPos, Message.YPos)) then
  begin
    Index := FindBWArea(Message.XPos, Message.YPos);
    Colors := FBWCombs;
    if (Index > -1) and (Colors <> nil) then
      FUnderCursor := Colors[Index].Color;
  end
  else
  if PtInRect(FColorCombRect, Point(Message.XPos, Message.YPos)) then
  begin
    Index := FindColorArea(Message.XPos, Message.YPos);
    Colors := FColorCombs;
    if (Index > -1) and (Colors <> nil) then
      FUnderCursor := Colors[Index].Color;
   end
   else
     FUnderCursor := clNone;
  // further process message
  Shift := KeysToShiftState(Message.Keys);
  if ssLeft in Shift then
    HandleCustomColors(Message);
end;

end.
