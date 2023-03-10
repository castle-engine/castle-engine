{
  Copyright 2023 Freedomax.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
unit CastleInternalRegionDialog;

{$I castleconf.inc}

interface

uses
  Generics.Collections, Contnrs,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls, ComCtrls, CastleVectors, CastleControl,
  CastleGLImages,
  castletransform, CastleKeysMouse,
  castlecontrols, CastleRectangles, CastleLCLUtils, CastleGLUtils, CastleColors;

type
  TDesignMode = (ModeRegion, ModeBorder);

  TRegionDesignDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    CastleControl1: TCastleControl;
    ColorDialog1: TColorDialog;
    StatusBar1: TStatusBar;

    procedure CastleControl1Motion(Sender: TObject; const Event: TInputMotion);
    procedure CastleControl1Press(Sender: TObject; const Event: TInputPressRelease);
    procedure CastleControl1Release(Sender: TObject;
      const Event: TInputPressRelease);
    procedure CastleControl1Render(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  strict private
    FBorder: TBorder;
  type
    TArrayScreenPoints = array[0..1] of TVector2;
    TArrayImagePoints = array[0..1] of TVector2Integer;

    TDirectionEnable = (EnableX, EnableY);
    TDirectionEnables = set of TDirectionEnable;

    TMovingRec = record
      Moving: boolean;
      StartTranslation: TVector2;
      StartMousePoint: TVector2;
    end;

    TControlPointRec = record
      Adjusting: boolean;
      Index: integer;
      DirectionEnables: TDirectionEnables;
      Points: TArrayImagePoints;
      function AdditionalPoints(const byX: boolean = True): TArrayImagePoints;
    end;

  const
    CircleRads = 8;
  var
    FImage: TDrawableImage;
    FRegion, FSourceRegion: TRegion;
    FScale: single;
    FTranslation: TVector2;
    FDesignMode: TDesignMode;
    FMovingImageRec: TMovingRec;
    FControlPointRec: TControlPointRec;
    procedure InitControlPoints;
    function ScreenRegionRectangle: TFloatRectangle;
    function ScreenImageFullRectangle: TFloatRectangle;
    function ImageRegionRectangle: TRectangle;

    function ScreenRegionPoints: TArrayScreenPoints;
    function ScreenAdditionalPoints: TArrayScreenPoints;

    procedure SetImage(const AImage: TDrawableImage);
    function ImageWidth(): cardinal;
    function ImageHeight(): cardinal;

    { Restrict the region within the image rectangle. }
    procedure FixControlPoints;

    procedure UpdateCursorPosInfo(const MousePoint: TVector2);
    procedure UpdateCursorShape(const MousePoint: TVector2);
    function HitTest(const AControlPoint: TVector2Integer;
      const AMousePoint: TVector2): TDirectionEnables;
    function GetDirectionEnables(const MousePoint: TVector2): TDirectionEnables;

    procedure Changed;
  protected
    function ScreenToImage(APoint: TVector2; bRound: boolean = True): TVector2Integer;
    function ImageToScreen(const APoint: TVector2Integer): TVector2; overload;
    function ImageToScreen(const APoints: TArrayImagePoints): TArrayScreenPoints;
      overload;

    function RegionFromBorder(ABorder: TBorder): TRegion;

    property Image: TDrawableImage read FImage write SetImage;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyChange;
    procedure ApplyToBorder(ABorder: TBorder);

    procedure Load(AImage: TDrawableImage; const ARegion: TRegion); overload;
    procedure Load(AImage: TDrawableImage; const ASourceRegion: TRegion;
      ABorder: TBorder); overload;


    property Region: TRegion read FRegion;
    property Border: TBorder read FBorder;
    property DesignMode: TDesignMode read FDesignMode;
  end;


implementation

uses Math, CastleRenderOptions, CastleUtils;

{$R *.lfm}

{ TRegionDesignDialog.TControlPointRec }

function TRegionDesignDialog.TControlPointRec.AdditionalPoints(
  const byX: boolean = True): TArrayImagePoints;
begin
  if byX then
  begin
    Result[0] := Vector2Integer(Points[0].X, Points[1].Y);
    Result[1] := Vector2Integer(Points[1].X, Points[0].Y);
  end
  else
  begin
    Result[0] := Vector2Integer(Points[1].X, Points[0].Y);
    Result[1] := Vector2Integer(Points[0].X, Points[1].Y);
  end;

end;

{ TRegionDesignDialog }

constructor TRegionDesignDialog.Create(AOwner: TComponent);
begin
  inherited;

  FScale := 1;
  FSourceRegion := TFloatRectangle.Empty;
  FRegion := TFloatRectangle.Empty;
  FImage := TDrawableImage.Create(nil, False, False);

end;

destructor TRegionDesignDialog.Destroy;
begin
  FreeAndNil(FImage);

  inherited;
end;

procedure TRegionDesignDialog.ApplyChange;
var
  rc: TRectangle;
begin
  rc := ImageRegionRectangle;
  FRegion := FloatRectangle(rc.Left, rc.Bottom, rc.Width, rc.Height);
end;

procedure TRegionDesignDialog.Load(AImage: TDrawableImage; const ARegion: TRegion);
begin
  Image := AImage;

  FSourceRegion := TFloatRectangle.Empty;
  FRegion := ARegion;
  FDesignMode := TDesignMode.ModeRegion;

  InitControlPoints;
end;

procedure TRegionDesignDialog.Load(AImage: TDrawableImage;
  const ASourceRegion: TRegion; ABorder: TBorder);
begin
  Image := AImage;

  FBorder := ABorder;
  FSourceRegion := ASourceRegion;
  FRegion := RegionFromBorder(FBorder);
  FDesignMode := TDesignMode.ModeBorder;

  InitControlPoints;
end;

procedure TRegionDesignDialog.ApplyToBorder(ABorder: TBorder);
begin
  if (FRegion.Left = FRegion.Bottom) and (FRegion.Left = ImageWidth - FRegion.Right) and
    (FRegion.Left = ImageHeight - FRegion.Top) then
  begin
    ABorder.AllSides := FRegion.Left;
    ABorder.Left := 0;
    ABorder.Bottom := 0;
    ABorder.Right := 0;
    ABorder.Top := 0;
  end
  else
  begin
    ABorder.AllSides := 0;
    ABorder.Left := FRegion.Left;
    ABorder.Bottom := FRegion.Bottom;
    ABorder.Right := ImageWidth - FRegion.Right;
    ABorder.Top := ImageHeight - FRegion.Top;
  end;
end;

function TRegionDesignDialog.RegionFromBorder(ABorder: TBorder): TRegion;
begin
  Result := FloatRectangle(FBorder.TotalLeft, FBorder.TotalBottom,
    ImageWidth - FBorder.TotalWidth, ImageHeight - FBorder.TotalHeight);
end;

function TRegionDesignDialog.ScreenRegionRectangle: TFloatRectangle;
var
  Points: TArrayScreenPoints;
  vLeft, vBottom, vWidth, vHeight: single;
begin
  Points := ScreenRegionPoints;

  vLeft := Min(Points[0].X, Points[1].X);
  vBottom := Min(Points[0].Y, Points[1].Y);
  vWidth := ABS(Points[0].X - Points[1].X);
  vHeight := ABS(Points[0].Y - Points[1].Y);

  Result := FloatRectangle(vLeft, vBottom, vWidth, vHeight);
end;

function TRegionDesignDialog.ScreenImageFullRectangle: TFloatRectangle;
var
  Points: TArrayScreenPoints;
  { FPC3.2.2 not support "Array Of Const" => "const array" }
  imgPoints: TArrayImagePoints;
begin
  imgPoints[0] := Vector2Integer(0, 0);
  imgPoints[1] := Vector2Integer(ImageWidth, ImageHeight);
  Points := ImageToScreen(imgPoints);

  Result.LeftBottom := Points[0];
  Result.Width := Points[1].X - Points[0].X;
  Result.Height := Points[1].Y - Points[0].Y;
end;

function TRegionDesignDialog.ImageRegionRectangle: TRectangle;
begin
  Result.Left := Min(FControlPointRec.Points[0].X, FControlPointRec.Points[1].X);
  Result.Bottom := Min(FControlPointRec.Points[0].Y, FControlPointRec.Points[1].Y);
  Result.Width := ABS(FControlPointRec.Points[0].X - FControlPointRec.Points[1].X);
  Result.Height := ABS(FControlPointRec.Points[0].Y - FControlPointRec.Points[1].Y);
end;

procedure TRegionDesignDialog.CastleControl1Render(Sender: TObject);

  function ScreenRect: TFloatRectangle;
  begin
    Result := FloatRectangle(0, 0, ImageWidth * FScale, ImageHeight *
      FScale).Translate(FTranslation);
  end;

  function ImageRect: TFloatRectangle;
  begin
    if FSourceRegion.IsEmpty then
      Result := FloatRectangle(0, 0, ImageWidth, ImageHeight)
    else
      Result := FloatRectangle(Floor(FSourceRegion.Left),
        Floor(FSourceRegion.Bottom), ImageWidth, ImageHeight);
  end;

  procedure RenderPoints(const Points: TArrayScreenPoints);
  var
    Point: TVector2;
  begin
    for Point in Points do
      DrawCircle(Point, CircleRads, CircleRads, Vector4(0.864, 0.17, 0.03, 1));
  end;

  procedure RenderImage;
  begin
    FImage.Draw(ScreenRect, ImageRect);
  end;

  procedure RenderLine(const Points: TArrayImagePoints;
  const LineColor: TCastleColor; const LineWidth: single);
  begin
    DrawPrimitive2D(pmLines,
      ImageToScreen(Points),
      LineColor, bsSrcAlpha, bdOneMinusSrcAlpha, False, LineWidth);
  end;

  procedure RenderControlPoints;
  var
    vRect: TFloatRectangle;
    vImageRegionRect: TRectangle;
    LineColor: TCastleColor;
    Points: TArrayImagePoints;
  const
    LineWidth: single = 2;
  begin
    vRect := ScreenRegionRectangle;
    LineColor := CastleColors.Red;

    case FDesignMode of
      TDesignMode.ModeRegion:
      begin
        DrawRectangle(vRect,
          Vector4(1, 1, 1, 0.4));
        DrawRectangleOutline(vRect,
          LineColor, 2);
        RenderPoints(ScreenRegionPoints);
        RenderPoints(ScreenAdditionalPoints);
      end;
      TDesignMode.ModeBorder:
      begin
        vImageRegionRect := ImageRegionRectangle;

        Points[0] := Vector2Integer(vImageRegionRect.Left, 0);
        Points[1] := Vector2Integer(vImageRegionRect.Left, ImageHeight);
        RenderLine(Points, LineColor, LineWidth);

        Points[0] := Vector2Integer(vImageRegionRect.Right, 0);
        Points[1] := Vector2Integer(vImageRegionRect.Right, ImageHeight);
        RenderLine(Points, LineColor, LineWidth);

        Points[0] := Vector2Integer(0, vImageRegionRect.Bottom);
        Points[1] := Vector2Integer(ImageWidth, vImageRegionRect.Bottom);
        RenderLine(Points, LineColor, LineWidth);

        Points[0] := Vector2Integer(0, vImageRegionRect.Top);
        Points[1] := Vector2Integer(ImageWidth, vImageRegionRect.Top);
        RenderLine(Points, LineColor, LineWidth);

        RenderPoints(ScreenRegionPoints);
        RenderPoints(ScreenAdditionalPoints);
      end;
    end;

  end;

begin
  RenderImage;
  RenderControlPoints;
end;

procedure TRegionDesignDialog.FormCreate(Sender: TObject);
begin
  CastleControl1.Container.BackgroundColor := Gray;
end;

procedure TRegionDesignDialog.HelpButtonClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
    CastleControl1.Container.BackgroundColor :=
      Vector4(byte(ColorDialog1.Color) / 255, byte(ColorDialog1.Color shr 8) /
      255, byte(ColorDialog1.Color shr 16) / 255, 1);
end;



function TRegionDesignDialog.HitTest(const AControlPoint: TVector2Integer;
  const AMousePoint: TVector2): TDirectionEnables;
var
  pt: TVector2;
  rc: TFloatRectangle;
begin
  Result := [];

  case FDesignMode of
    TDesignMode.ModeRegion:
    begin
      rc := ScreenRegionRectangle;
      rc := rc.Grow(CircleRads);
    end;
    TDesignMode.ModeBorder:
    begin
      rc := ScreenImageFullRectangle;
      rc := rc.Grow(CircleRads);
    end;
  end;

  if not rc.Contains(AMousePoint) then Exit;

  pt := ImageToScreen(AControlPoint);

  if ABS(pt.X - AMousePoint.X) < CircleRads then Include(Result, EnableX);
  if ABS(pt.Y - AMousePoint.Y) < CircleRads then Include(Result, EnableY);

end;

function TRegionDesignDialog.GetDirectionEnables(
  const MousePoint: TVector2): TDirectionEnables;
var
  Point: TVector2Integer;
begin
  Result := [];
  for Point in FControlPointRec.Points do
    Result := Result + HitTest(Point, MousePoint);
end;

procedure TRegionDesignDialog.CastleControl1Press(Sender: TObject;
  const Event: TInputPressRelease);

  procedure GrowScale(const beIncrease: boolean; const ScreenPoint: TVector2);
  const
    PX = 0.7;
  var
    ImagePoint: TVector2Integer;
  begin
    { Limit the scaling point to be inside the image to prevent the coordinates from losing control. }

    ImagePoint := ScreenToImage(ScreenPoint);
    ImagePoint.X := Clamped(ImagePoint.X, 0, ImageWidth);
    ImagePoint.Y := Clamped(ImagePoint.Y, 0, ImageHeight);

    if beIncrease then
    begin
      { When FScale is less than 6, it increases linearly, otherwise it increases exponentially. }
      if FScale < 1 then
        FScale := FScale + 0.1
      else if FScale < 2 then
        FScale := FScale + 0.2
      else if FScale < 6 then
        FScale := FScale + 0.4
      else
        FScale := FScale / PX;

      FScale := Min(FScale, 10000);
    end
    else
    begin
      {
        Same as above.
        Between 0.5 and 6 : linearly,
        otherwise : exponentially.
      }
      if FScale > 6 then FScale := FScale * PX
      else if FScale > 2 then
        FScale := FScale - 0.4
      else if FScale > 1 then
        FScale := FScale - 0.2
      else
      if FScale > 0.5 then
        FScale := FScale - 0.1
      else
        FScale := FScale * PX;

      FScale := Max(FScale, 0.0001);
    end;

    { Zoom in on the pixel at the mouse location. }
    FTranslation := FTranslation + ScreenPoint - ImageToScreen(ImagePoint);

  end;

var
  RegionPts: TArrayImagePoints;
  i, vIndex: integer;
  AllDirectionEnables, DirectionEnables: TDirectionEnables;
begin
  { LeftMouseButton Pressed. }

  if Event.IsMouseButton(TCastleMouseButton.buttonLeft) and
    (not FControlPointRec.Adjusting) then
  begin
    AllDirectionEnables := [];
    vIndex := FControlPointRec.Index;
    RegionPts := FControlPointRec.Points;

    for i := Low(RegionPts) to High(RegionPts) do
    begin
      DirectionEnables := HitTest(RegionPts[i], Event.Position);
      AllDirectionEnables := AllDirectionEnables + DirectionEnables;

      if DirectionEnables = [EnableX, EnableY] then
      begin
        vIndex := i;
        Break;
      end
      else
      begin
        if AllDirectionEnables = [EnableX, EnableY] then
        begin
          { Exchange control points.The original control points become additional control points.
            We only ever need two anchors.
            Obviously here i must equal 1. Using 0 and 1 to represent two original control points,
            if DirectionEnables = [EnableX], then point 1 has the same X value as the currently selected point.
            Then calling 'AdditionalPoints' with True will ensure that the current point still has an index of 1,
            which is the correct order. On the other hand,
            if DirectionEnables = [EnableY], pass False to get the correct order. }
          FControlPointRec.Points :=
            FControlPointRec.AdditionalPoints(DirectionEnables = [EnableX]);
          vIndex := i;
          Break;
        end
        else if DirectionEnables <> [] then
          vIndex := i;
      end;
    end;

    if AllDirectionEnables <> [] then
    begin
      FControlPointRec.Index := vIndex;
      FControlPointRec.DirectionEnables := AllDirectionEnables;
    end
    else
    begin
      { new region }
      FControlPointRec.Points[0] := ScreenToImage(Event.Position);
      FControlPointRec.Points[1] := FControlPointRec.Points[0];
      FControlPointRec.Index := 1;
      { Newly created regions are always free to move. }
      FControlPointRec.DirectionEnables := [EnableX, EnableY];
    end;

    FControlPointRec.Adjusting := True;
    Changed;
  end;

  { RightMouseButton pressed. }

  if Event.IsMouseButton(TCastleMouseButton.buttonRight) and
    (not FMovingImageRec.Moving) then
  begin
    FMovingImageRec.StartMousePoint := Event.Position;
    FMovingImageRec.StartTranslation := FTranslation;
    FMovingImageRec.Moving := True;
  end;

  { MouseWheel. }

  if (Event.EventType = TInputPressReleaseType.itMouseWheel) then
  begin
    GrowScale(Event.MouseWheelScroll > 0, Event.Position);
    UpdateCursorPosInfo(Event.Position);
  end;

  UpdateCursorShape(Event.Position);
end;

procedure TRegionDesignDialog.UpdateCursorPosInfo(const MousePoint: TVector2);
var
  Point: TVector2Integer;
begin
  Point := ScreenToImage(MousePoint, False);
  StatusBar1.Panels.Items[4].Text := Format('CursorPos: %d , %d', [Point.X, Point.Y]);
end;

procedure TRegionDesignDialog.UpdateCursorShape(const MousePoint: TVector2);
var
  DirectionEnables: TDirectionEnables;
begin
  if FControlPointRec.Adjusting then DirectionEnables :=
      FControlPointRec.DirectionEnables
  else
    DirectionEnables := GetDirectionEnables(MousePoint);

  if DirectionEnables = [EnableX, EnableY] then
    CastleControl1.Cursor := crHandPoint
  else
  if DirectionEnables = [EnableX] then CastleControl1.Cursor := crSizeWE
  else
  if DirectionEnables = [EnableY] then CastleControl1.Cursor := crSizeNS
  else
    CastleControl1.Cursor := crDefault;
end;

procedure TRegionDesignDialog.CastleControl1Motion(Sender: TObject;
  const Event: TInputMotion);
var
  Point: TVector2Integer;
begin
  if FControlPointRec.Adjusting and not (FControlPointRec.DirectionEnables = []) then
  begin
    Point := ScreenToImage(Event.Position);

    if (EnableX in FControlPointRec.DirectionEnables) then
      FControlPointRec.Points[FControlPointRec.Index].X := Point.X;
    if (EnableY in FControlPointRec.DirectionEnables) then
      FControlPointRec.Points[FControlPointRec.Index].Y := Point.Y;

    Changed;
  end;

  if FMovingImageRec.Moving then
  begin
    FTranslation := Event.Position - FMovingImageRec.StartMousePoint +
      FMovingImageRec.StartTranslation;
  end;

  UpdateCursorShape(Event.Position);
  UpdateCursorPosInfo(Event.Position);
end;

procedure TRegionDesignDialog.FixControlPoints;
var
  i: integer;
begin
  for  i := Low(FControlPointRec.Points) to High(FControlPointRec.Points) do
  begin
    FControlPointRec.Points[i].X := Clamped(FControlPointRec.Points[i].X, 0, ImageWidth);
    FControlPointRec.Points[i].Y := Clamped(FControlPointRec.Points[i].Y, 0, ImageHeight);
  end;
end;

procedure TRegionDesignDialog.CastleControl1Release(Sender: TObject;
  const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(TCastleMouseButton.buttonRight) and
    FMovingImageRec.Moving then
    FMovingImageRec.Moving := False;

  if Event.IsMouseButton(TCastleMouseButton.buttonLeft) and
    FControlPointRec.Adjusting then
  begin
    FControlPointRec.Adjusting := False;
    FixControlPoints;
    UpdateCursorShape(Event.Position);
    Changed;
  end;
end;

procedure TRegionDesignDialog.InitControlPoints;

  function CenterPoint: TVector2Integer;
  begin
    Result := FControlPointRec.Points[0] + FControlPointRec.Points[1];
    Result.X := Result.X div 2;
    Result.Y := Result.Y div 2;
  end;

begin
  { "empty region" means using the whole image. }
  if FRegion.IsEmpty then
  begin
    FControlPointRec.Points[0] :=
      TVector2Integer.Zero;
    FControlPointRec.Points[1] :=
      Vector2Integer(ImageWidth, ImageHeight);
  end
  else
  begin
    FControlPointRec.Points[0] :=
      Vector2Integer(Floor(FRegion.Left), Floor(FRegion.Bottom));
    FControlPointRec.Points[1] :=
      Vector2Integer(Floor(FRegion.Right), Floor(FRegion.Top));
  end;

  FixControlPoints;

  { Center view then region. }
  FTranslation := ImageToScreen(CastleControl1.Rect.Center) -
    ImageToScreen(CenterPoint);

  Changed;
end;

function TRegionDesignDialog.ScreenRegionPoints: TArrayScreenPoints;
begin
  Result := ImageToScreen(FControlPointRec.Points);
end;

function TRegionDesignDialog.ScreenAdditionalPoints: TArrayScreenPoints;
begin
  Result := ImageToScreen(FControlPointRec.AdditionalPoints);
end;

procedure TRegionDesignDialog.SetImage(const AImage: TDrawableImage);
begin
  if not Assigned(AImage) then Exit;
  if not Assigned(AImage.Image) then Exit;

  FImage.Load(AImage.Image);
end;

function TRegionDesignDialog.ImageWidth(): cardinal;
begin
  if FSourceRegion.IsEmpty then  Result := FImage.Width
  else
    Result := Floor(FSourceRegion.Width);
end;

function TRegionDesignDialog.ImageHeight(): cardinal;
begin
  if FSourceRegion.IsEmpty then  Result := FImage.Height
  else
    Result := Floor(FSourceRegion.Height);
end;

procedure TRegionDesignDialog.Changed;
begin
  StatusBar1.Panels.Items[0].Text :=
    Format('Left: %d', [Min(FControlPointRec.Points[0].X,
    FControlPointRec.Points[1].X)]);
  StatusBar1.Panels.Items[1].Text :=
    Format('Bottom: %d', [Min(FControlPointRec.Points[0].Y,
    FControlPointRec.Points[1].Y)]);

  if FDesignMode = TDesignMode.ModeRegion then
  begin
    StatusBar1.Panels.Items[2].Text :=
      Format('Width: %d', [Abs(FControlPointRec.Points[0].X -
      FControlPointRec.Points[1].X)]);
    StatusBar1.Panels.Items[3].Text :=
      Format('Height: %d', [Abs(FControlPointRec.Points[0].Y -
      FControlPointRec.Points[1].Y)]);
  end
  else
  begin
    StatusBar1.Panels.Items[2].Text :=
      Format('Right: %d', [ImageWidth - Max(FControlPointRec.Points[0].X,
      FControlPointRec.Points[1].X)]);
    StatusBar1.Panels.Items[3].Text :=
      Format('Top: %d', [ImageHeight - Max(FControlPointRec.Points[0].Y,
      FControlPointRec.Points[1].Y)]);
  end;
end;

function TRegionDesignDialog.ScreenToImage(APoint: TVector2;
  bRound: boolean = True): TVector2Integer;
begin
  APoint := (APoint - FTranslation) / FScale;

  if bRound then
    Result := Vector2Integer(Round(APoint.X), Round(APoint.Y))
  else
    Result := Vector2Integer(Floor(APoint.X), Floor(APoint.Y));
end;

function TRegionDesignDialog.ImageToScreen(const APoint: TVector2Integer): TVector2;
var
  pt: TVector2;
begin
  pt.X := APoint.X;
  pt.Y := APoint.Y;

  Result := pt * FScale + FTranslation;
end;

function TRegionDesignDialog.ImageToScreen(
  const APoints: TArrayImagePoints): TArrayScreenPoints;
var
  i: integer;
begin
  for i := Low(APoints) to High(APoints) do
    Result[i] := ImageToScreen(APoints[i]);
end;

end.
