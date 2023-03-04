{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Possible touch control UI, for @link(TCastleTouchControl.TouchMode). }
  TCastleTouchCtlMode = (
    ctcmWalking, ctcmWalkWithSideRot, ctcmHeadRotation, ctcmFlyUpdown, ctcmPanXY);
  TCastleTouchPosition = (tpManual, tpLeft, tpRight);

  { Touch user interface to navigate in a 3D world.
    Shows one "lever", that can be moved up/down/left/right,
    and controls the movement while Walking or Flying. }
  TCastleTouchControl = class(TCastleUserInterface)
  strict private
    FTouchMode: TCastleTouchCtlMode;
    FLeverOffset: TVector2;
    FDragging: Integer; //< finger index that started drag, -1 if none
    FPosition: TCastleTouchPosition;
    FScale: Single;
    function TotalScale: Single;
    procedure SetPosition(const Value: TCastleTouchPosition);
    procedure SetScale(const Value: Single);
    function MaxOffsetDist: Single;
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure SetTouchMode(const Value: TCastleTouchCtlMode);
    procedure GetSensorRotation(var X, Y, Z, Angle: Double);
    procedure GetSensorTranslation(var X, Y, Z, Length: Double);
  published
    property TouchMode: TCastleTouchCtlMode
      read FTouchMode write SetTouchMode default ctcmWalking;

    { Set position of touch control. Right now this simply sets
      the anchor using @link(TCastleUserInterface.Anchor) and friends.
      Tip: Use @link(TCastleContainer.UIScaling) to have the anchors automatically
      scale with screen size.

      The size of the control is set to be constant physical size,
      so it's not affected by @link(TCastleContainer.UIScaling), only by
      @link(TCastleContainer.Dpi). }
    property Position: TCastleTouchPosition
      read FPosition write SetPosition default tpManual;

    property Scale: Single read FScale write SetScale {$ifdef FPC}default 1{$endif};
  end deprecated 'use TCastleTouchNavigation';

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTouchControl ---------------------------------------------------------------- }

constructor TCastleTouchControl.Create(AOwner: TComponent);
begin
  inherited;
  FDragging := -1;
  FScale := 1;
end;

procedure TCastleTouchControl.SetScale(const Value: Single);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    VisibleChange([chRectangle]);
  end;
end;

function TCastleTouchControl.TotalScale: Single;
begin
  { We used to have here

      Scale * Container.Dpi / 96

    and we ignored UIScale. This makes it adjust to physical size.
    But it's not so obviously good idea -- the control could get very large
    (obscuring view) on small devices.
    Better to default to our standard scaling. This also allows to clearly see
    the visual size of this on desktops.

    User can always set "TouchControl.UIScaling := false; TouchControl.Scale := Container.Dpi / 96;"
    to get this effect. }

  Result := Scale * UIScale;
end;

procedure TCastleTouchControl.PreferredSize(var PreferredWidth, PreferredHeight: Single);
begin
  inherited;
  // do not apply UIScale here, it is already inside TotalScale
  PreferredWidth  := Theme.ImagesPersistent[tiTouchCtlOuter].Width  * TotalScale;
  PreferredHeight := Theme.ImagesPersistent[tiTouchCtlOuter].Height * TotalScale;
end;

procedure TCastleTouchControl.SetPosition(const Value: TCastleTouchPosition);
const
  CtlBorder = 24;
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    case Position of
      tpManual:
        begin
          Anchor(hpLeft, 0);
          Anchor(vpBottom, 0);
        end;
      tpLeft:
        begin
          Anchor(hpLeft, CtlBorder);
          Anchor(vpBottom, CtlBorder);
        end;
      tpRight:
        begin
          Anchor(hpRight, -CtlBorder);
          Anchor(vpBottom, CtlBorder);
        end;
    end;
    // VisibleChange([chRectangle]); // no need to call this, Anchor already called it
  end;
end;

function TCastleTouchControl.MaxOffsetDist: Single;
begin
  Result := TotalScale *
    (Theme.ImagesPersistent[tiTouchCtlOuter].Width -
     Theme.ImagesPersistent[tiTouchCtlInner].Width) / 2;
end;

procedure TCastleTouchControl.Render;
var
  LevOffsetTrimmedX, LevOffsetTrimmedY, MaxDist: Single;
  LeverDist: Double;
  InnerRect: TFloatRectangle;
  ImageInner, ImageOuter: TThemeImage;
  SR: TFloatRectangle;
begin
  inherited;
  SR := RenderRect;

  if FTouchMode = ctcmFlyUpdown then
  begin
    ImageInner := tiTouchCtlFlyInner;
    ImageOuter := tiTouchCtlFlyOuter;
  end else
  begin
    ImageInner := tiTouchCtlInner;
    ImageOuter := tiTouchCtlOuter;
  end;
  Theme.Draw(SR, ImageOuter, UIScale);

  // compute lever offset (must not move outside outer ring)
  LeverDist := FLeverOffset.Length;
  MaxDist := MaxOffsetDist;
  if LeverDist <= MaxDist then
  begin
    LevOffsetTrimmedX := FLeverOffset[0];
    LevOffsetTrimmedY := FLeverOffset[1];
  end else
  begin
    LevOffsetTrimmedX := (FLeverOffset[0] * MaxDist) / LeverDist;
    LevOffsetTrimmedY := (FLeverOffset[1] * MaxDist) / LeverDist;
  end;
  if FTouchMode = ctcmFlyUpdown then
    LevOffsetTrimmedX := 0;

  // draw lever
  InnerRect := FloatRectangle(Theme.ImagesPersistent[ImageInner].Image.Rect); // rectangle at (0,0)
  InnerRect.Width  := InnerRect.Width  * TotalScale;
  InnerRect.Height := InnerRect.Height * TotalScale;
  InnerRect.Left   := SR.Left   + (SR.Width  - InnerRect.Width ) / 2 + LevOffsetTrimmedX;
  InnerRect.Bottom := SR.Bottom + (SR.Height - InnerRect.Height) / 2 + LevOffsetTrimmedY;

  Theme.Draw(InnerRect, ImageInner, UIScale);
end;

procedure TCastleTouchControl.SetTouchMode(const Value: TCastleTouchCtlMode);
begin
  if FTouchMode <> Value then
  begin
    FTouchMode := Value;
    { we may swap outer image depending on the TouchMode in some later version }
    VisibleChange([chRectangle]);
  end;
end;

function TCastleTouchControl.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (Event.EventType <> itMouseButton) then Exit;

  Result := true;
  FDragging := Event.FingerIndex;
  FLeverOffset := TVector2.Zero;
end;

function TCastleTouchControl.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (Event.EventType <> itMouseButton) then Exit;

  if FDragging = Event.FingerIndex then
  begin
    Result := true;

    FDragging := -1;
    FLeverOffset := TVector2.Zero;
    VisibleChange([chRender]); { repaint with lever back in the center }
  end;
end;

function TCastleTouchControl.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;

  if (not Result) and (FDragging = Event.FingerIndex) then
  begin
    FLeverOffset := FLeverOffset + Event.Position - Event.OldPosition;
    VisibleChange([chRender]);
    Result := true;
  end;
end;

procedure TCastleTouchControl.GetSensorRotation(var X, Y, Z, Angle: Double);
var
  FxConst: Double;
begin
  FxConst := 10 / MaxOffsetDist;
  if FTouchMode = ctcmHeadRotation then
  begin
    X :=  FLeverOffset[1] * FxConst;
    Y := -FLeverOffset[0] * FxConst;
    Angle := Pi * 1 / 180;
  end else
  if FTouchMode = ctcmWalkWithSideRot then
  begin
    Y := -FLeverOffset[0] * FxConst;
    Angle := Pi * 1 / 180;
  end;
end;

procedure TCastleTouchControl.GetSensorTranslation(var X, Y, Z, Length: Double);
var
  FxConst: Double;
begin
  FxConst := 200/MaxOffsetDist;
  case FTouchMode of
    ctcmWalking:
      begin
        X :=  FLeverOffset[0] * FxConst / 1.5;  { walking to the sides should be slower }
        Z := -FLeverOffset[1] * FxConst;
        Length := 20;
      end;
    ctcmWalkWithSideRot:
      begin
        Z := -FLeverOffset[1] * FxConst;
        Length := 20;
      end;
    ctcmFlyUpdown:
      begin
        Y := FLeverOffset[1] * FxConst;
        Length := 20;
      end;
    ctcmPanXY:
      begin
        X := -FLeverOffset[0] * FxConst;
        Y := -FLeverOffset[1] * FxConst;
        Length := 5;
      end;
    else ;
  end;
end;

{$endif read_implementation}
