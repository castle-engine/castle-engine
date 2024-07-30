{%MainUnit castleviewport.pas}
{
  Copyright 2013-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Value for @link(TCastleTouchNavigation.TouchInterface). }
  TTouchInterface = (
    tiNone,
    tiWalk,
    tiWalkRotate,
    tiFlyWalk,
    tiPan
  );

  { Show draggable controls in the corner, to navigate
    in the viewport comfortably on touch devices.

    Depending on @link(AutoTouchInterface) and @link(TouchInterface),
    we will show 0, 1 or 2 controls to navigate at the bottom,
    in the left-bottom and right-bottom.
    It is easiest to just set

    @longCode(#
      MyTouchNavigation.TouchInterface := tiNone; // default
      MyTouchNavigation.AutoTouchInterface := ApplicationProperties.TouchDevice;
    #)

    to activate this automatically on mobile devices, leaving @code(MyTouchNavigation.TouchInterface)
    equal tiNone on non-mobile devices.
    Alternatively, you can do something like this to control it manually on mobile devices:

    @longCode(#
      MyTouchNavigation.AutoTouchInterface := false; // default
      MyTouchNavigation.Exists := ApplicationProperties.TouchDevice;
      MyTouchNavigation.TouchInterface := tiSomething;
    #)

    The size and position of this control determines where the controls appear.
    Typically this control is added as a direct child of TCastleViewport
    and has @link(FullSize) = @true, this way it just fills the TCastleViewport.

    The @link(Viewport) determines the viewport where navigation is affected
    by this control.
    Typically the @link(Viewport) is also our visual parent, but it doesn't have to be.
  }
  TCastleTouchNavigation = class(TCastleUserInterface)
  strict private
    type
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
      end;

    var
      FViewport: TCastleViewport;
      FLastSeenNavigationType: TNavigationType;
      FAutoTouchInterface: Boolean;
      FControl: array [Boolean { right side? }] of TCastleTouchControl;
      FTouchInterface: TTouchInterface;
      FAutoWalkTouchInterface, FAutoExamineTouchInterface: TTouchInterface;
      FScale: Single;

    { Apply current TouchInterface value. Call when TouchInterface value changed. }
    procedure UpdateTouchInterface;
    { Update current TouchInterface if AutoTouchInterface enabled.
      Call when AutoTouchInterface, Viewport,
      AutoExamineTouchInterface, AutoWalkTouchInterface, current navigation changed. }
    procedure UpdateAutoTouchInterface;
    procedure SetViewport(const Value: TCastleViewport);
    procedure SetTouchInterface(const Value: TTouchInterface);
    procedure SetAutoTouchInterface(const Value: Boolean);
    procedure SetAutoWalkTouchInterface(const Value: TTouchInterface);
    procedure SetAutoExamineTouchInterface(const Value: TTouchInterface);
    function TouchInterfaceStored: Boolean;
    procedure SetScale(const Value: Single);
  public
    const
      DefaultAutoWalkTouchInterface = tiWalk;
      DefaultAutoExamineTouchInterface = tiPan;
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Viewport where navigation is affected by this control.

      You must set this property to something non-nil,
      otherwise this UI control doesn't do much, it will not affect anything. }
    property Viewport: TCastleViewport read FViewport write SetViewport;

    { Configure controls to be visible and available to navigate.
      This automatically manages under the hood 0, 1 or 2
      controls to navigate, placing them at suitable positions
      and handling their operations.

      Note that you can set AutoTouchInterface = @true to have this property
      automatically adjusted. (In which case you should not set this directly.) }
    property TouchInterface: TTouchInterface
      read FTouchInterface write SetTouchInterface stored TouchInterfaceStored
      default tiNone;

    { Automatically adjust @link(TouchInterface) (showing / hiding proper
      touch controls) based on the detected current @link(Viewport) navigation type. }
    property AutoTouchInterface: boolean
      read FAutoTouchInterface write SetAutoTouchInterface
      default false;

    { When using AutoTouchInterface = @true,
      which touch interface should be used when walking
      (since there are multiple sensible choices).
      Select between tiWalkRotate or tiWalk (default).}
    property AutoWalkTouchInterface: TTouchInterface
      read FAutoWalkTouchInterface write SetAutoWalkTouchInterface
      default DefaultAutoWalkTouchInterface;

    { When using AutoTouchInterface = @true,
      which touch interface should be used in examine camera
      (since examine camera can use multi-touch gesture instead).
      Select between tiPan (default) or tiNone.}
    property AutoExamineTouchInterface: TTouchInterface
      read FAutoExamineTouchInterface write SetAutoExamineTouchInterface
      default DefaultAutoExamineTouchInterface;

    { Visual scale of the device. }
    property Scale: Single read FScale write SetScale {$ifdef FPC}default 1.0{$endif};

    { Detect if mouse is currently over one of the touch control gizmos. }
    class function MouseOverTouchGizmo(const AContainer: TCastleContainer): Boolean;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTouchNavigation.TCastleTouchControl --------------------------------- }

constructor TCastleTouchNavigation.TCastleTouchControl.Create(AOwner: TComponent);
begin
  inherited;
  FDragging := -1;
  FScale := 1;
end;

procedure TCastleTouchNavigation.TCastleTouchControl.SetScale(const Value: Single);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    VisibleChange([chRectangle]);
  end;
end;

function TCastleTouchNavigation.TCastleTouchControl.TotalScale: Single;
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

procedure TCastleTouchNavigation.TCastleTouchControl.PreferredSize(var PreferredWidth, PreferredHeight: Single);
begin
  inherited;
  // do not apply UIScale here, it is already inside TotalScale
  PreferredWidth  := Theme.ImagesPersistent[tiTouchCtlOuter].Width  * TotalScale;
  PreferredHeight := Theme.ImagesPersistent[tiTouchCtlOuter].Height * TotalScale;
end;

procedure TCastleTouchNavigation.TCastleTouchControl.SetPosition(const Value: TCastleTouchPosition);
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

function TCastleTouchNavigation.TCastleTouchControl.MaxOffsetDist: Single;
begin
  Result := TotalScale *
    (Theme.ImagesPersistent[tiTouchCtlOuter].Width -
     Theme.ImagesPersistent[tiTouchCtlInner].Width) / 2;
end;

procedure TCastleTouchNavigation.TCastleTouchControl.Render;
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

procedure TCastleTouchNavigation.TCastleTouchControl.SetTouchMode(const Value: TCastleTouchCtlMode);
begin
  if FTouchMode <> Value then
  begin
    FTouchMode := Value;
    { we may swap outer image depending on the TouchMode in some later version }
    VisibleChange([chRectangle]);
  end;
end;

function TCastleTouchNavigation.TCastleTouchControl.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (Event.EventType <> itMouseButton) then Exit;

  Result := true;
  FDragging := Event.FingerIndex;
  FLeverOffset := TVector2.Zero;
end;

function TCastleTouchNavigation.TCastleTouchControl.Release(const Event: TInputPressRelease): boolean;
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

function TCastleTouchNavigation.TCastleTouchControl.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;

  if (not Result) and (FDragging = Event.FingerIndex) then
  begin
    FLeverOffset := FLeverOffset + Event.Position - Event.OldPosition;
    VisibleChange([chRender]);
    Result := true;
  end;
end;

procedure TCastleTouchNavigation.TCastleTouchControl.GetSensorRotation(var X, Y, Z, Angle: Double);
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

procedure TCastleTouchNavigation.TCastleTouchControl.GetSensorTranslation(var X, Y, Z, Length: Double);
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

{ TCastleTouchNavigation ----------------------------------------------------- }

constructor TCastleTouchNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FAutoWalkTouchInterface := DefaultAutoWalkTouchInterface;
  FAutoExamineTouchInterface := DefaultAutoExamineTouchInterface;
  FScale := 1.0;
end;

procedure TCastleTouchNavigation.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  Tx, Ty, Tz, TLength, Rx, Ry, Rz, RAngle: Double;
  RightSide: Boolean;
begin
  inherited;

  if (FControl[false] <> nil) or
     (FControl[true] <> nil) then
  begin
    Tx := 0; Ty := 0; Tz := 0; TLength := 0;
    Rx := 0; Ry := 0; Rz := 0; RAngle := 0;

    for RightSide := Low(Boolean) to High(Boolean) do
      if FControl[RightSide] <> nil then
      begin
        FControl[RightSide].GetSensorTranslation(Tx, Ty, Tz, TLength);
        FControl[RightSide].GetSensorRotation(Rx, Ry, Rz, RAngle);
      end;

    {$warnings off} // ignore using deprecated Navigation, TODO: we should fix the way we pass 3D mouse events
    if (Viewport <> nil) and
       (Viewport.Navigation <> nil) then
    begin
      Viewport.Navigation.SensorTranslation(Tx, Ty, Tz, TLength, SecondsPassed);
      Viewport.Navigation.SensorRotation(Rx, Ry, Rz, RAngle, SecondsPassed);
    end;
    {$warnings on}
  end;

  {$warnings off} // ignore using deprecated NavigationType
  if (Viewport <> nil) and
     (FLastSeenNavigationType <> TCastleAutoNavigationViewport.InternalGetNavigationType(Viewport)) then
  begin
    UpdateAutoTouchInterface; // will update FLastSeenNavigationType
  end;
  {$warnings on}
end;

procedure TCastleTouchNavigation.SetTouchInterface(const Value: TTouchInterface);
begin
  if FTouchInterface <> Value then
  begin
    FTouchInterface := Value;
    UpdateTouchInterface;
  end;
end;

procedure TCastleTouchNavigation.UpdateTouchInterface;

  procedure UpdateTouchController(
    const RightSide, CtlVisible: Boolean; const Mode: TCastleTouchCtlMode);
  var
    NewControl: TCastleTouchControl;
  begin
    if FControl[RightSide] <> nil then
    begin
      if CtlVisible then
        FControl[RightSide].TouchMode := Mode
      else
        FreeAndNil(FControl[RightSide]); // this automatically removes FControl[RightSide] from Controls list
    end else
    if CtlVisible then
    begin
      NewControl := TCastleTouchControl.Create(Self);
      NewControl.Scale := Scale;
      NewControl.SetTransient;
      NewControl.TouchMode := Mode;
      if not RightSide then
        NewControl.Position := tpLeft
      else
        NewControl.Position := tpRight;
      InsertFront(NewControl);
      FControl[RightSide] := NewControl;
    end;
  end;

  procedure UpdateTouchControllers(
    const LeftVisible, RightVisible: Boolean;
    const LeftMode: TCastleTouchCtlMode = ctcmWalking;
    const RightMode: TCastleTouchCtlMode = ctcmWalking);
  begin
    UpdateTouchController(false, LeftVisible , LeftMode);
    UpdateTouchController(true , RightVisible, RightMode);
  end;

begin
  case TouchInterface of
    tiNone:
      UpdateTouchControllers(false, false);
    tiWalkRotate:
      UpdateTouchControllers(true, true, ctcmWalking, ctcmHeadRotation);
    tiWalk:
      UpdateTouchControllers(false, true, ctcmWalking, ctcmWalking);
    tiFlyWalk:
      UpdateTouchControllers(true, true, ctcmFlyUpdown, ctcmWalking);
    tiPan:
      UpdateTouchControllers(false, true, ctcmPanXY, ctcmPanXY);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TouchInterface?');
    {$endif}
  end;
end;

function TCastleTouchNavigation.TouchInterfaceStored: Boolean;
begin
  Result := not AutoTouchInterface;
end;

procedure TCastleTouchNavigation.UpdateAutoTouchInterface;
begin
  if (Viewport <> nil) and AutoTouchInterface then
  begin
    {$warnings off} // ignore using deprecated NavigationType
    FLastSeenNavigationType := TCastleAutoNavigationViewport.InternalGetNavigationType(Viewport);
    {$warnings on}
    case FLastSeenNavigationType of
      ntNone:      TouchInterface := tiNone;
      ntWalk:      TouchInterface := FAutoWalkTouchInterface;
      ntFly:       TouchInterface := tiFlyWalk;
      ntExamine:   TouchInterface := FAutoExamineTouchInterface;
      ntTurntable: TouchInterface := FAutoExamineTouchInterface;
      // nt2D: TODO
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleTouchNavigation.UpdateAutoTouchInterface not implemented for this NavigationType value');
      {$endif}
    end;
  end;
end;

procedure TCastleTouchNavigation.SetAutoTouchInterface(const Value: Boolean);
begin
  if FAutoTouchInterface <> Value then
  begin
    FAutoTouchInterface := Value;
    { change TouchInterface immediately, in case we just set
      AutoTouchInterface := true }
    UpdateAutoTouchInterface;
  end;
end;

procedure TCastleTouchNavigation.SetAutoWalkTouchInterface(const Value: TTouchInterface);
begin
  if FAutoWalkTouchInterface <> Value then
  begin
    FAutoWalkTouchInterface := Value;
    UpdateAutoTouchInterface;
  end;
end;

procedure TCastleTouchNavigation.SetAutoExamineTouchInterface(const Value: TTouchInterface);
begin
  if FAutoExamineTouchInterface <> Value then
  begin
    FAutoExamineTouchInterface := Value;
    UpdateAutoTouchInterface;
  end;
end;

procedure TCastleTouchNavigation.SetViewport(const Value: TCastleViewport);
begin
  if FViewport <> Value then
  begin
    FViewport := Value;
    UpdateAutoTouchInterface;
  end;
end;

function TCastleTouchNavigation.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Viewport') or
     (PropertyName = 'TouchInterface') or
     (PropertyName = 'AutoTouchInterface') or
     (PropertyName = 'Scale') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleTouchNavigation.SetScale(const Value: Single);
var
  RightSide: Boolean;
begin
  if FScale <> Value then
  begin
    FScale := Value;
    for RightSide := Low(Boolean) to High(Boolean) do
      if FControl[RightSide] <> nil then
        FControl[RightSide].Scale := FScale;
  end;
end;

class function TCastleTouchNavigation.MouseOverTouchGizmo(
  const AContainer: TCastleContainer): Boolean;
var
  F: TCastleUserInterface;
begin
  { Note that we process all AContainer.Focus, not only AContainer.FocusFront,
    since TCastleTouchNavigation may be either in front or behind
    TCastleWalkNavigation. }
  for F in AContainer.Focus do
    if F is TCastleTouchControl then
      Exit(true);
  Result := false;
end;

{$endif read_implementation}
