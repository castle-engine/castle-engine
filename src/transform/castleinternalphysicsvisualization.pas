{
  Copyright 2022-2022 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Visualization of physics stuff. }
unit CastleInternalPhysicsVisualization;

interface

uses
  Classes, SysUtils, Math,
  CastleTransform, CastleColors, CastleRenderOptions,
  CastleVectors, CastleScene;

type
  TSphereVisualization = class(TCastleSphere)
  public
    procedure LocalRender(const Params: TRenderParams); override;
  end;

  TTemporaryJointTransform = class(TCastleTransform)
  strict private
    FColor: TCastleColor;
    FJoint: TAbstractJoint;

    procedure SetColor(const Value: TCastleColor);
  protected
    FSphere: TSphereVisualization;

    // For now it's actually more natural to just *not* adjust sphere size
    //function EstimateSphereRadius: Single;

    procedure SetValue(const AValue: TVector3);
    procedure SetObservedValue(const AValue: TVector3); virtual; abstract;
    function GetObservedValue: TVector3; virtual; abstract;

    procedure CheckTransformInsideParent;
    procedure ChangedTransform; override;
  public
    constructor Create(AOwner: TComponent;
      const AJoint: TAbstractJoint); reintroduce; virtual;
    destructor Destroy; override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    property Value: TVector3 read GetObservedValue write SetValue;
    property Joint: TAbstractJoint read FJoint;
    property Color: TCastleColor read FColor write SetColor;
  end;

  TTemporaryJointAnchor = class(TTemporaryJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  end;

  TTemporaryJointConnectedAnchor = class(TTemporaryJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;

  TTemporaryJointWorldPoint = class(TTemporaryJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;

  TTemporaryJointWorldAnchor = class(TTemporaryJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;

  TTemporaryJointConnectedWorldAnchor = class(TTemporaryJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;

  TTemporaryJointWorldGroundAnchor = class(TTemporaryJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;

  TTemporaryJointConnectedWorldGroundAnchor = class(TTemporaryJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;

implementation

uses CastleRenderContext;

{ TSphereVisualization ------------------------------------------------------- }

procedure TSphereVisualization.LocalRender(const Params: TRenderParams);
const
  RenderOnTop = true;
begin
  if RenderOnTop and (Params.RenderingCamera.Target <> rtShadowMap) then
    RenderContext.DepthRange := drNear;
  inherited;
  if RenderOnTop and (Params.RenderingCamera.Target <> rtShadowMap) then
    RenderContext.DepthRange := drFar;
end;

{ TTemporaryJointTransform --------------------------------------------------- }

procedure TTemporaryJointTransform.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    if FSphere <> nil then
      FSphere.Color := FColor;
  end;
end;

(*
function TTemporaryJointTransform.EstimateSphereRadius: Single;
var
  Viewport: TCastleViewport;
  SphereWorldPos1, SphereWorldPos2: TVector3;
  SphereViewportPos1, SphereViewportPos2: TVector2;
  DistanceOnViewport: Single;
begin
  if (Parent <> nil) and (Parent.World <> nil) and (Parent.World.Owner <> nil) and
     (Parent.World.Owner is TCastleViewport) then
  begin
    Viewport := Parent.World.Owner as TCastleViewport;
    SphereWorldPos1 := FSphere.LocalToWorld(CastleVectors.Vector3(0, 0, 0));
    SphereViewportPos1 := Viewport.PositionFromWorld(SphereWorldPos1);

    SphereWorldPos2 := SphereWorldPos1 + CastleVectors.Vector3(0, 1, 0);
    SphereViewportPos2 := Viewport.PositionFromWorld(SphereWorldPos2);
    DistanceOnViewport := PointsDistance(SphereViewportPos1, SphereViewportPos2);

    { Make size about 10 pixels }
    Result := 10 / DistanceOnViewport;
  end else
    Result := 1;
end;
*)

procedure TTemporaryJointTransform.SetValue(const AValue: TVector3);
begin
  SetObservedValue(AValue);
  if not TVector3.PerfectlyEquals(Translation, AValue) then
    Translation := AValue;
end;

procedure TTemporaryJointTransform.CheckTransformInsideParent;
begin
  if Parent = nil then
    Exit;
end;

procedure TTemporaryJointTransform.ChangedTransform;
begin
  inherited ChangedTransform;
  if not TVector3.PerfectlyEquals(Translation, Value) then
    Value := Translation;
end;

constructor TTemporaryJointTransform.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner);

  FColor := Red;
  FJoint := AJoint;
  SetTransient;

  FSphere := TSphereVisualization.Create(nil);
  FSphere.SetTransient;
  FSphere.UseInternalGlobalRenderOptions := false; // never change rendering to global
  FSphere.Color := FColor;
  FSphere.Material := pmUnlit;
  FSphere.Radius := 0.1;
  Add(FSphere);
end;

destructor TTemporaryJointTransform.Destroy;
begin
  FreeAndNil(FSphere);
  inherited Destroy;
end;

procedure TTemporaryJointTransform.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
// var
//   NewRadius: Single;
begin
  inherited Update(SecondsPassed, RemoveMe);

  (*
  if FSphere <> nil then
  begin
    NewRadius := EstimateSphereRadius;
    if not SameValue(NewRadius, FSphere.Radius) then
    begin
      FSphere.Radius := NewRadius;
    end;
  end;
  *)

  CheckTransformInsideParent;
end;

{ TTemporaryJointAnchor ----------------------------------------------------------- }

procedure TTemporaryJointAnchor.SetObservedValue(const AValue: TVector3);
begin
  // TODO: move anchor to abstract anchor class or virtual function?

  if Joint is TCastleHingeJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleHingeJoint(Joint).Anchor, AValue) then
      TCastleHingeJoint(Joint).Anchor := AValue;
  end;

  if Joint is TCastleRopeJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleRopeJoint(Joint).Anchor, AValue) then
      TCastleRopeJoint(Joint).Anchor := AValue;
  end;

  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastleFixedJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleFixedJoint(Joint).Anchor, AValue) then
      TCastleFixedJoint(Joint).Anchor := AValue;
  end;
  {$endif CASTLE_EXPERIMENTAL_JOINTS}

  if Joint is TCastleBallJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleBallJoint(Joint).Anchor, AValue) then
      TCastleBallJoint(Joint).Anchor := AValue;
  end;

  if Joint is TCastleDistanceJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleDistanceJoint(Joint).Anchor, AValue) then
      TCastleDistanceJoint(Joint).Anchor := AValue;
  end;

  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastleWorldPlaneDistanceJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleWorldPlaneDistanceJoint(Joint).Anchor, AValue) then
      TCastleWorldPlaneDistanceJoint(Joint).Anchor := AValue;
  end;
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
end;

function TTemporaryJointAnchor.GetObservedValue: TVector3;
begin
  // TODO: move anchor to abstract anchor class or virtual function?

  if Joint is TCastleHingeJoint then
    Exit(TCastleHingeJoint(Joint).Anchor);

  if Joint is TCastleRopeJoint then
    Exit(TCastleRopeJoint(Joint).Anchor);

  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastleFixedJoint then
    Exit(TCastleFixedJoint(Joint).Anchor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}

  if Joint is TCastleBallJoint then
    Exit(TCastleBallJoint(Joint).Anchor);

  if Joint is TCastleDistanceJoint then
    Exit(TCastleDistanceJoint(Joint).Anchor);

  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastleWorldPlaneDistanceJoint then
    Exit(TCastleWorldPlaneDistanceJoint(Joint).Anchor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
end;

{ TTemporaryJointConnectedAnchor --------------------------------------------- }

procedure TTemporaryJointConnectedAnchor.SetObservedValue(const AValue: TVector3);
begin
  if Joint is TCastleRopeJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleRopeJoint(Joint).ConnectedAnchor, AValue) then
      TCastleRopeJoint(Joint).ConnectedAnchor := AValue;
  end;

  if Joint is TCastleDistanceJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleDistanceJoint(Joint).ConnectedAnchor, AValue) then
      TCastleDistanceJoint(Joint).ConnectedAnchor := AValue;
  end;
end;

function TTemporaryJointConnectedAnchor.GetObservedValue: TVector3;
begin
  if Joint is TCastleRopeJoint then
    Exit(TCastleRopeJoint(Joint).ConnectedAnchor);
end;

constructor TTemporaryJointConnectedAnchor.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Green;
end;

{ TTemporaryJointWorldPoint -------------------------------------------------- }

procedure TTemporaryJointWorldPoint.SetObservedValue(const AValue: TVector3);
begin
  if Joint is TCastleGrabJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleGrabJoint(Joint).WorldPoint, AValue) then
      TCastleGrabJoint(Joint).WorldPoint := AValue;
  end;
end;

function TTemporaryJointWorldPoint.GetObservedValue: TVector3;
begin
  if Joint is TCastleGrabJoint then
    Exit(TCastleGrabJoint(Joint).WorldPoint);
end;

constructor TTemporaryJointWorldPoint.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Blue;
end;


{ TTemporaryJointWorldAnchor ------------------------------------------------- }

procedure TTemporaryJointWorldAnchor.SetObservedValue(const AValue: TVector3);
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastlePulleyJoint(Joint).WorldAnchor, AValue) then
      TCastlePulleyJoint(Joint).WorldAnchor := AValue;
  end;

  if Joint is TCastleSliderJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleSliderJoint(Joint).WorldAnchor, AValue) then
      TCastleSliderJoint(Joint).WorldAnchor := AValue;
  end;
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
end;

function TTemporaryJointWorldAnchor.GetObservedValue: TVector3;
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
    Exit(TCastlePulleyJoint(Joint).WorldAnchor);

  if Joint is TCastleSliderJoint then
    Exit(TCastleSliderJoint(Joint).WorldAnchor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}

  Result := TVector3.Zero;
end;

constructor TTemporaryJointWorldAnchor.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Red;
end;

{ TTemporaryJointConnectedWorldAnchor ---------------------------------------- }

procedure TTemporaryJointConnectedWorldAnchor.SetObservedValue(const AValue: TVector3
  );
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastlePulleyJoint(Joint).ConnectedWorldAnchor, AValue) then
      TCastlePulleyJoint(Joint).ConnectedWorldAnchor := AValue;
  end;
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
end;

function TTemporaryJointConnectedWorldAnchor.GetObservedValue: TVector3;
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
    Exit(TCastlePulleyJoint(Joint).ConnectedWorldAnchor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}

  Result := TVector3.Zero;
end;

constructor TTemporaryJointConnectedWorldAnchor.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Green;
end;


{ TTemporaryJointWorldGroundAnchor ------------------------------------------- }

procedure TTemporaryJointWorldGroundAnchor.SetObservedValue(const AValue: TVector3);
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastlePulleyJoint(Joint).WorldGroundAnchor, AValue) then
      TCastlePulleyJoint(Joint).WorldGroundAnchor := AValue;
  end;
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
end;

function TTemporaryJointWorldGroundAnchor.GetObservedValue: TVector3;
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
    Exit(TCastlePulleyJoint(Joint).WorldGroundAnchor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}

  Result := TVector3.Zero;
end;

constructor TTemporaryJointWorldGroundAnchor.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Orange;
end;

{ TTemporaryJointConnectedWorldGroundAnchor ---------------------------------- }

procedure TTemporaryJointConnectedWorldGroundAnchor.SetObservedValue(
  const AValue: TVector3);
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastlePulleyJoint(Joint).ConnectedWorldGroundAnchor, AValue) then
      TCastlePulleyJoint(Joint).ConnectedWorldGroundAnchor := AValue;
  end;
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
end;

function TTemporaryJointConnectedWorldGroundAnchor.GetObservedValue: TVector3;
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
    Exit(TCastlePulleyJoint(Joint).ConnectedWorldGroundAnchor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}

  Result := TVector3.Zero;
end;

constructor TTemporaryJointConnectedWorldGroundAnchor.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Teal;
end;

end.
