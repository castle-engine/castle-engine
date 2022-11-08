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
  CastleVectors, CastleScene, CastleClassUtils, CastleUtils;

type
  { Ancestor for design-time tools created by behaviors to manipulate e.g. joints. }
  TDesignTransform = class(TCastleTransform);

  { Ancestor for design-time tools created by physics joints. }
  TDesignJointTransform = class(TDesignTransform)
  strict private
    FColor: TCastleColor;
    FJoint: TCastleAbstractJoint;
    procedure SetColor(const Value: TCastleColor);
  protected
    FSphere: TCastleSphere;

    // For now it's actually more natural to just *not* adjust sphere size
    //function EstimateSphereRadius: Single;

    procedure SetValue(const AValue: TVector3);
    procedure SetObservedValue(const AValue: TVector3); virtual; abstract;
    function GetObservedValue: TVector3; virtual; abstract;
    procedure CheckTransformInsideParent;
    procedure ChangedTransform(const UpdatePhysicsTransform: Boolean = true); override;
  public
    constructor Create(AOwner: TComponent;
      const AJoint: TCastleAbstractJoint); reintroduce; virtual;
    destructor Destroy; override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Value: TVector3 read GetObservedValue write SetValue;
    property Joint: TCastleAbstractJoint read FJoint;
    property Color: TCastleColor read FColor write SetColor;
  end;

  TDesignJointAnchor = class(TDesignJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  end;

  TDesignJointConnectedAnchor = class(TDesignJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TCastleAbstractJoint); override;
  end;

  TDesignJointWorldPoint = class(TDesignJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TCastleAbstractJoint); override;
  end;

  TDesignJointWorldAnchor = class(TDesignJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TCastleAbstractJoint); override;
  end;

  TDesignJointConnectedWorldAnchor = class(TDesignJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TCastleAbstractJoint); override;
  end;

  TDesignJointWorldGroundAnchor = class(TDesignJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TCastleAbstractJoint); override;
  end;

  TDesignJointConnectedWorldGroundAnchor = class(TDesignJointTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;
  public
    constructor Create(AOwner: TComponent; const AJoint: TCastleAbstractJoint); override;
  end;

implementation

uses CastleRenderContext;

{ TDesignJointTransform --------------------------------------------------- }

constructor TDesignJointTransform.Create(AOwner: TComponent;
  const AJoint: TCastleAbstractJoint);
begin
  inherited Create(AOwner);

  FColor := Red;
  FJoint := AJoint;
  SetTransient;

  FSphere := TCastleSphere.Create(nil);
  FSphere.RenderLayer := rlFront;
  FSphere.SetTransient;
  FSphere.Color := FColor;
  FSphere.Material := pmUnlit;
  FSphere.Radius := 0.1;
  Add(FSphere);
end;

destructor TDesignJointTransform.Destroy;
begin
  FreeAndNil(FSphere);
  inherited Destroy;
end;

procedure TDesignJointTransform.Update(const SecondsPassed: Single;
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

function TDesignJointTransform.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'TranslationPersistent'
    ]) then
    Result := [psBasic, psLayout]
  else
    Result := [];
end;

procedure TDesignJointTransform.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    if FSphere <> nil then
      FSphere.Color := FColor;
  end;
end;

(*
function TDesignJointTransform.EstimateSphereRadius: Single;
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

procedure TDesignJointTransform.SetValue(const AValue: TVector3);
begin
  SetObservedValue(AValue);
  if not TVector3.PerfectlyEquals(Translation, AValue) then
    Translation := AValue;
end;

procedure TDesignJointTransform.CheckTransformInsideParent;
begin
  if Parent = nil then
    Exit;
end;

procedure TDesignJointTransform.ChangedTransform(const UpdatePhysicsTransform: Boolean);
begin
  inherited;
  if not TVector3.PerfectlyEquals(Translation, Value) then
    Value := Translation;
end;

{ TDesignJointAnchor ----------------------------------------------------------- }

procedure TDesignJointAnchor.SetObservedValue(const AValue: TVector3);
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

function TDesignJointAnchor.GetObservedValue: TVector3;
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

{ TDesignJointConnectedAnchor --------------------------------------------- }

constructor TDesignJointConnectedAnchor.Create(AOwner: TComponent;
  const AJoint: TCastleAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Green;
end;

procedure TDesignJointConnectedAnchor.SetObservedValue(const AValue: TVector3);
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

function TDesignJointConnectedAnchor.GetObservedValue: TVector3;
begin
  if Joint is TCastleRopeJoint then
    Exit(TCastleRopeJoint(Joint).ConnectedAnchor);
end;

{ TDesignJointWorldPoint -------------------------------------------------- }

constructor TDesignJointWorldPoint.Create(AOwner: TComponent;
  const AJoint: TCastleAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Blue;
end;

procedure TDesignJointWorldPoint.SetObservedValue(const AValue: TVector3);
begin
  if Joint is TCastleGrabJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastleGrabJoint(Joint).WorldPoint, AValue) then
      TCastleGrabJoint(Joint).WorldPoint := AValue;
  end;
end;

function TDesignJointWorldPoint.GetObservedValue: TVector3;
begin
  if Joint is TCastleGrabJoint then
    Exit(TCastleGrabJoint(Joint).WorldPoint);
end;

{ TDesignJointWorldAnchor ------------------------------------------------- }

constructor TDesignJointWorldAnchor.Create(AOwner: TComponent;
  const AJoint: TCastleAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Red;
end;

procedure TDesignJointWorldAnchor.SetObservedValue(const AValue: TVector3);
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

function TDesignJointWorldAnchor.GetObservedValue: TVector3;
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
    Exit(TCastlePulleyJoint(Joint).WorldAnchor);

  if Joint is TCastleSliderJoint then
    Exit(TCastleSliderJoint(Joint).WorldAnchor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}

  Result := TVector3.Zero;
end;

{ TDesignJointConnectedWorldAnchor ---------------------------------------- }

constructor TDesignJointConnectedWorldAnchor.Create(AOwner: TComponent;
  const AJoint: TCastleAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Green;
end;

procedure TDesignJointConnectedWorldAnchor.SetObservedValue(const AValue: TVector3
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

function TDesignJointConnectedWorldAnchor.GetObservedValue: TVector3;
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
    Exit(TCastlePulleyJoint(Joint).ConnectedWorldAnchor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}

  Result := TVector3.Zero;
end;

{ TDesignJointWorldGroundAnchor ------------------------------------------- }

constructor TDesignJointWorldGroundAnchor.Create(AOwner: TComponent;
  const AJoint: TCastleAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Orange;
end;

procedure TDesignJointWorldGroundAnchor.SetObservedValue(const AValue: TVector3);
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
  begin
    if not TVector3.PerfectlyEquals(TCastlePulleyJoint(Joint).WorldGroundAnchor, AValue) then
      TCastlePulleyJoint(Joint).WorldGroundAnchor := AValue;
  end;
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
end;

function TDesignJointWorldGroundAnchor.GetObservedValue: TVector3;
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
    Exit(TCastlePulleyJoint(Joint).WorldGroundAnchor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}

  Result := TVector3.Zero;
end;

{ TDesignJointConnectedWorldGroundAnchor ---------------------------------- }

constructor TDesignJointConnectedWorldGroundAnchor.Create(AOwner: TComponent;
  const AJoint: TCastleAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Teal;
end;

procedure TDesignJointConnectedWorldGroundAnchor.SetObservedValue(
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

function TDesignJointConnectedWorldGroundAnchor.GetObservedValue: TVector3;
begin
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if Joint is TCastlePulleyJoint then
    Exit(TCastlePulleyJoint(Joint).ConnectedWorldGroundAnchor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}

  Result := TVector3.Zero;
end;

end.
