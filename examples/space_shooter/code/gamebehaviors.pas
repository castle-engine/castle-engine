{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Small useful behaviors (TCastleBehavior descendants) used in this game. }
unit GameBehaviors;

interface

uses Classes,
  CastleVectors, CastleTransform;

type
  { Remove the parent when it goes outside of the possibly visible space. }
  TAutoRemoveBehavior = class(TCastleBehavior)
  public
    RemoveOwner: TComponent; //< Remove by freeing this
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  { Remove the parent when it goes outside of the @italic(left border)
    of possibly visible space.
    Compared to TAutoRemoveBehavior, this doesn't check other borders. }
  TAutoRemoveLeftBehavior = class(TCastleBehavior)
  public
    RemoveOwner: TComponent; //< Remove by freeing this
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  { Slowly rotate around a random axis.}
  TRotateBehavior = class(TCastleBehavior)
  private
    Axis: TVector3;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  { Behavior for rockets.
    You can add here any additional rocket-specific behavior.
    It is also useful to detect what collided with what in TRocksManager.RockCollisionEnter. }
  TRocketBehavior = class(TCastleBehavior)
  public
    RemoveOwner: TComponent; //< Remove by freeing this
    // Does the rocket explode when it hits something.
    Armed: Boolean;
  end;

  { Behavior for rocks.
    You can add here any additional rock-specific behavior.
    It is also useful to detect what collided with what in TRocksManager.RockCollisionEnter. }
  TRockBehavior = class(TCastleBehavior)
  public
    RemoveOwner: TComponent; //< Remove by freeing this
  end;

implementation

uses Math,
  CastleRectangles, CastleApplicationProperties;

{ TAutoRemoveBehavior -------------------------------------------------------- }

procedure TAutoRemoveBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
const
  { Rockets are removed when they go far away.
    This rectangle was chosen to be large enough to cover the whole screen.

    Vertically, it covers the screen for 100%:
    Height is 1200, for sure larger than Viewport.Camera.Orthographic.Height = 1000.

    Horizontally, it covers the screen as long as aspect ratio is <= 2:1.

    We *could* adjust it to Viewport.Camera.Orthographic.EffectiveRect
    sizes, and be more precise. But this would mean that game balance works
    a bit differently (rockets reach more or less further) depending on
    the window size aspect ratio. }
  RocketAllowedPositions: TFloatRectangle = (
    Left: -1000; Bottom: -600; Width: 2000; Height: 1200
  );
begin
  inherited;
  if not RocketAllowedPositions.Contains(Parent.WorldTranslation.XY) then
    ApplicationProperties.FreeDelayed(RemoveOwner);
end;

{ TAutoRemoveLeftBehavior -------------------------------------------------------- }

procedure TAutoRemoveLeftBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
begin
  inherited;
  if Parent.WorldTranslation.X < -1500 then
    ApplicationProperties.FreeDelayed(RemoveOwner);
end;

{ TRotateBehavior ------------------------------------------------------------ }

constructor TRotateBehavior.Create(AOwner: TComponent);

  { Good uniform random choice of direction.
    See CastleInternalSphereSampling . }
  function RandomHemispherePointConstXYZ: TVector3;
  var
    R1, R2, SqRoot: Single;
    C, S: Single;
  begin
    R1 := Random;
    R2 := Random;
    SinCos(2 * Pi * R1, S, C);
    SqRoot := Sqrt(1 - Sqr(R2));

    Result.X := C * SqRoot;
    Result.Y := S * SqRoot;
    Result.Z := R2;
  end;

begin
  inherited;
  Axis := RandomHemispherePointConstXYZ;
end;

procedure TRotateBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
const
  RotationSpeed = 0.5;
begin
  inherited;
  Parent.Rotation := Vector4(Axis, Parent.Rotation.W + RotationSpeed * SecondsPassed);
end;

end.
