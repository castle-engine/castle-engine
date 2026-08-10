{
  Copyright 2024-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Component to manage explosions. }
unit GameExplosionsManager;

interface

uses Classes,
  CastleVectors, CastleTransform, CastleUiControls, CastleComponentSerialize,
  CastleControls, CastleTimeUtils, CastleViewport, CastleSceneCore,
  X3DNodes, CastleScene;

type
  { Manage explosions. }
  TExplosionsManager = class(TCastleUserInterface)
  strict private
    Factory: TCastleComponentFactory;
    procedure AnimationStopNotification(const Scene: TCastleSceneCore;
      const Animation: TTimeSensorNode);
  public
    { Set this to the parent of all explosions.
      Should not be transformed, because (for simplicity) code here assumes
      that translations/rotations inside this correspond to
      the translations/rotations in the world coordinates of the viewport. }
    ExplosionsParent: TCastleTransform;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;

    { Make sure resources, like the texture, are initialized.
      Otherwise the first creation could cause a noticeable stutter
      when spawning. }
    procedure InitializeResources(const ParentViewport: TCastleViewport);

    { Add and start animating new explosion at given position. }
    procedure Spawn(const Position: TVector3);
  end;

implementation

uses SysUtils,
  CastleUtils, CastleRectangles, CastleApplicationProperties,
  GameBehaviors;

constructor TExplosionsManager.Create(AOwner: TComponent);
begin
  inherited;
  Factory := TCastleComponentFactory.Create(Self);
  Factory.Url := 'castle-data:/explosion/explosion.castle-transform';
end;

procedure TExplosionsManager.InitializeResources(const ParentViewport: TCastleViewport);
var
  TestInstance: TCastleTransform;
begin
  TestInstance := Factory.ComponentLoad(Self) as TCastleTransform;
  ParentViewport.PrepareResources(TestInstance);
  // FreeAndNil(TestInstance) // do not free it, let it exist, to hold reference count to the texture
end;

procedure TExplosionsManager.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  // nothing to do
end;

procedure TExplosionsManager.Spawn(const Position: TVector3);
var
  Explosion: TCastleTransform;
  ExplosionOwner: TComponent;
  ExplosionScene: TCastleScene;
  AnimationParams: TPlayAnimationParameters;
  ExplosionMove: TMoveBehavior;
begin
  ExplosionOwner := TComponent.Create(Self);
  Explosion := Factory.ComponentLoad(ExplosionOwner) as TCastleTransform;
  Explosion.Translation := Position;
  ExplosionsParent.Add(Explosion);

  ExplosionMove := TMoveBehavior.Create(ExplosionOwner);
  ExplosionMove.MoveSpeed := Vector3(-500, 0, 0); // just like rocks
  Explosion.AddBehavior(ExplosionMove);

  { For now, for simplicity, this just assumes that translation design
    has TCastleScene scene as top-level component. }
  ExplosionScene := Explosion as TCastleScene;
  AnimationParams := TPlayAnimationParameters.Create;
  try
    AnimationParams.Name := 'explode';
    AnimationParams.Loop := false;
    AnimationParams.StopNotification := {$ifdef FPC}@{$endif} AnimationStopNotification;
    ExplosionScene.PlayAnimation(AnimationParams);
  finally FreeAndNil(AnimationParams) end;
end;

procedure TExplosionsManager.AnimationStopNotification(const Scene: TCastleSceneCore;
  const Animation: TTimeSensorNode);
begin
  { Free the explosion when the animation finishes,
    we don't need the TCastleScene anymore.
    Free Scene.Owner, which is always ExplosionOwner created by
    TExplosionsManager.Spawn in this case. }
  ApplicationProperties.FreeDelayed(Scene.Owner);
end;

end.