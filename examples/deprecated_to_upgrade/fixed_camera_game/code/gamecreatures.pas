{
  Copyright 2007-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Creatures and (as a special creature descendant) player. }
unit GameCreatures;

interface

uses SysUtils, Classes, Generics.Collections,
  CastleUtils, CastleClassUtils, CastleScene,
  CastleVectors, CastleTransform, CastleFrustum, CastleApplicationProperties,
  CastleTimeUtils, X3DNodes, CastleColors, CastleDebugTransform,
  CastleComponentSerialize;

type
  TCreatureState = (csIdle, csBored, csWalk);

type
  TCreatureKind = class
  private
    FName: string;
    Template: TSerializedComponent;
    Url: String;
    Loaded: boolean;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    { Create things necessary for playing (displaying this creature). }
    procedure Load(const PrepareParams: TPrepareParams);

    { Creature short name, must be simple (needs to be valid XML element
      name and X3D node name, for easy data reading). }
    property Name: string read FName;

    { Loads creature properties from GameConfig file.
      This is normally called by constructor, so you don't have to call it.
      However, it may be useful for debugging purposes to "reload index.xml"
      file, and then you should call this (and probably you should reload
      this creature kind, if it's already loaded; unless you know what
      you're doing :) ). }
    procedure LoadFromConfig;
  end;

  TCreatureKindList = class({$ifdef FPC}specialize{$endif} TObjectList<TCreatureKind>)
  public
    PlayerKind: TCreatureKind;

    { Create creatures and set their parameters from GameConfig. }
    constructor Create;
  end;

  TCreature = class(TCastleTransform)
  private
    FKind: TCreatureKind;
    FDebugTransform: TDebugTransform;
    FState: TCreatureState;
    LifeTime: TFloatTime;
    Scene: TCastleScene;
    procedure SetState(const Value: TCreatureState);
  public
    constructor Create(AKind: TCreatureKind); reintroduce;
    property Kind: TCreatureKind read FKind;
    property State: TCreatureState read FState write SetState;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TPlayer = class(TCreature)
  private
    { This is set and used only if csWalk }
    WantsToWalkPos, WantsToWalkDir: TVector3;
    FTargetVisualize: TTransformNode;
    FTargetVisualizeShape: TShapeNode;
  public
    constructor Create(AKind: TCreatureKind);
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure LocationChanged;
    procedure WantsToWalk(const Value: TVector3);
  end;

var
  CreatureKinds: TCreatureKindList;

implementation

uses Math,
  CastleLog, CastleGLUtils, CastleUIControls, CastleSceneCore,
  GameConfiguration, CastleViewport;

const
  Debug = true;

  CreatureAnimationName: array [TCreatureState] of string =
  ( 'idle', 'bored', 'walk' );

{ TCreatureKind -------------------------------------------------------------- }

constructor TCreatureKind.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  LoadFromConfig;
end;

destructor TCreatureKind.Destroy;
begin
  FreeAndNil(Template);
  inherited;
end;

procedure TCreatureKind.LoadFromConfig;
begin
  Url := GameConfig.GetURL('creatures/' + Name + '/url');
end;

procedure TCreatureKind.Load(const PrepareParams: TPrepareParams);
begin
  if Loaded then Exit;
  Loaded := true;
  Template := TSerializedComponent.Create(Url);
end;

{ TCreature ------------------------------------------------------------------ }

constructor TCreature.Create(AKind: TCreatureKind);
begin
  inherited Create(nil);

  { initialize state fields }
  FState := csIdle;
  LifeTime := 0;

  FKind := AKind;

  FDebugTransform := TDebugTransform.Create(Self);
  FDebugTransform.Parent := Self;

  Scene := AKind.Template.TransformLoad(Self) as TCastleScene;
  Scene.AutoAnimation := CreatureAnimationName[FState];
  Add(Scene);
end;

procedure TCreature.SetState(const Value: TCreatureState);
begin
  if FState <> Value then
  begin
    FState := Value;
    Scene.AutoAnimation := CreatureAnimationName[FState];
  end;
end;

procedure TCreature.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  // TODO: Switch to csBored (and back to csIdle) from time to time
  LifeTime := LifeTime + SecondsPassed;
  FDebugTransform.Exists := Debug;
end;

{ TCreatureKindList ---------------------------------------------------------- }

constructor TCreatureKindList.Create;
begin
  inherited Create(true);
  PlayerKind := TCreatureKind.Create('player');
  Add(PlayerKind);
end;

{ TPlayer -------------------------------------------------------------------- }

constructor TPlayer.Create(AKind: TCreatureKind);

  procedure CreateTargetVisualize;
  var
    Sphere: TSphereNode;
    Material: TMaterialNode;
  begin
    Sphere := TSphereNode.Create;
    Sphere.Radius := 0.1;

    Material := TMaterialNode.Create;
    Material.DiffuseColor := RedRGB;

    FTargetVisualizeShape := TShapeNode.Create;
    FTargetVisualizeShape.Material := Material;
    FTargetVisualizeShape.Geometry := Sphere;

    FTargetVisualize := TTransformNode.Create;
    FTargetVisualize.AddChildren(FTargetVisualizeShape);
  end;

begin
  inherited;
  CreateTargetVisualize;
  FDebugTransform.ParentSpace.AddChildren(FTargetVisualize);
  FDebugTransform.ChangedScene;
end;

destructor TPlayer.Destroy;
begin
  inherited;
end;

procedure TPlayer.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  RotationAxis: TVector3;
  AngleToTarget: Single;

  procedure Rotate;
  const
    RotationSpeed = 10.0; //< radians per second
  var
    AngleRotate: Single;
  begin
    { first adjust direction }
    AngleRotate := RotationSpeed * SecondsPassed;
    if AngleToTarget < 0 then
      AngleRotate := Max(-AngleRotate, AngleToTarget) else
      AngleRotate := Min( AngleRotate, AngleToTarget);
    Direction := RotatePointAroundAxisRad(AngleRotate, Direction, RotationAxis);
  end;

  procedure Move;
  const
    MoveSpeed = 1;
  var
    MoveDirectionCurrent, MoveDirectionMax: TVector3;
    MoveDirectionCurrentScale: Single;
  begin
    { since Position <> WantsToWalkPos, we know that
      MoveDirectionMax is non-zero }
    MoveDirectionMax := WantsToWalkPos - Translation;
    MoveDirectionCurrentScale := MoveSpeed * SecondsPassed / MoveDirectionMax.Length;
    if MoveDirectionCurrentScale >= 1.0 then
    begin
      { This means that
          Translation + MoveDirectionMax * MoveDirectionCurrentScale
        would actually get us too far. So we instead just go to target and
        stop there. }
      Translation := WantsToWalkPos;
      State := csIdle;
    end else
    begin
      MoveDirectionCurrent := MoveDirectionMax * MoveDirectionCurrentScale;
      Translation := Translation + MoveDirectionCurrent;
    end;
  end;

var
  IsTargetPos, IsTargetDir: boolean;
begin
  if State = csWalk then
  begin
    { Do walking. If walking ends (because target reached, or can't reach target),
      change to csIdle. }

    { TODO: check collisions with the scene before changing Translation }

    IsTargetPos := TVector3.Equals(Translation, WantsToWalkPos);
    IsTargetDir := TVector3.Equals(Direction, WantsToWalkDir);

    if not IsTargetDir then
    begin
      { Calculate RotationAxis and AngleToTarget used by Rotate.
        By the way, compare Direction and WantsToWalkDir with more tolerance,
        so possibly IsTargetDir changes to true. }
      RotationAxis := //TVector3.CrossProduct(Direction, WantsToWalkDir);
        Vector3(0, 1, 0);
      AngleToTarget := RotationAngleRadBetweenVectors(Direction, WantsToWalkDir, RotationAxis);
      if Abs(AngleToTarget) < 0.01 then
        IsTargetDir := true;
    end;

    if IsTargetPos and IsTargetDir then
      State := csIdle
    else
    if not IsTargetDir then
      Rotate
    else
      Move;
  end;

  FTargetVisualizeShape.Render := Debug and (State = csWalk);
  FTargetVisualize.Translation := WantsToWalkPos;

  { Update children 3D objects *after* updating our own transformation,
    this way they are always synchronized when displaying.
    Otherwise, the FTargetVisualize would have a bit of "shaking",
    because the TDebugTransform transformation would be updated with 1-frame delay. }
  inherited;
end;

procedure TPlayer.LocationChanged;
begin
  State := csIdle;
end;

procedure TPlayer.WantsToWalk(const Value: TVector3);
begin
  WantsToWalkPos := Value;
  WantsToWalkDir := WantsToWalkPos - Translation;
  WantsToWalkDir.Y := 0; // do not move in Y axis, only move horizontally
  WantsToWalkDir := WantsToWalkDir.Normalize;
  State := csWalk;
end;

initialization
finalization
  FreeAndNil(CreatureKinds);
end.
