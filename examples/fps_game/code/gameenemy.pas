{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Enemy behavior. }
unit GameEnemy;

interface

uses Classes, Generics.Collections,
  CastleVectors, CastleScene, CastleTransform;

type
  { Enemy intelligence. }
  TEnemy = class(TCastleBehavior)
  strict private
    Scene: TCastleScene;
    Dead: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentAfterAttach; override;
    //procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure Hurt;
  end;

  TEnemyList = {$ifdef FPC}specialize{$endif} TObjectList<TEnemy>;

implementation

uses CastleComponentSerialize;

constructor TEnemy.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TEnemy.ParentAfterAttach;
begin
  inherited;
  Scene := Parent as TCastleScene; // TEnemy can only be added as behavior to TCastleScene
  Scene.PlayAnimation('Idle', true);
end;

procedure TEnemy.Hurt;
begin
  Scene.PlayAnimation('Die', false);
  // dead corpse no longer collides
  Scene.Pickable := false;
  Scene.Collides := false;
  Dead := true;
end;

initialization
  RegisterSerializableComponent(TEnemy, 'Enemy');
end.
