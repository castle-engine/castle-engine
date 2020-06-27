{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Enemy behaviour.

  This is an exact copy of enemy AI from standard CGE template "3D FPS game".
  It was not modified at all for the 3rd-person navigation demo. }
unit GameEnemy;

interface

uses Generics.Collections,
  CastleVectors, CastleScene, CastleTransform;

type
  { Simple enemy AI (Artificial Intellgence).
    It controls the given Scene (TCastleScene): moves it, runs animations of it etc.

    This object is a TCastleTransform descendant,
    and inserts itself as the first child of Scene when it is created.
    This way you can get the TEnemy instance of a TCastleScene,
    by taking "Scene.Items[0] as TEnemy".
    This is a relatively "clean" approach and extensible.
    By adding children to the Scene, that control this scene,
    you can make flexible systems (e.g. various behaviours could affect one scene).

    This is just a TCastleTransform without any collidable stuff,
    so it will not be detected by e.g. MainViewport.MouseRayHit.
    But it is possible to change this (you could add some TCastleScene here,
    loaded from files or constructed by code using X3D nodes,
    to add additional geometry to enemies).

    Other ways of making an association TCastleScene -> TEnemy logic are possible:

    - You could set TCastleScene.Tag to the instance of TEnemy.

    - You could make TEnemy a descendant of TCastleScene,
      and then in the CGE editor you would be able to insert TEnemy instance.
      This requires registering a custom component in CGE editor,
      https://github.com/castle-engine/castle-engine/blob/master/tools/castle-editor/README.md#include-custom-project-specific-components-in-the-visual-designer
  }
  TEnemy = class(TCastleTransform)
  public
    Scene: TCastleScene;
    MoveDirection: Integer; //< Always 1 or -1
    Dead: Boolean;
    constructor Create(const AScene: TCastleScene); reintroduce;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure Hurt;
  end;

  TEnemyList = specialize TObjectList<TEnemy>;

implementation

constructor TEnemy.Create(const AScene: TCastleScene);
begin
  { Using nil as Owner.
    The state's Enemies list "owns" instances of this class, it will take care of freeing us. }
  inherited Create(nil);

  MoveDirection := -1;

  Scene := AScene;
  Scene.PlayAnimation('walk', true);
  Scene.Add(Self);
end;

procedure TEnemy.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
const
  MovingSpeed = 2;
begin
  inherited;

  if Dead then Exit;

  // We modify the Z coordinate, responsible for enemy going forward
  Scene.Translation := Scene.Translation +
    Vector3(0, 0, MoveDirection * SecondsPassed * MovingSpeed);

  Scene.Direction := Vector3(0, 0, MoveDirection);

  // Toggle MoveDirection between 1 and -1
  if Scene.Translation.Z > 5 then
    MoveDirection := -1
  else
  if Scene.Translation.Z < -5 then
    MoveDirection := 1;
end;

procedure TEnemy.Hurt;
begin
  Scene.PlayAnimation('die', false);
  // dead corpse no longer collides
  Scene.Pickable := false;
  Scene.Collides := false;
  Dead := true;
end;

end.
