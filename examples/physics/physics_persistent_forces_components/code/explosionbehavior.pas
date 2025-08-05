{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijański.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TExplosionBehavior }
unit ExplosionBehavior;

interface

uses Classes, SysUtils,
  CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, CastleClassUtils, CastleUtils,
  AbstractIterateRigidBodyBehavior;

type
  { Add this behavior to CastleTransform }
  TExplosionBehavior = class(TAbstractIterateRigidBodyBehavior)
  private
    FValue: Single;

  protected
    { Updates all found rigid bodies. }
    procedure UpdateRigidBody(const RigidBody: TCastleRigidBody;
      const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

  public
    function PropertySections(const PropertyName: String): TPropertySections; override;

  published
    property Value: Single read FValue write FValue;
  end;



implementation

{ TExplosionBehavior --------------------------------------------------------- }

procedure TExplosionBehavior.UpdateRigidBody(const RigidBody: TCastleRigidBody;
  const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  Direction: TVector3;
  Transform: TCastleTransform;
begin
  Transform := RigidBody.Parent;
  { TODO: Parent.LocalToWorld(Parent.Translation) is wrong -
    Parent.Translation is in Parent.Parent coord system. }
  Direction := Transform.LocalToWorld(Transform.Translation) - Parent.LocalToWorld(Parent.Translation);
  Direction := Direction.Normalize;
  RigidBody.AddForceAtPosition(Direction * Value, Parent.LocalToWorld(Parent.Translation));
  RigidBody.WakeUp;
end;

function TExplosionBehavior.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Value'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TExplosionBehavior, 'Explosion Behavior');


end.

