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

{ TGravityForceBehavior }
unit GravityForceBehavior;

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, CastleClassUtils, AbstractIterateRigidBodyBehavior;

type
  { Transform with this behavior attracts other rigid bodies in world. }
  TGravityForceBehavior = class(TAbstractIterateRigidBodyBehavior)
  private
    FValue: Single;

  protected
    { Updates all found rigid bodies. }
    procedure UpdateRigidBody(const RigidBody: TCastleRigidBody;
      const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

  public
    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Value: Single read FValue write FValue;
  end;

implementation

uses CastleLog;

{ TGravityForceBehavior ------------------------------------------------------ }

procedure TGravityForceBehavior.UpdateRigidBody(
  const RigidBody: TCastleRigidBody; const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  Direction: TVector3;
  Transform: TCastleTransform;
begin
  Transform := RigidBody.Parent;
  Direction := Parent.LocalToWorld(Parent.Translation) - Transform.LocalToWorld(Transform.Translation);
  Direction := Direction.Normalize;
  RigidBody.AddForceAtPosition(Direction * Value, Parent.LocalToWorld(Parent.Translation));
  RigidBody.WakeUp;
end;

constructor TGravityForceBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TGravityForceBehavior.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if PropertyName = 'Value' then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TGravityForceBehavior, 'Gravity Force Behavior');

end.

