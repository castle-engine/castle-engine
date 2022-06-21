unit ExplosionBehavior;

{$ifdef FPC}{$mode ObjFPC}{$H+}{$endif}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, CastleClassUtils, AbstractIterateRigidBodyBehavior;

type
  { Add this behavior to CastleTransform }
  TExplosionBehavior = class (TAbstractIterateRigidBodyBehavior)
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
  Direction := Transform.LocalToWorld(Transform.Translation) - Parent.LocalToWorld(Parent.Translation);
  Direction := Direction.Normalize;
  RigidBody.AddForce(Direction * Value, Parent.LocalToWorld(Parent.Translation));
  RigidBody.WakeUp;
end;

function TExplosionBehavior.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if PropertyName = 'Value' then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TExplosionBehavior, 'Explosion Behavior');


end.

