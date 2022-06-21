unit WindForceBehavior;

{$ifdef FPC}{$mode ObjFPC}{$H+}{$endif}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, CastleClassUtils, AbstractTimeDurationBehavior,
  AbstractIterateRigidBodyBehavior;

type
  { Add this behavior to CastleTransform }
  TWindForceBehavior = class (TAbstractIterateRigidBodyBehavior)
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

{ TWindForceBehavior --------------------------------------------------------- }

procedure TWindForceBehavior.UpdateRigidBody(const RigidBody: TCastleRigidBody;
  const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  Direction: TVector3;
begin
  Direction := Vector3(0,0,0) - Parent.LocalToWorld(Parent.Translation);
  Direction := Direction.Normalize;
  RigidBody.AddForce(Direction * Value, Parent.LocalToWorld(Parent.Translation));
  RigidBody.WakeUp;
end;

function TWindForceBehavior.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'Value') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TWindForceBehavior, 'Wind Force Behavior');


end.

