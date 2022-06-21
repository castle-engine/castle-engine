unit GravityForceBehavior;

{$ifdef FPC}{$mode ObjFPC}{$H+}{$endif}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, CastleClassUtils, AbstractIterateRigidBodyBehavior;

type
  { Transform with this behavior attracts other rigid bodies in world. }
  TGravityForceBehavior = class (TAbstractIterateRigidBodyBehavior)
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
  RigidBody.AddForce(Direction * Value, Parent.LocalToWorld(Parent.Translation));
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

