unit WindForceBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, CastleClassUtils, AbstractTimeDurationBehavior;

type
  { Add this behavior to CastleTransform }
  TWindForceBehavior = class (TAbstractTimeDurationBehavior)
  private
    FValue: Single;

  public
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Value: Single read FValue write FValue;
  end;



implementation

{ TWindForceBehavior --------------------------------------------------------- }

procedure TWindForceBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  Transform: TCastleTransform;
  RigidBody: TCastleRigidBody;
  I: Integer;
  Direction: TVector3;
begin
  inherited Update(SecondsPassed, RemoveMe);

  if not ShouldUpdate then
    Exit;

  for I := 0 to World.Count -1 do
  begin
    Transform := World.Items[I];

    if Transform = Parent then
      continue;

    if not Transform.ExistsInRoot then
      continue;

    RigidBody := Transform.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if (RigidBody <> nil) and (RigidBody.ExistsInRoot) then
    begin
      Direction := Vector3(0,0,0) - Parent.LocalToWorld(Parent.Translation);
      Direction := Direction.Normalize;
      RigidBody.AddForce(Direction * Value, Parent.LocalToWorld(Parent.Translation));
      RigidBody.WakeUp;
    end;
  end;
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

