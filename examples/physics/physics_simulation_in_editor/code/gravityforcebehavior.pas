unit GravityForceBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, AbstractTimeDurationBehavior;

type
  { Transform with this behavior attracts other rigid bodies in world. }
  TGravityForceBehavior = class (TAbstractTimeDurationBehavior)
  private
    FValue: Single;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  published
    property Value: Single read FValue write FValue;
  end;

implementation

{ TGravityForceBehavior }

constructor TGravityForceBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TGravityForceBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  Transform: TCastleTransform;
  RigidBody: TCastleRigidBody;
  I: Integer;
  Direction: TVector3;
begin
  inherited Update(SecondsPassed, RemoveMe);

  if not World.IsPhysicsRunning then
    Exit;

  if OneShot then
  begin
    if WasShot then
      Exit
    else
      Shot;
  end else
  if (not ShouldStart) or (ShouldStop) then
    Exit;

  for I := 0 to World.Count -1 do
  begin
    Transform := World.Items[I];

    if Transform = Parent then
      continue;

    RigidBody := Transform.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if RigidBody <> nil then
    begin
      Direction := Parent.LocalToWorld(Parent.Translation) - Transform.LocalToWorld(Transform.Translation);
      Direction := Direction.Normalize;
      RigidBody.AddForce(Direction * Value, Parent.LocalToWorld(Parent.Translation));
      RigidBody.WakeUp;
    end;
  end;
end;

initialization
  RegisterSerializableComponent(TGravityForceBehavior, 'Gravity Force Behavior');

end.

