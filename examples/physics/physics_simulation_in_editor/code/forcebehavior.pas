unit ForceBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, AbstractTimeDurationBehavior;

type
  { Add this behavior to another body and select body you want to use it }
  TForceBehavior = class (TAbstractTimeDurationBehavior)
  private
    FValue: Single;
    FTarget: TCastleTransform;

  public
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  published
    property Target: TCastleTransform read FTarget write FTarget;
    property Value: Single read FValue write FValue;
  end;

implementation

{ TForceBehavior }

procedure TForceBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  RigidBody: TCastleRigidBody;
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

  if FTarget = nil then
    Exit;

  RigidBody := FTarget.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RigidBody <> nil then
  begin
    Direction := FTarget.LocalToWorld(FTarget.Translation) - Parent.LocalToWorld(Parent.Translation);
    Direction := Direction.Normalize;
    RigidBody.AddForce(Direction * Value, Parent.LocalToWorld(Parent.Translation));
    RigidBody.WakeUp;
  end;
end;

initialization
  RegisterSerializableComponent(TForceBehavior, 'Force Behavior');

end.

