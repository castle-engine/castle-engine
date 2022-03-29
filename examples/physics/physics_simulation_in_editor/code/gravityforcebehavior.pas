unit GravityForceBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, AbstractTimeDurationBehavior;

type
  { Add this behavior to another body and select body you want to use it }
  TGravityForceBehavior = class (TAbstractTimeDurationBehavior)
  private
    FValue: Single;
    FTarget: TCastleTransform;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  published
    property Target: TCastleTransform read FTarget write FTarget;
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
  RigidBody: TCastleRigidBody;
  Direction: TVector3;
begin
  inherited Update(SecondsPassed, RemoveMe);

  if not World.IsPhysicsRunning then
    Exit;

  if OneShot then
  begin
    if Shoted then
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
    Direction := Parent.Translation - FTarget.Translation;
    Direction := Direction.Normalize;
    RigidBody.AddForce(Direction * Value, Parent.LocalToWorld(Parent.OutsideToLocal(Parent.Translation)));
    RigidBody.WakeUp;
  end;
end;

initialization
  RegisterSerializableComponent(TGravityForceBehavior, 'Gravity Force Behavior');

end.

