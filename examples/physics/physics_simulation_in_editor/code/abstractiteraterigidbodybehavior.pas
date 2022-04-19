unit AbstractIterateRigidBodyBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, AbstractTimeDurationBehavior;

type
  { Inherite from this class if you want update all other rigid bodies in world.
    It searches for TCastleRigidBody in world items list and its childern if
    castle transform doesn't have rigid body. All you need to do is
    override UpdateRigidBody(). }
  TAbstractIterateRigidBodyBehavior = class (TAbstractTimeDurationBehavior)
  private
    { Check for rigid bodies in transform children and call on it
      UpdateRigidBody method. }
    procedure UpdateTransformChildren(const ParentTransform: TCastleTransform; const SecondsPassed: Single;
      var RemoveMe: TRemoveType);
  protected
    { Override it to update found rigid bodies. }
    procedure UpdateRigidBody(const RigidBody: TCastleRigidBody; const SecondsPassed: Single;
      var RemoveMe: TRemoveType) virtual; abstract;
  public
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

implementation

{ TAbstractIterateRigidBodyBehavior }

procedure TAbstractIterateRigidBodyBehavior.UpdateTransformChildren(
  const ParentTransform: TCastleTransform; const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  I: Integer;
  Transform: TCastleTransform;
  RigidBody: TCastleRigidBody;
begin
  for I := 0 to ParentTransform.Count -1 do
  begin
    Transform := ParentTransform.Items[I];

    if Transform = Parent then
      continue;

    if not Transform.ExistsInRoot then
      continue;

    RigidBody := Transform.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if (RigidBody <> nil) and (RigidBody.ExistsInRoot) then
      UpdateRigidBody(RigidBody, SecondsPassed, RemoveMe)
    else
      UpdateTransformChildren(Transform, SecondsPassed, RemoveMe);
  end;
end;

procedure TAbstractIterateRigidBodyBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  I: Integer;
  Transform: TCastleTransform;
  RigidBody: TCastleRigidBody;
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
      UpdateRigidBody(RigidBody, SecondsPassed, RemoveMe)
    else
      UpdateTransformChildren(Transform, SecondsPassed, RemoveMe);
  end;
end;

end.

