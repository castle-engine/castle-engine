unit AddVelocityBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, AbstractTimeDurationBehavior;

type
  TAddVelocityBehavior = class (TAbstractTimeDurationBehavior)
  private
    FDeltaVelocity: TVector3;
    FDeltaVelocityPersistent: TCastleVector3Persistent;
    function GetDeltaVelocityForPersistent: TVector3;
    procedure SetDeltaVelocity(const AValue: TVector3);
    procedure SetDeltaVelocityForPersistent(const AValue: TVector3);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    property DeltaVelocity: TVector3 read FDeltaVelocity write SetDeltaVelocity;
  published
    property DeltaVelocityPersistent: TCastleVector3Persistent read FDeltaVelocityPersistent;
  end;

implementation

{ TAddVelocityBehavior }

procedure TAddVelocityBehavior.SetDeltaVelocity(const AValue: TVector3);
begin
  FDeltaVelocity := AValue;
end;

function TAddVelocityBehavior.GetDeltaVelocityForPersistent: TVector3;
begin
  Result := DeltaVelocity;
end;

procedure TAddVelocityBehavior.SetDeltaVelocityForPersistent(
  const AValue: TVector3);
begin
  DeltaVelocity := AValue;
end;

constructor TAddVelocityBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDeltaVelocityPersistent := TCastleVector3Persistent.Create;
  FDeltaVelocityPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetDeltaVelocityForPersistent;
  FDeltaVelocityPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetDeltaVelocityForPersistent;
  FDeltaVelocityPersistent.InternalDefaultValue := DeltaVelocity; // current value is default

  StartTime := 0;
  DurationTime := 0;
end;

procedure TAddVelocityBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  RigidBody: TCastleRigidBody;
begin
  inherited Update(SecondsPassed, RemoveMe);

  if OneShot then
  begin
    if Shoted then
      Exit
    else
      Shot;
  end else
  if (not ShouldStart) or (ShouldStop) then
    Exit;

  RigidBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RigidBody <> nil then
  begin
    RigidBody.LinearVelocity := RigidBody.LinearVelocity + DeltaVelocity;
  end;
end;

initialization

  RegisterSerializableComponent(TAddVelocityBehavior, 'Add Velocity Behavior');

end.

