unit AddTorqueBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, AbstractTimeDurationBehavior;

type
  TAddTorqueBehavior = class (TAbstractTimeDurationBehavior)
  private
    FTorque: TVector3;

    FTorquePersistent: TCastleVector3Persistent;

    function GetTorqueForPersistent: TVector3;
    procedure SetTorqueForPersistent(const AValue: TVector3);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    property Torque: TVector3 read FTorque write FTorque;
  published
    property TorquePersistent: TCastleVector3Persistent read FTorquePersistent;
  end;

implementation

{ TAddTorqueBehavior --------------------------------------------------------- }

function TAddTorqueBehavior.GetTorqueForPersistent: TVector3;
begin
  Result := Torque;
end;

procedure TAddTorqueBehavior.SetTorqueForPersistent(const AValue: TVector3);
begin
  Torque := AValue;
end;

constructor TAddTorqueBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTorquePersistent := TCastleVector3Persistent.Create;
  FTorquePersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetTorqueForPersistent;
  FTorquePersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetTorqueForPersistent;
  FTorquePersistent.InternalDefaultValue := Torque; // current value is default
end;

destructor TAddTorqueBehavior.Destroy;
begin
  FreeAndNil(FTorquePersistent);
  inherited;
end;

procedure TAddTorqueBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  RigidBody: TCastleRigidBody;
begin
  inherited Update(SecondsPassed, RemoveMe);

  if not ShouldUpdate then
    Exit;

  RigidBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if (RigidBody <> nil) and (RigidBody.ExistsInRoot) then
  begin
    RigidBody.AddTorque(Torque);
    RigidBody.WakeUp;
  end;
end;

initialization

RegisterSerializableComponent(TAddTorqueBehavior, 'Add Torque Behavior');

end.

