unit AddForceBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, AbstractTimeDurationBehavior;

type
  TAddForceBehavior = class (TAbstractTimeDurationBehavior)
  private
    FForce: TVector3;
    FPosition: TVector3;

    FForcePersistent: TCastleVector3Persistent;
    FPositionPersistent: TCastleVector3Persistent;

    function GetForceForPersistent: TVector3;
    procedure SetForceForPersistent(const AValue: TVector3);
    function GetPositionForPersistent: TVector3;
    procedure SetPositionForPersistent(const AValue: TVector3);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    property Force: TVector3 read FForce write FForce;
    property Position: TVector3 read FPosition write FPosition;
  published
    property ForcePersistent: TCastleVector3Persistent read FForcePersistent;
    property PositionPersistent: TCastleVector3Persistent read FPositionPersistent;
  end;

implementation

{ TAddForceBehavior }

function TAddForceBehavior.GetForceForPersistent: TVector3;
begin
  Result := Force;
end;

procedure TAddForceBehavior.SetForceForPersistent(const AValue: TVector3);
begin
  Force := AValue;
end;

function TAddForceBehavior.GetPositionForPersistent: TVector3;
begin
  Result := Position;
end;

procedure TAddForceBehavior.SetPositionForPersistent(const AValue: TVector3);
begin
  Position := AValue;
end;

constructor TAddForceBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FForcePersistent := TCastleVector3Persistent.Create;
  FForcePersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetForceForPersistent;
  FForcePersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetForceForPersistent;
  FForcePersistent.InternalDefaultValue := Force; // current value is default

  FPositionPersistent := TCastleVector3Persistent.Create;
  FPositionPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetPositionForPersistent;
  FPositionPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetPositionForPersistent;
  FPositionPersistent.InternalDefaultValue := Position; // current value is default
end;

destructor TAddForceBehavior.Destroy;
begin
  FreeAndNil(FForcePersistent);
  FreeAndNil(FPositionPersistent);
  inherited;
end;

procedure TAddForceBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  RigidBody: TCastleRigidBody;
begin
  inherited Update(SecondsPassed, RemoveMe);

  if not ShouldUpdate then
    Exit;

  RigidBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RigidBody <> nil then
  begin
    RigidBody.AddForce(Force, Position);
    RigidBody.WakeUp;
  end;
end;

initialization

RegisterSerializableComponent(TAddForceBehavior, 'Add Force Behavior');

end.

