{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijański.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TApplyImpulseBehavior }
unit ApplyImpulseBehavior;

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, CastleClassUtils, AbstractTimeDurationBehavior;

type
  TApplyImpulseBehavior = class(TAbstractTimeDurationBehavior)
  private
    FImpulse: TVector3;
    FPosition: TVector3;

    FImpulsePersistent: TCastleVector3Persistent;
    FPositionPersistent: TCastleVector3Persistent;

    function GetImpulseForPersistent: TVector3;
    procedure SetImpulseForPersistent(const AValue: TVector3);
    function GetPositionForPersistent: TVector3;
    procedure SetPositionForPersistent(const AValue: TVector3);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Impulse: TVector3 read FImpulse write FImpulse;
    property Position: TVector3 read FPosition write FPosition;
  published
    property ImpulsePersistent: TCastleVector3Persistent read FImpulsePersistent;
    property PositionPersistent: TCastleVector3Persistent read FPositionPersistent;
  end;

implementation

{ TApplyImpulseBehavior ------------------------------------------------------ }

function TApplyImpulseBehavior.GetImpulseForPersistent: TVector3;
begin
  Result := Impulse;
end;

procedure TApplyImpulseBehavior.SetImpulseForPersistent(const AValue: TVector3);
begin
  Impulse := AValue;
end;

function TApplyImpulseBehavior.GetPositionForPersistent: TVector3;
begin
  Result := Position;
end;

procedure TApplyImpulseBehavior.SetPositionForPersistent(const AValue: TVector3);
begin
  Position := AValue;
end;

constructor TApplyImpulseBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImpulsePersistent := TCastleVector3Persistent.Create;
  FImpulsePersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetImpulseForPersistent;
  FImpulsePersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetImpulseForPersistent;
  FImpulsePersistent.InternalDefaultValue := Impulse; // current value is default

  FPositionPersistent := TCastleVector3Persistent.Create;
  FPositionPersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetPositionForPersistent;
  FPositionPersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetPositionForPersistent;
  FPositionPersistent.InternalDefaultValue := Position; // current value is default
end;

destructor TApplyImpulseBehavior.Destroy;
begin
  FreeAndNil(FImpulsePersistent);
  FreeAndNil(FPositionPersistent);
  inherited;
end;

procedure TApplyImpulseBehavior.Update(const SecondsPassed: Single;
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
    RigidBody.ApplyImpulse(Impulse, Position);
    RigidBody.WakeUp;
  end;
end;

function TApplyImpulseBehavior.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'Impulse') or
     (PropertyName = 'Position') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization

RegisterSerializableComponent(TApplyImpulseBehavior, 'Apply Impulse Behavior');

end.

