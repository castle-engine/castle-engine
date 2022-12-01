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

{ TAddVelocityBehavior }
unit AddVelocityBehavior;

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, CastleClassUtils, AbstractTimeDurationBehavior;

type
  TAddVelocityBehavior = class(TAbstractTimeDurationBehavior)
  private
    FDeltaVelocity: TVector3;
    FDeltaVelocityPersistent: TCastleVector3Persistent;
    function GetDeltaVelocityForPersistent: TVector3;
    procedure SetDeltaVelocityForPersistent(const AValue: TVector3);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    property DeltaVelocity: TVector3 read FDeltaVelocity write FDeltaVelocity;
  published
    property DeltaVelocityPersistent: TCastleVector3Persistent read FDeltaVelocityPersistent;
  end;

implementation

{ TAddVelocityBehavior ------------------------------------------------------- }

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

destructor TAddVelocityBehavior.Destroy;
begin
  FreeAndNil(FDeltaVelocityPersistent);
  inherited Destroy;
end;

function TAddVelocityBehavior.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'DVelocity') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TAddVelocityBehavior.Update(const SecondsPassed: Single;
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
    RigidBody.LinearVelocity := RigidBody.LinearVelocity + DeltaVelocity;
  end;
end;

initialization

  RegisterSerializableComponent(TAddVelocityBehavior, 'Add Velocity Behavior');

end.
