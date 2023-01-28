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

{ TAddForceBehavior }
unit AddForceBehavior;

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, CastleClassUtils, AbstractTimeDurationBehavior;

type
  TAddForceBehavior = class(TAbstractTimeDurationBehavior)
  private
    FForce: TVector3;

    FForcePersistent: TCastleVector3Persistent;

    function GetForceForPersistent: TVector3;
    procedure SetForceForPersistent(const AValue: TVector3);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Force: TVector3 read FForce write FForce;
  published
    property ForcePersistent: TCastleVector3Persistent read FForcePersistent;
  end;

implementation

{ TAddForceBehavior --------------------------------------------------- }

function TAddForceBehavior.GetForceForPersistent: TVector3;
begin
  Result := Force;
end;

procedure TAddForceBehavior.SetForceForPersistent(const AValue: TVector3);
begin
  Force := AValue;
end;

constructor TAddForceBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FForcePersistent := TCastleVector3Persistent.Create;
  FForcePersistent.InternalGetValue := {$ifdef FPC}@{$endif}GetForceForPersistent;
  FForcePersistent.InternalSetValue := {$ifdef FPC}@{$endif}SetForceForPersistent;
  FForcePersistent.InternalDefaultValue := Force; // current value is default
end;

destructor TAddForceBehavior.Destroy;
begin
  FreeAndNil(FForcePersistent);
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
  if (RigidBody <> nil) and (RigidBody.ExistsInRoot) then
  begin
    RigidBody.AddForce(Force, false);
    RigidBody.WakeUp;
  end;
end;

function TAddForceBehavior.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if PropertyName = 'Force' then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TAddForceBehavior, 'Add Force Behavior');
end.
