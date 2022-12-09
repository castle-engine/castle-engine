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

{ TAddForceAtPositionBehavior }
unit AddForceAtPositionBehavior;

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleVectors,
  CastleComponentSerialize, CastleClassUtils, AbstractTimeDurationBehavior;

type
  TAddForceAtPositionBehavior = class(TAbstractTimeDurationBehavior)
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

    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Force: TVector3 read FForce write FForce;
    property Position: TVector3 read FPosition write FPosition;
  published
    property ForcePersistent: TCastleVector3Persistent read FForcePersistent;
    property PositionPersistent: TCastleVector3Persistent read FPositionPersistent;
  end;

implementation

{ TAddForceAtPositionBehavior ---------------------------------------------------------- }

function TAddForceAtPositionBehavior.GetForceForPersistent: TVector3;
begin
  Result := Force;
end;

procedure TAddForceAtPositionBehavior.SetForceForPersistent(const AValue: TVector3);
begin
  Force := AValue;
end;

function TAddForceAtPositionBehavior.GetPositionForPersistent: TVector3;
begin
  Result := Position;
end;

procedure TAddForceAtPositionBehavior.SetPositionForPersistent(const AValue: TVector3);
begin
  Position := AValue;
end;

constructor TAddForceAtPositionBehavior.Create(AOwner: TComponent);
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

destructor TAddForceAtPositionBehavior.Destroy;
begin
  FreeAndNil(FForcePersistent);
  FreeAndNil(FPositionPersistent);
  inherited;
end;

procedure TAddForceAtPositionBehavior.Update(const SecondsPassed: Single;
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
    RigidBody.AddForceAtPosition(Force, Position);
    RigidBody.WakeUp;
  end;
end;

function TAddForceAtPositionBehavior.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'Force') or
     (PropertyName = 'Position') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TAddForceAtPositionBehavior, 'Add Force At Position Behavior');
end.
