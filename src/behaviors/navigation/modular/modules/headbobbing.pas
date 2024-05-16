{
  Copyright 2023-2024 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TODO docs. }
unit HeadBobbing;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, ModularMovement, CastleTransform, CastleVectors,
  CastleClassUtils;

type
  { Head bobbing for TModularMovement FPS games. Add to your player transform,
    near TModularMovement behavior. }
  THeadBobbing = class(TAbstractMovementModule)
  strict private
    HeadBobbingPosition: Single;
    HeadBobbingTime: Single;
    FHeadBobbing: Single;
    FUseHeadBobbing: Boolean;
    const
      DefaultHeadBobbingTime = 0.5;
      DefaultHeadBobbing = 0.02;

    function HeadBobbingHeight(const PlayerBodyHeight: Single;
      const IsPlayerOnGround, IsPlayerMoving, IsGravity: Boolean): Single;
  protected
    function GetParentCamera: TCastleCamera; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property UseHeadBobbing: Boolean read FUseHeadBobbing write FUseHeadBobbing
      default true;
  end;



implementation

uses CastleUtils, CastleComponentSerialize;

{ THeadBobbing --------------------------------------------------------------- }

function THeadBobbing.HeadBobbingHeight(const PlayerBodyHeight: Single;
  const IsPlayerOnGround, IsPlayerMoving, IsGravity: Boolean): Single;
var
  BobbingModifier: Single;
begin
  Result := PlayerBodyHeight;

  if FUseHeadBobbing and IsPlayerOnGround and IsPlayerMoving and IsGravity then
  begin
    { HeadBobbingPosition = 0 means that head is at lowest position.
      HeadBobbingPosition = 0.5 means that head is at highest position.
      HeadBobbingPosition = 1.0 means that head is at lowest position again.

      Larger HeadBobbingPosition work like Frac(HeadBobbingPosition)
      (i.e. function HeadBobbingPosition -> BobbingModifier
      is periodic with period = 1.0). }

    BobbingModifier := Frac(HeadBobbingPosition);

    if BobbingModifier <= 0.5 then
      BobbingModifier := MapRange(BobbingModifier, 0.0, 0.5, -1, +1) else
      BobbingModifier := MapRange(BobbingModifier, 0.5, 1.0, +1, -1);

    { Most game tutorials and codes advice that head bobbing be done with sinus,
      as below. But actually I found that the visual difference between
      sin-based head bobbing and linear-based (like above) head bobbing
      is not noticeable, so I'm using linear-based right now (as it's
      a little faster --- no trig calculation needed, although this
      could be avoided with sinus lookup table).

      If however you prefer sin-based head bobbing, uncomment line below
      and comment out 3 lines "if BobbingModifier <= 0.5 then ...." above.

    BobbingModifier := Sin(BobbingModifier * 2 * Pi);
    }

    BobbingModifier := BobbingModifier * (Result * FHeadBobbing);
    Result := Result + BobbingModifier;
  end;
end;

function THeadBobbing.GetParentCamera: TCastleCamera;
var
  I: Integer;
begin
  for I := 0 to Parent.Count -1 do
  begin
    if Parent.Items[I] is TCastleCamera then
      Exit(Parent.Items[I] as TCastleCamera);
  end;
end;

procedure THeadBobbing.UpdateMovement(
  const MovementState: TModularMovementState);
var
  Camera: TCastleCamera;
  CamTransl: TVector3;
  PlayerHeight: Single;
begin
  Camera := GetParentCamera;
  if Camera = nil then
    Exit;

  PlayerHeight := MovementState.Collider.ScaledLocalBoundingBox.SizeY;
  HeadBobbingPosition := HeadBobbingPosition + (MovementState.SecondsPassed / HeadBobbingTime);

  { Calculate new camera position }
  CamTransl := Camera.Translation;
  CamTransl.Y := HeadBobbingHeight(PlayerHeight, MovementState.IsPlayerOnGround,
    MovementState.IsMoving, MovementState.RigidBody.Gravity) - PlayerHeight;
  Camera.Translation := CamTransl;
end;

function THeadBobbing.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'UseHeadBobbing'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

constructor THeadBobbing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HeadBobbingPosition := 0;
  HeadBobbingTime := DefaultHeadBobbingTime;
  FHeadBobbing := DefaultHeadBobbing;
  FUseHeadBobbing := true;
end;

initialization
  RegisterSerializableComponent(THeadBobbing, ['Navigation', 'Modules', 'Head Bobbing']);

end.

