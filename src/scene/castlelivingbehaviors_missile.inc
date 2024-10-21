{%MainUnit castlelivingbehaviors.pas}
{
  Copyright 2006-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Missile that flies in the given direction @link(TCastleTransform.Direction)
    with the given @link(MoveSpeed). On impact, it will hurt a living thing.

    Missile may have also @link(TCastleMissile) if the missile can be destroyed
    in-flight (e.g. by other missile or short-range attack of TCastleMoveAttack).
    If the missile doesn't have @link(TCastleMissile), it is indestructible. }
  TCastleMissile = class(TCastleBehavior)
  strict private
    FDamage: TCastleDamage;
  protected
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    { Damage caused by the missile impact. }
    property Damage: TCastleDamage read FDamage;

    // TODO: define MoveSpeed
    // TODO: make missile fly (with physics, to also be affected by gravity)
    // TODO: on impact (physical detect), apply Damage
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleMissile ------------------------------------------------------- }

constructor TCastleMissile.Create(AOwner: TComponent);
begin
  inherited;
  FDamage := TCastleDamage.Create(Self);
  FDamage.SetSubComponent(true);
  FDamage.Name := 'Damage';
end;

function TCastleMissile.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  Result := inherited;
  if not Result then Exit;

  if NewParent.FindBehavior(TCastleMissile) <> nil then
  begin
    ReasonWhyCannot := 'Only one TCastleMissile can be added to a given TCastleTransform';
    Result := false;
  end;
end;

{$endif read_implementation}
