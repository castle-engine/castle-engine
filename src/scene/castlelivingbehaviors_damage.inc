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
  { Damage done by something (like short-range attack of @link(TCastleMoveAttack)
    or missile impact of @link(TCastleMissile)). }
  TCastleDamage = class(TCastleComponent)
  strict private
    FDamageConst, FDamageRandom, FKnockbackDistance: Single;
  public
    const
      DefaultDamageConst = 0.0;
      DefaultDamageRandom = 0.0;
      DefaultKnockbackDistance = 0.0;
    constructor Create(AOwner: TComponent); override;
  published
    { Constant damage.
      Decreases the victim's @link(TCastleLiving.Life) by a constant amount.

      The complete damage dealt is a random float in the range
      @code([DamageConst ... DamageConst + DamageRandom]).

      Both DamageConst and DamageRandom must be >= 0. }
    property DamageConst: Single
      read FDamageConst write FDamageConst
      {$ifdef FPC}default DefaultDamageConst{$endif};

    { Random damage.
      Decreases the victim's @link(TCastleLiving.Life) by a random amount.

      The complete damage dealt is a random float in the range
      @code([DamageConst ... DamageConst + DamageRandom]).

      Both DamageConst and DamageRandom must be >= 0. }
    property DamageRandom: Single
      read FDamageRandom write FDamageRandom
      {$ifdef FPC}default DefaultDamageRandom{$endif};

    { Attack knockback (how far will the victim be pushed back)
      done by the short-range attack.

      Must be >= 0. Value equal exactly 0 disables any knockback. }
    property KnockbackDistance: Single
      read FKnockbackDistance write FKnockbackDistance
      {$ifdef FPC}default DefaultKnockbackDistance{$endif};
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleDamage ------------------------------------------------------- }

constructor TCastleDamage.Create(AOwner: TComponent);
begin
  inherited;
  FDamageConst := DefaultDamageConst;
  FDamageRandom := DefaultDamageRandom;
  FKnockbackDistance := DefaultKnockbackDistance;
end;

{$endif read_implementation}
