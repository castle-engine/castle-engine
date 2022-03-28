{%MainUnit castlebehaviors.pas}
{
  Copyright 2006-2022 Michalis Kamburelis.

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
  { Behavior that tracks life points, and determines being alive/dead
    for game purposes.

    Using this class in your own games is completely optional.
    You can implement "alive" functionality yourself trivially,
    and then make your own decisions about various details
    (e.g. is life a float, or integer? does life equal "precisely
    zero" means being still alive, or dead?). }
  TCastleAliveBehavior = class(TCastleBehavior)
  strict private
    FLife: Single;
    FMaxLife: Single;
    FHurtDirection: TVector3;
    FHurtStrength: Single;
    FAttacker: TCastleAliveBehavior;
    FAttackerObserver: TFreeNotificationObserver;
    procedure SetAttacker(const Value: TCastleAliveBehavior);
    procedure AttackerFreeNotification(const Sender: TFreeNotificationObserver);
  protected
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;
  public
    const
      { Default value for @link(MaxLife) and @link(Life). }
      DefaultLife = 100.0;

    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Shortcut for checking Life <= 0. }
    function Dead: Boolean;

    { Hurt given creature, decreasing its @link(Life) by LifeLoss,
      also setting some additional properties that describe the damage.
      These additional properties do not do anything in this class --
      but they may be useful by other effects, e.g. "knockback",
      done by other behaviors.

      Note: If all you want to do is to decrease @link(Life),
      you can also just set @link(Life) property directly.

      @param(AHurtDirection Should be a normalized vector indicating direction
        from which the attack came.

        In this class, it does nothing, merely sets @link(HurtDirection) property.
        Which may be used by other effects.)

      @param(AHurtStrength Describes "strength" of the attack.
        What this "strengh" exactly means is not defined in this class.
        It may cause a "knockback" effect, in which case it may be a knockback
        distance, or a physical force strength, and is meaningful only when
        @code(AHurtDirection) parameter is non-zero.

        In this class, it does nothing, merely sets @link(HurtStrength) property.
        Which may be used by other effects.)

      @param(AnAttacker The other alive creature that caused this damage.
        It may be @nil if no other TCastleAliveBehavior is directly responsible
        for this damage. This may be useful for various purposes,
        for example the victim may become aware of attacker presence
        when it's attacked.

        In this class, it does nothing, merely sets @link(Attacker) property.
        Which may be used by other effects.)
      )}
    procedure Hurt(const LifeLoss: Single;
      const AHurtDirection: TVector3;
      const AHurtStrength: Single = 0;
      const AnAttacker: TCastleAliveBehavior = nil); virtual;

    { Direction from where the last attack came, set by @link(Hurt).
      Zero if there was no specific direction of last attack,
      otherwise a normalized (length 1) vector. }
    property HurtDirection: TVector3 read FHurtDirection;

    { Strengh of the last attack, set by @link(Hurt).
      What this "strengh" exactly means is not defined in this class.
      It may cause a "knockback" effect, in which case it may be a knockback
      distance, or a physical force strength, and is meaningful only when
      @link(HurtDirection) is non-zero. }
    property HurtStrength: Single read FHurtStrength;

    { Last attacker. Set by the last @link(Hurt) call,
      it may also be set directly. }
    property Attacker: TCastleAliveBehavior read FAttacker write SetAttacker;
  published
    { Current Life. The object is considered "dead" when this is <= 0. }
    property Life: Single read FLife write FLife {$ifdef FPC}default DefaultLife{$endif};

    { Maximum amount of life.
      Can be also used for information (to display on player HUDs and such).

      This is not a strict limit on @link(Life),
      i.e. all the code allows the have @link(Life) > @link(MaxLife)
      to account for special game mechanisms,
      like "magic life boost to make health temporarily larger than normal".
      It is up to your game whether such situation will actually happen. }
    property MaxLife: Single read FMaxLife write FMaxLife {$ifdef FPC}default DefaultLife{$endif};
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleAliveBehavior ------------------------------------------------------- }

constructor TCastleAliveBehavior.Create(AOwner: TComponent);
begin
  inherited;
  FLife := DefaultLife;
  FMaxLife := DefaultLife;
  FAttackerObserver := TFreeNotificationObserver.Create(Self);
  FAttackerObserver.OnFreeNotification := {$ifdef FPC}@{$endif}AttackerFreeNotification;
end;

function TCastleAliveBehavior.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  Result := inherited;
  if not Result then Exit;

  if NewParent.FindBehavior(TCastleAliveBehavior) <> nil then
  begin
    ReasonWhyCannot := 'Only one TCastleAliveBehavior can be added to a given TCastleTransform';
    Result := false;
  end;
end;

procedure TCastleAliveBehavior.AttackerFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Attacker := nil;
end;

procedure TCastleAliveBehavior.SetAttacker(const Value: TCastleAliveBehavior);
begin
  if FAttacker <> Value then
  begin
    FAttackerObserver.Observed := Value;
    FAttacker := Value;
  end;
end;

procedure TCastleAliveBehavior.Hurt(const LifeLoss: Single;
  const AHurtDirection: TVector3;
  const AHurtStrength: Single; const AnAttacker: TCastleAliveBehavior);
begin
  Life := Life - LifeLoss;
  FHurtDirection := AHurtDirection;
  FHurtStrength := AHurtStrength;
  Attacker := AnAttacker;
end;

function TCastleAliveBehavior.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Life') or
     (PropertyName = 'MaxLife') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleAliveBehavior.Dead: Boolean;
begin
  Result := Life <= 0;
end;

{$endif read_implementation}
