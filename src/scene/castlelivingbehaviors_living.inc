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
  { Represents a living (dead or alive at given point) creature.
    Tracks @link(Life) points which determine being @link(Alive)
    or @link(Dead) for game purposes.
    It is also a basis for creatures to be aware of each other.

    E.g. we can remember the last @link(Attacker).
    This may be used by some other logic (not implemented by this behavior)
    to either run away from the last attacker (e.g. when health low)
    or run toward it (to retaliate).

    This behavior is suitable both for player characters (controlled by humans)
    and for creatures / NPCs / bots (controller by code).
    For the latter (creatures / NPCs / bots) consider adding
    another behavior that implements actual AI, like @link(TCastleMoveAttack)
    or your own logic (see "3D FPS Game" for a simple example).
    See https://castle-engine.io/behaviors about using and implementing
    behaviors.

    Note that using this class in your own games is completely optional.
    You can implement "has hit points" functionality yourself trivially,
    and then make your own decisions about various details
    (e.g. is life a float, or integer? does life equal "precisely
    zero" means being still alive, or dead?).
    This class is just a convenience, we made some decisions
    trying to address a wide range of games. }
  TCastleLiving = class(TCastleBehavior)
  strict private
    FLife: Single;
    FMaxLife: Single;
    FHurtDirection: TVector3;
    FHurtStrength: Single;
    FAttacker: TCastleLiving;
    FAttackerObserver: TFreeNotificationObserver;
    FOnHurt: TNotifyEvent;
    procedure SetAttacker(const Value: TCastleLiving);
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

    { Shortcut for checking Life > 0. Always equal to @code(not Dead). }
    function Alive: Boolean;
    { Shortcut for checking Life <= 0. Always equal to @code(not Alive). }
    function Dead: Boolean;

    { Hurt given creature, decreasing its @link(Life) by LifeLoss,
      also setting some additional properties that describe the damage.
      These additional properties do not do anything in this class --
      but they may be useful by other effects, e.g. "knockback",
      done by other behaviors.

      Note: If all you want to do is to decrease @link(Life),
      you can also just set @link(Life) property directly, like
      @code(MyCreature.Life := MyCreature.Life - 10;).

      @param(AHurtDirection Should be a normalized vector indicating direction
        from which the attack came, in world coordinates.

        In this class, it does nothing, merely sets @link(HurtDirection) property.
        Which may be used by other effects.)

      @param(AHurtStrength Describes "strength" of the attack.
        What this "strengh" exactly means is not defined in this class.
        It may cause a "knockback" effect, in which case it may be a knockback
        distance, or a physical force strength, and is meaningful only when
        @code(AHurtDirection) parameter is non-zero.

        In this class, it does nothing, merely sets @link(HurtStrength) property.
        Which may be used by other effects.)

      @param(AnAttacker The other living creature that caused this damage.
        It may be @nil if no other TCastleLiving is directly responsible
        for this damage. This may be useful for various purposes,
        for example the victim may become aware of attacker presence
        when it's attacked.

        In this class, it does nothing, merely sets @link(Attacker) property.
        Which may be used by other effects.)
      ) }
    procedure Hurt(const LifeLoss: Single;
      const AHurtDirection: TVector3;
      const AHurtStrength: Single = 0;
      const AnAttacker: TCastleLiving = nil); virtual;

    { Direction from where the last attack came, set by @link(Hurt),
      in world coordinates.
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
    property Attacker: TCastleLiving read FAttacker write SetAttacker;
  published
    { Current Life (hit points). The object is considered "dead" when this is <= 0.

      Expressed as float, allows to represent fractional life,
      allows to animate life (e.g. increasing life by some
      "SecondsPassed * RegenerationRate" each frame,
      or decreasing by "SeconsPassed * PoisonSpeed"). }
    property Life: Single read FLife write FLife {$ifdef FPC}default DefaultLife{$endif};

    { Maximum amount of life.
      Can be also used for information (to display on player HUDs and such).

      This is not a strict limit on @link(Life),
      i.e. all the code allows the have @link(Life) > @link(MaxLife)
      to account for special game mechanisms,
      like "magic life boost to make health temporarily larger than normal".
      It is up to your game logic whether such situation will actually happen. }
    property MaxLife: Single read FMaxLife write FMaxLife {$ifdef FPC}default DefaultLife{$endif};

    { Event called right after creature is hurt and properties like
      @link(HurtDirection), @link(HurtStrength) and @link(Attacker) are set. }
    property OnHurt: TNotifyEvent read FOnHurt write FOnHurt;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleLiving ------------------------------------------------------- }

constructor TCastleLiving.Create(AOwner: TComponent);
begin
  inherited;
  FLife := DefaultLife;
  FMaxLife := DefaultLife;
  FAttackerObserver := TFreeNotificationObserver.Create(Self);
  FAttackerObserver.OnFreeNotification := {$ifdef FPC}@{$endif}AttackerFreeNotification;
end;

function TCastleLiving.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  Result := inherited;
  if not Result then Exit;

  if NewParent.FindBehavior(TCastleLiving) <> nil then
  begin
    ReasonWhyCannot := 'Only one TCastleLiving can be added to a given TCastleTransform';
    Result := false;
  end;
end;

procedure TCastleLiving.AttackerFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Attacker := nil;
end;

procedure TCastleLiving.SetAttacker(const Value: TCastleLiving);
begin
  if FAttacker <> Value then
  begin
    FAttackerObserver.Observed := Value;
    FAttacker := Value;
  end;
end;

procedure TCastleLiving.Hurt(const LifeLoss: Single;
  const AHurtDirection: TVector3;
  const AHurtStrength: Single; const AnAttacker: TCastleLiving);
begin
  Life := Life - LifeLoss;
  FHurtDirection := AHurtDirection;
  FHurtStrength := AHurtStrength;
  Attacker := AnAttacker;
  if Assigned(FOnHurt) then
    FOnHurt(Self);
end;

function TCastleLiving.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Life') or
     (PropertyName = 'MaxLife') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleLiving.Dead: Boolean;
begin
  Result := Life <= 0;
end;

function TCastleLiving.Alive: Boolean;
begin
  Result := Life > 0;
end;

{$endif read_implementation}
