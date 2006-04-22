{
  Copyright 2006 Michalis Kamburelis.

  This file is part of "castle".

  "castle" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ See TALSoundAllocator. }
unit ALSoundAllocator;

interface

uses OpenAL, KambiClassUtils, KambiUtils;

{$define read_interface}

type
  TALAllocatedSound = class;

  TALAllocatedSoundEvent = procedure(Sender: TALAllocatedSound) of object;

  { Allocated OpenAL sound. }
  TALAllocatedSound = class
  private
    FUsed: boolean;
    FOnUsingEnd: TALAllocatedSoundEvent;
    FImportance: Integer;
    FALSound: TALuint;
    FUserData: TObject;
  public
    constructor Create;
    destructor Destroy; override;

    property ALSound: TALuint read FALSound;

    { Used means that we actually use this sound to play something.
      Sounds that are not Used are simply OpenAL allocated sounds
      that are not used right now, and will be used when we will
      need them.
      @noAutoLinkHere }
    property Used: boolean read FUsed default false;

    { If this sound is @link(Used), this is the priority of keeping it.

      Higher Importance means that it's more important to keep it.
      (I didn't name this property "Priority" so that it's obvious
      that higher Importance means more important sound). }
    property Importance: Integer read FImportance default 0;

    { This is intended to be used by the caller of
      TALSoundAllocator.AllocateSound. It should be initialized
      after calling TALSoundAllocator.AllocateSound, and should
      be finalized in OnUsingEnd. }
    property UserData: TObject read FUserData write FUserData;

    { This will be used when this OpenAL allocated sound will stop
      be used. It may stop be used because there are more demanding
      sounds (see @link(Importance) and to keep MaxAllocatedSounds)
      and we must assign this OpenAL sound slot to something else,
      or it may stop be used because it simply stopped playing.

      (but note that we do not make any guarantees that sounds that
      stopped playing

      In this event you should make sure to delete all references
      to this sound, because the TALAllocatedSound instance may
      be freed after calling  OnUsingEnd.

      It's guaranteed that when this will be called,
      Used will be @false and ALSound will not be in AL_PLAYING
      or AL_PAUSED state. }
    property OnUsingEnd: TALAllocatedSoundEvent
      read FOnUsingEnd write FOnUsingEnd;

    { In this class, it stops playing the source indicated by ALSound,
      sets Used to @false, and calls OnUsingEnd (if assigned).

      You can call this yourself if you want to stop playing the sound.
      It's preferable to call this (instead of manually calling
      alSourceStop), because this will immediately mark Used property
      as @false and will call OnUsingEnd. Otherwise we would have to
      get source state at some time (they are checked in AllocateSound)
      and check it, then see that it's no longer playing.

      You can call this only when Used = @true. }
    procedure DoUsingEnd; virtual;
  end;

  TObjectsListItem_1 = TALAllocatedSound;
  {$I objectslist_1.inc}
  TALAllocatedSoundsList = class(TObjectsList_1)
  private
    function IsSmallerByImportance(const AA, BB: TALAllocatedSound): boolean;
  public
    { This sorts sounds by Used + Importance, descending.
      First all sounds with Used = @true are placed,
      starting from the sound with largest Importance, and so on
      until the sound with smallest Importance.
      Then all sounds with Used = @false are placed (in any, arbitrary order).

      List must not contain nil values when calling this. }
    procedure SortByImportance;
  end;

  { This class manages a list of allocated OpenAL sounds.

    The idea is that you leave to this class creating and deleting
    of OpenAL sounds. When you need OpenAL sound to do something,
    just call AllocateSound method.

    This class will manage OpenAL sources in an intelligent manner,
    which means when you need new sound, we may
    @orderedList(
      @item(reuse already allocated sound that is not used to play anything)
      @item(allocate new sound (but we will keep allocated sounds count
        within MaxAllocatedSounds sound limit, to not overload OpenAL
        implementation with work).)
      @item(we may simply interrupt already allocated sound, if new
        sound is more important.)
    )

    This class may exist only when OpenAL context is active
    (and it's the same OpenAL context). }
  TALSoundAllocator = class
  private
    FAllocatedSounds: TALAllocatedSoundsList;
    FMinAllocatedSounds: Cardinal;
    FMaxAllocatedSounds: Cardinal;
    procedure SetMinAllocatedSounds(const Value: Cardinal);
    procedure SetMaxAllocatedSounds(const Value: Cardinal);
  public
    constructor Create(const AMinAllocatedSounds, AMaxAllocatedSounds: Cardinal);
    destructor Destroy; override;

    { For the sake of speed, this class always has allocated at least
      MinAllocatedSounds OpenAL sounds.
      This must be >= 1. }
    property MinAllocatedSounds: Cardinal
      read FMinAllocatedSounds write SetMinAllocatedSounds;

    { This class always has allocated at most
      MaxAllocatedSounds OpenAL sounds. That's why whenever you try to
      get more sounds, sounds with less Importance may stop playing
      (because their OpenAL sounds may be allocated to some other sound).
      This limit must exist, because allocating too many sounds
      is bad for OpenAL speed (not to mention that it may be impossible
      under some OpenAL implementations, like Windows one).

      This must always be >= MinAllocatedSounds. }
    property MaxAllocatedSounds: Cardinal
      read FMaxAllocatedSounds write SetMaxAllocatedSounds;

    { Call this when you need new OpenAL sound.
      This indicates that you want to use new sound.

      This is the most important function of this class --- actually
      the sole purpose of TALSoundAllocator and TALAllocatedSound
      classes is to provide you this function.

      If we can't allocate new OpenAL sound for this
      (because we already allocated MaxAllocatedSounds, and all existing
      sounds are used and their Importance is > given here Importance),
      returns nil.

      Else returns non-nil TALAllocatedSound instance, with Used to to @true
      (to indicate that you'll use it) and Importance set as required.
      You should initialize all properties of this sound
      (it's buffer, gain, looping --- everything), and start playing it.
      Note for looping sounds: just like any other sound, looping sound
      may be stopped because the sounds are needed for other sounds.
      If you want to try to restart the looping sound, you will have
      to implement it yourself. Or you can just set Importance of looping
      sounds high enough, and don't use too many looping sounds,
      to never let them be eliminated by other sounds. }
    function AllocateSound(const Importance: Integer): TALAllocatedSound;

    { This is read-only from outside.
      You can read AllocatedSounds properties to know various things
      about current state of TALSoundAllocator. But generally, this should
      be avoided --- the way AllocatedSounds are managed is internal
      for this class. }
    property AllocatedSounds: TALAllocatedSoundsList read FAllocatedSounds;
  end;

{$undef read_interface}

implementation

uses SysUtils, ALUtils;

{$define read_implementation}
{$I objectslist_1.inc}

{ TALAllocatedSound ---------------------------------------------------------- }

constructor TALAllocatedSound.Create;
begin
  inherited;
  alCreateSources(1, @FALSound);
end;

destructor TALAllocatedSound.Destroy;
begin
  alDeleteSources(1, @FALSound);
  inherited;
end;

procedure TALAllocatedSound.DoUsingEnd;
begin
  FUsed := false;

  { Note that alSourceStop is a valid NOP for source states like
    AL_STOPPED or AL_INITIAL. So I don't check here current state
    (like CurrentState := alGetSource1i(ALSound, AL_SOURCE_STATE))
    and simply always call alSourceStop. }
  alSourceStop(ALSound);

  if Assigned(OnUsingEnd) then
    OnUsingEnd(Self);
end;

{ TALAllocatedSoundsList ----------------------------------------------------- }

function TALAllocatedSoundsList.IsSmallerByImportance(
  const AA, BB: TALAllocatedSound): boolean;
begin
  Result :=
    (AA.Used and (not BB.Used)) or
    (AA.Used and BB.Used and (AA.Importance > BB.Importance));
end;

procedure TALAllocatedSoundsList.SortByImportance;
begin
  Sort(IsSmallerByImportance);
end;

{ TALSoundAllocator ---------------------------------------------------------- }

constructor TALSoundAllocator.Create(
  const AMinAllocatedSounds, AMaxAllocatedSounds: Cardinal);
var
  I: Integer;
begin
  inherited Create;
  FMinAllocatedSounds := AMinAllocatedSounds;
  FMaxAllocatedSounds := AMaxAllocatedSounds;

  FAllocatedSounds := TALAllocatedSoundsList.Create;
  FAllocatedSounds.Count := MinAllocatedSounds;
  for I := 0 to FAllocatedSounds.High do
    FAllocatedSounds[I] := TALAllocatedSound.Create;
end;

destructor TALSoundAllocator.Destroy;
var
  I: Integer;
begin
  { Stop using and free allocated sounds. }
  for I := 0 to FAllocatedSounds.High do
  begin
    if FAllocatedSounds[I].Used then
      FAllocatedSounds[I].DoUsingEnd;
    FAllocatedSounds.FreeAndNil(I);
  end;

  FreeAndNil(FAllocatedSounds);
  inherited;
end;

function TALSoundAllocator.AllocateSound(
  const Importance: Integer): TALAllocatedSound;

  function alSourcePlayingOrPaused(ALSound: TALuint): boolean;
  var
    SoundState: TALuint;
  begin
    SoundState := alGetSource1i(ALSound, AL_SOURCE_STATE);
    Result := (SoundState = AL_PLAYING) or (SoundState = AL_PAUSED);
  end;

var
  I: Integer;
  MinImportanceIndex: Integer;
begin
  Result := nil;

  { Try: maybe we have already allocated unused sound ?
    If no unused sound will be found, it will calculate
    MinImportanceIndex, this will be useful later. }
  MinImportanceIndex := -1;
  for I := 0 to FAllocatedSounds.High do
    if not FAllocatedSounds[I].Used then
    begin
      Result := FAllocatedSounds[I];
      { Breaking here means that MinImportanceIndex will not be calculated
        correctly (because we did not iterate to the end of FAllocatedSounds
        list). But that's OK, because if Result <> nil here, then we will
        not need MinImportanceIndex later. }
      Break;
    end else
    begin
      { Update MinImportanceIndex }
      if (MinImportanceIndex = -1) or
         (FAllocatedSounds[I].Importance <
          FAllocatedSounds[MinImportanceIndex].Importance) then
         MinImportanceIndex := I;
    end;

  { Try: maybe one of the allocated sounds is marked as Used,
    but actually it's not used anymore ? }
  if Result = nil then
  begin
    for I := 0 to FAllocatedSounds.High do
      if not alSourcePlayingOrPaused(FAllocatedSounds[I].ALSound) then
      begin
        Result := FAllocatedSounds[I];
        Break;
      end;
  end;

  { Try: maybe we can allocate one more sound ? }
  if (Result = nil) and
     (Cardinal(FAllocatedSounds.Count) < MaxAllocatedSounds) then
  begin
    Result := TALAllocatedSound.Create;
    FAllocatedSounds.Add(Result);
  end;

  { Try: maybe we can remove one more sound ?

    If Result = nil then we know that MinImportanceIndex <> -1, because
    all sounds must be used and MinAllocatedSounds is always > 0,
    so some sound must be used.

    Note that if FAllocatedSounds[MinImportanceIndex].Importance
    is equal to Importance, we *do* interrupt already playing sound.
    The assumption is here that the newer sound is more imoportant. }
  if (Result = nil) and
     (FAllocatedSounds[MinImportanceIndex].Importance <= Importance) then
  begin
    Result := FAllocatedSounds[MinImportanceIndex];
  end;

  if Result <> nil then
  begin
    { Prepare Result }
    if Result.Used then
      Result.DoUsingEnd;
    Result.FImportance := Importance;
    Result.FUsed := true;
  end;
end;

procedure TALSoundAllocator.SetMinAllocatedSounds(const Value: Cardinal);
var
  I: Integer;
  OldAllocatedSoundsCount: Cardinal;
begin
  if Value <> FMinAllocatedSounds then
  begin
    FMinAllocatedSounds := Value;
    if Cardinal(FAllocatedSounds.Count) < MinAllocatedSounds then
    begin
      OldAllocatedSoundsCount := FAllocatedSounds.Count;
      FAllocatedSounds.Count := MinAllocatedSounds;
      for I := OldAllocatedSoundsCount to FAllocatedSounds.High do
        FAllocatedSounds[I] := TALAllocatedSound.Create;
    end;
  end;
end;

procedure TALSoundAllocator.SetMaxAllocatedSounds(const Value: Cardinal);
var
  I: Integer;
begin
  if Value <> FMaxAllocatedSounds then
  begin
    FMaxAllocatedSounds := Value;
    if Cardinal(FAllocatedSounds.Count) > MaxAllocatedSounds then
    begin
      { TODO: we should here update Used state of every sound,
        optionally calling their DoUsingEnd. }
      FAllocatedSounds.SortByImportance;
      for I := MaxAllocatedSounds to FAllocatedSounds.High do
      begin
        if FAllocatedSounds[I].Used then
          FAllocatedSounds[I].DoUsingEnd;
        FAllocatedSounds.FreeAndNil(I);
      end;
      FAllocatedSounds.Count := MaxAllocatedSounds;
    end;
  end;
end;

end.