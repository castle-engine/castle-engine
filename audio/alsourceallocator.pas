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

{ See TALSourceAllocator.

  The very reason for this unit is that when OpenAL implementation will run
  out of sources then *no error will be raised*. I.e. no OpenAL error
  (alGetError) will be left, and no exception will be raised.
  In the worst case TALSourceAllocator.AllocateSource will return nil,
  but in more probable cases some other sources (unused, or with
  less priority) will be reused.

  Note that above statement means that the code in this unit must
  read in some situations alGetError. That's because reading alGetError
  is the only way to know when OpenAL implementation has run out of sources.
  So the code in this unit may in various places raise EALError if you
  made some error in your OpenAL code. }
unit ALSourceAllocator;

interface

uses SysUtils, OpenAL, KambiClassUtils, KambiUtils;

{$define read_interface}

type
  TALAllocatedSource = class;

  TALAllocatedSourceEvent = procedure(Sender: TALAllocatedSource) of object;

  ENoMoreOpenALSources = class(Exception);

  { Allocated OpenAL source. }
  TALAllocatedSource = class
  private
    FUsed: boolean;
    FOnUsingEnd: TALAllocatedSourceEvent;
    FImportance: Integer;
    FALSource: TALuint;
    { This must be @true for the whole lifetime of this object
      except the situation at the beginning of the constructor,
      and in destructor (if constructor exited with ENoMoreOpenALSources). }
    FALSourceAllocated: boolean;
    FUserData: TObject;
  public
    { Create source. This allocates actual OpenAL source.
      Will raise ENoMoreOpenALSources if no more sources available
      (ENoMoreOpenALSources should be catched and silenced by
      TALSourceAllocator.AllocateSource). }
    constructor Create;
    destructor Destroy; override;

    property ALSource: TALuint read FALSource;

    { Used means that we actually use this source to play something.
      Sources that are not Used are simply OpenAL allocated sources
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
      TALSourceAllocator.AllocateSource. It should be initialized
      after calling TALSourceAllocator.AllocateSource, and should
      be finalized in OnUsingEnd. }
    property UserData: TObject read FUserData write FUserData;

    { This will be used when this OpenAL allocated sound will stop
      be used. It may stop be used because there are more demanding
      sources (see @link(Importance) and to keep MaxAllocatedSources)
      and we must assign this OpenAL sound slot to something else,
      or it may stop be used because it simply stopped playing.

      But note that we do not make any guarantees that sources that
      stopped playing will be immediately reported to OnUsingEnd.
      In fact, a source may be considered in Used = @true state
      for a long time until it stopped playing. That's not a problem
      for this unit --- TALSourceAllocator.AllocateSource is smart,
      and it may actually check (and eventually mark with DoUsingEnd)
      whether some sources are in playing state,
      to avoid allocating unnecessary sources.
      However, if this is a problem for you (because e.g. you do
      some expensive operations to update all used sources every time)
      and you really desire OnUsingEnd to be called quickly after
      sound stoppped playing, you may call TALSourceAllocator.RefreshUsed
      from time to time.

      In this event you should make sure to delete all references
      to this sound, because the TALAllocatedSource instance may
      be freed after calling  OnUsingEnd.

      It's guaranteed that when this will be called,
      Used will be @false and ALSource will not be in AL_PLAYING
      or AL_PAUSED state. }
    property OnUsingEnd: TALAllocatedSourceEvent
      read FOnUsingEnd write FOnUsingEnd;

    { In this class, it stops playing the source indicated by ALSource,
      sets Used to @false, and calls OnUsingEnd (if assigned).

      You can call this yourself if you want to stop playing the sound.
      It's preferable to call this (instead of manually calling
      alSourceStop), because this will immediately mark Used property
      as @false and will call OnUsingEnd. Otherwise we would have to
      get source state at some time (they are checked in AllocateSource)
      and check it, then see that it's no longer playing.

      You can call this only when Used = @true. }
    procedure DoUsingEnd; virtual;
  end;

  TObjectsListItem_1 = TALAllocatedSource;
  {$I objectslist_1.inc}
  TALAllocatedSourcesList = class(TObjectsList_1)
  private
    function IsSmallerByImportance(const AA, BB: TALAllocatedSource): boolean;
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
    just call AllocateSource method.

    This class will manage OpenAL sources in an intelligent manner,
    which means when you need new sound, we may
    @orderedList(
      @item(reuse already allocated sound that is not used to play anything)
      @item(allocate new sound (but we will keep allocated sounds count
        within MaxAllocatedSources sound limit, to not overload OpenAL
        implementation with work).)
      @item(we may simply interrupt already allocated sound, if new
        sound is more important.)
    )

    This class may exist only when OpenAL context is active
    (and it's the same OpenAL context). }
  TALSourceAllocator = class
  private
    FAllocatedSources: TALAllocatedSourcesList;
    FMinAllocatedSources: Cardinal;
    FMaxAllocatedSources: Cardinal;
    procedure SetMinAllocatedSources(const Value: Cardinal);
    procedure SetMaxAllocatedSources(const Value: Cardinal);
  public
    constructor Create(const AMinAllocatedSources, AMaxAllocatedSources: Cardinal);
    destructor Destroy; override;

    { For the sake of speed, this class always has allocated at least
      MinAllocatedSources OpenAL sources. This must be >= 1.

      @raises(ENoMoreOpenALSources
        Setting this to too large value (so large that OpenAL cannot create
        so many sources) will raise ENoMoreOpenALSources.
        Also creating the TALSourceAllocator instance with initial
        value for MinAllocatedSources too large will raise ENoMoreOpenALSources.
        So set this property to really *the minimal* number or required sources.
        If you don't know what to do, just set this to 1.) }
    property MinAllocatedSources: Cardinal
      read FMinAllocatedSources write SetMinAllocatedSources;

    { This class always has allocated at most
      MaxAllocatedSources OpenAL sounds. That's why whenever you try to
      get more sounds, sounds with less Importance may stop playing
      (because their OpenAL sounds may be allocated to some other sound).
      This limit must exist, because allocating too many sounds
      is bad for OpenAL speed (not to mention that it may be impossible
      under some OpenAL implementations, like Windows one).

      This must always be >= MinAllocatedSources. }
    property MaxAllocatedSources: Cardinal
      read FMaxAllocatedSources write SetMaxAllocatedSources;

    { Call this when you need new OpenAL sound.
      This indicates that you want to use new sound.

      This is the most important function of this class --- actually
      the sole purpose of TALSourceAllocator and TALAllocatedSource
      classes is to provide you this function.

      If we can't allocate new OpenAL sound for this
      (because we already allocated MaxAllocatedSources, or
      OpenAL cannot allocate so many sources (so ENoMoreOpenALSources
      is catched and silenced by this function), and all existing
      sounds are used and their Importance is > given here Importance),
      returns nil.

      Else returns non-nil TALAllocatedSource instance, with Used to to @true
      (to indicate that you'll use it) and Importance set as required.
      You should initialize all properties of this sound
      (it's buffer, gain, looping --- everything), and start playing it.
      Note for looping sounds: just like any other sound, looping sound
      may be stopped because the sounds are needed for other sounds.
      If you want to try to restart the looping sound, you will have
      to implement it yourself. Or you can just set Importance of looping
      sounds high enough, and don't use too many looping sounds,
      to never let them be eliminated by other sounds. }
    function AllocateSource(const Importance: Integer): TALAllocatedSource;

    { This is read-only from outside.
      You can read AllocatedSources properties to know various things
      about current state of TALSourceAllocator. But generally, this should
      be avoided --- the way AllocatedSources are managed is internal
      for this class. }
    property AllocatedSources: TALAllocatedSourcesList read FAllocatedSources;

    { Check and eventually set some sources from used to unused state.

      For every source that is marked as Used, this checks
      whether this source is actually in playing/paused state
      right now. If not, it calls DoUsingEnd (thus setting
      Used to @false and triggering OnUsingEnd) for this source.

      See TALAllocatedSource.OnUsingEnd for more description.
      Generally, you don't need to call this procedure ---
      this unit is smart, and every operation does such refreshing
      (at least partially) by itself, when it's really needed
      (e.g. AllocateSource may do this if no unused source will be found).
      You may need to call this procedure from time to time if you
      frequently perform some expensive operation for all used sources. }
    procedure RefreshUsed;
  end;

{$undef read_interface}

implementation

uses ALUtils;

{$define read_implementation}
{$I objectslist_1.inc}

{ TALAllocatedSource ---------------------------------------------------------- }

constructor TALAllocatedSource.Create;
var
  ErrorCode: TALenum;
begin
  inherited;

  { I must check alGetError now, because I may need to catch
    (and convert to ENoMoreOpenALSources exception) alGetError after
    alCreateSources. So I want to have "clean error state" first. }
  CheckAL('prevention OpenAL check in TALAllocatedSource.Create');

  alCreateSources(1, @FALSource);

  ErrorCode := alGetError();
  if ErrorCode = AL_INVALID_VALUE then
    raise ENoMoreOpenALSources.Create('No more sound sources available') else
  if ErrorCode <> AL_NO_ERROR then
    raise EALError.Create(ErrorCode,
      'OpenAL error AL_xxx at creation of sound : ' + alGetString(ErrorCode));

  { This signals to TALAllocatedSource.Destroy that FALSource contains
    valid source name, that should be deleted by alDeleteSources. }
  FALSourceAllocated := true;
end;

destructor TALAllocatedSource.Destroy;
begin
  if FALSourceAllocated then
    alDeleteSources(1, @FALSource);
  inherited;
end;

procedure TALAllocatedSource.DoUsingEnd;
begin
  FUsed := false;

  { Note that alSourceStop is a valid NOP for source states like
    AL_STOPPED or AL_INITIAL. So I don't check here current state
    (like CurrentState := alGetSource1i(ALSource, AL_SOURCE_STATE))
    and simply always call alSourceStop. }
  alSourceStop(ALSource);

  if Assigned(OnUsingEnd) then
    OnUsingEnd(Self);
end;

{ TALAllocatedSourcesList ----------------------------------------------------- }

function TALAllocatedSourcesList.IsSmallerByImportance(
  const AA, BB: TALAllocatedSource): boolean;
begin
  Result :=
    (AA.Used and (not BB.Used)) or
    (AA.Used and BB.Used and (AA.Importance > BB.Importance));
end;

procedure TALAllocatedSourcesList.SortByImportance;
begin
  Sort(IsSmallerByImportance);
end;

{ TALSourceAllocator ---------------------------------------------------------- }

constructor TALSourceAllocator.Create(
  const AMinAllocatedSources, AMaxAllocatedSources: Cardinal);
var
  I: Integer;
begin
  inherited Create;
  FMinAllocatedSources := AMinAllocatedSources;
  FMaxAllocatedSources := AMaxAllocatedSources;

  FAllocatedSources := TALAllocatedSourcesList.Create;
  FAllocatedSources.Count := MinAllocatedSources;
  for I := 0 to FAllocatedSources.High do
    FAllocatedSources[I] := TALAllocatedSource.Create;
end;

destructor TALSourceAllocator.Destroy;
var
  I: Integer;
begin
  if FAllocatedSources <> nil then
  begin
    { Stop using and free allocated sounds. }
    for I := 0 to FAllocatedSources.High do
      { Although usually we are sure that every FAllocatedSources[I] <> nil,
        in this case we must take into account that maybe our constructor
        raise ENonMoreOpenALSources and so some FAllocatedSources[I] were
        not initialized. }
      if FAllocatedSources[I] <> nil then
      begin
        if FAllocatedSources[I].Used then
          FAllocatedSources[I].DoUsingEnd;
        FAllocatedSources.FreeAndNil(I);
      end;

    FreeAndNil(FAllocatedSources);
  end;

  inherited;
end;

function TALSourceAllocator.AllocateSource(
  const Importance: Integer): TALAllocatedSource;
var
  I: Integer;
  MinImportanceIndex: Integer;
begin
  Result := nil;

  { Try: maybe we have already allocated unused sound ?
    If no unused sound will be found, it will calculate
    MinImportanceIndex, this will be useful later. }
  MinImportanceIndex := -1;
  for I := 0 to FAllocatedSources.High do
    if not FAllocatedSources[I].Used then
    begin
      Result := FAllocatedSources[I];
      { Breaking here means that MinImportanceIndex will not be calculated
        correctly (because we did not iterate to the end of FAllocatedSources
        list). But that's OK, because if Result <> nil here, then we will
        not need MinImportanceIndex later. }
      Break;
    end else
    begin
      { Update MinImportanceIndex }
      if (MinImportanceIndex = -1) or
         (FAllocatedSources[I].Importance <
          FAllocatedSources[MinImportanceIndex].Importance) then
         MinImportanceIndex := I;
    end;

  { Try: maybe one of the allocated sounds is marked as Used,
    but actually it's not used anymore ? }
  if Result = nil then
  begin
    for I := 0 to FAllocatedSources.High do
      if not alSourcePlayingOrPaused(FAllocatedSources[I].ALSource) then
      begin
        Result := FAllocatedSources[I];
        Break;
      end;
  end;

  { Try: maybe we can allocate one more sound ? }
  if (Result = nil) and
     (Cardinal(FAllocatedSources.Count) < MaxAllocatedSources) then
  begin
    try
      Result := TALAllocatedSource.Create;
      FAllocatedSources.Add(Result);
    except
      { If TALAllocatedSource.Create raises ENoMoreOpenALSources ---
        then silence the exception and leave Result = nil. }
      on ENoMoreOpenALSources do ;
    end;
  end;

  { Try: maybe we can remove one more sound ?

    If Result = nil then we know that MinImportanceIndex <> -1, because
    all sounds must be used and MinAllocatedSources is always > 0,
    so some sound must be used.

    Note that if FAllocatedSources[MinImportanceIndex].Importance
    is equal to Importance, we *do* interrupt already playing sound.
    The assumption is here that the newer sound is more imoportant. }
  if (Result = nil) and
     (FAllocatedSources[MinImportanceIndex].Importance <= Importance) then
  begin
    Result := FAllocatedSources[MinImportanceIndex];
  end;

  if Result <> nil then
  begin
    { Prepare Result }
    if Result.Used then
      Result.DoUsingEnd;
    Result.FImportance := Importance;
    Result.FUsed := true;
  end;

  CheckAL('allocating sound source (TALSourceAllocator.AllocateSource)');
end;

procedure TALSourceAllocator.SetMinAllocatedSources(const Value: Cardinal);
var
  I: Integer;
  OldAllocatedSourcesCount: Cardinal;
begin
  if Value <> FMinAllocatedSources then
  begin
    FMinAllocatedSources := Value;
    if Cardinal(FAllocatedSources.Count) < MinAllocatedSources then
    begin
      OldAllocatedSourcesCount := FAllocatedSources.Count;
      FAllocatedSources.Count := MinAllocatedSources;
      for I := OldAllocatedSourcesCount to FAllocatedSources.High do
        FAllocatedSources[I] := TALAllocatedSource.Create;
    end;
  end;
end;

procedure TALSourceAllocator.SetMaxAllocatedSources(const Value: Cardinal);
var
  I: Integer;
begin
  if Value <> FMaxAllocatedSources then
  begin
    FMaxAllocatedSources := Value;
    if Cardinal(FAllocatedSources.Count) > MaxAllocatedSources then
    begin
      { RefreshUsed is useful here, so that we really cut off
        the *currently* unused sources. }
      RefreshUsed;
      FAllocatedSources.SortByImportance;

      for I := MaxAllocatedSources to FAllocatedSources.High do
      begin
        if FAllocatedSources[I].Used then
          FAllocatedSources[I].DoUsingEnd;
        FAllocatedSources.FreeAndNil(I);
      end;
      FAllocatedSources.Count := MaxAllocatedSources;
    end;
  end;
end;

procedure TALSourceAllocator.RefreshUsed;
var
  I: Integer;
begin
  for I := 0 to FAllocatedSources.High do
    if FAllocatedSources[I].Used and
       (not alSourcePlayingOrPaused(FAllocatedSources[I].ALSource)) then
    begin
      FAllocatedSources[I].DoUsingEnd;
    end;
end;

end.