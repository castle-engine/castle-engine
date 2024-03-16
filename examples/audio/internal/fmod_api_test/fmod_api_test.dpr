{
  Copyright 2019-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

{ Simple application to test direct FMOD calls. }

uses
  { FMOD on Windows platform docs,
    https://www.fmod.com/resources/documentation-api?version=2.0&page=platforms-windows.html
    say to call
      CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    and
      CoUninitialize()
    It seems that ComObj unit in FPC calls this automatically.

    TODO: In Delphi we should use
       Windows, ActiveX,
    and call
      CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    manually? }
  {$ifdef MSWINDOWS} ComObj, {$endif}
  SysUtils, TypInfo, CTypes,
  CastleInternalFMOD, CastleUtils, CastleStringUtils, CastleTimeUtils;

type
  EFMODError = class(Exception);

procedure CheckFMOD(const FMODResult: TFMOD_RESULT);
var
  ErrorStr: String;
begin
  if FMODResult <> FMOD_OK then
  begin
    ErrorStr := GetEnumName(TypeInfo(TFMOD_RESULT), Ord(FMODResult));
    raise EFMODError.CreateFmt('FMOD error: %s', [ErrorStr]);
  end;
end;

function SoundTypeToStr(const SoundType: TFMOD_SOUND_TYPE): String;
begin
  // WriteStr was better in FPC, when TFMOD_SOUND_TYPE had assignments.
  //WriteStr(Result, SoundType);
  // GetEnumName would make FPC error then "No type info available for this type".
  // But it no longer has assignments.

  Result := GetEnumName(TypeInfo(TFMOD_SOUND_TYPE), Ord(SoundType));
end;

function SoundFormatToStr(const SoundFormat: TFMOD_SOUND_FORMAT): String;
begin
  Result := GetEnumName(TypeInfo(TFMOD_SOUND_FORMAT), Ord(SoundFormat));
end;

var
  FMODSystem: PFMOD_SYSTEM;

function PlaySound(const Url: String; const Loop: Boolean): PFMOD_CHANNEL;
var
  // SoundInfo: TFMOD_CREATESOUNDEXINFO; // not needed now
  Sound: PFMOD_SOUND;
  SoundType: TFMOD_SOUND_TYPE;
  SoundFormat: TFMOD_SOUND_FORMAT;
  SoundChannels, SoundBits: CInt;
begin
  // Not needed now
  // FillChar(SoundInfo, SizeOf(cbsize), );
  // SoundInfo.cbsize := SizeOf(cbsize);

  CheckFMOD(FMOD_System_CreateSound(FMODSystem, PCharOrNil(Url), FMOD_DEFAULT or FMOD_2D,
    nil { @SoundInfo }, @Sound));

  CheckFMOD(FMOD_Sound_GetFormat(Sound, @SoundType, @SoundFormat, @SoundChannels, @SoundBits));

  Writeln('Loaded sound information:' + NL +
    '  Type: ' + SoundTypeToStr(SoundType) + NL +
    '  Format: ' + SoundFormatToStr(SoundFormat) + NL +
    '  Channels: ' + IntToStr(SoundChannels) + NL +
    '  Bits: ' + IntToStr(SoundBits));

  // Start in paused state, allows to adjust parameters like loop before starting
  CheckFMOD(FMOD_System_PlaySound(FMODSystem, Sound, nil, { paused } 1, @Result));
  if Loop then
    CheckFMOD(FMOD_Channel_SetMode(Result, FMOD_LOOP_NORMAL))
  else
    CheckFMOD(FMOD_Channel_SetMode(Result, FMOD_LOOP_OFF));
  CheckFMOD(FMOD_Channel_SetPaused(Result, 0));
end;

function IsPlaying(const Channel: PFMOD_CHANNEL): Boolean;
var
  B: TFMOD_BOOL;
begin
  { Note that looping sound will have IsPlaying forever until it's explicitly stopped,
    and that's what we want. }
  CheckFMOD(FMOD_Channel_IsPlaying(Channel, @B));
  Result := B <> 0;
end;

var
  TimeStart: TTimerResult;
  Time: TFloatTime;
  NewVolume, NewPitch: Single;
  ChannelWaiting: PFMOD_CHANNEL;
begin
  Check(FmodLibraryAvailable, 'FMOD library cannot be loaded');
  FmodLibraryUsingBegin;

  CheckFMOD(FMOD_System_Create(@FMODSystem));

  CheckFMOD(FMOD_System_Init(FMODSystem, 256, FMOD_INIT_NORMAL, nil));

  PlaySound('./data/tone.wav', true);
  ChannelWaiting := PlaySound('./data/temple-adam-goh.ogg', false);

  TimeStart := Timer;

  while IsPlaying(ChannelWaiting) do
  begin
    Time := TimeStart.ElapsedTime;

    { test: modulate sound volume while playing }
    NewVolume := Abs(Cos(Time));
    CheckFMOD(FMOD_Channel_SetVolume(ChannelWaiting, NewVolume));

    NewPitch := Abs(Cos(Time)) + 1.0;
    CheckFMOD(FMOD_Channel_SetPitch(ChannelWaiting, NewPitch));

    { test: forcibly stop sound after 10 seconds }
    if Time > 10 then
    begin
      CheckFMOD(FMOD_Channel_Stop(ChannelWaiting));
      Break; // do not call FMOD_Channel_IsPlaying anymore, ChannelWaiting should not be used anymore
    end;
  end;

  CheckFMOD(FMOD_System_Close(FMODSystem));

  CheckFMOD(FMOD_System_Release(FMODSystem));
  FMODSystem := nil;

  FmodLibraryUsingEnd;

  Writeln('Finished OK.');
end.
