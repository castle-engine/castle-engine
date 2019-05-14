{ -*- compile-command: "castle-engine simple-compile fmod_api_test.lpr && ./fmod_api_test" -*- }

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  SysUtils, TypInfo, CTypes,
  CastleInternalFMOD, CastleUtils, CastleStringUtils;

type
  EFMODError = class(Exception);

procedure CheckFMOD(const FMODResult: TFMOD_RESULT);
var
  ErrorStr: String;
begin
  if FMODResult <> FMOD_OK then
  begin
    // FPC error "No type info available for this type", because it's an enum with assignments
    //ErrorStr := GetEnumName(TypeInfo(TFMOD_RESULT), Ord(FMODResult));
    WriteStr(ErrorStr, FMODResult);
    raise EFMODError.CreateFmt('FMOD error: %s', [ErrorStr]);
  end;
end;

function SoundTypeToStr(const SoundType: TFMOD_SOUND_TYPE): String;
begin
  // FPC error "No type info available for this type", because it's an enum with assignments
  //Result := GetEnumName(TypeInfo(TFMOD_SOUND_TYPE), Ord(SoundType));
  WriteStr(Result, SoundType);
end;

function SoundFormatToStr(const SoundFormat: TFMOD_SOUND_FORMAT): String;
begin
  // FPC error "No type info available for this type", because it's an enum with assignments
  //Result := GetEnumName(TypeInfo(TFMOD_SOUND_FORMAT), Ord(SoundFormat));
  WriteStr(Result, SoundFormat);
end;

var
  FMODSystem: PFMOD_SYSTEM;

procedure PlaySound(const URL: String);
var
  // SoundInfo: TFMOD_CREATESOUNDEXINFO; // not needed now
  Sound: PFMOD_SOUND;
  Channel: PFMOD_CHANNEL;
  SoundType: TFMOD_SOUND_TYPE;
  SoundFormat: TFMOD_SOUND_FORMAT;
  SoundChannels, SoundBits: CInt;
begin
  // Not needed now
  // FillChar(SoundInfo, SizeOf(cbsize), );
  // SoundInfo.cbsize := SizeOf(cbsize);

  CheckFMOD(FMOD_System_CreateSound(FMODSystem, PCharOrNil(URL), FMOD_DEFAULT or FMOD_2D,
    nil { @SoundInfo }, @Sound));

  CheckFMOD(FMOD_Sound_GetFormat(Sound, @SoundType, @SoundFormat, @SoundChannels, @SoundBits));

  Writeln('Loaded sound information:' + NL +
    '  Type: ' + SoundTypeToStr(SoundType) + NL +
    '  Format: ' + SoundFormatToStr(SoundFormat) + NL +
    '  Channels: ' + IntToStr(SoundChannels) + NL +
    '  Bits: ' + IntToStr(SoundBits));

  CheckFMOD(FMOD_System_PlaySound(FMODSystem, Sound, nil, { paused } 0, @Channel));
end;

begin
  { Following FMOD on Windows platform docs,
    https://www.fmod.com/resources/documentation-api?version=2.0&page=platforms-windows.html }
  // TODO: {$ifdef MSWINDOWS} CoInitializeEx(nil, COINIT_APARTMENTTHREADED); {$endif}

  CheckFMOD(FMOD_System_Create(@FMODSystem));

  CheckFMOD(FMOD_System_Init(FMODSystem, 16, FMOD_INIT_NORMAL, nil));

  PlaySound('data/boss_destroy.wav');
  PlaySound('data/gamemusic_1.ogg');

  // TODO test controlling Channel

  Writeln('Played sound, waiting 5 secs...');

  Sleep(5000);

  CheckFMOD(FMOD_System_Close(FMODSystem));

  CheckFMOD(FMOD_System_Release(FMODSystem));
  FMODSystem := nil;

  Writeln('Finished OK.');

  // TODO: {$ifdef MSWINDOWS} CoUninitialize(); {$endif}
end.
