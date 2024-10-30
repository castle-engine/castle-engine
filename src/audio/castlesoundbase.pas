{
  Copyright 2010-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sound engine basic types. }
unit CastleSoundBase;

{$I castleconf.inc}

interface

uses SysUtils, Generics.Collections;

type
  ENoMoreSources = class(Exception);
  ESoundFileError = class(Exception);

  { How does distance affect spatial sounds, used for @link(TSoundEngine.DistanceModel). }
  TSoundDistanceModel = (
    { Sound is fully audible (with @link(TCastleSound.Volume)) at the distance
      @link(TCastleSound.ReferenceDistance), and then it drops down with distance
      following a realistic curve.
      Sound is not attenuated more when it is further than @link(TCastleSound.MaxDistance).

      This is the default distance model, that is realistic and is easy to use.

      The description above is not 100% precise, because different sound backends have
      different details how is this realized.
      For OpenAL, this means using default AL_INVERSE_DISTANCE_CLAMPED
      (the exact equation is on https://www.openal.org/documentation/openal-1.1-specification.pdf ).
      For FMOD, this means using default FMOD_3D_INVERSEROLLOFF
      (see https://www.fmod.com/resources/documentation-api?version=2.01&page=core-api-common.html#fmod_3d_inverserolloff ,
      https://www.fmod.com/resources/documentation-api?version=2.01&page=white-papers-3d-sounds.html#inverse ,
      "for every doubling of this mindistance, the sound volume will halve" -- so it is inverse squared). }
    dmInverse,

    { Sound is fully audible (with @link(TCastleSound.Volume)) at the distance
      @link(TCastleSound.ReferenceDistance), and then it drops down linearly to 0
      at @link(TCastleSound.MaxDistance).
      Sound is no more audible at @link(TCastleSound.MaxDistance).

      It is easy to control, but remember to set @link(TCastleSound.MaxDistance)
      to sensibly large value.

      This matches the X3D sound model most. }
    dmLinear
  );

  TSoundDevice = class
  private
    FName, FCaption: string;
  public
    { Short device name, used for @link(TSoundEngine.Device). }
    property Name: string read FName;
    { Nice device name to show user. }
    property Caption: string read FCaption;
  end;

  TSoundDeviceList = class({$ifdef FPC}specialize{$endif} TObjectList<TSoundDevice>)
    procedure Add(const AName, ACaption: string); reintroduce;
  end;

  { Sound sample format.

    8-bit data is unsigned.
    Just like in case of 8-bit WAV files, and OpenAL AL_FORMAT_MONO8 / AL_FORMAT_STEREO8:
    It is expressed as an unsigned value over the range 0 to 255, 128 being an audio output level of zero.

    16-bit data is signed.
    Just like in case of 16-bit WAV files, and OpenAL AL_FORMAT_MONO16 / AL_FORMAT_STEREO16:
    It is expressed as a signed value over the range -32768 to 32767, 0 being an audio output level of zero.

    Stereo data is expressed in an interleaved format, left channel sample followed by the right channel sample.
  }
  TSoundDataFormat = (
    sfMono8,
    sfMono16,
    sfStereo8,
    sfStereo16
  );

  { How to load a sound buffer. }
  TSoundLoading = (
    { Load entire sound file at once.
      The advantage is that once the sound buffer is loaded, there's zero overhead at runtime
      for playing it, and loading the sound buffer multiple times uses the cache properly.
      The disadvantage is that loading time may be long, for longer files. }
    slComplete,

    { Decompress the sound (like OggVorbis) during playback.
      It allows for much quicker sound loading (almost instant, if you use streaming
      for everything) but means that sounds will be loaded (in parts)
      during playback.
      In general case, we advise to use it for longer sounds (like music tracks). }
    slStreaming
  );

function DataFormatToStr(const DataFormat: TSoundDataFormat): string;

implementation

{ TSoundDeviceList ----------------------------------------------------------- }

procedure TSoundDeviceList.Add(const AName, ACaption: string);
var
  D: TSoundDevice;
begin
  D := TSoundDevice.Create;
  D.FName := AName;
  D.FCaption := ACaption;
  inherited Add(D);
end;

{ global functions ----------------------------------------------------------- }

function DataFormatToStr(const DataFormat: TSoundDataFormat): string;
const
  DataFormatStr: array [TSoundDataFormat] of String = (
    'mono 8',
    'mono 16',
    'stereo 8',
    'stereo 16'
  );
begin
  Result := DataFormatStr[DataFormat];
end;

end.
