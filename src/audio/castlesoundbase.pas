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

  TSoundDistanceModel = (dmNone,
    dmInverseDistance , dmInverseDistanceClamped,
    dmLinearDistance  , dmLinearDistanceClamped,
    dmExponentDistance, dmExponentDistanceClamped);

  TSoundDevice = class
  private
    FName, FCaption: string;
  public
    { Short device name, used for @link(TSoundEngine.Device). }
    property Name: string read FName;
    { Nice device name to show user. }
    property Caption: string read FCaption;
    property NiceName: string read FCaption; deprecated 'use Caption';
  end;

  TSoundDeviceList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSoundDevice>)
    procedure Add(const AName, ACaption: string); reintroduce;
  end;

  { Sound sample format.

    8-bit data is unsigned.
    Just like in case of 8-bit WAV files, and OpenAL AL_FORMAT_MONO8 / AL_FORMAT_STEREO8:
    It is expressed as an unsigned value over the range 0 to 255, 128 being an audio output level of zero.

    16-bit data is signed.
    Just like in case of 16-bit WAV files, and OpenAL AL_FORMAT_MONO16 / AL_FORMAT_STEREO16:
    It is expressed as a signed value over the range -32768 to 32767, 0 being an audio output level of zero.

    Stereo data is expressed in an interleaved format, left channel sample followed by theright channel sample.
  }
  TSoundDataFormat = (
    sfMono8,
    sfMono16,
    sfStereo8,
    sfStereo16
  );

  TSoundBufferType = (
    sbtFullLoad,
    sbtStreamed
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
