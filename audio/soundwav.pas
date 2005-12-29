{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi's audio Pascal units".

  "Kambi's audio Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's audio Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's audio Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Handle sounds in WAV format.)

  Note that this unit doesn't use OpenAL. }

unit SoundWAV;

interface

uses SysUtils, KambiUtils, Classes;

type
  { For now this is just an useless descendant of TObject.

    If at some time I'll need other sounds formats
    then I'll move some part of TSoundWAV functionality
    (or at least require it in all descendants, using abstract methods)
    to this TSound class.

    Also TSound will be probably moved to other unit then.
    Or maybe unit SoundWAV will be renamed to Sound unit ?
    We'll see. For now, I only read files in WAV format. }
  TSound = class end;
  EInvalidSoundFormat = class(Exception);

  TSoundDataFormat = (sdMono8, sdMono16, sdStereo8, sdStereo16);

  EInvalidWAV = class(EInvalidSoundFormat);
  TSoundWAV = class(TSound)
  private
    FData: Pointer;
    FDataSize: Longword;
    FDataFormat: TSoundDataFormat;
    FFrequency: LongWord;
  public
    property Data: Pointer read FData; { contents of Data are readonly }
    property DataSize: Longword read FDataSize; { bytes allocated for Data }
    property DataFormat: TSoundDataFormat read FDataFormat;
    property Frequency: LongWord read FFrequency;

    { @groupBegin

      @raises(EInvalidWAV when loading will fail because the file
        is not a valid (and supported) WAV format.) }
    constructor CreateFromStream(Stream: TStream);
    constructor CreateFromFile(const FileName: string);
    { @groupEnd }

    destructor Destroy; override;
  end;

const
  SoundDataFormatToStr: array[TSoundDataFormat] of string =
  ('mono 8', 'mono 16', 'stereo 8', 'stereo 16');

implementation

{ TSoundWAV ------------------------------------------------------------ }

constructor TSoundWAV.CreateFromStream(Stream: TStream);
{ Odczytywanie plikow WAVe. Napisane na podstawie wielu zrodel -
  przede wszystkim jednej strony w Internecie
    http://www.technology.niagarac.on.ca/courses/comp630/WavFileFormat.html
  oraz takiego starego wydruku jaki zostal mi po starym.
  Zerkalem tez caly czas na implementacje alutLoadWAVFile aby sie upewnic
  czy wszystko dobrze rozumiem.
}

type
  TID = array[0..3]of char;

  function IdCompare(const id: TID; const s: string): boolean;
  begin
   result := (Length(s) = 4) and (id[0] = s[1]) and (id[1] = s[2])
                             and (id[2] = s[3]) and (id[3] = s[4]);
  end;

  function IdToStr(const id: TID): string;
  begin
   result := SReadableForm(id[0]+id[1]+id[2]+id[3]);
  end;

type
  TWavChunkHeader = record
    ID: TID;
    Len: LongWord; { NIE liczac samego SizeOf(TWavChunkHeader) }
  end;

  { caly plik WAV to jest jeden chunk RIFF }
  TWavRiffChunk = record
    Header: TWavChunkHeader; { Header.rID = 'RIFF' }
    wID: TID; { indicates RIFF type; in this case it must be 'WAVE' }
    { More chunks follow. Format and Data chunks are mandatory and
      Format _must_ be before Data. }
  end;

  { "_Cont" means that this structure does not contain field
    Header: TWavChunkHeader;. However, in a stream, a chunk must always be
    preceeded by appropriate header. Format chunk must have Header with
    ID = 'fmt ' }
  TWavFormatChunk_Cont = record
    FormatTag: Word; { 1 = PCM, but other values are also possible }
    { 1 = mono, 2 = stereo. I'm not sure, theoretically other values
      are probably possible ? }
    Channels: Word;
    SamplesPerSec: LongWord;
    AvgBytesPerSec: LongWord;
    BlockAlign: Word;
    { meaning of FormatSpecific depends on FormatTag value. For 1 (PCM)
      it means BitsPerSample. }
    FormatSpecific: Word;
  end;

var
  Riff: TWavRiffChunk;
  Format: TWavFormatChunk_Cont;
  Header: TWavChunkHeader;
begin
 inherited Create;
 Stream.ReadBuffer(Riff, SizeOf(Riff));
 if not (IdCompare(Riff.Header.ID, 'RIFF') and IdCompare(Riff.wID, 'WAVE')) then
  raise EInvalidWAV.Create('WAV file must start with RIFF....WAVE signature');

 while Stream.Position < Int64(Riff.Header.Len + SizeOf(TWavChunkHeader)) do
 begin
  Stream.ReadBuffer(Header, SizeOf(Header));

  if IdCompare(Header.ID, 'fmt ') then
  begin
   Stream.ReadBuffer(Format, SizeOf(Format));
   { interpretuj dane w Format }
   if Format.FormatTag <> 1 then
    raise EInvalidWAV.Create('Loading WAV files not in PCM format not implemented');
   { ustal FDataFormat }
   case Format.Channels of
    1: if Format.FormatSpecific = 8 then FDataFormat := sdMono8   else FDataFormat := sdMono16;
    2: if Format.FormatSpecific = 8 then FDataFormat := sdStereo8 else FDataFormat := sdStereo16;
    else raise EInvalidWAV.Create('Only WAV files with 1 or 2 channels are allowed');
   end;
   { ustal FFrequency }
   FFrequency := Format.SamplesPerSec;
  end else

  if IdCompare(Header.ID, 'data') then
  begin
   if Data <> nil then
    raise EInvalidWAV.Create('WAV file must not contain mulitple data chunks');
   { ustal FDataSize i FData (i FData^) }
   FDataSize := Header.Len;
   FData := GetMem(DataSize);
   Stream.ReadBuffer(Data^, DataSize);
  end else

  begin
   { skip any unknown chunks }
   { Writeln('Skipping unknown chunk '+IdToStr(Header.ID)); }
   Stream.Seek(Header.Len, soFromCurrent);
  end;

 end;
end;

constructor TSoundWAV.CreateFromFile(const FileName: string);
var S: TFileStream;
begin
 S := TFileStream.Create(FileName, fmOpenRead);
 try
  CreateFromStream(S);
 finally S.Free end;
end;

destructor TSoundWAV.Destroy;
begin
 FreeMemNiling(FData);
 inherited;
end;

end.