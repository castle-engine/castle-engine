{
  Copyright 2003-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load sound files in various formats. }
unit CastleInternalSoundFile;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  CastleUtils, CastleTimeUtils, CastleSoundBase, CastleInternalVorbisFile;

type
  ESoundFormatNotSupportedByOpenAL = class(ESoundFileError)
  end deprecated 'do not use, this is not raised by anything anymore';

  ESoundFormatAlreadyRegistered = class(Exception);

  TSoundFile = class
  strict private
    const
      SampleSize: array [TSoundDataFormat] of Cardinal = (1, 2, 2, 4);
    var
      FURL: String;
      DataStream: TMemoryStream;
      FDataFormat: TSoundDataFormat;
      FFrequency: LongWord;
    procedure CheckCorrectness;
    { Analyze sound data. }
    function DataStatistics: String;
  public
    { Load a sound data from a given URL.

      @raises(ESoundFileError If loading of this sound file failed.

        E.g. in case of decoding problems
        (e.g. we do not have vorbisfile / tremolo to decompress OggVorbis,
        or the OggVorbis stream is invalid.)

        Also if reading from the underlying stream failed
        (e.g. stream ended prematurely).
      )
    }
    constructor Create(const AURL: string);
    destructor Destroy; override;

    { URL from which we loaded this sound file. }
    property URL: String read FURL;

    { Sound data, according to DataFormat.
      Contents of Data are readonly. }
    function Data: Pointer;
    { Bytes allocated for @link(Data). }
    function DataSize: LongWord;
    property DataFormat: TSoundDataFormat read FDataFormat;
    property Frequency: LongWord read FFrequency;

    { Duration in seconds. Returns -1 if not known (DataSize or Frequency are zero). }
    function Duration: TFloatTime;

    { Convert sound data to ensure it is 16bit (DataFormat is sfMono16 or sfStereo16,
      not sfMono8 or sfStereo8).

      The default implementation just raises an exception if data is not 16-bit.
      When overriding this you call "inherited" at the end. }
    procedure ConvertTo16bit; virtual;
  end;

  TStreamedSoundFile = class
  strict private
  var
    FURL: String;
    CompressedStream, DecompressedStream: TStream;
    FDataFormat: TSoundDataFormat;
    FFrequency: LongWord;
  public
    { Load a sound from a given URL.

      @raises(ESoundFileError If loading of this sound file failed.
        E.g. in case of decoding problems
        (e.g. we do not have vorbisfile / tremolo to decompress OggVorbis,
        or the OggVorbis stream is invalid.)

        Also when reading from the underlying stream failed
        (e.g. strean ended prematurely).
      )
    }
    constructor Create(const AURL: string);
    destructor Destroy; override;

    { URL from which we loaded this sound file. }
    property URL: String read FURL;

    property DataFormat: TSoundDataFormat read FDataFormat;
    property Frequency: LongWord read FFrequency;

    { Returns read size. }
    function Read(var Buffer; const BufferSize: LongInt): LongInt;
    { Rewind streamed sound file, this is necessary for looping. }
    procedure Rewind;
  end;

var
  { Show in the log loading of sounds. }
  LogSoundLoading: Boolean;

type
  { Read (decompress) a sound file from given Stream.
    Returns a stream with uncompressed sound data in format described
    by DataFormat. }
  TSoundReadEvent = function (
    const Url: string; const Stream: TStream;
    out DataFormat: TSoundDataFormat; out Frequency: LongWord): TStream
    of object;

{ Register sound format. }
procedure RegisterSoundFormat(const MimeType: String;
  const SoundReader: TSoundReadEvent);

implementation

uses Generics.Collections,
  CastleStringUtils, CastleInternalVorbisDecoder,
  CastleLog, CastleDownload, CastleURIUtils, CastleClassUtils;

{ Registering sound formats - declare interface ------------------------------ }

type
  TRegisteredSoundFormat = class
    MimeType: String;
    ReadEvent: TSoundReadEvent;
  end;

  TRegisteredSoundFormats = class(specialize TObjectList<TRegisteredSoundFormat>)
    { @nil if not found. }
    function Find(const MimeType: String): TRegisteredSoundFormat;

    procedure Add(const MimeType: String;
      const ReadEvent: TSoundReadEvent); reintroduce;
  end;

function RegisteredSoundFormats: TRegisteredSoundFormats; forward;

{ TSoundFile ----------------------------------------------------------------- }

constructor TSoundFile.Create(const AURL: string);

  { Call ReadEvent and put complete uncompressed data in DataStream. }
  procedure DecodeStream(const CompressedStream: TStream;
    const ReadEvent: TSoundReadEvent);
  const
    { Noone uses OggVorbis with small files, so it's sensible to make
      this buffer at least 1 MB. Above this, increasing BufferSize doesn't
      seem to make loading OggVorbis faster. }
    BufferSize = 1000 * 1000;
  var
    DecodingStream: TStream;
  begin
    DecodingStream := ReadEvent(AURL, CompressedStream, FDataFormat, FFrequency);
    if DecodingStream is TMemoryStream then
      DataStream := TMemoryStream(DecodingStream)
    else
    try
      DataStream := TMemoryStream.Create;
      ReadGrowingStream(DecodingStream, DataStream, true, BufferSize);
    finally FreeAndNil(DecodingStream) end;
  end;

var
  CompressedStream: TStream;
  MimeType: string;
  TimeStart: TCastleProfilerTime;
  F: TRegisteredSoundFormat;
begin
  inherited Create;
  FURL := AURL;

  TimeStart := Profiler.Start('Loading "' + URIDisplay(AURL) + '" (TSoundFile)');
  try
    try
      { soForceMemoryStream as current TSoundWAV and TSoundOggVorbis need seeking }
      CompressedStream := Download(AURL, [soForceMemoryStream], MimeType);
      try
        F := RegisteredSoundFormats.Find(MimeType);
        if F = nil then
          raise ESoundFileError.CreateFmt('Not recognized (not supported) sound file format: %s for file "%s"', [
            MimeType,
            URIDisplay(AURL)
          ]);

        DecodeStream(CompressedStream, F.ReadEvent);
      finally FreeAndNil(CompressedStream) end;

      CheckCorrectness;

      if LogSoundLoading then
      begin
        WritelnLog('Sound', 'Loaded "%s": %s, %s, size: %d, frequency: %d, duration: %f', [
          URIDisplay(AURL),
          MimeType,
          DataFormatToStr(DataFormat),
          DataSize,
          Frequency,
          Duration
        ]);
        { This is informative, but takes some time, so is commented out.
        WritelnLog('Sound', '"%s" data analysis: %s', [
          URIDisplay(AURL),
          DataStatistics
        ]);
        }
      end;
    except
      { May be raised by Download in case opening the underlying stream failed. }
      on E: EFOpenError do
        { Reraise as ESoundFileError, and add URL to exception message }
        raise ESoundFileError.Create('Error while opening URL "' + URIDisplay(AURL) + '": ' + E.Message);

      on E: EStreamError do
        { Reraise as ESoundFileError, and add URL to exception message }
        raise ESoundFileError.Create('Error while reading URL "' + URIDisplay(AURL) + '": ' + E.Message);
    end;

  finally Profiler.Stop(TimeStart) end;
end;

destructor TSoundFile.Destroy;
begin
  FreeAndNil(DataStream);
  inherited;
end;

function TSoundFile.Data: Pointer;
begin
  Result := DataStream.Memory;
end;

function TSoundFile.DataSize: LongWord;
begin
  Result := DataStream.Size;
end;

function TSoundFile.Duration: TFloatTime;
begin
  if (Frequency = 0) or (DataSize = 0) then
    Exit(-1);
  Result := DataSize / (Frequency * SampleSize[DataFormat]);
end;

procedure TSoundFile.ConvertTo16bit;
begin
  if not (DataFormat in [sfMono16, sfStereo16]) then
    raise ESoundFileError.CreateFmt('Cannot convert this sound class to 16-bit: %s', [ClassName]);

(*
// TODO: Implement this independently from sound format, remaking the code below

procedure TSoundWAV.ConvertTo16bit;
var
  PSource: PByte;
  // To unsigned 16-bit:
  //PDest: PWord;
  // To signed 16-bit:
  PDest: PSmallInt;
  NewData: Pointer;
begin
  if DataFormat in [sfMono8, sfStereo8] then
  begin
    WritelnWarning('Sound', 'Converting to 16-bit "%s".', [URIDisplay(URL)]);

    { create NewData with 16-bit samples }
    NewData := GetMem(DataSize * 2);
    PSource := Data;
    PDest := NewData;
    while PtrUInt(PSource) < PtrUInt(Data) + DataSize do
    begin
      // To unsigned 16-bit:
      // PDest^ := Word(PSource^) shl 8;
      // To signed 16-bit:
      PDest^ := (SmallInt(PSource^) - 128) shl 8;
      Inc(PSource);
      Inc(PDest);
    end;

    { update fields }
    FreeMemNiling(FData);
    FData := NewData;
    FDataSize := DataSize * 2;
    case DataFormat of
      sfMono8  : FDataFormat := sfMono16;
      sfStereo8: FDataFormat := sfStereo16;
    end;
  end;
  inherited;
end;
*)
end;

procedure TSoundFile.CheckCorrectness;
begin
  if DataSize mod SampleSize[DataFormat] <> 0 then
    raise ESoundFileError.CreateFmt('Invalid size for the sound file "%s": %d is not a multiple of sample size (%d)', [
      URIDisplay(URL),
      DataSize,
      SampleSize[DataFormat]
    ]);
  if DataSize = 0 then
    raise ESoundFileError.CreateFmt('Invalid size for the sound file "%s": size cannot be zero', [
      URIDisplay(URL)
    ]);
end;

function TSoundFile.DataStatistics: String;

  procedure Mono8(out MinValue, MaxValue: SmallInt);
  type
    TSample = packed record Main: Byte; end;
    PSample = ^TSample;
  var
    Sample: PSample;
    MinSample, MaxSample: TSample;
    EndSample: PtrUInt;
  begin
    Sample := Data;
    EndSample := PtrUInt(Sample) + DataSize;
    MinSample := Sample^;
    MaxSample := Sample^;
    Inc(Sample);
    while PtrUInt(Sample) < EndSample do
    begin
      if Sample^.Main < MinSample.Main then MinSample.Main := Sample^.Main;
      if Sample^.Main > MaxSample.Main then MaxSample.Main := Sample^.Main;
      Inc(Sample);
    end;
    MinValue := MinSample.Main;
    MaxValue := MaxSample.Main;
  end;

  procedure Mono16(out MinValue, MaxValue: SmallInt);
  type
    TSample = packed record Main: SmallInt; end;
    PSample = ^TSample;
  var
    Sample: PSample;
    MinSample, MaxSample: TSample;
    EndSample: PtrUInt;
  begin
    Sample := Data;
    EndSample := PtrUInt(Sample) + DataSize;
    MinSample := Sample^;
    MaxSample := Sample^;
    Inc(Sample);
    while PtrUInt(Sample) < EndSample do
    begin
      if Sample^.Main < MinSample.Main then MinSample.Main := Sample^.Main;
      if Sample^.Main > MaxSample.Main then MaxSample.Main := Sample^.Main;
      Inc(Sample);
    end;
    MinValue := MinSample.Main;
    MaxValue := MaxSample.Main;
  end;

  procedure Stereo8(out LeftMinValue, LeftMaxValue, RightMinValue, RightMaxValue: SmallInt);
  type
    TSample = packed record Left, Right: Byte; end;
    PSample = ^TSample;
  var
    Sample: PSample;
    MinSample, MaxSample: TSample;
    EndSample: PtrUInt;
  begin
    Sample := Data;
    EndSample := PtrUInt(Sample) + DataSize;
    MinSample := Sample^;
    MaxSample := Sample^;
    Inc(Sample);
    while PtrUInt(Sample) < EndSample do
    begin
      if Sample^.Left < MinSample.Left then MinSample.Left := Sample^.Left;
      if Sample^.Left > MaxSample.Left then MaxSample.Left := Sample^.Left;
      if Sample^.Right < MinSample.Right then MinSample.Right := Sample^.Right;
      if Sample^.Right > MaxSample.Right then MaxSample.Right := Sample^.Right;
      Inc(Sample);
    end;
    LeftMinValue := MinSample.Left;
    LeftMaxValue := MaxSample.Left;
    RightMinValue := MinSample.Right;
    RightMaxValue := MaxSample.Right;
  end;

  procedure Stereo16(out LeftMinValue, LeftMaxValue, RightMinValue, RightMaxValue: SmallInt);
  type
    TSample = packed record Left, Right: SmallInt; end;
    PSample = ^TSample;
  var
    Sample: PSample;
    MinSample, MaxSample: TSample;
    EndSample: PtrUInt;
  begin
    Sample := Data;
    EndSample := PtrUInt(Sample) + DataSize;
    MinSample := Sample^;
    MaxSample := Sample^;
    Inc(Sample);
    while PtrUInt(Sample) < EndSample do
    begin
      if Sample^.Left < MinSample.Left then MinSample.Left := Sample^.Left;
      if Sample^.Left > MaxSample.Left then MaxSample.Left := Sample^.Left;
      if Sample^.Right < MinSample.Right then MinSample.Right := Sample^.Right;
      if Sample^.Right > MaxSample.Right then MaxSample.Right := Sample^.Right;
      Inc(Sample);
    end;
    LeftMinValue := MinSample.Left;
    LeftMaxValue := MaxSample.Left;
    RightMinValue := MinSample.Right;
    RightMaxValue := MaxSample.Right;
  end;

var
  LeftMinValue, LeftMaxValue, RightMinValue, RightMaxValue: SmallInt;
begin
  case DataFormat of
    sfMono8: Mono8(LeftMinValue, LeftMaxValue);
    sfMono16: Mono16(LeftMinValue, LeftMaxValue);
    sfStereo8: Stereo8(LeftMinValue, LeftMaxValue, RightMinValue, RightMaxValue);
    sfStereo16: Stereo16(LeftMinValue, LeftMaxValue, RightMinValue, RightMaxValue);
    else raise EInternalError.Create('TSoundFile.DataStatistics:DataFormat?');
  end;
  if DataFormat in [sfMono8, sfMono16] then
    Result := Format('Mono data. Min Sample: %d. Max Sample: %d.', [
      LeftMinValue,
      LeftMaxValue
    ]);
  if DataFormat in [sfStereo8, sfStereo16] then
    Result := Format('Stereo data. Min Sample Left / Right: %d / %d. Max Sample Left / Right: %d / %d.', [
      LeftMinValue,
      RightMinValue,
      LeftMaxValue,
      RightMaxValue
    ]);
end;

{ TStreamedSoundFile --------------------------------------------------------- }

constructor TStreamedSoundFile.Create(const AURL: string);
var
  MimeType: string;
  TimeStart: TCastleProfilerTime;
  F: TRegisteredSoundFormat;
begin
  inherited Create;
  FURL := AURL;

  TimeStart := Profiler.Start('Loading "' + URIDisplay(AURL) + '" (TStreamedSoundFile)');
  try
    try
      { soForceMemoryStream as current TSoundWAV and TSoundOggVorbis need seeking }
      CompressedStream := Download(AURL, [soForceMemoryStream], MimeType);

      F := RegisteredSoundFormats.Find(MimeType);
      if F = nil then
        raise ESoundFileError.CreateFmt('Not recognized (not supported) sound file format: %s for file "%s"', [
          MimeType,
          URIDisplay(AURL)
        ]);

      DecompressedStream := F.ReadEvent(AURL, CompressedStream, FDataFormat, FFrequency);

      if LogSoundLoading then
      begin
        WritelnLog('Sound', 'Loaded "%s": %s, %s, frequency: %d', [
          URIDisplay(AURL),
          MimeType,
          DataFormatToStr(DataFormat),
          Frequency
        ]);
        { This is informative, but takes some time, so is commented out.
        WritelnLog('Sound', '"%s" data analysis: %s', [
          URIDisplay(AURL),
          DataStatistics
        ]);
        }
      end;
    except
      { May be raised by Download in case opening the underlying stream failed. }
      on E: EFOpenError do
      begin
        { Reraise as ESoundFileError, and add URL to exception message }
        raise ESoundFileError.Create('Error while opening URL "' + URIDisplay(AURL) + '": ' + E.Message);
      end;

      on E: EStreamError do
      begin
        { Reraise as ESoundFileError, and add URL to exception message }
        raise ESoundFileError.Create('Error while reading URL "' + URIDisplay(AURL) + '": ' + E.Message);
      end;
    end;
  finally
    Profiler.Stop(TimeStart)
  end;
end;

destructor TStreamedSoundFile.Destroy;
begin
  FreeAndNil(DecompressedStream);
  FreeAndNil(CompressedStream);
  inherited;
end;

function TStreamedSoundFile.Read(var Buffer; const BufferSize: LongInt): LongInt;
begin
  Result := DecompressedStream.Read(Buffer, BufferSize);
end;

procedure TStreamedSoundFile.Rewind;
begin
  DecompressedStream.Position := 0;
end;

{ TOggVorbisReader ------------------------------------------------------------ }

type
  { OggVorbis file loader. Loads
    Loads using libvorbisfile or tremolo. Both are open-source libraries
    for reading OggVorbis music from https://xiph.org/.
    Tremolo is used on mobile devices, libvorbisfile on desktop. }
  TOggVorbisReader = class
    class function Read(const Url: string; const Stream: TStream;
      out DataFormat: TSoundDataFormat; out Frequency: LongWord): TStream;
  end;

class function TOggVorbisReader.Read(const Url: string; const Stream: TStream;
  out DataFormat: TSoundDataFormat; out Frequency: LongWord): TStream;
begin
  Result := TOggVorbisStream.Create(Stream, DataFormat, Frequency);
end;

{ TWAVReader ------------------------------------------------------------ }

type
  EWavLoadError = class(ESoundFileError);

  TWAVReader = class
    class function Read(const Url: string; const Stream: TStream;
      out DataFormat: TSoundDataFormat; out Frequency: LongWord): TStream;
  end;

class function TWAVReader.Read(const Url: string; const Stream: TStream;
  out DataFormat: TSoundDataFormat; out Frequency: LongWord): TStream;

{ WAV file reader. Written mostly based on
    http://www.technology.niagarac.on.ca/courses/comp630/WavFileFormat.html
  and looking at alutLoadWAVFile implementation.
  See also http://www.sonicspot.com/guide/wavefiles.html , this seems
  a little more updated. }

type
  TID = array [0..3] of char;

  function IdCompare(const id: TID; const s: string): boolean;
  begin
    Result := (Length(s) = 4) and (id[0] = s[1]) and (id[1] = s[2])
                              and (id[2] = s[3]) and (id[3] = s[4]);
  end;

type
  TWavChunkHeader = packed record
    ID: TID;
    Len: LongWord; {< This *doesn't* include SizeOf(TWavChunkHeader) itself. }
  end;

  { The whole WAV file is just one RIFF chunk. }
  TWavRiffChunk = packed record
    Header: TWavChunkHeader; {< Header.rID = 'RIFF' }
    wID: TID; {< Indicates RIFF type. In this case it must be 'WAVE' }
    { More chunks follow. Format and Data chunks are mandatory and
      Format _must_ be before Data. }
  end;

  { WAV format description.

    Note that this must be preceded by the TWavChunkHeader
    in a stream, with Header.ID = 'fmt ' }
  TWavFormatChunk = packed record
    { 1 means PCM, but other values are also possible }
    FormatTag: Word;
    { 1 channel means mono, 2 = stereo. Theoretically other values
      are probably possible? }
    Channels: Word;
    SamplesPerSec: LongWord;
    AvgBytesPerSec: LongWord;
    BlockAlign: Word;
    BitsPerSample: Word;
  end;

var
  Riff: TWavRiffChunk;
  Format: TWavFormatChunk;
  Header: TWavChunkHeader;
begin
  Stream.ReadBuffer(Riff, SizeOf(Riff));
  if not (IdCompare(Riff.Header.ID, 'RIFF') and IdCompare(Riff.wID, 'WAVE')) then
    raise EWavLoadError.Create('WAV file must start with RIFF....WAVE signature');

  { Workaround for buggy WAV files generated by OpenAL waveout device,
    see http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=435754
    gstreamer crashes on them, some other programs handle them.

    They contain fmt and data sections OK, but Riff.Header.Len is too large.
    So at the end, we stand at the end of the file but
    Stream.Position < Int64(Riff.Header.Len + SizeOf(TWavChunkHeader))
    says we can read another chunk.
    In general, these are invalid WAV files, but let's handle them... }
  if Riff.Header.Len = Stream.Size then
  begin
    Riff.Header.Len := Riff.Header.Len - SizeOf(TWavChunkHeader);
    WritelnWarning('WAV', 'Invalid WAV file: Riff.Header.Len equals Stream.Size, but it should be <= Stream.Size - SizeOf(TWavChunkHeader). Reading anyway.');
  end;

  Result := nil;

  while Stream.Position < Int64(Riff.Header.Len + SizeOf(TWavChunkHeader)) do
  begin
    Stream.ReadBuffer(Header, SizeOf(Header));

 {
    Writeln('Got chunk "',
      SReadableForm(Header.ID[0] + Header.ID[1] + Header.ID[2] + Header.ID[3]) +
      '", length ', Header.Len,
      ', remaining stream size ', Stream.Size - Stream.Position);
 }

    if IdCompare(Header.ID, 'fmt ') then
    begin
      Stream.ReadBuffer(Format, SizeOf(Format));
      if Format.FormatTag <> 1 then
        raise EWavLoadError.Create('Loading WAV files not in PCM format not implemented');
      { calculate DataFormat }
      case Format.Channels of
        1:case Format.BitsPerSample of
            8 : DataFormat := sfMono8;
            16: DataFormat := sfMono16;
            else raise EWavLoadError.CreateFmt('Invalid WAV file %s: Only 8 or 16-bit encodings are supported', [Url]);
          end;
        2:case Format.BitsPerSample of
            8 : DataFormat := sfStereo8;
            16: DataFormat := sfStereo16;
            else raise EWavLoadError.CreateFmt('Invalid WAV file %s: Only 8 or 16-bit encodings are supported', [Url]);
          end;
        else raise EWavLoadError.CreateFmt('Invalid WAV file %s: Only 1 or 2 channels are supported', [Url]);
      end;
      { calculate Frequency }
      Frequency := Format.SamplesPerSec;
      { There may be some additional stuff here in format chunk.
        The meaning depends on FormatTag value.
        http://www.sonicspot.com/guide/wavefiles.html
        says they can only happen for compressed WAV data, but I have examples
        of files created (probably) by Windows 95 wav recorder that
        are uncompressed and still have some data here
        (szklane_lasy/sounds/cantDoIt.wav). So be prepared always for some data here. }
      Stream.Seek(Header.Len - SizeOf(Format), soFromCurrent);
    end else

    if IdCompare(Header.ID, 'data') then
    begin
      { calculate Result }
      if Result <> nil then
        raise EWavLoadError.Create('WAV file must not contain multiple data chunks');
      Result := TMemoryStream.Create;
      Result.Size := Header.Len;
      Stream.ReadBuffer(TMemoryStream(Result).Memory^, Header.Len);
    end else

    begin
      { skip any unknown chunks }
      Stream.Seek(Header.Len, soFromCurrent);
    end;

    { all RIFF chunks are 2-byte-aligned, and DataSize doesn't include this padding,
      according to http://www.sonicspot.com/guide/wavefiles.html
      We have to account for it, and skip this padding (otherwise we would get
      nonsense header next, that is cut off by eof and/or has wild Header.Len).
      Testcase with szklane_lasy/sounds/cantDoIt.wav. }
    if Odd(Header.Len) then
      Stream.Seek(1, soFromCurrent);
  end;

  if Result = nil then
    raise EWavLoadError.Create('WAV file has no data chunk');
end;

{ Registering sound formats -------------------------------------------------- }

function TRegisteredSoundFormats.Find(const MimeType: String): TRegisteredSoundFormat;
begin
  for Result in Self do
    if Result.MimeType = MimeType then
      Exit;
  Result := nil;
end;

procedure TRegisteredSoundFormats.Add(const MimeType: String;
  const ReadEvent: TSoundReadEvent);
var
  F: TRegisteredSoundFormat;
begin
  if Find(MimeType) <> nil then
    raise ESoundFormatAlreadyRegistered.CreateFmt('Sound format "%s" is already registered', [MimeType]);
  F := TRegisteredSoundFormat.Create;
  F.MimeType := MimeType;
  F.ReadEvent := ReadEvent;
  inherited Add(F);
end;

var
  FRegisteredSoundFormats: TRegisteredSoundFormats;

function RegisteredSoundFormats: TRegisteredSoundFormats;
begin
  { initialize FRegisteredSoundFormats on-demand }
  if FRegisteredSoundFormats = nil then
  begin
    FRegisteredSoundFormats := TRegisteredSoundFormats.Create(true);
    // register default formats, handled in this unit
    FRegisteredSoundFormats.Add('audio/x-wav', @TWAVReader(nil).Read);
    FRegisteredSoundFormats.Add('audio/ogg', @TOggVorbisReader(nil).Read);
  end;
  Result := FRegisteredSoundFormats;
end;

procedure RegisterSoundFormat(const MimeType: String;
  const SoundReader: TSoundReadEvent);
begin
  RegisteredSoundFormats.Add(MimeType, SoundReader);
end;

finalization
  FreeAndNil(FRegisteredSoundFormats);
end.
