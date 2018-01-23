{
  Copyright 2003-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Handle sound files in various formats.)

  While this unit does use some OpenAL constants, most parts of
  this unit can be used even when OpenAL is not initilized and not
  even available. The methods that require OpenAL to be available and
  initialized are clearly marked as such in the documentation. }
unit CastleInternalSoundFile;

{$I castleconf.inc}

interface

uses SysUtils, CastleUtils, Classes, CastleInternalOpenAL, CastleTimeUtils;

type
  ESoundFileError = class(Exception);

  ESoundFormatNotSupportedByOpenAL = class(ESoundFileError);
  EOggVorbisLoadError = class(ESoundFileError);
  EWavLoadError = class(ESoundFileError);

  TSoundFile = class
  protected
    procedure CheckALExtension(const S: string);
  public
    { Load a sound from a stream.

      @raises(ESoundFileError If loading of this sound file failed.
        E.g. in case of decoding problems
        (e.g. we do not have vorbisfile / tremolo to decompress OggVorbis,
        or the OggVorbis stream is invalid.)
      )

      @raises(EStreamError If case reading from the underlying stream failed
        (e.g. strean ended prematurely).

        The class function CreateFromFile will catch and reraise them
        as ESoundFileError.
      )
    }
    constructor CreateFromStream(Stream: TStream); virtual; abstract;

    { Load a sound data from a given URL.

      @raises(ESoundFileError If loading of this sound file failed.
        See @link(CreateFromStream) for various possible reasons.) }
    class function CreateFromFile(const URL: string): TSoundFile;

    { Sound data, according to DataFormat.
      Contents of Data are readonly. }
    function Data: Pointer; virtual; abstract;
    { Bytes allocated for @link(Data). }
    function DataSize: LongWord; virtual; abstract;
    { Data format, as understood by OpenAL. }
    function DataFormat: TALuint; virtual; abstract;
    function Frequency: LongWord; virtual; abstract;

    { Duration in seconds. Returns -1 if not known (DataSize or Frequency are zero,
      or DataFormat is unknown). }
    function Duration: TFloatTime;
  end;

  TSoundFileClass = class of TSoundFile;

  { OggVorbis file loader.
    Loads using libvorbisfile or tremolo. Both are open-source libraries
    for reading OggVorbis music from https://xiph.org/.
    Tremolo is used on mobile devices, libvorbisfile on desktop. }
  TSoundOggVorbis = class(TSoundFile)
  private
    DataStream: TMemoryStream;
    FDataFormat: TALuint;
    FFrequency: LongWord;
  public
    constructor CreateFromStream(Stream: TStream); override;
    destructor Destroy; override;

    function Data: Pointer; override;
    function DataSize: LongWord; override;
    function DataFormat: TALuint; override;
    function Frequency: LongWord; override;
  end;

  TSoundWAV = class(TSoundFile)
  private
    FData: Pointer;
    FDataSize: LongWord;
    FDataFormat: TALuint;
    FFrequency: LongWord;
  public
    constructor CreateFromStream(Stream: TStream); override;

    destructor Destroy; override;

    function Data: Pointer; override;
    function DataSize: LongWord; override;
    function DataFormat: TALuint; override;
    function Frequency: LongWord; override;
  end;

function ALDataFormatToStr(DataFormat: TALuint): string;

implementation

uses CastleStringUtils, CastleInternalVorbisDecoder, CastleInternalVorbisFile,
  CastleLog, CastleDownload, CastleURIUtils;

{ TSoundFile ----------------------------------------------------------------- }

class function TSoundFile.CreateFromFile(const URL: string): TSoundFile;
var
  C: TSoundFileClass;
  S: TStream;
  MimeType: string;
begin
  try
    { soForceMemoryStream as current TSoundWAV and TSoundOggVorbis need seeking }
    S := Download(URL, [soForceMemoryStream], MimeType);
    try
      { calculate class to read based on MimeType }
      if MimeType = 'audio/x-wav' then
        C := TSoundWAV
      else
      if MimeType = 'audio/ogg' then
        C := TSoundOggVorbis
      else
      if MimeType = 'audio/mpeg' then
        raise ESoundFileError.Create('TODO: Reading MP3 sound files not supported. Convert them to OggVorbis.')
      else
      begin
        WritelnWarning('Audio', Format('Not recognized MIME type "%s" for sound file "%s", trying to load it as wav', [MimeType, URL]));
        C := TSoundWAV;
      end;

      Result := C.CreateFromStream(S);
    finally S.Free end;
  except

    { May be raised by Download in case opening the underlying stream failed. }
    on E: EFOpenError do
      { Reraise as ESoundFileError, and add URL to exception message }
      raise ESoundFileError.Create('Error while opening URL "' + URIDisplay(URL) + '": ' + E.Message);

    { May be raised by C.CreateFromStream. }
    on E: EStreamError do
      { Reraise as ESoundFileError, and add URL to exception message }
      raise ESoundFileError.Create('Error while reading URL "' + URIDisplay(URL) + '": ' + E.Message);
  end;
end;

procedure TSoundFile.CheckALExtension(const S: string);
begin
  if not alIsExtensionPresent(PChar(S)) then
    raise ESoundFormatNotSupportedByOpenAL.CreateFmt('OpenAL extension "%s" ' +
      'required to play this file is not available', [S]);
end;

function TSoundFile.Duration: TFloatTime;
var
  SampleSize: Cardinal;
begin
  case DataFormat of
    AL_FORMAT_MONO8   : SampleSize := 1;
    AL_FORMAT_MONO16  : SampleSize := 2;
    AL_FORMAT_STEREO8 : SampleSize := 2;
    AL_FORMAT_STEREO16: SampleSize := 4;
    else Exit(-1);
  end;
  if (Frequency = 0) or (DataSize = 0) then
    Exit(-1);
  Result := DataSize / (Frequency * SampleSize);
end;

{ TSoundOggVorbis ------------------------------------------------------------ }

constructor TSoundOggVorbis.CreateFromStream(Stream: TStream);
begin
  inherited Create;
  try
    DataStream := VorbisDecode(Stream, FDataFormat, FFrequency);
  except
    { convert EVorbisLoadError to ESoundFileError }
    on E: EVorbisLoadError do
      raise ESoundFileError.Create(E.Message);
  end;
end;

destructor TSoundOggVorbis.Destroy;
begin
  FreeAndNil(DataStream);
  inherited;
end;

function TSoundOggVorbis.DataFormat: TALuint;
begin
  Result := FDataFormat;
end;

function TSoundOggVorbis.Frequency: LongWord;
begin
  Result := FFrequency;
end;

function TSoundOggVorbis.Data: Pointer;
begin
  Result := DataStream.Memory;
end;

function TSoundOggVorbis.DataSize: LongWord;
begin
  Result := DataStream.Size;
end;

{ TSoundWAV ------------------------------------------------------------ }

constructor TSoundWAV.CreateFromStream(Stream: TStream);

{ WAV file reader. Written mostly based on
    http://www.technology.niagarac.on.ca/courses/comp630/WavFileFormat.html
  and looking at alutLoadWAVFile implementation.
  See also http://www.sonicspot.com/guide/wavefiles.html , this seems
  a little more updated. }

type
  TID = array[0..3]of char;

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
  inherited Create;

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
      { calculate FDataFormat }
      case Format.Channels of
        1: if Format.BitsPerSample = 8 then
             FDataFormat := AL_FORMAT_MONO8 else
             FDataFormat := AL_FORMAT_MONO16;
        2: if Format.BitsPerSample = 8 then
             FDataFormat := AL_FORMAT_STEREO8 else
             FDataFormat := AL_FORMAT_STEREO16;
        else raise EWavLoadError.Create('Only WAV files with 1 or 2 channels are allowed');
      end;
      { calculate FFrequency }
      FFrequency := Format.SamplesPerSec;
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
      if Data <> nil then
        raise EWavLoadError.Create('WAV file must not contain multiple data chunks');
      { calculate FDataSize and FData (and FData^) }
      FDataSize := Header.Len;
      FData := GetMem(DataSize);
      Stream.ReadBuffer(Data^, DataSize);
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
end;

destructor TSoundWAV.Destroy;
begin
  FreeMemNiling(FData);
  inherited;
end;

function TSoundWAV.Data: Pointer;
begin
  Result := FData;
end;

function TSoundWAV.DataSize: LongWord;
begin
  Result := FDataSize;
end;

function TSoundWAV.DataFormat: TALuint;
begin
  Result := FDataFormat;
end;

function TSoundWAV.Frequency: LongWord;
begin
  Result := FFrequency;
end;

{ global functions ----------------------------------------------------------- }

function ALDataFormatToStr(DataFormat: TALuint): string;
begin
  case DataFormat of
    AL_FORMAT_MONO8: Result := 'mono 8';
    AL_FORMAT_MONO16: Result := 'mono 16';
    AL_FORMAT_STEREO8: Result := 'stereo 8';
    AL_FORMAT_STEREO16: Result := 'stereo 16';
    AL_FORMAT_MP3_EXT: Result := 'mp3';
    AL_FORMAT_VORBIS_EXT: Result := 'ogg vorbis';
    else raise EInternalError.CreateFmt('ALDataFormatToStr unknown parameter: %d',
      [DataFormat]);
  end;
end;

end.
