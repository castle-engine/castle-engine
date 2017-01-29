{
  Copyright 2003-2017 Michalis Kamburelis.

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
unit CastleSoundFile;

{$I castleconf.inc}

interface

uses SysUtils, CastleUtils, Classes, CastleOpenAL, CastleTimeUtils;

type
  ESoundFormatNotSupportedByOpenAL = class(Exception);

  TSoundFile = class
  protected
    procedure CheckALExtension(const S: string);
  public
    { This will load a sound from a stream. }
    constructor CreateFromStream(Stream: TStream); virtual; abstract;

    { Load a sound data, given an URL. This just opens the file as
      stream and then calls CreateFromStream of appropriate class,
      so see CreateFromStream for more info. For now, file format
      (which TSoundFile to use) is decided by the URL extension. }
    class function CreateFromFile(const URL: string): TSoundFile;

    { Call this on this sound always after OpenAL is initialized
      and before passing this sound data to OpenAL.
      This may fix or check some things for this sound, checking
      e.g. whether some OpenAL extensions are supported.

      @raises(ESoundFormatNotSupportedByOpenAL if some OpenAL extension
      required to support this format is not present.) }
    procedure PrepareOpenAL; virtual;

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

  EInvalidSoundFormat = class(Exception);

  TSoundMP3 = class(TSoundFile)
  private
    FData: Pointer;
    FDataSize: LongWord;
  public
    constructor CreateFromStream(Stream: TStream); override;
    destructor Destroy; override;

    procedure PrepareOpenAL; override;

    function Data: Pointer; override;
    function DataSize: LongWord; override;
    function DataFormat: TALuint; override;
    function Frequency: LongWord; override;
  end;

  { OggVorbis file loader.

    Internally we can use two implementations of OggVorbis handling:

    @orderedList(
      @item(If AL_EXT_vorbis extension is available, then we will
        use this.

        The advantage of using AL_EXT_vorbis extension is that
        OpenAL does all the work, so 1. it's easy for us
        2. OpenAL does it in a best way (uses streaming inside,
        so the OggVorbis data in decoded partially, on as-needed basis).

        The disadvantage is obviously that AL_EXT_vorbis must be present...
        And on Windows there doesn't seem a way to get the extension
        working anymore with new OpenAL. This hilarious message
        [http://opensource.creative.com/pipermail/openal/2006-April/009488.html]
        basically says that Creative will not fix AL_EXT_vorbis extension
        in Windows, because it's @italic(too easy) to do.)

      @item(If AL_EXT_vorbis extension is not available but we
        have vorbisfile library available then we use vorbisfile
        functions to decode the file.

        While this works OK, the disadvantages of our current approach
        are that we decode the whole OggVorbis file in one go.
        This means that 1. we waste potentially a lot of memory to keep
        the whole uncompressed data --- 5 MB OggVorbis file can easily
        take 50 MB in memory after decoding 2. whole decoding is done in one go,
        so there is a noticeable time delay when this takes place.
      )
    )

    The check for AL_EXT_vorbis extension and eventual decompression
    using vorbisfile directly take place in the first DataFormat call.
    You can also call method VorbisMethod to check which approach
    (if any) will be used.

    Note that both approaches require vorbisfile library to be installed
    (OpenAL AL_EXT_vorbis extension also works using vorbisfile library).
    If vorbisfile is not available, we cannot load OggVorbis sounds. }
  TSoundOggVorbis = class(TSoundFile)
  private
    DataStream: TMemoryStream;
    FDataFormat: TALuint;
    FFrequency: LongWord;
  public
    constructor CreateFromStream(Stream: TStream); override;
    destructor Destroy; override;

    procedure PrepareOpenAL; override;

    function Data: Pointer; override;
    function DataSize: LongWord; override;
    function DataFormat: TALuint; override;
    function Frequency: LongWord; override;

    class function VorbisMethod: string;
  end;

  EInvalidOggVorbis = class(EInvalidSoundFormat);

  TSoundWAV = class(TSoundFile)
  private
    FData: Pointer;
    FDataSize: LongWord;
    FDataFormat: TALuint;
    FFrequency: LongWord;
  public
    { @raises(EInvalidWAV when loading will fail because the file
      is not a valid (and supported) WAV format.) }
    constructor CreateFromStream(Stream: TStream); override;

    destructor Destroy; override;

    function Data: Pointer; override;
    function DataSize: LongWord; override;
    function DataFormat: TALuint; override;
    function Frequency: LongWord; override;
  end;

  EInvalidWAV = class(EInvalidSoundFormat);

function ALDataFormatToStr(DataFormat: TALuint): string;

implementation

uses CastleStringUtils, CastleVorbisDecoder, CastleVorbisFile, CastleLog,
  CastleDownload, CastleURIUtils;

{ TSoundFile ----------------------------------------------------------------- }

class function TSoundFile.CreateFromFile(const URL: string): TSoundFile;
var
  C: TSoundFileClass;
  S: TStream;
  MimeType: string;
begin
  { soForceMemoryStream as current TSoundWAV and TSoundOggVorbis need seeking }
  S := Download(URL, [soForceMemoryStream], MimeType);
  try
    try
      { calculate class to read based on MimeType }
      if MimeType = 'audio/x-wav' then C := TSoundWAV else
      if MimeType = 'audio/ogg'   then C := TSoundOggVorbis else
      if MimeType = 'audio/mpeg'  then C := TSoundMP3 else
      begin
        WritelnWarning('Audio', Format('Not recognized MIME type "%s" for sound file "%s", trying to load it as wav', [MimeType, URL]));
        C := TSoundWAV;
      end;

      Result := C.CreateFromStream(S);
    except
      on E: EStreamError do
      begin
        { Add URL to exception message }
        E.Message := 'Error while reading URL "' + URIDisplay(URL) + '": ' + E.Message;
        raise;
      end;
    end;
  finally S.Free end;
end;

procedure TSoundFile.CheckALExtension(const S: string);
begin
  if not alIsExtensionPresent(PChar(S)) then
    raise ESoundFormatNotSupportedByOpenAL.CreateFmt('OpenAL extension "%s" ' +
      'required to play this file is not available', [S]);
end;

procedure TSoundFile.PrepareOpenAL;
begin
  { Nothing to do in this class. }
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

{ TSoundMP3 ------------------------------------------------------------------ }

constructor TSoundMP3.CreateFromStream(Stream: TStream);
begin
  inherited Create;
  FDataSize := Stream.Size;
  FData := GetMem(FDataSize);
  Stream.ReadBuffer(Data^, FDataSize);
end;

destructor TSoundMP3.Destroy;
begin
  FreeMemNiling(FData);
  inherited;
end;

function TSoundMP3.Data: Pointer;
begin
  Result := FData;
end;

function TSoundMP3.DataSize: LongWord;
begin
  Result := FDataSize;
end;

procedure TSoundMP3.PrepareOpenAL;
begin
  inherited;

  { Although my OpenAL under Debian reports this extension present,
    it's implementation is actually not finished (looking at the sources),
    and alBufferData raises always "Invalid Value" when passed AL_EXT_mp3.
    So for now, I always raise here ESoundFormatNotSupportedByOpenAL. }
  raise ESoundFormatNotSupportedByOpenAL.Create('MP3 playing not supported');
  { CheckALExtension('AL_EXT_mp3'); }
end;

function TSoundMP3.DataFormat: TALuint;
begin
  Result := AL_FORMAT_MP3_EXT;
end;

function TSoundMP3.Frequency: LongWord;
begin
  { Below is to be completed when MP3 support will be really implemented
    in OpenAL. }
  Result := 0;
end;

{ TSoundOggVorbis ------------------------------------------------------------ }

constructor TSoundOggVorbis.CreateFromStream(Stream: TStream);
begin
  inherited Create;
  DataStream := TMemoryStream.Create;
  DataStream.CopyFrom(Stream, 0);
  DataStream.Position := 0;

  { At the beginning, let's try to use AL_FORMAT_VORBIS_EXT extension.
    Later (in DataFormat call) we will actually check is extension
    present, and if not we will try to use vorbisfile directly. }
  FDataFormat := AL_FORMAT_VORBIS_EXT;
  { The way I understand this, there's no way and no need to pass here
    Frequency, since Ogg Vorbis file's frequency changes during the file.
    This is confirmed by tests (things work OK with returning 0 here),
    and by looking at OpenAL source code (
    openal-0.0.8/src/extensions/al_ext_vorbis.c from Debian libopenal0a
    package) :
      ALint Vorbis_Callback(UNUSED(ALuint sid),
                      ALuint bid,
                      ALshort *outdata,
                      ALenum format,
                      UNUSED(ALint freq),
                      ALint samples)
    ... and freq is really unused. }
  FFrequency := 0;
end;

destructor TSoundOggVorbis.Destroy;
begin
  FreeAndNil(DataStream);
  inherited;
end;

procedure TSoundOggVorbis.PrepareOpenAL;

  procedure ConvertToDirectVorbisFileUse;
  var
    NewDataStream: TMemoryStream;
  begin
    NewDataStream := VorbisDecode(DataStream, FDataFormat, FFrequency);
    FreeAndNil(DataStream);
    DataStream := NewDataStream;
  end;

begin
  inherited;

  if (FDataFormat = AL_FORMAT_VORBIS_EXT) and
    (not alIsExtensionPresent('AL_EXT_vorbis')) then
    ConvertToDirectVorbisFileUse;
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

class function TSoundOggVorbis.VorbisMethod: string;
begin
  if alIsExtensionPresent('AL_EXT_vorbis') then
    Result := 'AL_EXT_vorbis extension' else
  if VorbisFileInited then
    Result := 'vorbisfile library' else
    Result := 'none';
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
    raise EInvalidWAV.Create('WAV file must start with RIFF....WAVE signature');

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
    Riff.Header.Len -= SizeOf(TWavChunkHeader);
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
        raise EInvalidWAV.Create('Loading WAV files not in PCM format not implemented');
      { calculate FDataFormat }
      case Format.Channels of
        1: if Format.BitsPerSample = 8 then
             FDataFormat := AL_FORMAT_MONO8 else
             FDataFormat := AL_FORMAT_MONO16;
        2: if Format.BitsPerSample = 8 then
             FDataFormat := AL_FORMAT_STEREO8 else
             FDataFormat := AL_FORMAT_STEREO16;
        else raise EInvalidWAV.Create('Only WAV files with 1 or 2 channels are allowed');
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
        raise EInvalidWAV.Create('WAV file must not contain mulitple data chunks');
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
