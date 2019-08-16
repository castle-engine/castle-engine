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

{ OggVorbis decoder.
  During the initialization, this unit registers handling of OggVorbis
  file format using RegisterSoundFormat.
}
unit CastleInternalVorbisDecoder;

{$I castleconf.inc}

interface

uses SysUtils;

type
  EOggVorbisLoadError = class(Exception);
  EOggVorbisMissingLibraryError = class(EOggVorbisLoadError);
  EOggVorbisFileError = class(EOggVorbisLoadError);

implementation

uses Classes, CTypes,
  CastleUtils, CastleClassUtils, CastleInternalVorbisCodec,
  CastleSoundBase, CastleInternalSoundFile, CastleInternalVorbisFile,
  CastleTimeUtils;

{ VorbisDecoder_ callbacks code based on Noeska code from
  [http://www.noeska.com/doal/tutorials.aspx].
  Heavily modified for CGE (e.g. to allow exceptions raising in case of
  stream errors (instead of silencing these exceptions), to check (ReadCount mod
  Size) in read_func and whence in seek_func, close does nothing). }

function VorbisDecoder_read_func(ptr: Pointer;
  Size: TSizeT; nmemb: TSizeT; DataSource: Pointer): TSizeT; cdecl;
{ Returns amount of items completely read successfully, returns indeterminate
  value on error. The value of a partially read item cannot be determined. Does
  not lead to valid feof or ferror responses, because they are not possible to
  supply to VorbisFile }
var
  ReadCount: Int64;
begin
  if (size = 0) or (nmemb = 0) then
  begin
    Result := 0;
    Exit;
  end;

  ReadCount := TStream(DataSource).Read(ptr^, Size * nmemb);
  Assert(ReadCount mod Size = 0);
  Result := ReadCount div Size;
end;

function VorbisDecoder_seek_func(DataSource: Pointer;
  offset: Int64; whence: CInt): CInt; cdecl;
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
begin
  try
    case whence of
      SEEK_CUR: TStream(DataSource).Seek(offset, soFromCurrent);
      SEEK_END: TStream(DataSource).Seek(offset, soFromEnd);
      SEEK_SET: TStream(DataSource).Seek(offset, soFromBeginning);
      else raise EInternalError.CreateFmt('Invalid VorbisDecoder_seek_func ' +
        'whence param: %d', [whence]);
    end;
    Result := 0;
  except
    { If the Stream is unseekable, vorbisfile allows us to return here -1. }
    Result := -1;
  end;
end;

function VorbisDecoder_close_func(DataSource: Pointer): CInt;
  cdecl;
begin
  Result := 0;
end;

function VorbisDecoder_tell_func(DataSource: Pointer): CLong;
  cdecl;
begin
  Result := TStream(DataSource).Position;
end;

procedure CheckVorbisFile(Err: CInt; const Event: string);
var
  ErrDescription: string;
begin
  { Errors list and ErrDescription values based on
    [http://xiph.org/vorbis/doc/vorbisfile/return.html] }
  case Err of
    OV_FALSE: ErrDescription := 'No data available';
    OV_HOLE: ErrDescription := 'Vorbisfile encountered missing or corrupt data in the bitstream'; {. Recovery is normally automatic and this return code is for informational purposes only. }
    OV_EREAD: ErrDescription := 'Read error while fetching compressed data for decode';
    OV_EFAULT: ErrDescription := 'Internal inconsistency in decode state'; {. Continuing is likely not possible. }
    OV_EIMPL: ErrDescription := 'Feature not implemented';
    OV_EINVAL: ErrDescription := 'Either an invalid argument, or incompletely initialized argument passed to libvorbisfile call';
    OV_ENOTVORBIS: ErrDescription := 'The given file/data was not recognized as Ogg Vorbis data';
    OV_EBADHEADER: ErrDescription := 'The file/data is apparently an Ogg Vorbis stream, but contains a corrupted or undecipherable header';
    OV_EVERSION: ErrDescription := 'The bitstream format revision of the given stream is not supported';
    OV_EBADLINK: ErrDescription := 'The given link exists in the Vorbis data stream, but is not decipherable due to garbacge or corruption';
    OV_ENOSEEK: ErrDescription := 'The given stream is not seekable';
    else ErrDescription := '(unknown vorbisfile error code)';
  end;

  if Err <> 0 then
    raise EOggVorbisFileError.CreateFmt('VorbisFile error %d at "%s": %s',
      [Err, Event, ErrDescription]);
end;

{ TOggVorbisStream ----------------------------------------------------------- }

type
  { Decode OggVorbis file contents.

    Uses LibVorbisFile or Tremolo libraries.
    Both are open-source libraries
    for reading OggVorbis music from https://xiph.org/.
    Tremolo is used on mobile devices, libvorbisfile on desktop.
    Checks VorbisFileInitialized at the beginning, so you don't have to
    worry about it.

    Gets data from ObjectPascal TStream (like TFileStream)
    and returns data as TStream.

    All methods may raise EOggVorbisLoadError If decoding OggVorbis stream failed,
    this may also happen if the vorbisfile / tremolo library is not available.
  }
  TOggVorbisStream = class(TOwnerStream)
  strict private
    VorbisFile: TOggVorbis_File;
    // Is VorbisFile valid, e.g. we can call ov_clear on it.
    VorbisFileValid: Boolean;
    procedure OpenVorbisFile(out DataFormat: TSoundDataFormat;
      out Frequency: LongWord; out Duration: TFloatTime);
    procedure CloseVorbisFile;
  public
    constructor Create(const ASourceStream: TStream;
      out DataFormat: TSoundDataFormat; out Frequency: LongWord;
      out Duration: TFloatTime);
    destructor Destroy; override;
    function Read(var Buffer; BufferSize: Longint): Longint; override;

    { Only supports seeking to the beginning (or seeking to current position,
      which does nothing). }
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    { Use this function to register OggVorbis handling with RegisterSoundFormat. }
    class function ReadStream(const Url: string; const Stream: TStream;
      out DataFormat: TSoundDataFormat; out Frequency: LongWord;
      out Duration: TFloatTime): TStream;
  end;

constructor TOggVorbisStream.Create(const ASourceStream: TStream;
  out DataFormat: TSoundDataFormat; out Frequency: LongWord;
  out Duration: TFloatTime);
begin
  inherited Create(ASourceStream);
  OpenVorbisFile(DataFormat, Frequency, Duration);
end;

destructor TOggVorbisStream.Destroy;
begin
  CloseVorbisFile;
  inherited;
end;

procedure TOggVorbisStream.OpenVorbisFile(
  out DataFormat: TSoundDataFormat; out Frequency: LongWord;
  out Duration: TFloatTime);
var
  OggInfo: Pvorbis_info;
  Callbacks: Tov_callbacks;
begin
  if not VorbisFileInitialized then
    raise EOggVorbisMissingLibraryError.Create('Library to decode OggVorbis (LibVorbisFile on desktops, Tremolo on mobile) is not available');

  Callbacks.read_func := @VorbisDecoder_read_func;
  Callbacks.seek_func := @VorbisDecoder_seek_func;
  Callbacks.close_func := @VorbisDecoder_close_func;
  Callbacks.tell_func := @VorbisDecoder_tell_func;
  CheckVorbisFile(ov_open_callbacks(Source, @VorbisFile, nil, 0, Callbacks),
    'ov_open_callbacks');

  VorbisFileValid := true; // ov_clear on VorbisFile now is valid

  OggInfo := ov_info(@VorbisFile, -1);

  if OggInfo^.channels = 1 then
    DataFormat := sfMono16
  else
    DataFormat := sfStereo16;

  Frequency := OggInfo^.rate;

  Duration := ov_time_total(@VorbisFile, -1);
end;

procedure TOggVorbisStream.CloseVorbisFile;
begin
  if VorbisFileValid then
  begin
    ov_clear(@VorbisFile);
    VorbisFileValid := false;
  end;
end;

function TOggVorbisStream.Read(var Buffer; BufferSize: Longint): Longint;
(*

  This simple implementation may not fill enough space to be useful to streaming.
  TODO: This is a valid Read implementation.
  Higher-level music streaming should wrap it in something like ReadSensibleAmountOfData,
  and thus be useful both for OggVorbis and other formats.

var
  BitStream: CInt;
begin
  Result := ov_read(@VorbisFile, Buffer, BufferSize,
    { OpenAL always wants little endian ? } 0,
    { Always give us 16 bits } 2, 1, @BitStream);

  if Result < 0 then
    CheckVorbisFile(Result, 'ov_read');
end;
*)

var
  BitStream: CInt;
  NowRead, TotalRead: LongInt;
  FreeSpace: LongInt;
  BufferPoint: PByte;
begin
  BufferPoint := @Buffer;

  NowRead := ov_read(@VorbisFile, Buffer, BufferSize,
    { OpenAL always wants little endian ? } 0,
    { Always give us 16 bits } 2, 1, @BitStream);

  if NowRead < 0 then
    CheckVorbisFile(NowRead, 'ov_read');

  TotalRead := NowRead;
  FreeSpace := BufferSize - TotalRead;

  if FreeSpace > 4096 then
  begin
    BufferPoint := BufferPoint + TotalRead;

    while FreeSpace > 4096 do
    begin
      NowRead := ov_read(@VorbisFile, BufferPoint^, FreeSpace,
      { OpenAL always wants little endian ? } 0,
      { Always give us 16 bits } 2, 1, @BitStream);

      if NowRead < 0 then
        CheckVorbisFile(NowRead, 'ov_read');

      if NowRead = 0 then
        Exit(TotalRead);

      Inc(TotalRead, NowRead);
      FreeSpace := FreeSpace - NowRead;
      BufferPoint := BufferPoint + NowRead;
    end;
  end;

  Result := TotalRead;
end;

function TOggVorbisStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  IgnoreDataFormat: TSoundDataFormat;
  IgnoreFrequency: LongWord;
  IgnoreDuration: TFloatTime;
begin
  if (Origin = soCurrent  ) and (Offset = 0) then
    { nothing needs to be done, ok }
    Exit;

  if (Origin = soBeginning) and (Offset = 0) then
  begin
    CloseVorbisFile;
    Source.Position := 0;
    OpenVorbisFile(IgnoreDataFormat, IgnoreFrequency, IgnoreDuration);
    Exit;
  end;

  raise EStreamNotImplementedSeek.Create('TOggVorbisStream.Seek not supported for these arguments (only seeking to current or beginning position are allowed)');
  Result := 0; // just to get rid of warning
end;

class function TOggVorbisStream.ReadStream(const Url: string; const Stream: TStream;
  out DataFormat: TSoundDataFormat; out Frequency: LongWord;
  out Duration: TFloatTime): TStream;
begin
  Result := TOggVorbisStream.Create(Stream, DataFormat, Frequency, Duration);
end;

initialization
  RegisterSoundFormat('audio/ogg', @TOggVorbisStream(nil).ReadStream);
end.
