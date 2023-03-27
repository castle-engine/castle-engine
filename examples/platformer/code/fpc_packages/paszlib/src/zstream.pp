unit ZStream;

{**********************************************************************
    This file is part of the Free Pascal free component library.

    Copyright (c) 2007 by Daniel Mantione
      member of the Free Pascal development team

    Implements a Tstream descendents that allow you to read and write
    compressed data according to the Deflate algorithm described in
    RFC1951.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ GZip specs: https://datatracker.ietf.org/doc/html/rfc1952 }

{$mode objfpc}

{***************************************************************************}
                                    interface
{***************************************************************************}

uses    classes,zbase,gzio;

type
        Tcompressionlevel=(
          clnone,                     {Do not use compression, just copy data.}
          clfastest,                  {Use fast (but less) compression.}
          cldefault,                  {Use default compression}
          clmax                       {Use maximum compression}
        );

        Tgzopenmode=(
          gzopenread,                 {Open file for reading.}
          gzopenwrite                 {Open file for writing.}
        );

        Tcustomzlibstream=class(Townerstream)
        protected
          Fstream:z_stream;
          Fbuffer:pointer;
          Fonprogress:Tnotifyevent;
          procedure progress(sender:Tobject);
          property onprogress:Tnotifyevent read Fonprogress write Fonprogress;
        public
          constructor create(stream:Tstream);
          destructor destroy;override;
        end;

        { Tcompressionstream }

        Tcompressionstream=class(Tcustomzlibstream)
        private
          procedure ClearOutBuffer;
        protected
          raw_written,compressed_written: int64;
        public
          constructor create(level:Tcompressionlevel;
                             dest:Tstream;
                             Askipheader:boolean=false);
          destructor destroy;override;
          function write(const buffer;count:longint):longint;override;
          procedure flush;
          function get_compressionrate:single;
          property OnProgress;
        end;

        Tdecompressionstream=class(Tcustomzlibstream)
        protected
          raw_read,compressed_read:int64;
          skipheader:boolean;
          procedure reset;
          function GetPosition() : Int64; override;
        public
          constructor create(Asource:Tstream;Askipheader:boolean=false);
          destructor destroy;override;
          function read(var buffer;count:longint):longint;override;
          function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;  override;
          function get_compressionrate:single;
          property OnProgress;
        end;

        TGZFileStream = Class(TStream)
        protected
          Fgzfile:gzfile;
          Ffilemode:Tgzopenmode;
        public
          constructor create(filename:ansistring;filemode:Tgzopenmode);
          function read(var buffer;count:longint):longint;override;
          function write(const buffer;count:longint):longint;override;
          function seek(offset:longint;origin:word):longint;override;
          destructor destroy;override;
        end;

        TGZipCompressionStream = class(TStream)
        private
          FLevel: TCompressionLevel;
          FCrc32Val: Longword;
          FUncompressedSize: Cardinal;
          FDest: TStream;
          FCompressionStream: TCompressionStream;
          procedure WriteHeader;
          procedure WriteFooter;
        public
          constructor Create(ADest: TStream); overload;
          constructor Create(ALevel: TCompressionLevel; ADest: TStream); overload;
          destructor Destroy; override;
          function Write(const Buffer; Count: Longint): Longint; override;
        end;

        TGZipDecompressionStream = class(TStream)
        private
          FCrc32Val: Longword;
          FUncompressedSize: Cardinal;
          FSource: TStream;
          FDecompressionStream: TDecompressionStream;
          procedure Assert(ACond: Boolean; AMsg: string = '');
          procedure ReadHeader;
          procedure ReadFooter;
        public
          constructor Create(ASource: TStream);
          destructor Destroy; override;
          function Read(var Buffer; Count: Longint): Longint; override;
          function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
        end;

        Ezliberror=class(Estreamerror)
        end;

        Egzfileerror=class(Ezliberror)
        end;

        Ecompressionerror=class(Ezliberror)
        end;

        Edecompressionerror=class(Ezliberror)
        end;

{***************************************************************************}
                                 implementation
{***************************************************************************}

uses    zdeflate,zinflate;

const   bufsize=16384;     {Size of the buffer used for temporarily storing
                            data from the child stream.}
  Crc_32_Tab : Array[0..255] of LongWord = (
    $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3,
    $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
    $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
    $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
    $3b6e20c8, $4c69105e, $d56041e4, $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
    $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
    $26d930ac, $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
    $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
    $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
    $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
    $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
    $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
    $4369e96a, $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
    $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
    $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
    $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
    $f00f9344, $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7,
    $fed41b76, $89d32be0, $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
    $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79,
    $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f,
    $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
    $9b64c2b0, $ec63f226, $756aa39c, $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
    $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
    $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
    $88085ae6, $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d, $3e6e77db,
    $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
    $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23d967bf,
    $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
  );

resourcestring Sgz_open_error='Could not open gzip compressed file %s.';
               Sgz_read_only='Gzip compressed file was opened for reading.';
               Sgz_write_only='Gzip compressed file was opened for writing.';
               Sgz_invalid_header='Invalid GZip header';
               Sgz_invalid_algorithm='Invalid compression algorithm';
               Sgz_invalid_crc32='Invalid crc32 checksum';
               Sgz_invalid_output_size='Invalid output size';
               Sseek_failed='Seek in deflate compressed stream failed.';

function UpdateCrc32(Crc: Longword; const Buffer; Count: Longint): Longword;
var
 PBuf: PByte;
 i: Longint;
begin
 PBuf := @Buffer;

 Result := Crc xor $FFFFFFFF;
 for i := 1 to Count do
 begin
   Result := Crc_32_Tab[(Result xor PBuf^) and $ff] xor (Result shr 8);
   Inc(PBuf);
 end;
 Result := Result xor $FFFFFFFF;
end;

constructor Tcustomzlibstream.create(stream:Tstream);

begin
  assert(stream<>nil);
  inherited create(stream);
  getmem(Fbuffer,bufsize);
end;

procedure Tcustomzlibstream.progress(sender:Tobject);

begin
  if Fonprogress<>nil then
    Fonprogress(sender);
end;

destructor Tcustomzlibstream.destroy;

begin
  freemem(Fbuffer);
  inherited destroy;
end;

{***************************************************************************}

constructor Tcompressionstream.create(level:Tcompressionlevel;
                                      dest:Tstream;
                                      Askipheader:boolean=false);

var err,l:smallint;

begin
  inherited create(dest);
  Fstream.next_out:=Fbuffer;
  Fstream.avail_out:=bufsize;

  case level of
    clnone:
      l:=Z_NO_COMPRESSION;
    clfastest:
      l:=Z_BEST_SPEED;
    cldefault:
      l:=Z_DEFAULT_COMPRESSION;
    clmax:
      l:=Z_BEST_COMPRESSION;
  end;

  if Askipheader then
    err:=deflateInit2(Fstream,l,Z_DEFLATED,-MAX_WBITS,DEF_MEM_LEVEL,0)
  else
    err:=deflateInit(Fstream,l);
  if err<>Z_OK then
    raise Ecompressionerror.create(zerror(err));
end;

function Tcompressionstream.write(const buffer;count:longint):longint;

var err:smallint;
    lastavail:longint;

begin
  Fstream.next_in:=@buffer;
  Fstream.avail_in:=count;
  lastavail:=count;
  while Fstream.avail_in<>0 do
    begin
      if Fstream.avail_out=0 then
        ClearOutBuffer;
      inc(raw_written,lastavail-Fstream.avail_in);
      lastavail:=Fstream.avail_in;
      err:=deflate(Fstream,Z_NO_FLUSH);
      if err<>Z_OK then
        raise Ecompressionerror.create(zerror(err));
    end;
  inc(raw_written,lastavail-Fstream.avail_in);
  write:=count;
end;

function Tcompressionstream.get_compressionrate:single;

begin
  get_compressionrate:=100*compressed_written/raw_written;
end;

procedure TCompressionstream.ClearOutBuffer;

begin
  { Flush the buffer to the stream and update progress }
  source.writebuffer(Fbuffer^,bufsize-Fstream.avail_out);
  inc(compressed_written,bufsize-Fstream.avail_out);
  progress(self);
  { reset output buffer }
  Fstream.next_out:=Fbuffer;
  Fstream.avail_out:=bufsize;
end;

procedure Tcompressionstream.flush;

var err:smallint;

begin
  {Compress remaining data still in internal zlib data buffers.}
  repeat
    if Fstream.avail_out=0 then
      ClearOutBuffer;
    err:=deflate(Fstream,Z_FINISH);
    if err=Z_STREAM_END then
      break;
    if (err<>Z_OK) then
      raise Ecompressionerror.create(zerror(err));
  until false;
  if Fstream.avail_out<bufsize then
    ClearOutBuffer;
end;


destructor Tcompressionstream.destroy;

begin
  try
    Flush;
  finally
    deflateEnd(Fstream);
    inherited destroy;
  end;
end;

{***************************************************************************}

constructor Tdecompressionstream.create(Asource:Tstream;Askipheader:boolean=false);

var err:smallint;

begin
  inherited create(Asource);

  skipheader:=Askipheader;
  if Askipheader then
    err:=inflateInit2(Fstream,-MAX_WBITS)
  else
    err:=inflateInit(Fstream);
  if err<>Z_OK then
    raise Edecompressionerror.create(zerror(err));
end;

function Tdecompressionstream.read(var buffer;count:longint):longint;

var err:smallint;
    lastavail:longint;

begin
  Fstream.next_out:=@buffer;
  Fstream.avail_out:=count;
  lastavail:=count;
  while Fstream.avail_out<>0 do
    begin
      if Fstream.avail_in=0 then
        begin
          {Refill the buffer.}
          Fstream.next_in:=Fbuffer;
          Fstream.avail_in:=source.read(Fbuffer^,bufsize);
          inc(compressed_read,Fstream.avail_in);
          inc(raw_read,lastavail-Fstream.avail_out);
          lastavail:=Fstream.avail_out;
          progress(self);
        end;
      err:=inflate(Fstream,Z_NO_FLUSH);
      if err=Z_STREAM_END then
        break;
      if err<>Z_OK then
        raise Edecompressionerror.create(zerror(err));
    end;
  if err=Z_STREAM_END then
    dec(compressed_read,Fstream.avail_in);
  inc(raw_read,lastavail-Fstream.avail_out);
  read:=count-Fstream.avail_out;
end;

procedure Tdecompressionstream.reset;

var err:smallint;

begin
  source.seek(-compressed_read,sofromcurrent);
  raw_read:=0;
  compressed_read:=0;
  inflateEnd(Fstream);
  if skipheader then
    err:=inflateInit2(Fstream,-MAX_WBITS)
  else
    err:=inflateInit(Fstream);
  if err<>Z_OK then
    raise Edecompressionerror.create(zerror(err));
end;

function Tdecompressionstream.GetPosition() : Int64;
begin
  GetPosition := raw_read;
end;

function Tdecompressionstream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;

var c,off: int64;
    buf: array[0..8191] of Byte;

begin
  off:=Offset;

  if origin=soCurrent then
    inc(off,raw_read);
  if (origin=soEnd) or (off<0) then
    raise Edecompressionerror.create(Sseek_failed);

  seek:=off;

  if off<raw_read then
    reset
  else
    dec(off,raw_read);

  while off>0 do
    begin
      c:=off;
      if c>SizeOf(buf) then
        c:=SizeOf(buf);
      if read(buf,c)<>c then
        raise Edecompressionerror.create(Sseek_failed);
      dec(off,c);
    end;
end;

function Tdecompressionstream.get_compressionrate:single;

begin
  get_compressionrate:=100*compressed_read/raw_read;
end;


destructor Tdecompressionstream.destroy;

begin
  inflateEnd(Fstream);
  inherited destroy;
end;


{***************************************************************************}

constructor Tgzfilestream.create(filename:ansistring;filemode:Tgzopenmode);

begin
  if filemode=gzopenread then
    Fgzfile:=gzopen(filename,'rb')
  else
    Fgzfile:=gzopen(filename,'wb');
  Ffilemode:=filemode;
  if Fgzfile=nil then
    raise Egzfileerror.createfmt(Sgz_open_error,[filename]);
end;

function Tgzfilestream.read(var buffer;count:longint):longint;

begin
  if Ffilemode=gzopenwrite then
    raise Egzfileerror.create(Sgz_write_only);
  read:=gzread(Fgzfile,@buffer,count);
end;

function Tgzfilestream.write(const buffer;count:longint):longint;

begin
  if Ffilemode=gzopenread then
    raise Egzfileerror.create(Sgz_write_only);
  write:=gzwrite(Fgzfile,@buffer,count);
end;

function Tgzfilestream.seek(offset:longint;origin:word):longint;

begin
  seek:=gzseek(Fgzfile,offset,origin);
  if seek=-1 then
    raise egzfileerror.create(Sseek_failed);
end;

destructor Tgzfilestream.destroy;

begin
  gzclose(Fgzfile);
  inherited destroy;
end;

{ TGZipCompressionStream }

constructor TGZipCompressionStream.Create(ADest: TStream);
begin
  Create(clDefault, ADest);
end;

constructor TGZipCompressionStream.Create(ALevel: TCompressionLevel; ADest: TStream);
begin
  inherited Create;
  FLevel := ALevel;
  FCrc32Val := 0;
  FUncompressedSize := 0;
  FDest := ADest;
  WriteHeader;
  FCompressionStream := TCompressionStream.Create(FLevel, FDest, True);
end;

destructor TGZipCompressionStream.Destroy;
begin
  FCompressionStream.Flush;
  FCompressionStream.Free;
  WriteFooter;
  inherited;
end;

procedure TGZipCompressionStream.WriteHeader;
begin
  FDest.WriteByte($1f);  // signature 1 of 2
  FDest.WriteByte($8b);  // signature 2 of 2
  FDest.WriteByte($08);  // deflate algorithm
  FDest.WriteByte($00);  // no flags
  FDest.WriteDWord($00); // modification time unknown. Source is stream, not a file
  if FLevel = clmax then // XFL = extra flags = compression level
    FDest.WriteByte($02)
  else if FLevel = clfastest then
    FDest.WriteByte($04)
  else
    FDest.WriteByte($00);
  FDest.WriteByte($ff);  // OS file system unknown. Source is stream, not a file
end;

procedure TGZipCompressionStream.WriteFooter;
var
  i: Integer;
begin
  // write crc32 in 4 bytes, least significant byte first
  for i := 1 to 4 do
  begin
    FDest.WriteByte(FCrc32Val and $ff);
    FCrc32Val := FCrc32Val shr 8;
  end;

  // write uncompressed size in 4 bytes, least significant byte first
  for i := 1 to 4 do
  begin
    FDest.WriteByte(FUncompressedSize and $ff);
    FUncompressedSize := FUncompressedSize shr 8;
  end;
end;

function TGZipCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  FCrc32Val := UpdateCrc32(FCrc32Val, Buffer, Count);
  Inc(FUncompressedSize, Count);
  Result := FCompressionStream.Write(Buffer, Count);
end;

{ TGZipDecompressionStream }

constructor TGZipDecompressionStream.Create(ASource: TStream);
begin
  inherited Create;
  FSource := ASource;
  FCrc32Val := 0;
  FUncompressedSize := 0;
  ReadHeader;
  FDecompressionStream := TDecompressionStream.Create(FSource, True);
end;

destructor TGZipDecompressionStream.Destroy;
begin
  FDecompressionStream.Free;
  inherited;
end;

procedure TGZipDecompressionStream.Assert(ACond: Boolean; AMsg: string = '');
begin
  if not ACond then
    raise EDecompressionError.Create(AMsg);
end;

procedure TGZipDecompressionStream.ReadHeader;
var
  Flags: Byte;
  XLEN: Cardinal;
begin
  Assert(FSource.ReadByte = $1f, Sgz_invalid_header);
  Assert(FSource.ReadByte = $8b, Sgz_invalid_header);
  Assert(FSource.ReadByte = $08, Sgz_invalid_algorithm);
  Flags := FSource.ReadByte;
  FSource.ReadDWord; // skip modification time. Dest is stream, not a file
  FSource.ReadByte;  // skip compression level, is not needed
  FSource.ReadByte;  // skip OS file system. Dest is stream, not a file

  if (Flags and $4) <> 0 then // FLG.FEXTRA
  begin
    XLEN := FSource.ReadByte + FSource.ReadByte shl 8; // least significant byte first
    while XLEN > 0 do
    begin
      FSource.ReadByte;
      Dec(XLEN);
    end;
  end;

  if (Flags and $8) <> 0 then // FLG.FNAME
  begin
    repeat
    until FSource.ReadByte = 0; // zero-terminated file name
  end;

  if (Flags and $10) <> 0 then // FLG.FCOMMENT
  begin
    repeat
    until FSource.ReadByte = 0; // zero-terminated file comment
  end;

  if (Flags and $2) <> 0 then // FLG.FHCRC
    FSource.ReadWord; // skip CRC16, check not implemented
end;

procedure TGZipDecompressionStream.ReadFooter;
var
  Crc32: Longword;
  OrigSize: Longword;
begin
  // The TDecompressionStream reads in buffers, so the footer
  // may already be skipped. Therefore, we need to Seek to the footer.
  // If FSource is non-seekable, we skip checking Crc32 and OrigSize.
  try
    FSource.Seek(-8, soEnd);
  except
    Exit;  // skip Crc32 and OrigSize checking
  end;

  Crc32 := FSource.ReadByte + FSource.ReadByte shl 8 + FSource.ReadByte shl 16 + FSource.ReadByte shl 24;
  Assert(FCrc32Val = Crc32, Sgz_invalid_crc32);
  OrigSize := FSource.ReadByte + FSource.ReadByte shl 8 + FSource.ReadByte shl 16 + FSource.ReadByte shl 24;
  Assert(FUncompressedSize = OrigSize, Sgz_invalid_output_size);
end;

function TGZipDecompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FDecompressionStream.Read(Buffer, Count);
  Inc(FUncompressedSize, Result);
  FCrc32Val := UpdateCrc32(FCrc32Val, Buffer, Result);

  if Result < Count then
    ReadFooter;
end;

function TGZipDecompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // accept Seek(0, soBeginning) if nothing read yet
  // this is needed for the TStream.CopyFrom() method
  if (Offset = 0) and (Origin = TSeekOrigin.soBeginning) and (FUncompressedSize = 0) then
    Result := 0
  else
    Result := inherited Seek(Offset, Origin);
end;

end.
