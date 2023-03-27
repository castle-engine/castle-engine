unit PasZLib;

{$inline on}

interface

uses
  zbase;

const
  ZLIB_VERSION = '1.2';

type
  { Compatibility types }
  z_off_t = longint;

  TInternalState = record
    end;
  PInternalState = ^TInternalstate;

  TZStream = z_stream;
  PZstream = ^TZStream;

  gzFile = pointer;


const
  Z_NO_FLUSH = 0;

  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;

  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = -(1);
  Z_STREAM_ERROR = -(2);
  Z_DATA_ERROR = -(3);
  Z_MEM_ERROR = -(4);
  Z_BUF_ERROR = -(5);
  Z_VERSION_ERROR = -(6);

  Z_NO_COMPRESSION = 0;
  Z_BEST_SPEED = 1;
  Z_BEST_COMPRESSION = 9;
  Z_DEFAULT_COMPRESSION = -(1);

  Z_FILTERED = 1;
  Z_HUFFMAN_ONLY = 2;
  Z_DEFAULT_STRATEGY = 0;

  Z_BINARY = 0;
  Z_ASCII = 1;
  Z_UNKNOWN = 2;

  Z_DEFLATED = 8;

  Z_NULL = nil;

function zlibVersion:string;inline;
function deflate(var strm:TZstream; flush:longint):longint;inline;
function deflateEnd(var strm:TZstream):longint;inline;
function inflate(var strm:TZstream; flush:longint):longint;inline;
function inflateEnd(var strm:TZstream):longint;inline;
function deflateSetDictionary(var strm:TZstream;dictionary : Pchar; dictLength:cardinal):longint;inline;
function deflateCopy(var dest,source:TZstream):longint;inline;
function deflateReset(var strm:TZstream):longint;inline;
function deflateParams(var strm:TZstream; level:longint; strategy:longint):longint;inline;
function inflateSetDictionary(var strm:TZStream;dictionary : Pchar; dictLength:cardinal):longint;inline;
function inflateSync(var strm:TZStream):longint;inline;
function inflateReset(var strm:TZStream):longint;inline;
function compress(dest:Pchar;var destLen:cardinal; source : Pchar; sourceLen:cardinal):longint;
function compress2(dest:Pchar;var destLen:cardinal; source : Pchar; sourceLen:cardinal; level:longint):longint;
function uncompress(dest:Pchar;var destLen:cardinal; source : Pchar; sourceLen:cardinal):longint;
function gzopen(path:Pchar; mode:Pchar):gzFile;inline;
function gzsetparams(Thefile:gzFile; level:longint; strategy:longint):longint;inline;
function gzread(thefile:gzFile; buf : pointer; len:cardinal):longint;inline;
function gzwrite(thefile:gzFile; buf: pointer; len:cardinal):longint;inline;
function gzputs(thefile:gzFile; s:Pchar):longint;inline;
function gzgets(thefile:gzFile; buf:Pchar; len:longint):Pchar;inline;
function gzputc(thefile:gzFile; c:char):longint;inline;
function gzgetc(thefile:gzFile):char;inline;
function gzflush(thefile:gzFile; flush:longint):longint;inline;
function gzseek(thefile:gzFile; offset:z_off_t; whence:longint):z_off_t;inline;
function gzrewind(thefile:gzFile):longint;inline;
function gztell(thefile:gzFile):z_off_t;inline;
function gzeof(thefile:gzFile):longbool;inline;
function gzclose(thefile:gzFile):longint;inline;
function gzerror(thefile:gzFile; var errnum:smallint):string;inline;
function adler32(theadler:cardinal;buf : Pchar; len:cardinal):cardinal;inline;
function crc32(thecrc:cardinal;buf : Pchar; len:cardinal):cardinal;inline;
function deflateInit_(var strm:TZStream; level:longint; version:Pchar; stream_size:longint):longint;inline;
function inflateInit_(var strm:TZStream; version:Pchar; stream_size:longint):longint;inline;
function deflateInit2_(var strm:TZStream; level:longint; method:longint; windowBits:longint; memLevel:longint;strategy:longint; version:Pchar; stream_size:longint):longint;inline;
function inflateInit2_(var strm:TZStream; windowBits:longint; version:Pchar; stream_size:longint):longint;inline;
function deflateInit(var strm:TZStream;level : longint) : longint;inline;
function inflateInit(var strm:TZStream) : longint;inline;
function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;inline;
function inflateInit2(var strm:TZStream; windowBits : longint) : longint;inline;
function zError(err:longint):string;inline;
function inflateSyncPoint(z:PZstream):longint;inline;
function get_crc_table:pointer;inline;

implementation

uses
  zdeflate,zinflate,zcompres,zuncompr,gzio,adler,crc;

function zlibVersion:string;inline;
begin
  zlibversion:=zbase.zlibversion;
end;

function deflate(var strm:TZstream; flush:longint):longint;inline;
begin
  deflate:=zdeflate.deflate(strm,flush);
end;

function deflateEnd(var strm:TZstream):longint;inline;
begin
  deflateEnd:=zdeflate.deflateEnd(strm);
end;

function inflate(var strm:TZstream; flush:longint):longint;inline;
begin
  inflate:=zinflate.inflate(strm,flush);
end;

function inflateEnd(var strm:TZstream):longint;inline;
begin
  inflateEnd:=zinflate.inflateEnd(strm);
end;

function deflateSetDictionary(var strm:TZstream;dictionary : Pchar; dictLength:cardinal):longint;inline;
begin
  deflateSetDictionary:=zdeflate.deflateSetDictionary(strm,Pbyte(dictionary),dictlength);
end;

function deflateCopy(var dest,source:TZstream):longint;inline;
begin
  deflateCopy:=zdeflate.deflateCopy(@dest,@source);
end;

function deflateReset(var strm:TZstream):longint;inline;
begin
  deflateReset:=zdeflate.deflateReset(strm);
end;

function deflateParams(var strm:TZstream; level:longint; strategy:longint):longint;inline;
begin
  deflateParams:=zdeflate.deflateParams(strm,level,strategy);
end;

function inflateSetDictionary(var strm:TZStream;dictionary : Pchar; dictLength:cardinal):longint;inline;
begin
  inflateSetDictionary:=zinflate.inflateSetDictionary(strm,Pbyte(dictionary),dictlength);
end;

function inflateSync(var strm:TZStream):longint;inline;
begin
  inflateSync:=zinflate.inflateSync(strm);
end;

function inflateReset(var strm:TZStream):longint;inline;
begin
  inflateReset:=zinflate.inflateReset(strm);
end;

function compress(dest:Pchar;var destLen:cardinal; source : Pchar; sourceLen:cardinal):longint;

type Pbytearray=^Tbytearray;
     Tbytearray=array[0..0] of byte;

begin
  compress:=zcompres.compress(Pbyte(dest),destlen,Pbytearray(source)^,sourcelen);
end;

function compress2(dest:Pchar;var destLen:cardinal; source : Pchar; sourceLen:cardinal; level:longint):longint;

type Pbytearray=^Tbytearray;
     Tbytearray=array[0..0] of byte;

begin
  compress2:=zcompres.compress2(Pbyte(dest),destlen,Pbytearray(source)^,sourcelen,level);
end;

function uncompress(dest:Pchar;var destLen:cardinal; source : Pchar; sourceLen:cardinal):longint;

type Pbytearray=^Tbytearray;
     Tbytearray=array[0..0] of byte;

begin
  uncompress:=zuncompr.uncompress(Pbyte(dest),destlen,Pbytearray(source)^,sourcelen);
end;

function gzopen(path:Pchar; mode:Pchar):gzFile;inline;
begin
  gzopen:=gzio.gzopen(path,mode);
end;

function gzsetparams(Thefile:gzFile; level:longint; strategy:longint):longint;inline;
begin
  gzsetparams:=gzio.gzsetparams(thefile,level,strategy);
end;

function gzread(thefile:gzFile; buf : pointer; len:cardinal):longint;inline;
begin
  gzread:=gzio.gzread(thefile,buf,len);
end;

function gzwrite(thefile:gzFile; buf: pointer; len:cardinal):longint;inline;
begin
  gzwrite:=gzio.gzwrite(thefile,buf,len);
end;

function gzputs(thefile:gzFile; s:Pchar):longint;inline;
begin
  gzputs:=gzio.gzputs(thefile,s);
end;

function gzgets(thefile:gzFile; buf:Pchar; len:longint):Pchar;inline;
begin
  gzgets:=gzio.gzgets(thefile,buf,len);
end;

function gzputc(thefile:gzFile; c:char):longint;inline;
begin
  gzputc:=gzio.gzputc(thefile,c);
end;

function gzgetc(thefile:gzFile):char;inline;
begin
  gzgetc:=chr(gzio.gzgetc(thefile));
end;

function gzflush(thefile:gzFile; flush:longint):longint;inline;
begin
  gzflush:=gzio.gzflush(thefile,flush);
end;

function gzseek(thefile:gzFile; offset:z_off_t; whence:longint):z_off_t;inline;
begin
  gzseek:=gzio.gzseek(thefile,offset,whence);
end;

function gzrewind(thefile:gzFile):longint;inline;
begin
  gzrewind:=gzio.gzrewind(thefile);
end;

function gztell(thefile:gzFile):z_off_t;inline;
begin
  gztell:=gzio.gztell(thefile);
end;

function gzeof(thefile:gzFile):longbool;inline;
begin
  gzeof:=gzio.gzeof(thefile);
end;

function gzclose(thefile:gzFile):longint;inline;
begin
  gzclose:=gzio.gzclose(thefile);
end;

function gzerror(thefile:gzFile; var errnum:smallint):string;inline;
begin
  gzerror:=gzio.gzerror(thefile,errnum);
end;

function adler32(theadler:cardinal;buf : Pchar; len:cardinal):cardinal;inline;
begin
  adler32:=adler.adler32(theadler,Pbyte(buf),len);
end;

function crc32(thecrc:cardinal;buf : Pchar; len:cardinal):cardinal;inline;
begin
  crc32:=crc.crc32(thecrc,Pbyte(buf),len);
end;

function deflateInit_(var strm:TZStream; level:longint; version:Pchar; stream_size:longint):longint;inline;
begin
  deflateInit_:=zdeflate.deflateInit_(@strm,level,version,stream_size);
end;

function inflateInit_(var strm:TZStream; version:Pchar; stream_size:longint):longint;inline;
begin
  inflateInit_:=zinflate.inflateInit_(@strm,version,stream_size);
end;

function deflateInit2_(var strm:TZStream; level:longint; method:longint; windowBits:longint; memLevel:longint;strategy:longint; version:Pchar; stream_size:longint):longint;inline;
begin
  deflateInit2_:=zdeflate.deflateInit2_(strm,level,method,windowBits,memlevel,strategy,version,stream_size);
end;

function inflateInit2_(var strm:TZStream; windowBits:longint; version:Pchar; stream_size:longint):longint;inline;
begin
  inflateInit2_:=zinflate.inflateInit2_(strm,windowBits,version,stream_size);
end;

function deflateInit(var strm:TZStream;level : longint) : longint;inline;
begin
  deflateInit:=zdeflate.deflateInit(strm,level);
end;

function inflateInit(var strm:TZStream) : longint;inline;
begin
  inflateInit:=zinflate.inflateInit(strm);
end;

function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;inline;
begin
  deflateInit2:=zdeflate.deflateInit2(strm,level,method,windowbits,memlevel,strategy);
end;

function inflateInit2(var strm:TZStream; windowBits : longint) : longint;inline;
begin
  inflateInit2:=zinflate.inflateInit2_(strm,windowBits,ZLIB_VERSION,sizeof(TZStream));
end;

function zError(err:longint):string;inline;
begin
  zerror:=zbase.zerror(err);
end;

function inflateSyncPoint(z:PZstream):longint;inline;
begin
  inflateSyncPoint:=zinflate.inflateSyncPoint(z^);
end;

function get_crc_table:pointer;inline;
begin
  get_crc_table:=crc.get_crc_table;
end;

end.
