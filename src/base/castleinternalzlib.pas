{ Zlib bindings, for Castle Game Engine.

  Based on FPC zlib.pp unit.
  Adjusted to:

  @unorderedList(
    @item(link to zlib1.dll under Windows (from [http://gnuwin32.sourceforge.net/]),
      this uses cdecl calling convention)
    @item(name changed from zlib to CastleInternalZLib to not collide with FPC and Kylix
      zlib units)
    @item(changed to link using CGE TDynLib instead of linking with
      "external libz ...")
    @item(just like CastleInternalPng: zlib library does not need to be
      actually present on the user system.
      If zlib is not be installed, CastleZLibInitialized will be simply set to @false.)
    @item(honor ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION)
    @item(For both FPC and Delphi (uses PAnsiChar for new unicode Delphis).)

  )

  @exclude (This is only a C header translation --- no nice PasDoc docs.)
}

unit CastleInternalZLib;

{$I castleconf.inc}

{$ifdef CASTLE_ZLIB_USING_PASZLIB}
interface
{ CastleInternalZLib unit doesn't expose anything when CASTLE_ZLIB_USING_PASZLIB
  is defined. Instead, use PasZLib, which implements Zlib API in pure Pascal. }
implementation
end.
{$else}

interface

{$ifdef FPC}
  { for linux for linking with libc }
  {$ifdef unix}
    {$linklib c}
  {$endif}
  {$PACKRECORDS 4}
{$else}
  {$ALIGN 4}
{$endif}

const
  ZLIB_VERSION = '1.1.3';

type
  { Compatible with paszlib }
  { }
  Uint    = Longint;
  Ulong   = Longint;
  Ulongf  = Longint;
  Pulongf = ^Ulongf;
  z_off_t = longint;

  TAllocfunc = function (opaque:pointer; items:uInt; size:uInt):pointer;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  TFreeFunc = procedure (opaque:pointer; address:pointer);{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};

  TInternalState = record
    end;
  PInternalState = ^TInternalstate;

  TZStream = record
    next_in : PByte;
    avail_in : uInt;
    total_in : uLong;
    next_out : PByte;
    avail_out : uInt;
    total_out : uLong;
    msg : PByte;
    state : PInternalState;
    zalloc : TAllocFunc;
    zfree : TFreeFunc;
    opaque : pointer;
    data_type : longint;
    adler : uLong;
    reserved : uLong;
  end;
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

  Z_NULL = 0;

var
  zlibVersionpchar: function:PAnsiChar;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflate: function(var strm:TZStream; flush:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateEnd: function(var strm:TZStream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflate: function(var strm:TZStream; flush:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateEnd: function(var strm:TZStream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateSetDictionary: function(var strm:TZStream;dictionary : PByte; dictLength:uInt):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateCopy: function(var dest,source:TZstream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateReset: function(var strm:TZStream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateParams: function(var strm:TZStream; level:longint; strategy:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateSetDictionary: function(var strm:TZStream;dictionary : PByte; dictLength:uInt):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateSync: function(var strm:TZStream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateReset: function(var strm:TZStream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  compress: function(dest:PByte;destLen:uLongf; source : PByte; sourceLen:uLong):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  compress2: function(dest:PByte;destLen:uLongf; source : PByte; sourceLen:uLong; level:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  uncompress: function(dest:PByte;destLen:uLongf; source : PByte; sourceLen:uLong):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzopen: function(path:PAnsiChar; mode:PAnsiChar):gzFile;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzdopen: function(fd:longint; mode:PAnsiChar):gzFile;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzsetparams: function(thefile:gzFile; level:longint; strategy:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzread: function(thefile:gzFile; buf:pointer; len:cardinal):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzwrite: function(thefile:gzFile; buf:pointer; len:cardinal):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  (* gzprintf commented out,
     FPC 2.2.0 warning "Warning: cdecl'ared functions have no high parameter"
     and on mailing list Jonas suggests that C varargs will never be reliably
     handled anyway.
     http://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg09040.html
     (although the point of thread is about something else completely,
     and even the question was about "array of PChar" which is not varargs...) *)
  //gzprintf: function(thefile:gzFile; format:PByte; args:array of const):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzputs: function(thefile:gzFile; s:PByte):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzgets: function(thefile:gzFile; buf:PByte; len:longint):PByte;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzputc: function(thefile:gzFile; c:char):char;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzgetc: function(thefile:gzFile):char;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzflush: function(thefile:gzFile; flush:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzseek: function(thefile:gzFile; offset:z_off_t; whence:longint):z_off_t;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzrewind: function(thefile:gzFile):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gztell: function(thefile:gzFile):z_off_t;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzeof: function(thefile:gzFile):longbool;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzclose: function(thefile:gzFile):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzerror: function(thefile:gzFile; var errnum:longint):PByte;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  adler32: function(adler:uLong;buf : PByte; len:uInt):uLong;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  crc32: function(crc:uLong;buf : PByte; len:uInt):uLong;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateInit_: function(var strm:TZStream; level:longint; version:PAnsiChar; stream_size:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateInit_: function(var strm:TZStream; version:PAnsiChar; stream_size:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateInit2_: function(var strm:TZStream; level:longint; method:longint; windowBits:longint; memLevel:longint;strategy:longint; version:PAnsiChar; stream_size:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateInit2_: function(var strm:TZStream; windowBits:longint; version:PAnsiChar; stream_size:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  zErrorpchar: function(err:longint):PAnsiChar;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateSyncPoint: function(z:PZstream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  get_crc_table: function:pointer;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};

{ These are simple comfortable Pascal wrappers above appropriate functions
  exported from zlib. }
{ }
function zlibVersion:string;
function deflateInit(var strm:TZStream;level : longint) : longint;
function inflateInit(var strm:TZStream) : longint;
function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;
function inflateInit2(var strm:TZStream;windowBits : longint) : longint;
function zError(err:longint):string;

{ Was zlib library found. If true that all function pointers in this unit
  are initialized and you can simply use zlib. If false then zlib was not
  installed, all function pointers in this unit are nil
  and you can't use anything fro zlib. }
function CastleZLibInitialized: boolean;

procedure ZLibInitialization;

implementation

uses SysUtils, CastleDynLib, CastleUtils, CastleLog;

function zlibversion : string;
  begin
     zlibversion:=strpas(zlibversionpchar());
  end;

function deflateInit(var strm:TZStream;level : longint) : longint;
  begin
     deflateInit:=deflateInit_(strm,level,ZLIB_VERSION,sizeof(TZStream));
  end;

function inflateInit(var strm:TZStream) : longint;
  begin
     inflateInit:=inflateInit_(strm,ZLIB_VERSION,sizeof(TZStream));
  end;

function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;
  begin
     deflateInit2:=deflateInit2_(strm,level,method,windowBits,memLevel,strategy,ZLIB_VERSION,sizeof(TZStream));
  end;

function inflateInit2(var strm:TZStream;windowBits : longint) : longint;
  begin
     inflateInit2:=inflateInit2_(strm,windowBits,ZLIB_VERSION,sizeof(TZStream));
  end;

function zError(err:longint):string;
  begin
     zerror:=Strpas(zErrorpchar(err));
  end;

var
  ZLibrary: TDynLib;

function CastleZLibInitialized: boolean;
begin
 Result := ZLibrary <> nil;
end;

procedure ZLibInitialization;
const
  ZLibraryName =
    {$ifdef UNIX}
      {$ifdef DARWIN} 'libz.dylib'
      {$else} 'libz.so.1'
      {$endif}
    {$endif}
    {$ifdef MSWINDOWS} 'zlib1.dll' {$endif};
begin
  ZLibrary := TDynLib.Load(ZLibraryName, false);

  {$ifdef FREEBSD}
  if ZLibrary = nil then
    ZLibrary := TDynLib.Load('libz.so.6', false);
  {$endif}

  if ZLibrary <> nil then
  begin
    Pointer({$ifndef FPC}@{$endif} zlibVersionpchar) := ZLibrary.Symbol('zlibVersion');
    Pointer({$ifndef FPC}@{$endif} deflate) := ZLibrary.Symbol('deflate');
    Pointer({$ifndef FPC}@{$endif} deflateEnd) := ZLibrary.Symbol('deflateEnd');
    Pointer({$ifndef FPC}@{$endif} inflate) := ZLibrary.Symbol('inflate');
    Pointer({$ifndef FPC}@{$endif} inflateEnd) := ZLibrary.Symbol('inflateEnd');
    Pointer({$ifndef FPC}@{$endif} deflateSetDictionary) := ZLibrary.Symbol('deflateSetDictionary');
    Pointer({$ifndef FPC}@{$endif} deflateCopy) := ZLibrary.Symbol('deflateCopy');
    Pointer({$ifndef FPC}@{$endif} deflateReset) := ZLibrary.Symbol('deflateReset');
    Pointer({$ifndef FPC}@{$endif} deflateParams) := ZLibrary.Symbol('deflateParams');
    Pointer({$ifndef FPC}@{$endif} inflateSetDictionary) := ZLibrary.Symbol('inflateSetDictionary');
    Pointer({$ifndef FPC}@{$endif} inflateSync) := ZLibrary.Symbol('inflateSync');
    Pointer({$ifndef FPC}@{$endif} inflateReset) := ZLibrary.Symbol('inflateReset');
    Pointer({$ifndef FPC}@{$endif} compress) := ZLibrary.Symbol('compress');
    Pointer({$ifndef FPC}@{$endif} compress2) := ZLibrary.Symbol('compress2');
    Pointer({$ifndef FPC}@{$endif} uncompress) := ZLibrary.Symbol('uncompress');
    Pointer({$ifndef FPC}@{$endif} gzopen) := ZLibrary.Symbol('gzopen');
    Pointer({$ifndef FPC}@{$endif} gzdopen) := ZLibrary.Symbol('gzdopen');
    Pointer({$ifndef FPC}@{$endif} gzsetparams) := ZLibrary.Symbol('gzsetparams');
    Pointer({$ifndef FPC}@{$endif} gzread) := ZLibrary.Symbol('gzread');
    Pointer({$ifndef FPC}@{$endif} gzwrite) := ZLibrary.Symbol('gzwrite');
  //  Pointer({$ifndef FPC}@{$endif} gzprintf) := ZLibrary.Symbol('gzprintf');
    Pointer({$ifndef FPC}@{$endif} gzputs) := ZLibrary.Symbol('gzputs');
    Pointer({$ifndef FPC}@{$endif} gzgets) := ZLibrary.Symbol('gzgets');
    Pointer({$ifndef FPC}@{$endif} gzputc) := ZLibrary.Symbol('gzputc');
    Pointer({$ifndef FPC}@{$endif} gzgetc) := ZLibrary.Symbol('gzgetc');
    Pointer({$ifndef FPC}@{$endif} gzflush) := ZLibrary.Symbol('gzflush');
    Pointer({$ifndef FPC}@{$endif} gzseek) := ZLibrary.Symbol('gzseek');
    Pointer({$ifndef FPC}@{$endif} gzrewind) := ZLibrary.Symbol('gzrewind');
    Pointer({$ifndef FPC}@{$endif} gztell) := ZLibrary.Symbol('gztell');
    Pointer({$ifndef FPC}@{$endif} gzeof) := ZLibrary.Symbol('gzeof');
    Pointer({$ifndef FPC}@{$endif} gzclose) := ZLibrary.Symbol('gzclose');
    Pointer({$ifndef FPC}@{$endif} gzerror) := ZLibrary.Symbol('gzerror');
    Pointer({$ifndef FPC}@{$endif} adler32) := ZLibrary.Symbol('adler32');
    Pointer({$ifndef FPC}@{$endif} crc32) := ZLibrary.Symbol('crc32');
    Pointer({$ifndef FPC}@{$endif} deflateInit_) := ZLibrary.Symbol('deflateInit_');
    Pointer({$ifndef FPC}@{$endif} inflateInit_) := ZLibrary.Symbol('inflateInit_');
    Pointer({$ifndef FPC}@{$endif} deflateInit2_) := ZLibrary.Symbol('deflateInit2_');
    Pointer({$ifndef FPC}@{$endif} inflateInit2_) := ZLibrary.Symbol('inflateInit2_');
    Pointer({$ifndef FPC}@{$endif} zErrorpchar) := ZLibrary.Symbol('zError');
    Pointer({$ifndef FPC}@{$endif} inflateSyncPoint) := ZLibrary.Symbol('inflateSyncPoint');
    Pointer({$ifndef FPC}@{$endif} get_crc_table) := ZLibrary.Symbol('get_crc_table');

    WritelnLog('ZLib detected (version %s).', [zlibVersionpchar()]);
  end;
end;

initialization
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  ZLibInitialization;
  {$endif}
finalization
  FreeAndNil(ZLibrary);
end.

{$endif CASTLE_ZLIB_USING_PASZLIB}
