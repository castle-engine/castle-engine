{ @abstract(Bindings to zlib library.)

  Copied from FPC zlib.pp in packages.
  Changed by Kambi to
  @unorderedList(
    @item(link to zlib1.dll under Windows (from [http://gnuwin32.sourceforge.net/]),
      this uses cdecl calling convention)
    @item(name changed from zlib to CastleZLib to not collide with FPC and Kylix
      zlib units)
    @item(changed to link using my TDynLib instead of linking with
      "external libz ...")
    @item(just like CastlePng: zlib library must not be actually present
      on target system. If zlib will not be installed, CastleZLibInited
      will be simply set to false.)
  )

  @exclude (This is only a C header translation --- no nice PasDoc docs.)
}

unit CastleZLib;

{$I castleconf.inc}

{$ifdef CASTLE_ZLIB_USING_PASZLIB}
interface
{ CastleZLib unit doesn't expose anything when CASTLE_ZLIB_USING_PASZLIB
  is defined. Instead, use PasZLib, which implement Zlib API in pure Pascal. }
implementation
end.
{$else}

interface

{$ifndef DELPHI}
  { for linux for linking with libc }
  {$ifdef unix}
    {$linklib c}
  {$endif}
  {$PACKRECORDS 4}
{$endif}

const
  ZLIB_VERSION = '1.1.3';

  ZLibraryName =
    {$ifdef UNIX}
      {$ifdef DARWIN} 'libz.dylib'
      {$else} 'libz.so.1'
      {$endif}
    {$endif}
    {$ifdef MSWINDOWS} 'zlib1.dll' {$endif};

type
  { Compatible with paszlib }
  { }
  Uint    = Longint;
  Ulong   = Longint;
  Ulongf  = Longint;
  Pulongf = ^Ulongf;
  z_off_t = longint;
  pbyte   = ^byte;
  pbytef  = ^byte;

  TAllocfunc = function (opaque:pointer; items:uInt; size:uInt):pointer;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  TFreeFunc = procedure (opaque:pointer; address:pointer);{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};

  TInternalState = record
    end;
  PInternalState = ^TInternalstate;

  TZStream = record
    next_in : pbytef;
    avail_in : uInt;
    total_in : uLong;
    next_out : pbytef;
    avail_out : uInt;
    total_out : uLong;
    msg : pbytef;
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
  zlibVersionpchar: function:pchar;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflate: function(var strm:TZStream; flush:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateEnd: function(var strm:TZStream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflate: function(var strm:TZStream; flush:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateEnd: function(var strm:TZStream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateSetDictionary: function(var strm:TZStream;dictionary : pbytef; dictLength:uInt):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateCopy: function(var dest,source:TZstream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateReset: function(var strm:TZStream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateParams: function(var strm:TZStream; level:longint; strategy:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateSetDictionary: function(var strm:TZStream;dictionary : pbytef; dictLength:uInt):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateSync: function(var strm:TZStream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateReset: function(var strm:TZStream):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  compress: function(dest:pbytef;destLen:uLongf; source : pbytef; sourceLen:uLong):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  compress2: function(dest:pbytef;destLen:uLongf; source : pbytef; sourceLen:uLong; level:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  uncompress: function(dest:pbytef;destLen:uLongf; source : pbytef; sourceLen:uLong):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzopen: function(path:pchar; mode:pchar):gzFile;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzdopen: function(fd:longint; mode:pchar):gzFile;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
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
  //gzprintf: function(thefile:gzFile; format:pbytef; args:array of const):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzputs: function(thefile:gzFile; s:pbytef):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzgets: function(thefile:gzFile; buf:pbytef; len:longint):pbytef;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzputc: function(thefile:gzFile; c:char):char;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzgetc: function(thefile:gzFile):char;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzflush: function(thefile:gzFile; flush:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzseek: function(thefile:gzFile; offset:z_off_t; whence:longint):z_off_t;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzrewind: function(thefile:gzFile):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gztell: function(thefile:gzFile):z_off_t;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzeof: function(thefile:gzFile):longbool;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzclose: function(thefile:gzFile):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  gzerror: function(thefile:gzFile; var errnum:longint):pbytef;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  adler32: function(adler:uLong;buf : pbytef; len:uInt):uLong;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  crc32: function(crc:uLong;buf : pbytef; len:uInt):uLong;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateInit_: function(var strm:TZStream; level:longint; version:pchar; stream_size:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateInit_: function(var strm:TZStream; version:pchar; stream_size:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  deflateInit2_: function(var strm:TZStream; level:longint; method:longint; windowBits:longint; memLevel:longint;strategy:longint; version:pchar; stream_size:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  inflateInit2_: function(var strm:TZStream; windowBits:longint; version:pchar; stream_size:longint):longint;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
  zErrorpchar: function(err:longint):pchar;{$ifdef ZLIB_STDCALL} stdcall {$else} cdecl {$endif};
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
  are inited and you can simply use zlib. If false then zlib was not
  installed, all function pointers in this unit are nil
  and you can't use anything fro zlib. }
function CastleZLibInited: boolean;

procedure ZLibInitialization;

implementation

uses SysUtils, CastleDynLib;

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

function CastleZLibInited: boolean;
begin
 Result := ZLibrary <> nil;
end;

procedure ZLibInitialization;
begin
  ZLibrary := TDynLib.Load(ZLibraryName, false);
  if ZLibrary <> nil then
  begin
    Pointer(zlibVersionpchar) := ZLibrary.Symbol('zlibVersion');
    Pointer(deflate) := ZLibrary.Symbol('deflate');
    Pointer(deflateEnd) := ZLibrary.Symbol('deflateEnd');
    Pointer(inflate) := ZLibrary.Symbol('inflate');
    Pointer(inflateEnd) := ZLibrary.Symbol('inflateEnd');
    Pointer(deflateSetDictionary) := ZLibrary.Symbol('deflateSetDictionary');
    Pointer(deflateCopy) := ZLibrary.Symbol('deflateCopy');
    Pointer(deflateReset) := ZLibrary.Symbol('deflateReset');
    Pointer(deflateParams) := ZLibrary.Symbol('deflateParams');
    Pointer(inflateSetDictionary) := ZLibrary.Symbol('inflateSetDictionary');
    Pointer(inflateSync) := ZLibrary.Symbol('inflateSync');
    Pointer(inflateReset) := ZLibrary.Symbol('inflateReset');
    Pointer(compress) := ZLibrary.Symbol('compress');
    Pointer(compress2) := ZLibrary.Symbol('compress2');
    Pointer(uncompress) := ZLibrary.Symbol('uncompress');
    Pointer(gzopen) := ZLibrary.Symbol('gzopen');
    Pointer(gzdopen) := ZLibrary.Symbol('gzdopen');
    Pointer(gzsetparams) := ZLibrary.Symbol('gzsetparams');
    Pointer(gzread) := ZLibrary.Symbol('gzread');
    Pointer(gzwrite) := ZLibrary.Symbol('gzwrite');
  //  Pointer(gzprintf) := ZLibrary.Symbol('gzprintf');
    Pointer(gzputs) := ZLibrary.Symbol('gzputs');
    Pointer(gzgets) := ZLibrary.Symbol('gzgets');
    Pointer(gzputc) := ZLibrary.Symbol('gzputc');
    Pointer(gzgetc) := ZLibrary.Symbol('gzgetc');
    Pointer(gzflush) := ZLibrary.Symbol('gzflush');
    Pointer(gzseek) := ZLibrary.Symbol('gzseek');
    Pointer(gzrewind) := ZLibrary.Symbol('gzrewind');
    Pointer(gztell) := ZLibrary.Symbol('gztell');
    Pointer(gzeof) := ZLibrary.Symbol('gzeof');
    Pointer(gzclose) := ZLibrary.Symbol('gzclose');
    Pointer(gzerror) := ZLibrary.Symbol('gzerror');
    Pointer(adler32) := ZLibrary.Symbol('adler32');
    Pointer(crc32) := ZLibrary.Symbol('crc32');
    Pointer(deflateInit_) := ZLibrary.Symbol('deflateInit_');
    Pointer(inflateInit_) := ZLibrary.Symbol('inflateInit_');
    Pointer(deflateInit2_) := ZLibrary.Symbol('deflateInit2_');
    Pointer(inflateInit2_) := ZLibrary.Symbol('inflateInit2_');
    Pointer(zErrorpchar) := ZLibrary.Symbol('zError');
    Pointer(inflateSyncPoint) := ZLibrary.Symbol('inflateSyncPoint');
    Pointer(get_crc_table) := ZLibrary.Symbol('get_crc_table');
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