{ @abstract(Bindings to zlib library.)

  Copied from FPC zlib.pp in packages.
  Changed by Kambi to
  @unorderedList(
    @item link to zlib1.dll under win32 (from [http://gnuwin32.sourceforge.net/])
    @item compile under FPC+Win32 and Delphi
    @item(name changed from zlib to KambiZlib to not collide with FPC and Kylix
      zlib units)
    @item(changed to link using my TDynLib instead of linking with
      "external libz ...")
    @item(just like KambiPng: zlib library must not be actually present
      on target system. If zlib will not be installed, KambiZlibInited
      will be simply set to false.)
  )
}

unit KambiZlib;

{$I kambiconf.inc}

interface

{$ifndef DELPHI}
  { Needed for array of const }
  { Kambi: delphi needed to handle "@" + function variables like in delphi }
  {$mode delphi} 
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
      {$ifdef DARWIN} 'libz.dylib' { TODO--confirm this works under Darwin }
      {$else} 'libz.so.1'
      {$endif}
    {$endif}
    {$ifdef WIN32} 'zlib1.dll' {$endif};

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

  TAllocfunc = function (opaque:pointer; items:uInt; size:uInt):pointer;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  TFreeFunc = procedure (opaque:pointer; address:pointer);{$ifdef WIN32} stdcall {$else} cdecl {$endif};

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
  zlibVersionpchar: function:pchar;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  deflate: function(var strm:TZStream; flush:longint):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  deflateEnd: function(var strm:TZStream):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  inflate: function(var strm:TZStream; flush:longint):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  inflateEnd: function(var strm:TZStream):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  deflateSetDictionary: function(var strm:TZStream;dictionary : pbytef; dictLength:uInt):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  deflateCopy: function(var dest,source:TZstream):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  deflateReset: function(var strm:TZStream):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  deflateParams: function(var strm:TZStream; level:longint; strategy:longint):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  inflateSetDictionary: function(var strm:TZStream;dictionary : pbytef; dictLength:uInt):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  inflateSync: function(var strm:TZStream):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  inflateReset: function(var strm:TZStream):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  compress: function(dest:pbytef;destLen:uLongf; source : pbytef; sourceLen:uLong):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  compress2: function(dest:pbytef;destLen:uLongf; source : pbytef; sourceLen:uLong; level:longint):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  uncompress: function(dest:pbytef;destLen:uLongf; source : pbytef; sourceLen:uLong):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzopen: function(path:pchar; mode:pchar):gzFile;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzdopen: function(fd:longint; mode:pchar):gzFile;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzsetparams: function(thefile:gzFile; level:longint; strategy:longint):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzread: function(thefile:gzFile; buf:pointer; len:cardinal):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzwrite: function(thefile:gzFile; buf:pointer; len:cardinal):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzprintf: function(thefile:gzFile; format:pbytef; args:array of const):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzputs: function(thefile:gzFile; s:pbytef):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzgets: function(thefile:gzFile; buf:pbytef; len:longint):pbytef;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzputc: function(thefile:gzFile; c:char):char;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzgetc: function(thefile:gzFile):char;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzflush: function(thefile:gzFile; flush:longint):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzseek: function(thefile:gzFile; offset:z_off_t; whence:longint):z_off_t;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzrewind: function(thefile:gzFile):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gztell: function(thefile:gzFile):z_off_t;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzeof: function(thefile:gzFile):longbool;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzclose: function(thefile:gzFile):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  gzerror: function(thefile:gzFile; var errnum:longint):pbytef;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  adler32: function(adler:uLong;buf : pbytef; len:uInt):uLong;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  crc32: function(crc:uLong;buf : pbytef; len:uInt):uLong;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  deflateInit_: function(var strm:TZStream; level:longint; version:pchar; stream_size:longint):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  inflateInit_: function(var strm:TZStream; version:pchar; stream_size:longint):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  deflateInit2_: function(var strm:TZStream; level:longint; method:longint; windowBits:longint; memLevel:longint;strategy:longint; version:pchar; stream_size:longint):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  inflateInit2_: function(var strm:TZStream; windowBits:longint; version:pchar; stream_size:longint):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  zErrorpchar: function(err:longint):pchar;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  inflateSyncPoint: function(z:PZstream):longint;{$ifdef WIN32} stdcall {$else} cdecl {$endif};
  get_crc_table: function:pointer;{$ifdef WIN32} stdcall {$else} cdecl {$endif};

{ These are simple comfortable Pascal wrappers above appropriate functions
  exported from zlib. }
{ }
function zlibVersion:string;
function deflateInit(var strm:TZStream;level : longint) : longint;
function inflateInit(var strm:TZStream) : longint;
function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;
function inflateInit2(var strm:TZStream;windowBits : longint) : longint;
function zError(err:longint):string;

{ Just like KambiPngInited: Returns whether zlib library was found
  at initialization. If true that all function pointers in this unit
  are inited and you can simply use zlib. If false then zlib was not
  installed, all function pointers in this unit are nil
  and you can't use anything fro zlib. }
function KambiZlibInited: boolean;

implementation

uses
  {$ifdef FPC} Strings, {$endif} SysUtils, KambiUtils;

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
  FKambiZlibInited: boolean;

function KambiZlibInited: boolean;
begin
 Result := FKambiZlibInited;
end;

initialization
 ZLibrary := TDynLib.Load(ZLibraryName, false);
 FKambiZlibInited := ZLibrary <> nil;

 if FKambiZlibInited then
 begin
  @zlibVersionpchar := ZLibrary.Symbol('zlibVersion');
  @deflate := ZLibrary.Symbol('deflate');
  @deflateEnd := ZLibrary.Symbol('deflateEnd');
  @inflate := ZLibrary.Symbol('inflate');
  @inflateEnd := ZLibrary.Symbol('inflateEnd');
  @deflateSetDictionary := ZLibrary.Symbol('deflateSetDictionary');
  @deflateCopy := ZLibrary.Symbol('deflateCopy');
  @deflateReset := ZLibrary.Symbol('deflateReset');
  @deflateParams := ZLibrary.Symbol('deflateParams');
  @inflateSetDictionary := ZLibrary.Symbol('inflateSetDictionary');
  @inflateSync := ZLibrary.Symbol('inflateSync');
  @inflateReset := ZLibrary.Symbol('inflateReset');
  @compress := ZLibrary.Symbol('compress');
  @compress2 := ZLibrary.Symbol('compress2');
  @uncompress := ZLibrary.Symbol('uncompress');
  @gzopen := ZLibrary.Symbol('gzopen');
  @gzdopen := ZLibrary.Symbol('gzdopen');
  @gzsetparams := ZLibrary.Symbol('gzsetparams');
  @gzread := ZLibrary.Symbol('gzread');
  @gzwrite := ZLibrary.Symbol('gzwrite');
  @gzprintf := ZLibrary.Symbol('gzprintf');
  @gzputs := ZLibrary.Symbol('gzputs');
  @gzgets := ZLibrary.Symbol('gzgets');
  @gzputc := ZLibrary.Symbol('gzputc');
  @gzgetc := ZLibrary.Symbol('gzgetc');
  @gzflush := ZLibrary.Symbol('gzflush');
  @gzseek := ZLibrary.Symbol('gzseek');
  @gzrewind := ZLibrary.Symbol('gzrewind');
  @gztell := ZLibrary.Symbol('gztell');
  @gzeof := ZLibrary.Symbol('gzeof');
  @gzclose := ZLibrary.Symbol('gzclose');
  @gzerror := ZLibrary.Symbol('gzerror');
  @adler32 := ZLibrary.Symbol('adler32');
  @crc32 := ZLibrary.Symbol('crc32');
  @deflateInit_ := ZLibrary.Symbol('deflateInit_');
  @inflateInit_ := ZLibrary.Symbol('inflateInit_');
  @deflateInit2_ := ZLibrary.Symbol('deflateInit2_');
  @inflateInit2_ := ZLibrary.Symbol('inflateInit2_');
  @zErrorpchar := ZLibrary.Symbol('zError');
  @inflateSyncPoint := ZLibrary.Symbol('inflateSyncPoint');
  @get_crc_table := ZLibrary.Symbol('get_crc_table');
 end;
finalization
 FKambiZlibInited := false;
 FreeAndNil(ZLibrary);
end.
