{ @abstract(Bindings to zlib library.)

  Copied from FPC zlib.pp in packages.
  Changed by Kambi to
  @unorderedList(
    @item(link to zlib1.dll under Windows (from [http://gnuwin32.sourceforge.net/]),
      this uses cdecl calling convention)
    @item compile under Delphi too
    @item(name changed from zlib to KambiZlib to not collide with FPC and Kylix
      zlib units)
    @item(changed to link using my TDynLib instead of linking with
      "external libz ...")
    @item(just like KambiPng: zlib library must not be actually present
      on target system. If zlib will not be installed, KambiZlibInited
      will be simply set to false.)
  )
  
  @exclude (This is only a C header translation --- no nice PasDoc docs.)
}

unit KambiZlib;

{$I kambiconf.inc}

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
      {$ifdef DARWIN} 'libz.dylib' { TODO--confirm this works under Darwin }
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

{ Just like KambiPngInited: Returns whether zlib library was found
  at initialization. If true that all function pointers in this unit
  are inited and you can simply use zlib. If false then zlib was not
  installed, all function pointers in this unit are nil
  and you can't use anything fro zlib. }
function KambiZlibInited: boolean;

implementation

uses SysUtils, KambiUtils, KambiDynLib;

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
  (* Note: at first I wrote it like
       {$ifdef FPC_OBJFPC} Pointer {$else} @ {$endif} (xxx)
     but unfortunately stupid Delphi doesn't get @(xxx) construct.
     I must use @xxx construct. *)

  {$ifdef FPC_OBJFPC} Pointer(zlibVersionpchar) {$else} @zlibVersionpchar {$endif} := ZLibrary.Symbol('zlibVersion');
  {$ifdef FPC_OBJFPC} Pointer(deflate) {$else} @deflate {$endif} := ZLibrary.Symbol('deflate');
  {$ifdef FPC_OBJFPC} Pointer(deflateEnd) {$else} @deflateEnd {$endif} := ZLibrary.Symbol('deflateEnd');
  {$ifdef FPC_OBJFPC} Pointer(inflate) {$else} @inflate {$endif} := ZLibrary.Symbol('inflate');
  {$ifdef FPC_OBJFPC} Pointer(inflateEnd) {$else} @inflateEnd {$endif} := ZLibrary.Symbol('inflateEnd');
  {$ifdef FPC_OBJFPC} Pointer(deflateSetDictionary) {$else} @deflateSetDictionary {$endif} := ZLibrary.Symbol('deflateSetDictionary');
  {$ifdef FPC_OBJFPC} Pointer(deflateCopy) {$else} @deflateCopy {$endif} := ZLibrary.Symbol('deflateCopy');
  {$ifdef FPC_OBJFPC} Pointer(deflateReset) {$else} @deflateReset {$endif} := ZLibrary.Symbol('deflateReset');
  {$ifdef FPC_OBJFPC} Pointer(deflateParams) {$else} @deflateParams {$endif} := ZLibrary.Symbol('deflateParams');
  {$ifdef FPC_OBJFPC} Pointer(inflateSetDictionary) {$else} @inflateSetDictionary {$endif} := ZLibrary.Symbol('inflateSetDictionary');
  {$ifdef FPC_OBJFPC} Pointer(inflateSync) {$else} @inflateSync {$endif} := ZLibrary.Symbol('inflateSync');
  {$ifdef FPC_OBJFPC} Pointer(inflateReset) {$else} @inflateReset {$endif} := ZLibrary.Symbol('inflateReset');
  {$ifdef FPC_OBJFPC} Pointer(compress) {$else} @compress {$endif} := ZLibrary.Symbol('compress');
  {$ifdef FPC_OBJFPC} Pointer(compress2) {$else} @compress2 {$endif} := ZLibrary.Symbol('compress2');
  {$ifdef FPC_OBJFPC} Pointer(uncompress) {$else} @uncompress {$endif} := ZLibrary.Symbol('uncompress');
  {$ifdef FPC_OBJFPC} Pointer(gzopen) {$else} @gzopen {$endif} := ZLibrary.Symbol('gzopen');
  {$ifdef FPC_OBJFPC} Pointer(gzdopen) {$else} @gzdopen {$endif} := ZLibrary.Symbol('gzdopen');
  {$ifdef FPC_OBJFPC} Pointer(gzsetparams) {$else} @gzsetparams {$endif} := ZLibrary.Symbol('gzsetparams');
  {$ifdef FPC_OBJFPC} Pointer(gzread) {$else} @gzread {$endif} := ZLibrary.Symbol('gzread');
  {$ifdef FPC_OBJFPC} Pointer(gzwrite) {$else} @gzwrite {$endif} := ZLibrary.Symbol('gzwrite');
//  {$ifdef FPC_OBJFPC} Pointer(gzprintf) {$else} @gzprintf {$endif} := ZLibrary.Symbol('gzprintf');
  {$ifdef FPC_OBJFPC} Pointer(gzputs) {$else} @gzputs {$endif} := ZLibrary.Symbol('gzputs');
  {$ifdef FPC_OBJFPC} Pointer(gzgets) {$else} @gzgets {$endif} := ZLibrary.Symbol('gzgets');
  {$ifdef FPC_OBJFPC} Pointer(gzputc) {$else} @gzputc {$endif} := ZLibrary.Symbol('gzputc');
  {$ifdef FPC_OBJFPC} Pointer(gzgetc) {$else} @gzgetc {$endif} := ZLibrary.Symbol('gzgetc');
  {$ifdef FPC_OBJFPC} Pointer(gzflush) {$else} @gzflush {$endif} := ZLibrary.Symbol('gzflush');
  {$ifdef FPC_OBJFPC} Pointer(gzseek) {$else} @gzseek {$endif} := ZLibrary.Symbol('gzseek');
  {$ifdef FPC_OBJFPC} Pointer(gzrewind) {$else} @gzrewind {$endif} := ZLibrary.Symbol('gzrewind');
  {$ifdef FPC_OBJFPC} Pointer(gztell) {$else} @gztell {$endif} := ZLibrary.Symbol('gztell');
  {$ifdef FPC_OBJFPC} Pointer(gzeof) {$else} @gzeof {$endif} := ZLibrary.Symbol('gzeof');
  {$ifdef FPC_OBJFPC} Pointer(gzclose) {$else} @gzclose {$endif} := ZLibrary.Symbol('gzclose');
  {$ifdef FPC_OBJFPC} Pointer(gzerror) {$else} @gzerror {$endif} := ZLibrary.Symbol('gzerror');
  {$ifdef FPC_OBJFPC} Pointer(adler32) {$else} @adler32 {$endif} := ZLibrary.Symbol('adler32');
  {$ifdef FPC_OBJFPC} Pointer(crc32) {$else} @crc32 {$endif} := ZLibrary.Symbol('crc32');
  {$ifdef FPC_OBJFPC} Pointer(deflateInit_) {$else} @deflateInit_ {$endif} := ZLibrary.Symbol('deflateInit_');
  {$ifdef FPC_OBJFPC} Pointer(inflateInit_) {$else} @inflateInit_ {$endif} := ZLibrary.Symbol('inflateInit_');
  {$ifdef FPC_OBJFPC} Pointer(deflateInit2_) {$else} @deflateInit2_ {$endif} := ZLibrary.Symbol('deflateInit2_');
  {$ifdef FPC_OBJFPC} Pointer(inflateInit2_) {$else} @inflateInit2_ {$endif} := ZLibrary.Symbol('inflateInit2_');
  {$ifdef FPC_OBJFPC} Pointer(zErrorpchar) {$else} @zErrorpchar {$endif} := ZLibrary.Symbol('zError');
  {$ifdef FPC_OBJFPC} Pointer(inflateSyncPoint) {$else} @inflateSyncPoint {$endif} := ZLibrary.Symbol('inflateSyncPoint');
  {$ifdef FPC_OBJFPC} Pointer(get_crc_table) {$else} @get_crc_table {$endif} := ZLibrary.Symbol('get_crc_table');
 end;
finalization
 FKambiZlibInited := false;
 FreeAndNil(ZLibrary);
end.
