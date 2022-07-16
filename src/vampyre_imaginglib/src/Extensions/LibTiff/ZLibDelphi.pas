unit ZLibDelphi;

{$IFDEF FPC}
  {$MODE OBJFPC}
{$ELSE}
  {$DEFINE DCC}
{$ENDIF}

interface

uses
  SysUtils;

const

  ZLIB_VERSION = '1.2.1';

  Z_NO_FLUSH = 0;
  Z_FINISH = 4;

  Z_OK = 0;
  Z_STREAM_END = 1;

type

  PRZStream = ^RZStream;

  RZStream = record
    NextIn: PByte;
    AvailIn: Cardinal;
    TotalIn: Cardinal;
    NextOut: PByte;
    AvailOut: Cardinal;
    TotalOut: Cardinal;
    Msg: PAnsiChar;
    State: Pointer;
    AllocFunc: Pointer;
    FreeFunc: Pointer;
    Opaque: Cardinal;
    DataType: Integer;
    Adler: Cardinal;
    Reserved: Cardinal;
  end;

function  inflateInit_(strm: Pointer; version: Pointer; stream_size: Integer): Integer; cdecl; external;
function  inflateReset(strm: Pointer): Integer; cdecl; external;
function  inflate(strm: Pointer; flush: Integer): Integer; cdecl; external;
function  inflateSync(strm: Pointer): Integer; cdecl; external;
function  deflateInit(strm: Pointer; level: Integer): Integer;
function  deflateInit_(strm: Pointer; level: Integer; version: Pointer; stream_size: Integer): Integer; cdecl; external;
function  deflateReset(strm: Pointer): Integer; cdecl; external;
function  deflate(strm: Pointer; flush: Integer): Integer; cdecl; external;
function  deflateEnd(strm: Pointer): Integer; cdecl; external;
function  inflateEnd(strm: Pointer): Integer; cdecl; external;
function  deflateParams(strm: Pointer; level: Integer; strategy: Integer): Integer; cdecl; external;

implementation

uses
  LibDelphi;

function deflateInit(strm: Pointer; level: Integer): Integer;
begin
  Result:=deflateInit_(strm,level,PAnsiChar(ZLIB_VERSION),SizeOf(RZStream));
end;

{$IF Defined(DCC) and Defined(MSWINDOWS) and not Defined(CPUX64)}
  // Windows 32bit Delphi only - OMF object format
  {$L Compiled\inflate.obj}
  {$L Compiled\crc32.obj}
  {$L Compiled\adler32.obj}
  {$L Compiled\inftrees.obj}
  {$L Compiled\inffast.obj}
  {$L Compiled\deflate.obj}
  {$L Compiled\zutil.obj}
  {$L Compiled\trees.obj}
  {$L Compiled\compress.obj}
  {$L Compiled\uncompr.obj}
{$IFEND}

end.








